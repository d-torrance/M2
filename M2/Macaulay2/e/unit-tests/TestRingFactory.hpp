#ifndef M2_UNIT_TESTS_TEST_RING_FACTORY_HPP_
#define M2_UNIT_TESTS_TEST_RING_FACTORY_HPP_

// Coefficient rings for the tests that need one built for them.
//
// TestRingFactory gives each ring type a single default parameterisation,
// built once and shared: gtest builds a fresh fixture per TYPED_TEST, and
// some of these constructors are expensive.  ARingFactory below parameterises
// the ZZ/p backends by modulus, and zzpModuli is the list of moduli worth
// covering; both were local to ARingZZpTest.cpp until the matrix suites
// needed the same sweep.
//
// Must not go in an anonymous namespace: the caches are statics in a class
// template, which the linker merges only with external linkage.
//
// Safe to share because neither suite calls random() or getGenerator(), the
// only accessors that write these rings' mutable state.

#include <memory>
#include <string>
#include <utility>
#include <type_traits>

#include "basic-rings/aring-ZZp.hpp"
#include "basic-rings/aring-ZZp-ffpack.hpp"
#include "basic-rings/aring-ZZp-flint.hpp"
#include "basic-rings/aring-ZZ-gmp.hpp"
#include "basic-rings/aring-ZZ-flint.hpp"
#include "basic-rings/aring-QQ-gmp.hpp"
#include "basic-rings/aring-QQ-flint.hpp"
#include "basic-rings/aring-RR.hpp"
#include "basic-rings/aring-RRR.hpp"
#include "basic-rings/aring-CC.hpp"
#include "basic-rings/aring-CCC.hpp"
#include "basic-rings/aring-RRi.hpp"
#include "basic-rings/aring-CCi.hpp"
#include "basic-rings/aring-GF-flint.hpp"
#include "basic-rings/aring-GF-flint-big.hpp"
#include "basic-rings/aring-m2-GF.hpp"
#include "coeffrings.hpp"
#include "rings/polyring.hpp"
#include "basic-rings/aring-glue.hpp"
#include "unit-tests/util-polyring-creation.hpp"

// Build GF(p^k) as a quotient of a polynomial ring.  FLINT's Zech
// representation requires a primitive defining polynomial.
template <typename RT>
std::unique_ptr<RT> makeGaloisField(int characteristic,
                                    const std::string& modulus)
{
  const auto* polynomialRing = simplePolynomialRing(characteristic, {"a"});
  if (polynomialRing == nullptr) return nullptr;
  const auto* quotient = simpleQuotientRing(polynomialRing, {modulus});
  if (quotient == nullptr) return nullptr;
  const auto* original = quotient->cast_to_PolynomialRing();
  if (original == nullptr) return nullptr;
  return std::make_unique<RT>(*original, original->var(0));
}

// ZZ/p[x]/(x^2): a coefficient ring with zero divisors, for the paths a
// field-only suite cannot reach.  Returns the quotient too, since callers
// need its variable as the nilpotent element.
inline CoefficientRingR makeNilpotentCoefficientRing(int characteristic,
                                                     const Ring*& quotient)
{
  quotient = simpleQuotientRing(simplePolynomialRing(characteristic, {"x"}),
                                {"x^2"});
  return CoefficientRingR(quotient);
}

template <typename RT>
struct TestRingFactory
{
  static std::unique_ptr<RT> make()
  {
    if constexpr (std::is_same_v<RT, M2::ARingZZp> ||
                  std::is_same_v<RT, M2::ARingZZpFFPACK> ||
                  std::is_same_v<RT, M2::ARingZZpFlint>)
      return std::make_unique<RT>(101);
    else if constexpr (std::is_same_v<RT, M2::ARingRRR> ||
                       std::is_same_v<RT, M2::ARingCCC> ||
                       std::is_same_v<RT, M2::ARingRRi> ||
                       std::is_same_v<RT, M2::ARingCCi>)
      return std::make_unique<RT>(100);
    else if constexpr (std::is_same_v<RT, CoefficientRingR>)
      return std::make_unique<RT>(globalQQ);
    else if constexpr (std::is_same_v<RT, M2::ARingGFFlint> ||
                       std::is_same_v<RT, M2::ARingGFFlintBig> ||
                       std::is_same_v<RT, M2::ARingGFM2>)
      {
        // 37 is about the smallest prime keeping the suites' worked values
        // (up to 34) distinct and nonzero; degree two keeps the table small.
        return makeGaloisField<RT>(37, "a^2-8*a+18");
      }
    else
      return std::make_unique<RT>();
  }

  static RT& shared() { return cached<0>(); }

  // A second, distinct ring, for the tests that check a swap moves the ring.
  static RT& alternate() { return cached<1>(); }

 private:
  // Leaked: these hold GC-allocated rings, so destroying them at exit would
  // touch memory the collector has already reclaimed.
  template <int Which>
  static RT& cached()
  {
    static RT* ring = make().release();
    return *ring;
  }
};

template <typename RT>
struct ARingFactory;

template <>
struct ARingFactory<M2::ARingZZp>
{
  static const char* name() { return "ARingZZp"; }
  // two newarray_atomic(int, p) tables and an O(p^2) primitive-root search
  static const char* limit() { return "table size; p <= 32749"; }
  static bool supports(unsigned long p) { return p <= 32749; }
  static std::unique_ptr<M2::ARingZZp> make(unsigned long p)
  {
    return std::unique_ptr<M2::ARingZZp>(new M2::ARingZZp(p));
  }
};

template <>
struct ARingFactory<M2::ARingZZpFFPACK>
{
  static const char* name() { return "ARingZZpFFPACK"; }
  // Givaro::Modular<double> needs p(p-1) exact in a 53-bit mantissa.  This is
  // the backend limit, not the smaller value getMaxModulus() advertises.
  static const char* limit() { return "double mantissa; p <= 94906266"; }
  static bool supports(unsigned long p)
  {
    return p <= static_cast<unsigned long>(
                    M2::ARingZZpFFPACK::FieldType::maxCardinality());
  }
  static std::unique_ptr<M2::ARingZZpFFPACK> make(unsigned long p)
  {
    return std::unique_ptr<M2::ARingZZpFFPACK>(
        new M2::ARingZZpFFPACK(static_cast<M2::ARingZZpFFPACK::UTT>(p)));
  }
};

template <>
struct ARingFactory<M2::ARingZZpFlint>
{
  static const char* name() { return "ARingZZpFlint"; }
  // flint nmod takes the whole unsigned 64-bit range.  Note that the generic
  // Ring interface holds the characteristic in a signed long, so above 2^63 a
  // ring reports a negative characteristic; the ARing class itself is fine.
  static const char* limit() { return "none below 2^64"; }
  static bool supports(unsigned long) { return true; }
  static std::unique_ptr<M2::ARingZZpFlint> make(unsigned long p)
  {
    return std::unique_ptr<M2::ARingZZpFlint>(new M2::ARingZZpFlint(p));
  }
};

struct ModulusCase
{
  unsigned long p;
  const char* why;
};

inline const ModulusCase zzpModuli[] = {
    {2UL, "smallest prime; char 2 was long suspected of failing for ffpack"},
    {3UL, "smallest odd prime"},
    {101UL, "small generic"},
    {32749UL, "largest prime ARingZZp accepts"},
    {32771UL, "first prime above the ffpack getMaxModulus() stub"},
    {33500479UL, "historical ffpack/flint case"},
    {66000007UL, "historical ffpack/flint case"},
    {67108859UL, "historical ffpack/flint case"},
    {94906249UL, "largest prime at or below Givaro's real ceiling"},
    {2147483647UL, "largest prime < 2^31"},
    {9223372036854775783UL, "largest prime < 2^63"},
    {18446744073709551557UL, "largest prime < 2^64"},
};

// Run 'check' over each parameterisation of RT worth covering.  The ZZ/p
// backends sweep zzpModuli, which is the only way the matrix suites reach
// characteristic 2 -- where negation is the identity, so a skew form's zero
// diagonal cannot be derived and must be checked.  The GF classes get a
// second field in characteristic 2 for the same reason.  Every other ring
// type has one parameterisation.
template <typename RT, typename Check>
void overParameterisations(Check check)
{
  if constexpr (std::is_same_v<RT, M2::ARingZZp> ||
                std::is_same_v<RT, M2::ARingZZpFFPACK> ||
                std::is_same_v<RT, M2::ARingZZpFlint>)
    {
      for (const ModulusCase& m : zzpModuli)
        {
          if (sizeof(unsigned long) <= 4 && m.p > 0xffffffffUL) continue;
          if (!ARingFactory<RT>::supports(m.p)) continue;
          SCOPED_TRACE(std::string(ARingFactory<RT>::name()) + " p=" +
                       std::to_string(m.p) + " (" + m.why + ")");
          check(*ARingFactory<RT>::make(m.p));
        }
    }
  else if constexpr (std::is_same_v<RT, M2::ARingGFFlint> ||
                     std::is_same_v<RT, M2::ARingGFFlintBig> ||
                     std::is_same_v<RT, M2::ARingGFM2>)
    {
      // a^2+a+1 is primitive over GF(2); a^2-8a+18 over GF(37).
      for (const auto& field : {std::pair<int, const char*> {2, "a^2+a+1"},
                                {37, "a^2-8*a+18"}})
        {
          SCOPED_TRACE(::testing::Message()
                       << "GF(" << field.first << "^2)");
          check(*makeGaloisField<RT>(field.first, field.second));
        }
    }
  else
    check(TestRingFactory<RT>::shared());
}

#endif
