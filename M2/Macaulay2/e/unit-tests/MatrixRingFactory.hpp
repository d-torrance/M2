#ifndef M2_UNIT_TESTS_MATRIX_RING_FACTORY_HPP_
#define M2_UNIT_TESTS_MATRIX_RING_FACTORY_HPP_

// Coefficient rings for the DMat and SMat typed suites, built once and shared
// by both.  gtest builds a fresh fixture per TYPED_TEST and some of these
// constructors are expensive.
//
// Must not go in an anonymous namespace: the caches are statics in a class
// template, which the linker merges only with external linkage.
//
// Safe to share because neither suite calls random() or getGenerator(), the
// only accessors that write these rings' mutable state.

#include <memory>
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
#include "unit-tests/util-polyring-creation.hpp"

template <typename RT>
struct MatrixRingFactory
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
        static const auto* quotient =
            dynamic_cast<const PolynomialRing*>(simpleQuotientRing(
                simplePolynomialRing(37, {"x"}), {"x^2-8*x+18"}));
        return std::make_unique<RT>(*quotient, quotient->var(0));
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

#endif
