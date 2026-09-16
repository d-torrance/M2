#include "basic-mutable-matrices/dmat.hpp"

#include <gtest/gtest.h>

#include <cstddef>
#include <type_traits>
#include <initializer_list>
#include <vector>

#include "error.h"
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
#include "basic-rings/aring-glue.hpp"
#include "basic-mutable-matrices/mat-arith.hpp"
#include "basic-mutable-matrices/mat-linalg.hpp"
#include "unit-tests/DMatTest.hpp"
#include "unit-tests/TestRingFactory.hpp"
#include "util.hpp"

namespace {

M2_arrayint indices(std::initializer_list<int> values)
{ return stdvector_to_M2_arrayint(std::vector<int>(values)); }

// Cancelled with itself: exactly zero, or containing zero over RRi/CCi.
template <typename Mat>
void expectCancelledToZero(const Mat& matrix)
{
  using Ring = typename Mat::CoeffRing;
  const Ring& R = matrix.ring();
  if constexpr (RingIsInterval<Ring>::value)
    {
      typename Ring::Element entry(R);
      for (size_t r = 0; r < matrix.numRows(); ++r)
        for (size_t c = 0; c < matrix.numColumns(); ++c)
          {
            SCOPED_TRACE(::testing::Message() << "entry (" << r << ", " << c
                                              << ")");
            MatElementaryOps<Mat>::getEntry(matrix, r, c, entry);
            EXPECT_TRUE(R.is_member(0L, entry))
                << "cancelled interval does not contain zero";
          }
    }
  else
    {
      Mat zero(R, matrix.numRows(), matrix.numColumns());
      EXPECT_TRUE(MatrixOps::isEqual(matrix, zero));
    }
}

// Rank is algebraic only over an exact field or domain.  Over the
// approximate rings it is a tolerance decision, and which ranks come out
// wrong depends on the LAPACK build: a rank-3 matrix with exact integer
// entries reports 4 here, and a rank-2 one does elsewhere.  Excluded rather
// than pinned -- there is no value to pin.
template <typename RT>
constexpr bool ranks()
{
  return !(std::is_same_v<RT, M2::ARingRR> ||
           std::is_same_v<RT, M2::ARingRRR> ||
           std::is_same_v<RT, M2::ARingCC> ||
           std::is_same_v<RT, M2::ARingCCC> ||
           std::is_same_v<RT, M2::ARingRRi> ||
           std::is_same_v<RT, M2::ARingCCi>);
}

// A property of DMat, not of the ring; see DISABLED_defaultConstruction.
template <typename RT>
constexpr bool defaultConstructibleDMat()
{
  return !(std::is_same_v<RT, M2::ARingZZ> ||
           std::is_same_v<RT, M2::ARingQQFlint> ||
           std::is_same_v<RT, M2::ARingZZpFlint> ||
           std::is_same_v<RT, M2::ARingGFFlint> ||
           std::is_same_v<RT, M2::ARingGFFlintBig>);
}

using DMatRings = ::testing::Types<M2::ARingZZp,
                                   M2::ARingZZpFFPACK,
                                   M2::ARingZZpFlint,
                                   M2::ARingZZGMP,
                                   M2::ARingZZ,
                                   M2::ARingQQGMP,
                                   M2::ARingQQFlint,
                                   M2::ARingRR,
                                   M2::ARingRRR,
                                   M2::ARingCC,
                                   M2::ARingCCC,
                                   M2::ARingRRi,
                                   M2::ARingCCi,
                                   M2::ARingGFFlint,
                                   M2::ARingGFFlintBig,
                                   M2::ARingGFM2,
                                   CoefficientRingR>;
// ARingTower is still under development and is not ready for DMat tests.
TYPED_TEST_SUITE(DMatTest, DMatRings);

TYPED_TEST(DMatTest, construction)
{
  using Ring = TypeParam;
  using Mat = DMat<Ring>;
  auto& ring = this->ring;
  for (const auto& shape : {std::vector<size_t> {0, 0}, {0, 3}, {3, 0}, {2, 3}})
    {
      SCOPED_TRACE(::testing::Message() << shape[0] << " x " << shape[1]);
      Mat matrix(ring, shape[0], shape[1]);
      EXPECT_EQ(&matrix.ring(), &ring);
      this->expectMatrix(
          matrix, shape[0], shape[1], std::vector<int>(shape[0] * shape[1], 0));
    }
}

// The five FLINT specializations set only mRing (dmat-zzp-flint.hpp:34),
// leaving the flint matrix uninitialized, while the destructor clears it
// regardless (:45): numRows() reads garbage and destruction corrupts the
// heap.  Disabled because it aborts rather than fails, taking the run with
// it.  Asserts the intended behavior, so a fix re-enables it unchanged.
// https://github.com/Macaulay2/M2/issues/4720
TYPED_TEST(DMatTest, DISABLED_defaultConstruction)
{
  // No coefficient ring, so only the shape can be inspected.
  using Ring = TypeParam;
  using Mat = DMat<Ring>;
  // A DISABLED_ test that passes documents nothing (STYLE.md section 11).
  if (defaultConstructibleDMat<Ring>())
    GTEST_SKIP() << "not affected by issue 4720; see "
                    "defaultConstructionOfTheGenericTemplate";
  Mat empty;
  EXPECT_EQ(empty.numRows(), 0);
  EXPECT_EQ(empty.numColumns(), 0);
}

TYPED_TEST(DMatTest, defaultConstructionOfTheGenericTemplate)
{
  // The same check for the rings issue 4720 does not affect.
  using Ring = TypeParam;
  using Mat = DMat<Ring>;
  if (!defaultConstructibleDMat<Ring>())
    GTEST_SKIP() << "FLINT DMat specialization; see "
                    "https://github.com/Macaulay2/M2/issues/4720";
  Mat empty;
  EXPECT_EQ(empty.numRows(), 0);
  EXPECT_EQ(empty.numColumns(), 0);
}

TYPED_TEST(DMatTest, resizeReplacesShapeAndClearsEntries)
{
  // Unlike SMat, DMat resizes in place; the result is always zero.
  using Ring = TypeParam;
  using Mat = DMat<Ring>;
  auto& ring = this->ring;
  Mat matrix(ring, 2, 3);
  this->fill(matrix, {2, 0, 3, 0, 5, 0});
  matrix.resize(3, 2);
  EXPECT_EQ(&matrix.ring(), &ring);
  this->expectMatrix(matrix, 3, 2, std::vector<int>(6, 0));
  matrix.resize(0, 2);
  this->expectMatrix(matrix, 0, 2, {});
}

TYPED_TEST(DMatTest, entryAssignmentAndReplacement)
{
  using Ring = TypeParam;
  using Mat = DMat<Ring>;
  using Ops = MatElementaryOps<Mat>;
  auto& ring = this->ring;
  Mat matrix(ring, 5, 2);
  Ops::setEntry(matrix, 2, 0, this->scalar(3));
  Ops::setEntry(matrix, 0, 0, this->scalar(-2));
  Ops::setEntry(matrix, 4, 0, this->scalar(7));
  Ops::setEntry(matrix, 3, 0, this->scalar(5));
  Ops::setEntry(matrix, 1, 0, this->scalar(0));
  Ops::setEntry(matrix, 2, 0, this->scalar(-4));
  this->expectMatrix(matrix, 5, 2, {-2, 0, 0, 0, -4, 0, 5, 0, 7, 0});

  for (size_t row : {3, 4, 0, 2})
    Ops::setEntry(matrix, row, 0, this->scalar(0));
  Ops::setEntry(matrix, 2, 1, this->scalar(0));
  this->expectMatrix(matrix, 5, 2, std::vector<int>(10, 0));
}

TYPED_TEST(DMatTest, entriesFromTriplesAndRingElements)
{
  // The ring-element forms carry coefficients a small integer cannot express,
  // such as a GF(p^k) element outside the prime subfield.  ElementArray owns
  // them; the vectors alias its storage and must not outlive it.
  using Ring = TypeParam;
  using Mat = DMat<Ring>;
  using Ops = MatElementaryOps<Mat>;  auto& ring = this->ring;

  Mat triples(ring, 3, 3);
  this->fill(triples, {{0, 1, 7}, {2, 0, -5}});
  this->expectMatrix(triples, 3, 3, {0, 7, 0, 0, 0, 0, -5, 0, 0});

  typename Ring::ElementArray owners(ring, 4);
  for (size_t i = 0; i < 4; ++i)
    {
      if constexpr (RingHasRandom<Ring>::value)
        ring.random(owners[i]);
      else
        ring.set(owners[i], static_cast<int>(2 * i + 1));
    }

  std::vector<typename Ring::ElementType> rowMajor;
  for (size_t i = 0; i < 4; ++i) rowMajor.push_back(owners[i]);
  Mat elements(ring, 2, 2);
  this->fill(elements, rowMajor);

  std::vector<MatrixElementEntry<typename Ring::ElementType>> placed;
  placed.push_back({1, 0, owners[3]});
  placed.push_back({0, 2, owners[1]});
  Mat scattered(ring, 2, 3);
  this->fill(scattered, placed);

  typename Ring::Element actual(ring);
  for (size_t i = 0; i < 4; ++i)
    {
      SCOPED_TRACE(::testing::Message() << "row-major element " << i);
      Ops::getEntry(elements, i / 2, i % 2, actual);
      EXPECT_TRUE(this->equal(ring, actual, owners[i]));
    }
  Ops::getEntry(scattered, 1, 0, actual);
  EXPECT_TRUE(this->equal(ring, actual, owners[3]));
  Ops::getEntry(scattered, 0, 2, actual);
  EXPECT_TRUE(this->equal(ring, actual, owners[1]));
  EXPECT_FALSE(MatrixOps::isZero(scattered));
}

TYPED_TEST(DMatTest, copiesOwnTheirEntries)
{
  using Ring = TypeParam;
  using Mat = DMat<Ring>;
  using Ops = MatElementaryOps<Mat>;
  auto& ring = this->ring;
  Mat source(ring, 3, 2);
  this->fill(source, {2, 0, 0, 0, -3, 5});
  Mat copied(source);
  Ops::setEntry(source, 0, 0, this->scalar(7));
  Ops::setEntry(copied, 2, 1, this->scalar(0));

  this->expectMatrix(source, 3, 2, {7, 0, 0, 0, -3, 5});
  this->expectMatrix(copied, 3, 2, {2, 0, 0, 0, -3, 0});
  EXPECT_EQ(&copied.ring(), &ring);
}

TYPED_TEST(DMatTest, swapExchangesRingShapeAndEntries)
{
  using Ring = TypeParam;
  using Mat = DMat<Ring>;
  using Ops = MatElementaryOps<Mat>;
  auto& ring = this->ring;
  Ring& otherRing = TestRingFactory<Ring>::alternate();
  Mat first(ring, 2, 1), second(otherRing, 1, 2);
  this->fill(first, {2, -3});
  typename Ring::Element value(otherRing);
  otherRing.set(value, 4);
  Ops::setEntry(second, 0, 1, value);

  first.swap(second);

  EXPECT_EQ(&first.ring(), &otherRing);
  EXPECT_EQ(&second.ring(), &ring);
  this->expectMatrix(first, 1, 2, {0, 4});
  this->expectMatrix(second, 2, 1, {2, -3});
  second.swap(second);
  this->expectMatrix(second, 2, 1, {2, -3});
}

TYPED_TEST(DMatTest, interchangeRows)
{
  using Ring = TypeParam;
  using Mat = DMat<Ring>;
  using Ops = MatElementaryOps<Mat>;
  auto& ring = this->ring;
  struct Case
  {
    const char* name;
    size_t first, second;
    std::vector<int> expected;
  };
  const Case cases[] = {
      {"both nonzero", 0, 4, {7, 0, 3, 0, 2}},
      {"zero row below the nonzero one", 0, 3, {0, 0, 3, 2, 7}},
      {"zero row above the nonzero one", 4, 1, {2, 7, 3, 0, 0}},
      {"adjacent, moving up", 2, 3, {2, 0, 0, 3, 7}},
      {"adjacent, moving down", 2, 1, {2, 3, 0, 0, 7}},
      {"both zero", 1, 3, {2, 0, 3, 0, 7}},
      {"same row", 2, 2, {2, 0, 3, 0, 7}}};
  for (const auto& sample : cases)
    {
      SCOPED_TRACE(sample.name);
      Mat matrix(ring, 5, 1);
      this->fill(matrix, {2, 0, 3, 0, 7});
      Ops::interchange_rows(matrix, sample.first, sample.second);
      this->expectMatrix(matrix, 5, 1, sample.expected);
    }
  Mat empty(ring, 5, 1);
  Ops::interchange_rows(empty, 0, 4);
  this->expectMatrix(empty, 5, 1, {0, 0, 0, 0, 0});
}

TYPED_TEST(DMatTest, interchangeColumns)
{
  using Ring = TypeParam;
  using Mat = DMat<Ring>;
  using Ops = MatElementaryOps<Mat>;
  auto& ring = this->ring;
  Mat matrix(ring, 2, 3);
  this->fill(matrix, {2, 0, 5, 0, 0, -3});
  Ops::interchange_columns(matrix, 0, 2);
  this->expectMatrix(matrix, 2, 3, {5, 0, 2, -3, 0, 0});
  Ops::interchange_columns(matrix, 0, 1);
  Ops::interchange_columns(matrix, 2, 2);
  this->expectMatrix(matrix, 2, 3, {0, 5, 2, 0, -3, 0});
}

TYPED_TEST(DMatTest, scaleAndDivideRows)
{
  // Division reverses scaling; scaling by zero clears the row.
  using Ring = TypeParam;
  using Mat = DMat<Ring>;
  using Ops = MatElementaryOps<Mat>;
  auto& ring = this->ring;
  Mat matrix(ring, 3, 3);
  this->fill(matrix, {2, 0, 0, 3, -4, 0, 0, 5, 0});
  Ops::scale_row(matrix, 1, this->scalar(-2));
  this->expectMatrix(matrix, 3, 3, {2, 0, 0, -6, 8, 0, 0, 5, 0});
  Ops::divide_row(matrix, 1, this->scalar(-2));
  this->expectMatrix(matrix, 3, 3, {2, 0, 0, 3, -4, 0, 0, 5, 0});
  Ops::scale_row(matrix, 1, this->scalar(0));
  // Scaling and dividing by one must leave a row untouched.
  Ops::scale_row(matrix, 2, this->scalar(1));
  Ops::divide_row(matrix, 2, this->scalar(1));
  this->expectMatrix(matrix, 3, 3, {2, 0, 0, 0, 0, 0, 0, 5, 0});
}

TYPED_TEST(DMatTest, scaleAndDivideColumns)
{
  // The divide_column on column 1 acts on an all-zero column.
  using Ring = TypeParam;
  using Mat = DMat<Ring>;
  using Ops = MatElementaryOps<Mat>;
  auto& ring = this->ring;
  Mat matrix(ring, 3, 3);
  this->fill(matrix, {2, 0, 3, 0, 0, -4, 5, 0, 0});
  Ops::scale_column(matrix, 0, this->scalar(-2));
  Ops::divide_column(matrix, 1, this->scalar(3));
  this->expectMatrix(matrix, 3, 3, {-4, 0, 3, 0, 0, -4, -10, 0, 0});
  Ops::divide_column(matrix, 0, this->scalar(-2));
  this->expectMatrix(matrix, 3, 3, {2, 0, 3, 0, 0, -4, 5, 0, 0});
  Ops::scale_column(matrix, 0, this->scalar(0));
  this->expectMatrix(matrix, 3, 3, {0, 0, 3, 0, 0, -4, 0, 0, 0});
}

TYPED_TEST(DMatTest, rowAddition)
{
  // row_op asserts the rows differ, so no case aliases as SMat's does.
  using Ring = TypeParam;
  using Mat = DMat<Ring>;
  using Ops = MatElementaryOps<Mat>;
  auto& ring = this->ring;
  Mat matrix(ring, 3, 4);
  this->fill(matrix, {2, 0, 3, 0, -2, 5, 0, 0, 2, -5, 0, 0});
  Ops::row_op(matrix, 0, this->scalar(1), 1);
  this->expectMatrix(matrix, 3, 4, {0, 5, 3, 0, -2, 5, 0, 0, 2, -5, 0, 0});
  Ops::row_op(matrix, 2, this->scalar(0), 1);
  this->expectMatrix(matrix, 3, 4, {0, 5, 3, 0, -2, 5, 0, 0, 2, -5, 0, 0});
  Ops::row_op(matrix, 2, this->scalar(1), 1);
  this->expectMatrix(matrix, 3, 4, {0, 5, 3, 0, -2, 5, 0, 0, 0, 0, 0, 0});
}

TYPED_TEST(DMatTest, columnAddition)
{
  // column_op likewise asserts the columns differ.
  using Ring = TypeParam;
  using Mat = DMat<Ring>;
  using Ops = MatElementaryOps<Mat>;
  auto& ring = this->ring;
  Mat matrix(ring, 5, 3);
  this->fill(matrix, {2, 0, 0, 0, 5, 0, 3, -3, 0, 0, 7, 0, 11, 0, 0});
  Ops::column_op(matrix, 0, this->scalar(1), 1);
  this->expectMatrix(
      matrix, 5, 3, {2, 0, 0, 5, 5, 0, 0, -3, 0, 7, 7, 0, 11, 0, 0});
  Ops::column_op(matrix, 2, this->scalar(0), 0);
  this->expectMatrix(
      matrix, 5, 3, {2, 0, 0, 5, 5, 0, 0, -3, 0, 7, 7, 0, 11, 0, 0});
  Ops::column_op(matrix, 1, this->scalar(-1), 0);
  this->expectMatrix(
      matrix, 5, 3, {2, -2, 0, 5, 0, 0, 0, -3, 0, 7, 0, 0, 11, -11, 0});
}

TYPED_TEST(DMatTest, simultaneousRowTransformation)
{
  // Both new rows must use the old values.
  using Ring = TypeParam;
  using Mat = DMat<Ring>;
  using Ops = MatElementaryOps<Mat>;
  auto& ring = this->ring;
  Mat matrix(ring, 3, 5);
  this->fill(matrix, {2, 0, 3, 0, 0, 2, 5, 0, 0, 0, 0, 0, 0, 7, 0});
  Ops::row2by2(matrix,
               0,
               1,
               this->scalar(1),
               this->scalar(-1),
               this->scalar(2),
               this->scalar(3));
  this->expectMatrix(
      matrix, 3, 5, {0, -5, 3, 0, 0, 10, 15, 6, 0, 0, 0, 0, 0, 7, 0});
}

TYPED_TEST(DMatTest, simultaneousColumnTransformation)
{
  // Unequal coefficients expose reuse of the updated first column.
  using Ring = TypeParam;
  using Mat = DMat<Ring>;
  using Ops = MatElementaryOps<Mat>;
  auto& ring = this->ring;
  Mat matrix(ring, 5, 3);
  this->fill(matrix, {2, 2, 0, 0, 5, 0, 3, 0, 0, 0, 0, 7, 0, 0, 0});
  Ops::column2by2(matrix,
                  0,
                  1,
                  this->scalar(1),
                  this->scalar(-1),
                  this->scalar(2),
                  this->scalar(3));
  this->expectMatrix(
      matrix, 5, 3, {0, 10, 0, -5, 15, 0, 3, 6, 0, 0, 0, 7, 0, 0, 0});
}

TYPED_TEST(DMatTest, dotProducts)
{
  using Ring = TypeParam;
  using Mat = DMat<Ring>;
  using Ops = MatElementaryOps<Mat>;
  auto& ring = this->ring;
  Mat matrix(ring, 5, 4);
  this->fill(matrix,
             {2, 0, 0, 0, 0, 5, 0, 0, 3, -4, 0, 0, 0, 0, 7, 0, -2, 6, 0, 0});
  struct Case
  {
    const char* name;
    size_t first, second;
    int expected;
  };
  const Case cases[] = {{"overlap", 0, 1, -24},
                        {"reverse overlap", 1, 0, -24},
                        {"self", 0, 0, 17},
                        {"disjoint", 0, 2, 0},
                        {"zero right", 0, 3, 0},
                        {"zero left", 3, 0, 0}};
  for (const auto& sample : cases)
    {
      SCOPED_TRACE(sample.name);
      typename Ring::Element result(ring);
      ring.set(result, 19);
      Ops::dot_product(matrix, sample.first, sample.second, result);
      EXPECT_TRUE(ring.is_equal(result, this->scalar(sample.expected)))
          << "expected " << sample.expected;
    }
}

TYPED_TEST(DMatTest, rowPermutations)
{
  // A nonzero offset and a three-cycle expose a reversed convention.
  using Ring = TypeParam;
  using Mat = DMat<Ring>;
  using Ops = MatElementaryOps<Mat>;
  auto& ring = this->ring;
  Mat matrix(ring, 5, 3);
  this->fill(matrix, {2, 0, 0, 3, 0, 0, 5, 11, 0, 7, 0, 0, -2, 0, 0});
    EXPECT_TRUE(Ops::row_permute(matrix, 1, indices({2, 0, 1})));
  this->expectMatrix(
      matrix, 5, 3, {2, 0, 0, 7, 0, 0, 3, 0, 0, 5, 11, 0, -2, 0, 0});
  EXPECT_TRUE(Ops::row_permute(matrix, 0, indices({0, 1, 2, 3, 4})));
  EXPECT_TRUE(Ops::row_permute(matrix, 0, indices({})));
  this->expectMatrix(
      matrix, 5, 3, {2, 0, 0, 7, 0, 0, 3, 0, 0, 5, 11, 0, -2, 0, 0});
}

TYPED_TEST(DMatTest, columnPermutations)
{
  using Ring = TypeParam;
  using Mat = DMat<Ring>;
  using Ops = MatElementaryOps<Mat>;
  auto& ring = this->ring;
  Mat matrix(ring, 2, 5);
  this->fill(matrix, {2, 3, 0, 5, 7, -2, 0, 0, 11, 0});
    EXPECT_TRUE(Ops::column_permute(matrix, 1, indices({2, 0, 1})));
  this->expectMatrix(matrix, 2, 5, {2, 5, 3, 0, 7, -2, 11, 0, 0, 0});
  EXPECT_TRUE(Ops::column_permute(matrix, 0, indices({0, 1, 2, 3, 4})));
  EXPECT_TRUE(Ops::column_permute(matrix, 0, indices({})));
  this->expectMatrix(matrix, 2, 5, {2, 5, 3, 0, 7, -2, 11, 0, 0, 0});
}

TYPED_TEST(DMatTest, duplicatePermutationsAreRejected)
{
  using Ring = TypeParam;
  using Mat = DMat<Ring>;
  using Ops = MatElementaryOps<Mat>;
  auto& ring = this->ring;
  Mat matrix(ring, 3, 3);
  this->fill(matrix, {2, 0, 3, 0, 5, 0, 7, 0, 11});
    EXPECT_FALSE(Ops::row_permute(matrix, 0, indices({1, 1, 0})));
  EXPECT_STREQ(error_message(), "expected permutation");
  this->expectMatrix(matrix, 3, 3, {2, 0, 3, 0, 5, 0, 7, 0, 11});
    EXPECT_FALSE(Ops::column_permute(matrix, 0, indices({1, 1, 0})));
  EXPECT_STREQ(error_message(), "expected permutation");
  this->expectMatrix(matrix, 3, 3, {2, 0, 3, 0, 5, 0, 7, 0, 11});
}

TYPED_TEST(DMatTest, insertAndDeleteRows)
{
  using Ring = TypeParam;
  using Mat = DMat<Ring>;
  using Ops = MatElementaryOps<Mat>;
  auto& ring = this->ring;
  for (size_t position : {0, 1, 3})
    {
      SCOPED_TRACE(::testing::Message() << "insert rows at " << position);
      Mat matrix(ring, 3, 2);
      this->fill(matrix, {2, 0, 0, 0, -3, 5});
      std::vector<int> expected {2, 0, 0, 0, -3, 5};
      expected.insert(expected.begin() + 2 * position, 4, 0);
      Ops::insert_rows(matrix, position, 2);
      this->expectMatrix(matrix, 5, 2, expected);
      Ops::delete_rows(matrix, position, position + 1);
      this->expectMatrix(matrix, 3, 2, {2, 0, 0, 0, -3, 5});
    }
  Mat matrix(ring, 4, 2);
  this->fill(matrix, {2, 0, 3, 5, 7, 0, 11, 13});
  Ops::insert_rows(matrix, 2, 0);
  Ops::delete_rows(matrix, 1, 2);
  this->expectMatrix(matrix, 2, 2, {2, 0, 11, 13});
  Ops::delete_rows(matrix, 0, 1);
  this->expectMatrix(matrix, 0, 2, {});
  Ops::insert_rows(matrix, 0, 1);
  this->expectMatrix(matrix, 1, 2, {0, 0});
}

TYPED_TEST(DMatTest, insertAndDeleteColumns)
{
  using Ring = TypeParam;
  using Mat = DMat<Ring>;
  using Ops = MatElementaryOps<Mat>;
  auto& ring = this->ring;
  for (size_t position : {0, 1, 3})
    {
      SCOPED_TRACE(::testing::Message() << "insert columns at " << position);
      Mat matrix(ring, 1, 3);
      this->fill(matrix, {2, 0, -3});
      std::vector<int> expected {2, 0, -3};
      expected.insert(expected.begin() + position, 2, 0);
      Ops::insert_columns(matrix, position, 2);
      this->expectMatrix(matrix, 1, 5, expected);
      Ops::delete_columns(matrix, position, position + 1);
      this->expectMatrix(matrix, 1, 3, {2, 0, -3});
    }
  Mat matrix(ring, 2, 4);
  this->fill(matrix, {2, 3, 0, 5, 0, 7, 11, 13});
  Ops::insert_columns(matrix, 2, 0);
  Ops::delete_columns(matrix, 1, 2);
  this->expectMatrix(matrix, 2, 2, {2, 5, 0, 13});
  Ops::delete_columns(matrix, 0, 1);
  this->expectMatrix(matrix, 2, 0, {});
  Ops::insert_columns(matrix, 0, 1);
  this->expectMatrix(matrix, 2, 1, {0, 0});
}

TYPED_TEST(DMatTest, matrixAdditionAndSubtraction)
{
  // mat-arith.hpp promises nothing about aliasing, but both loops are
  // elementwise, so self-addition doubles and self-subtraction clears.
  using Ring = TypeParam;
  using Mat = DMat<Ring>;
  auto& ring = this->ring;
  Mat left(ring, 4, 4), right(ring, 4, 4);
  this->fill(left, {2, 0, 0, 0, 0, 0, 0, 0, 3, 0, 7, 0, 0, 0, 0, 0});
  this->fill(right, {0, 0, 17, 0, 5, 11, 0, 0, -3, 0, -7, 0, 13, 0, 0, 0});
  MatrixOps::addInPlace(left, right);
  this->expectMatrix(
      left, 4, 4, {2, 0, 17, 0, 5, 11, 0, 0, 0, 0, 0, 0, 13, 0, 0, 0});
  this->expectMatrix(
      right, 4, 4, {0, 0, 17, 0, 5, 11, 0, 0, -3, 0, -7, 0, 13, 0, 0, 0});
  MatrixOps::addInPlace(left, left);
  this->expectMatrix(
      left, 4, 4, {4, 0, 34, 0, 10, 22, 0, 0, 0, 0, 0, 0, 26, 0, 0, 0});
  MatrixOps::subtractInPlace(left, right);
  this->expectMatrix(
      left, 4, 4, {4, 0, 17, 0, 5, 11, 0, 0, 3, 0, 7, 0, 13, 0, 0, 0});
  MatrixOps::subtractInPlace(left, left);
  this->expectMatrix(left, 4, 4, std::vector<int>(16, 0));

  // Rectangular too: a square-only check hides a row/column mix-up.
  Mat wide(ring, 2, 3), addend(ring, 2, 3);
  this->fill(wide, {2, 0, 3, 5, 7, 0});
  this->fill(addend, {1, 11, -3, 0, -7, 13});
  MatrixOps::addInPlace(wide, addend);
  this->expectMatrix(wide, 2, 3, {3, 11, 0, 5, 0, 13});
}

TYPED_TEST(DMatTest, multiplication)
{
  // The only operation with per-ring implementations: mult dispatches through
  // DMatLinAlg and the primary template throws.  All 17 rings resolve.
  using Ring = TypeParam;
  using Mat = DMat<Ring>;
  using Ops = MatElementaryOps<Mat>;
  auto& ring = this->ring;

  // Rectangular operands catch a transposed index; a square product hides it.
  Mat left(ring, 2, 3), right(ring, 3, 2), product(ring, 2, 2);
  this->fill(left, {2, 0, 3, 5, 7, 0});
  this->fill(right, {1, 4, 2, 5, 3, 6});
  MatrixOps::mult(left, right, product);
  this->expectMatrix(product, 2, 2, {11, 26, 19, 55});
  this->expectMatrix(left, 2, 3, {2, 0, 3, 5, 7, 0});
  this->expectMatrix(right, 3, 2, {1, 4, 2, 5, 3, 6});

  Mat identity(ring, 3, 3), unchanged(ring, 2, 3);
  for (size_t i = 0; i < 3; ++i) Ops::setEntry(identity, i, i, this->scalar(1));
  MatrixOps::mult(left, identity, unchanged);
  EXPECT_TRUE(MatrixOps::isEqual(unchanged, left));
}

TYPED_TEST(DMatTest, scalingByTheCharacteristicAnnihilates)
{
  // In characteristic p the element p is zero and must annihilate.
  using Ring = TypeParam;
  using Mat = DMat<Ring>;
  auto& ring = this->ring;
  // CoefficientRingR exposes no characteristic(); it wraps QQ here anyway.
  if constexpr (std::is_same_v<Ring, CoefficientRingR>)
    GTEST_SKIP() << "CoefficientRingR does not report a characteristic";
  else
    {
  const auto characteristic = ring.characteristic();
  if (characteristic == 0)
    GTEST_SKIP() << "characteristic zero: there is no such element";

  // The factory builds only small characteristics; one near 2^64 would
  // overflow the cast and need set_from_mpz (STYLE.md section 15).
  ASSERT_LT(characteristic, 1000);
  typename Ring::Element zero(ring);
  ring.set(zero, static_cast<int>(characteristic));
  EXPECT_TRUE(ring.is_zero(zero));

  Mat matrix(ring, 3, 3);
  this->fill(matrix, {2, 0, 3, 0, 5, 0, 7, 0, 11});
  EXPECT_FALSE(MatrixOps::isZero(matrix));
  MatrixOps::scalarMultInPlace(matrix, zero);
  this->expectMatrix(matrix, 3, 3, std::vector<int>(9, 0));
    }
}

TYPED_TEST(DMatTest, generatedSymmetricShapes)
{
  // The skew diagonal must be zeroed, not derived: in characteristic 2
  // a == -a would not force it, and pfaffians need the alternating form.
  using Ring = TypeParam;
  using Mat = DMat<Ring>;
  using Ops = MatElementaryOps<Mat>;
  auto& ring = this->ring;

  Mat symmetric(ring, 5, 5), transposed(ring, 5, 5);
  this->fillShape(symmetric, MatrixShape::Symmetric);
  MatrixOps::transpose(symmetric, transposed);
  EXPECT_TRUE(MatrixOps::isEqual(symmetric, transposed));
  EXPECT_FALSE(MatrixOps::isZero(symmetric));

  Mat skew(ring, 5, 5), skewTransposed(ring, 5, 5);
  this->fillShape(skew, MatrixShape::SkewSymmetric);
  MatrixOps::transpose(skew, skewTransposed);
  MatrixOps::negateInPlace(skewTransposed);
  EXPECT_TRUE(MatrixOps::isEqual(skew, skewTransposed));
  typename Ring::Element diagonal(ring);
  for (size_t i = 0; i < 5; ++i)
    {
      SCOPED_TRACE(::testing::Message() << "diagonal entry " << i);
      Ops::getEntry(skew, i, i, diagonal);
      EXPECT_TRUE(ring.is_zero(diagonal));
    }
}

TYPED_TEST(DMatTest, generatedTriangularAndIdentityShapes)
{
  // lead_row reports the last nonzero row, so upper-triangular column c
  // ends at row c.
  using Ring = TypeParam;
  using Mat = DMat<Ring>;
  using Ops = MatElementaryOps<Mat>;
  auto& ring = this->ring;

  Mat upper(ring, 4, 4);
  this->fillShape(upper, MatrixShape::UpperTriangular);
  for (size_t c = 0; c < 4; ++c)
    {
      SCOPED_TRACE(::testing::Message() << "upper column " << c);
      EXPECT_EQ(Ops::lead_row(upper, c), c);
    }

  Mat lower(ring, 4, 4), lowerTransposed(ring, 4, 4);
  this->fillShape(lower, MatrixShape::LowerTriangular);
  MatrixOps::transpose(lower, lowerTransposed);
  for (size_t c = 0; c < 4; ++c)
    {
      SCOPED_TRACE(::testing::Message() << "transposed lower column " << c);
      EXPECT_EQ(Ops::lead_row(lowerTransposed, c), c);
    }

  Mat identity(ring, 4, 4), dense(ring, 4, 4), product(ring, 4, 4);
  this->fillShape(identity, MatrixShape::Identity);
  this->fillShape(dense, MatrixShape::Dense);
  MatrixOps::mult(dense, identity, product);
  EXPECT_TRUE(MatrixOps::isEqual(product, dense));
}

TYPED_TEST(DMatTest, generatedRankIsExact)
{
  // Unit-trapezoidal outer products give the rank exactly over any ring with
  // 1 != 0, where a random fill would only reach it generically.
  using Ring = TypeParam;
  using Mat = DMat<Ring>;
  auto& ring = this->ring;
  if (!ranks<Ring>())
    GTEST_SKIP() << "MatrixOps::rank is not implemented for this ring";

  for (size_t rank : {size_t(0), size_t(1), size_t(3), size_t(4)})
    {
      SCOPED_TRACE(::testing::Message() << "prescribed rank " << rank);
      Mat matrix(ring, 4, 6);
      this->fillShape(matrix, MatrixShape::PrescribedRank, 1.0, rank);
      EXPECT_EQ(MatrixOps::rank(matrix), rank);
      EXPECT_EQ(MatrixOps::isZero(matrix), rank == 0);
    }
}

TYPED_TEST(DMatTest, randomEntriesSurviveArithmetic)
{
  // Worked values elsewhere are exact small integers; only a ring draw
  // reaches values that round or widen.
  using Ring = TypeParam;
  using Mat = DMat<Ring>;
  auto& ring = this->ring;

  Mat matrix(ring, 4, 4), copy(ring, 4, 4), zero(ring, 4, 4);
  this->fillShape(matrix, MatrixShape::Dense, 1.0, 0, /* random */ true);
  submatrix(copy) = submatrix(matrix);
  ASSERT_TRUE(MatrixOps::isEqual(copy, matrix));

  // Exactly zero even where addition rounds -- except over intervals, where
  // [a,b] - [a,b] contains zero without being it (STYLE.md section 7).
  MatrixOps::subtractInPlace(copy, matrix);
  expectCancelledToZero(copy);

  submatrix(copy) = submatrix(matrix);
  MatrixOps::negateInPlace(copy);
  MatrixOps::addInPlace(copy, matrix);
  expectCancelledToZero(copy);

  typename Ring::Element one(ring);
  ring.set(one, 1);
  submatrix(copy) = submatrix(matrix);
  MatrixOps::scalarMultInPlace(copy, one);
  EXPECT_TRUE(MatrixOps::isEqual(copy, matrix));
}

TYPED_TEST(DMatTest, shapesOverEveryParameterisation)
{
  // The worked-value tests run at one parameterisation per ring.  Sweep the
  // rest here with characteristic-independent checks -- this is the only
  // place the suite sees characteristic 2, where -1 == 1 and a skew form's
  // zero diagonal cannot be derived from a == -a.
  using Ring = TypeParam;
  using Mat = DMat<Ring>;
  using Ops = MatElementaryOps<Mat>;
  overParameterisations<Ring>([&](const Ring& R) {
    Mat symmetric(R, 5, 5), transposed(R, 5, 5);
    this->fillShape(symmetric, MatrixShape::Symmetric);
    MatrixOps::transpose(symmetric, transposed);
    EXPECT_TRUE(MatrixOps::isEqual(symmetric, transposed));
    EXPECT_FALSE(MatrixOps::isZero(symmetric));

    Mat skew(R, 5, 5), skewTransposed(R, 5, 5);
    this->fillShape(skew, MatrixShape::SkewSymmetric);
    MatrixOps::transpose(skew, skewTransposed);
    MatrixOps::negateInPlace(skewTransposed);
    EXPECT_TRUE(MatrixOps::isEqual(skew, skewTransposed));
    typename Ring::Element diagonal(R);
    for (size_t i = 0; i < 5; ++i)
      {
        SCOPED_TRACE(::testing::Message() << "skew diagonal " << i);
        Ops::getEntry(skew, i, i, diagonal);
        EXPECT_TRUE(R.is_zero(diagonal));
      }

    Mat upper(R, 4, 4);
    this->fillShape(upper, MatrixShape::UpperTriangular);
    for (size_t c = 0; c < 4; ++c)
      {
        SCOPED_TRACE(::testing::Message() << "upper column " << c);
        EXPECT_EQ(Ops::lead_row(upper, c), c);
      }

    if (ranks<Ring>())
      for (size_t rank : {size_t(0), size_t(1), size_t(3), size_t(4)})
        {
          SCOPED_TRACE(::testing::Message() << "prescribed rank " << rank);
          Mat matrix(R, 4, 6);
          this->fillShape(matrix, MatrixShape::PrescribedRank, 1.0, rank);
          EXPECT_EQ(MatrixOps::rank(matrix), rank);
        }
  });
}

TYPED_TEST(DMatTest, negationAndScalarMultiplication)
{
  using Ring = TypeParam;
  using Mat = DMat<Ring>;
  auto& ring = this->ring;
  Mat matrix(ring, 3, 3);
  this->fill(matrix, {2, 0, 0, 0, -3, 0, 5, 0, 0});
  MatrixOps::negateInPlace(matrix);
  this->expectMatrix(matrix, 3, 3, {-2, 0, 0, 0, 3, 0, -5, 0, 0});
  MatrixOps::scalarMultInPlace(matrix, this->scalar(-2));
  this->expectMatrix(matrix, 3, 3, {4, 0, 0, 0, -6, 0, 10, 0, 0});
  MatrixOps::scalarMultInPlace(matrix, this->scalar(0));
  MatrixOps::negateInPlace(matrix);
  this->expectMatrix(matrix, 3, 3, std::vector<int>(9, 0));
}

TYPED_TEST(DMatTest, equalityWithMatchingEntriesAndDifferentShapes)
{
  using Ring = TypeParam;
  using Mat = DMat<Ring>;
  using Ops = MatElementaryOps<Mat>;
  auto& ring = this->ring;
  Mat matrix(ring, 2, 3);
  this->fill(matrix, {2, 0, 0, -3, 0, 5});
  Mat copy(matrix), otherRows(ring, 3, 3), otherCols(ring, 2, 2);
  EXPECT_TRUE(MatrixOps::isEqual(matrix, matrix));
  EXPECT_TRUE(MatrixOps::isEqual(matrix, copy));
  EXPECT_FALSE(MatrixOps::isEqual(matrix, otherRows));
  EXPECT_FALSE(MatrixOps::isEqual(matrix, otherCols));
  Ops::setEntry(copy, 1, 0, this->scalar(7));
  EXPECT_FALSE(MatrixOps::isEqual(matrix, copy));
  EXPECT_FALSE(MatrixOps::isEqual(copy, matrix));
}

TYPED_TEST(DMatTest, transposition)
{
  // Dense only: the SMat overload throws.
  using Ring = TypeParam;
  using Mat = DMat<Ring>;
  auto& ring = this->ring;
  Mat source(ring, 3, 2), result(ring, 2, 3);
  this->fill(source, {2, 0, 3, 5, 7, 0});
  MatrixOps::transpose(source, result);
  this->expectMatrix(result, 2, 3, {2, 3, 7, 0, 5, 0});
  this->expectMatrix(source, 3, 2, {2, 0, 3, 5, 7, 0});

  Mat symmetric(ring, 3, 3), transposed(ring, 3, 3);
  this->fill(symmetric, {2, 3, 5, 3, 7, 11, 5, 11, 13});
  MatrixOps::transpose(symmetric, transposed);
  EXPECT_TRUE(MatrixOps::isEqual(symmetric, transposed));
}

TYPED_TEST(DMatTest, submatrixSelection)
{
  // Unlike SMat the destination is an argument, and must already carry the
  // ring: resizing it reads ring().
  using Ring = TypeParam;
  using Mat = DMat<Ring>;
  using Ops = MatElementaryOps<Mat>;
  auto& ring = this->ring;
  Mat source(ring, 3, 4), selected(ring, 0, 0), columns(ring, 0, 0);
  this->fill(source, {2, 0, 3, 0, 0, 5, 0, 0, 7, 0, 11, 0});
  Ops::setFromSubmatrix(source, indices({2, 0, 2}), indices({2, 1, 0}),
                        selected);
  Ops::setFromSubmatrix(source, indices({3, 2, 2}), columns);
  EXPECT_EQ(&selected.ring(), &ring);
  EXPECT_EQ(&columns.ring(), &ring);
  this->expectMatrix(selected, 3, 3, {11, 0, 7, 3, 0, 2, 11, 0, 7});
  this->expectMatrix(columns, 3, 3, {0, 3, 3, 0, 0, 0, 0, 11, 11});
  Ops::setEntry(selected, 0, 0, this->scalar(19));
  Ops::setEntry(columns, 0, 1, this->scalar(23));
  this->expectMatrix(selected, 3, 3, {19, 0, 7, 3, 0, 2, 11, 0, 7});
  this->expectMatrix(columns, 3, 3, {0, 23, 3, 0, 0, 0, 0, 11, 11});
  this->expectMatrix(source, 3, 4, {2, 0, 3, 0, 0, 5, 0, 0, 7, 0, 11, 0});
}

TYPED_TEST(DMatTest, emptySubmatrices)
{
  // An empty row selection still takes its column count from the indices.
  using Ring = TypeParam;
  using Mat = DMat<Ring>;
  using Ops = MatElementaryOps<Mat>;
  auto& ring = this->ring;
  Mat source(ring, 2, 3), noRows(ring, 0, 0), noColumns(ring, 0, 0);
  this->fill(source, {2, 0, 3, 0, 5, 0});
  Ops::setFromSubmatrix(source, indices({}), indices({2, 0}), noRows);
  Ops::setFromSubmatrix(source, indices({}), noColumns);
  EXPECT_EQ(&noRows.ring(), &ring);
  EXPECT_EQ(&noColumns.ring(), &ring);
  this->expectMatrix(noRows, 0, 2, {});
  this->expectMatrix(noColumns, 2, 0, {});
}

TYPED_TEST(DMatTest, submatrixWindows)
{
  // Dense only: submatrix() writes through entry(), which SMat lacks.
  using Ring = TypeParam;
  using Mat = DMat<Ring>;
  using Ops = MatElementaryOps<Mat>;
  auto& ring = this->ring;
  {
    SCOPED_TRACE("assign: separate one-entry windows");
    Mat matrix(ring, 5, 5);
    Ops::setEntry(matrix, 0, 2, this->scalar(7));
    submatrix(matrix, 0, 0, 1, 1) = submatrix(matrix, 0, 2, 1, 1);
    std::vector<int> expected(25, 0);
    expected[0] = 7;
    expected[2] = 7;
    this->expectMatrix(matrix, 5, 5, expected);
  }
  {
    SCOPED_TRACE("zero: partial window");
    Mat matrix(ring, 5, 5);
    Ops::setEntry(matrix, 0, 0, this->scalar(3));
    Ops::setEntry(matrix, 0, 2, this->scalar(7));
    submatrix(matrix, 0, 0, 2, 2) = 0;
    std::vector<int> expected(25, 0);
    expected[2] = 7;
    this->expectMatrix(matrix, 5, 5, expected);
  }
  {
    SCOPED_TRACE("zero: covering window");
    Mat matrix(ring, 5, 5);
    Ops::setEntry(matrix, 0, 2, this->scalar(7));
    submatrix(matrix, 0, 2, 2, 2) = 0;
    this->expectMatrix(matrix, 5, 5, std::vector<int>(25, 0));
  }
  {
    SCOPED_TRACE("zero: full window");
    Mat matrix(ring, 5, 5);
    Ops::setEntry(matrix, 4, 4, this->scalar(7));
    submatrix(matrix) = 0;
    this->expectMatrix(matrix, 5, 5, std::vector<int>(25, 0));
  }
  {
    SCOPED_TRACE("assign, add, and scale: shifted identity");
    Mat matrix(ring, 5, 5), identity(ring, 2, 2);
    Ops::setEntry(identity, 0, 0, this->scalar(1));
    Ops::setEntry(identity, 1, 1, this->scalar(1));
    submatrix(matrix, 0, 1, 2, 2) = submatrix(identity);
    submatrix(matrix, 0, 0, 2, 2) += submatrix(identity);
    // operator*= takes a non-const reference, so the scalar needs a name.
    typename TestFixture::Scalar seven = this->scalar(7);
    submatrix(matrix, 0, 0, 2, 2) *= seven;
    std::vector<int> expected(25, 0);
    expected[0] = 7;
    expected[1] = 7;
    expected[5 + 1] = 7;
    expected[5 + 2] = 1;
    this->expectMatrix(matrix, 5, 5, expected);
  }
}

// Out of the typed suite: the complex rings report their real subring
// differently.
TEST(DMatCC, normSquared)
{
  // The squared Frobenius norm of the 5x5 identity is 5.
  M2::ARingCC C;
  DMat<M2::ARingCC> matrix(C, 5, 5);
  for (size_t i = 0; i < 5; ++i)
    C.set(matrix.entry(i, i), 1);

  const auto& R = C.real_ring();
  M2::ARingCC::RealRingType::Element result(R), expected(R);
  R.set(expected, 5);
  normSquared(submatrix(matrix), result);
  EXPECT_TRUE(R.is_equal(result, expected));
}

}  // namespace
