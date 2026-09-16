#ifndef M2_UNIT_TESTS_DMAT_TEST_HPP_
#define M2_UNIT_TESTS_DMAT_TEST_HPP_

#include "basic-mutable-matrices/dmat.hpp"
#include <gtest/gtest.h>
#include <cstddef>
#include <initializer_list>
#include <limits>
#include <vector>
#include "buffer.hpp"
#include "basic-mutable-matrices/mat-arith.hpp"
#include "basic-mutable-matrices/mat-elem-ops.hpp"
#include "unit-tests/MatrixShape.hpp"
#include "unit-tests/TestRingFactory.hpp"

namespace {
template <typename RT>
class DMatTest : public ::testing::Test
{
 protected:
  using Ring = RT;
  using Mat = DMat<Ring>;
  // DMat has no member arithmetic; row and column operations are statics.
  using Ops = MatElementaryOps<Mat>;
  Ring& ring = TestRingFactory<Ring>::shared();

  // Returning an owning temporary keeps MPFR/GMP coefficients alive through
  // the matrix call, without copying their resource-owning C structs.
  class Scalar : public Ring::Element
  {
   public:
    Scalar(const Ring& R, int value) : Ring::Element(R) { R.set(*this, value); }
  };

  Scalar scalar(int value) const { return Scalar(ring, value); }

  ::testing::AssertionResult equal(const Ring& R,
                                   const typename Ring::ElementType& actual,
                                   const typename Ring::ElementType& expected)
  {
    // All worked values are integers or exact dyadic quotients, including
    // for RR/CC and interval rings; no rounding tolerance is needed here.
    if (R.is_equal(actual, expected)) return ::testing::AssertionSuccess();
    buffer a, e;
    R.elem_text_out(a, actual, true, false, false);
    R.elem_text_out(e, expected, true, false, false);
    return ::testing::AssertionFailure()
           << "expected " << e.str() << ", actual " << a.str();
  }

  void fill(Mat& matrix, std::initializer_list<int> values)
  {
    ASSERT_EQ(values.size(), matrix.numRows() * matrix.numColumns());
    auto value = values.begin();
    for (size_t r = 0; r < matrix.numRows(); ++r)
      for (size_t c = 0; c < matrix.numColumns(); ++c)
        Ops::setEntry(matrix, r, c, scalar(*value++));
  }

  // Triples, integer coefficients: {{0, 1, 7}, {2, 3, 9}}.  Only the listed
  // positions are written, so these compose with a shaped fill.
  void fill(Mat& matrix, std::initializer_list<MatrixEntry> entries)
  {
    for (const auto& e : entries)
      {
        ASSERT_LT(e.row, matrix.numRows());
        ASSERT_LT(e.col, matrix.numColumns());
        Ops::setEntry(matrix, e.row, e.col, scalar(static_cast<int>(e.coeff)));
      }
  }

  // Ring-element coefficients, for values with no integer form -- an element
  // of GF(p^k) outside the prime subfield.  Vectors rather than initializer
  // lists: where ElementType is int the latter would be ambiguous above.
  void fill(Mat& matrix,
            const std::vector<typename Ring::ElementType>& values)
  {
    ASSERT_EQ(values.size(), matrix.numRows() * matrix.numColumns());
    for (size_t i = 0; i < values.size(); ++i)
      Ops::setEntry(matrix, i / matrix.numColumns(), i % matrix.numColumns(), values[i]);
  }

  void fill(Mat& matrix,
            const std::vector<MatrixElementEntry<typename Ring::ElementType>>&
                entries)
  {
    for (const auto& e : entries)
      {
        ASSERT_LT(e.row, matrix.numRows());
        ASSERT_LT(e.col, matrix.numColumns());
        Ops::setEntry(matrix, e.row, e.col, e.coeff);
      }
  }

  // For the properties a literal fill cannot express: an exact rank, a zero
  // skew diagonal, a sparsity pattern.  random = true draws from the ring,
  // the only way to reach values a small integer cannot represent.
  void fillShape(Mat& matrix,
                 MatrixShape shape,
                 double density = 1.0,
                 size_t rank = 0,
                 bool random = false)
  {
    const Ring& ring = matrix.ring();
    int counter = 0;
    auto next = [&](typename Ring::ElementType& out) {
      if constexpr (RingHasRandom<Ring>::value)
        if (random)
          {
            ring.random(out);
            return;
          }
      // Must be nonzero for the shapes to mean anything structurally, and a
      // small fixed sequence is zero half the time in characteristic 2.
      for (int i = 0; i < 100; ++i)
        {
          ring.set(out, 1 + (counter++ % 7));
          if (!ring.is_zero(out)) return;
        }
      ring.set(out, 1);
    };
    fillMatrixShape(ring,
                    matrix.numRows(),
                    matrix.numColumns(),
                    shape,
                    density,
                    rank,
                    next,
                    [&](size_t r, size_t c, const typename Ring::ElementType& a)
                    { Ops::setEntry(matrix, r, c, a); });
  }

  void expectMatrix(const Mat& matrix,
                    size_t rows,
                    size_t cols,
                    const std::vector<int>& values)
  {
    ASSERT_EQ(matrix.numRows(), rows);
    ASSERT_EQ(matrix.numColumns(), cols);
    ASSERT_EQ(values.size(), rows * cols);
    const Ring& R = matrix.ring();
    typename Ring::Element actual(R), expected(R);
    bool zero = true;
    for (size_t c = 0; c < cols; ++c)
      {
        SCOPED_TRACE(::testing::Message() << "column " << c);
        size_t lead = std::numeric_limits<size_t>::max();
        for (size_t r = 0; r < rows; ++r)
          {
            SCOPED_TRACE(::testing::Message() << "row " << r);
            R.set(expected, values[r * cols + c]);
            Ops::getEntry(matrix, r, c, actual);
            EXPECT_TRUE(equal(R, actual, expected));
            if (!R.is_zero(expected))
              {
                lead = r;
                zero = false;
              }
          }

        // Dense storage keeps every position, so lead_row is the only report
        // of where a column ends.
        EXPECT_EQ(Ops::lead_row(matrix, c), lead);
        typename Ring::Element leading(R);
        EXPECT_EQ(Ops::lead_row(matrix, c, leading), lead);
        if (lead != std::numeric_limits<size_t>::max())
          {
            R.set(expected, values[lead * cols + c]);
            EXPECT_TRUE(equal(R, leading, expected));
          }
      }
    EXPECT_EQ(MatrixOps::isZero(matrix), zero);
  }
};

}  // namespace
#endif
