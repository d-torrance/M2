// Copyright 2026, The Macaulay2 Authors.
//
// Shapes shared by every matrix test here: fillMatrixShape serves DMat and
// SMat, fillRingElemShape in MatrixTest.hpp serves MutableMatrix and Matrix.
// Separate because ARingTest.hpp and RingTest.hpp share an include guard.

#ifndef M2_UNITTESTS__MATRIX_SHAPE_HPP__
#define M2_UNITTESTS__MATRIX_SHAPE_HPP__

#include <cassert>
#include <cstddef>
#include <type_traits>
#include <utility>

#include "interface/random.h"

enum class MatrixShape {
  Zero,             // leave the zero-initialized matrix alone
  Dense,            // every entry drawn from the element generator
  Sparse,           // each entry nonzero with probability mDensity
  Identity,         // 1 on the diagonal (square only)
  UpperTriangular,  // fill r <= c
  LowerTriangular,  // fill r >= c
  Symmetric,        // fill r <= c, mirror to (c,r) (square only)
  SkewSymmetric,    // fill r < c, mirror negated, zero diagonal (square only)
  PrescribedRank    // sum of mRank outer products; rank <= mRank
};

// An explicitly specified entry, as a (row, column, coefficient) triple.
// MatrixEntry carries an integer coefficient, which the ring converts; use
// MatrixElementEntry<E> to give a coefficient that has no integer form, such
// as a particular element of GF(p^k).
struct MatrixEntry
{
  size_t row;
  size_t col;
  long coeff;
};

template <typename E>
struct MatrixElementEntry
{
  size_t row;
  size_t col;
  E coeff;
};

// Does position (r,c) get a nonzero entry, at the given density?
// Uses the global PRNG, so a generator's reset() cannot reproduce the pattern.
inline bool selectPosition(double density)
{
  if (density >= 1.0) return true;
  if (density <= 0.0) return false;
  return rawRandomInt(1000000) < static_cast<int32_t>(density * 1000000);
}

// Which (r,c) does a shape write to?  Shapes needing more than a per-position
// decision (Identity, Symmetric, SkewSymmetric, PrescribedRank) are handled by
// the generators themselves.
inline bool shapeCoversPosition(MatrixShape shape, size_t r, size_t c)
{
  switch (shape)
    {
      case MatrixShape::UpperTriangular: return r <= c;
      case MatrixShape::LowerTriangular: return r >= c;
      default: return true;
    }
}


// CoefficientRingR has no random(); detect that rather than name it.
template <typename R, typename = void>
struct RingHasRandom : std::false_type
{
};
template <typename R>
struct RingHasRandom<R,
                     std::void_t<decltype(std::declval<const R&>().random(
                         std::declval<typename R::ElementType&>()))>>
    : std::true_type
{
};

// Interval rings: is_zero() means degenerate at zero, is_member() contains.
template <typename R, typename = void>
struct RingIsInterval : std::false_type
{
};
template <typename R>
struct RingIsInterval<R,
                      std::void_t<decltype(std::declval<const R&>().is_member(
                          0L, std::declval<const typename R::ElementType&>()))>>
    : std::true_type
{
};

// Ring operations only, so this serves every ARing and both DMat and SMat;
// the caller supplies next(Element&) and setEntry(r, c, ElementType).  Only
// the positions a shape owns are written, so shapes compose with explicit
// entries, and a reused matrix must be zeroed first.
template <typename RingType, typename NextElement, typename SetEntry>
void fillMatrixShape(const RingType& R,
                     size_t nrows,
                     size_t ncols,
                     MatrixShape shape,
                     double density,
                     size_t rank,
                     NextElement next,
                     SetEntry setEntry)
{
  typename RingType::Element a(R), b(R);
  switch (shape)
    {
      case MatrixShape::Zero:
        return;
      case MatrixShape::Identity:
        {
          assert(nrows == ncols && "Identity requires a square matrix");
          R.set(a, 1);
          for (size_t i = 0; i < nrows; i++) setEntry(i, i, a);
          return;
        }
      case MatrixShape::Symmetric:
      case MatrixShape::SkewSymmetric:
        {
          assert(nrows == ncols && "Symmetric requires a square matrix");
          // The skew diagonal is left zero rather than derived: in
          // characteristic 2, a == -a would not force it.
          bool skew = (shape == MatrixShape::SkewSymmetric);
          for (size_t r = 0; r < nrows; r++)
            for (size_t c = (skew ? r + 1 : r); c < ncols; c++)
              {
                next(a);
                setEntry(r, c, a);
                if (c == r) continue;
                if (skew)
                  {
                    R.negate(b, a);
                    setEntry(c, r, b);
                  }
                else
                  setEntry(c, r, a);
              }
          return;
        }
      case MatrixShape::PrescribedRank:
        {
          // sum_k u_k v_k^T with u_k, v_k unit-trapezoidal, so the leading
          // minor is unit triangular and the rank is exact over any ring with
          // 1 != 0; random outer products would only reach it generically.
          if (rank > nrows) rank = nrows;
          if (rank > ncols) rank = ncols;
          if (rank == 0) return;

          typename RingType::ElementArray u(R, rank * nrows);
          typename RingType::ElementArray v(R, rank * ncols);
          for (size_t k = 0; k < rank; k++)
            {
              for (size_t i = 0; i < nrows; i++)
                {
                  if (i < k) R.set_zero(u[k * nrows + i]);
                  else if (i == k) R.set(u[k * nrows + i], 1);
                  else next(u[k * nrows + i]);
                }
              for (size_t j = 0; j < ncols; j++)
                {
                  if (j < k) R.set_zero(v[k * ncols + j]);
                  else if (j == k) R.set(v[k * ncols + j], 1);
                  else next(v[k * ncols + j]);
                }
            }

          for (size_t i = 0; i < nrows; i++)
            for (size_t j = 0; j < ncols; j++)
              {
                R.set_zero(a);
                for (size_t k = 0; k < rank; k++)
                  {
                    R.mult(b, u[k * nrows + i], v[k * ncols + j]);
                    R.add(a, a, b);
                  }
                setEntry(i, j, a);
              }
          return;
        }
      case MatrixShape::Sparse:
        {
          // Bounded redraw: fall back to 1 rather than spin if next() only
          // ever yields zero.
          for (size_t r = 0; r < nrows; r++)
            for (size_t c = 0; c < ncols; c++)
              {
                if (!selectPosition(density)) continue;
                for (int i = 0; i < 100; i++)
                  {
                    next(a);
                    if (!R.is_zero(a)) break;
                  }
                if (R.is_zero(a)) R.set(a, 1);
                setEntry(r, c, a);
              }
          return;
        }
      default:
        for (size_t r = 0; r < nrows; r++)
          for (size_t c = 0; c < ncols; c++)
            if (shapeCoversPosition(shape, r, c))
              {
                next(a);
                setEntry(r, c, a);
              }
        return;
    }
}

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
