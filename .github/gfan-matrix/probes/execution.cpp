// Blocker 1: std::execution::par.
//
// gfan0.8beta/src/log.h line 1 is literally
//     #include <execution> // This header does not like the macros below.
// and log.h is included by 110 translation units, so this header alone decides
// whether the tree compiles at all.  The parallel loops it enables look like
// gfan0.8beta/src/gfanlib_hypersurfaceintersection.cpp:
//     std::for_each(std::execution::par, v.begin(), v.end(), f);
//
// libstdc++ gained <execution> in GCC 9; libc++ has never shipped it.
#include <execution>
#include <algorithm>
#include <vector>

int main()
{
  std::vector<int> v{3, 1, 2};
  std::for_each(std::execution::par, v.begin(), v.end(), [](int &x) { ++x; });
  return v[0];
}
