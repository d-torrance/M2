// Blocker 4a: std::erase_if.
//
// gfan0.8beta/src/paralleltraversal.cpp:216
//     auto erased=std::erase_if(theSet,[a](StoredArc &A){...});
//
// The uniform container std::erase_if is C++20; libstdc++ has it from GCC 9.
#include <vector>

int main()
{
  std::vector<int> v{1, 2, 3};
  auto erased = std::erase_if(v, [](int x) { return x == 2; });
  return static_cast<int>(erased);
}
