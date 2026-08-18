// Blocker 6: a multimap default-constructing a lambda comparator.
//
// gfan0.8beta/src/app_hashemikapur.cpp:47-56
//     auto cmp=[](IntegerVector const &a, IntegerVector const &b){...};
//     multimap<IntegerVector,pair<int,int>,decltype(cmp)> P;
//
// The default constructor of std::multimap value-initialises the comparator,
// and closure types only became default-constructible in C++20.  GCC 7 and
// GCC 8 reject this outright.
#include <map>
#include <utility>

int main()
{
  auto cmp = [](int a, int b) { return a < b; };
  std::multimap<int, std::pair<int, int>, decltype(cmp)> P;
  P.insert({1, {2, 3}});
  return static_cast<int>(P.size()) - 1;
}
