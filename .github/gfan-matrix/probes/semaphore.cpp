// Blocker 2: std::counting_semaphore.
//
// gfan0.8beta/src/paralleltraversal.cpp:14  #include <semaphore>
// gfan0.8beta/src/paralleltraversal.cpp:521 std::counting_semaphore<0> S(0);
//
// <semaphore> arrived in GCC 11 and in Xcode 14's libc++.
#include <semaphore>

std::counting_semaphore<0> S(0);

int main() { return 0; }
