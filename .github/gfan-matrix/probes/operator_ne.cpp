// Blocker 5: operator!= left to C++20's rewritten candidate for operator==.
//
// gfan0.8beta/src/gfanlib_circuittableint.h:320
//     friend bool operator==(CircuitTableIntPOD const &a, CircuitTableIntPOD const &b)
//     {return a.v==b.v;}
// with no operator!=, while the code goes on to write a != b.  Only C++20
// synthesises that, and GCC 9 does not synthesise it even at -std=c++2a.
struct CircuitTableIntPOD
{
  int v;
  friend bool operator==(CircuitTableIntPOD const &a, CircuitTableIntPOD const &b)
  {
    return a.v == b.v;
  }
};

int main()
{
  CircuitTableIntPOD a{1}, b{2};
  return a != b;
}
