#include "Word.hpp"
#include <iostream>

std::ostream& operator<<(std::ostream& o, const Word& w)
{
  o << "[";
  int i = 0;
  for (; i < w.size() - 1; ++i)
    {
      o << w.begin()[i] << ",";
    }
  o << w.begin()[i];
  o << "]";
  return o;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
