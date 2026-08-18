// Blocker 3: std::pmr, woven through gfan's core matrix and vector types.
//
// gfan0.8beta/src/gfanlib_memoryresource.h:9,45,46,79
//     #include <memory_resource>
//     template<class a> using pmrvector = std::pmr::vector<a>;
//     typedef std::pmr::memory_resource MR;
//     class StackResource: public std::pmr::memory_resource { ... };
//
// libstdc++ gained <memory_resource> in GCC 9.  The Library Fundamentals TS
// version it came from, <experimental/memory_resource>, has been there since
// GCC 6 -- which is what the patch falls back to.
#include <memory_resource>
#include <vector>

template<class a> using pmrvector = std::pmr::vector<a>;

class StackResource : public std::pmr::memory_resource
{
  void *do_allocate(std::size_t n, std::size_t) override { return ::operator new(n); }
  void do_deallocate(void *p, std::size_t, std::size_t) override { ::operator delete(p); }
  bool do_is_equal(const std::pmr::memory_resource &o) const noexcept override
  {
    return this == &o;
  }
};

int main()
{
  StackResource r;
  pmrvector<int> v(4, 0, &r);
  return v[0];
}
