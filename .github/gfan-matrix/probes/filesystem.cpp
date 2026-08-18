// Blocker 4b: std::filesystem.
//
// gfan0.8beta/src/app_components.cpp:12,172
//     #include <filesystem>
//     std::filesystem::create_directory("POTENTIALS");
//
// GCC 8 has the header but needs -lstdc++fs, which gfan's Makefile never
// passes; GCC 7 has no <filesystem> at all.  This probe therefore *links*,
// so that the GCC 8 case shows up as a failure rather than a pass.
#include <filesystem>

int main()
{
  std::filesystem::create_directory("POTENTIALS");
  return 0;
}
