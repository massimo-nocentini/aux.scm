
// The amalgamation of simdutf (`make simdutf-fetch`) compiled in this object, together with its C API
// (simdutf_c.h, defined at the end of simdutf.cpp) and the entry points of chicken-simdutf.h.

#include "simdutf.cpp"
#include "chicken-simdutf.h"

extern "C"
{
  const char *chicken_simdutf_get_version(void)
  {
    return SIMDUTF_VERSION;
  }

  // the name is a string_view over a string literal of the library, hence NUL-terminated
  const char *chicken_simdutf_get_implementation(void)
  {
    return simdutf::get_active_implementation()->name().data();
  }

  size_t chicken_simdutf_utf16_length_from_utf32(const char32_t *input, size_t length)
  {
    return simdutf::utf16_length_from_utf32(input, length);
  }
}
