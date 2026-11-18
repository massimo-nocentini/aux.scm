/* The C side of (aux simdutf): simdutf's own C API, simdutf_c.h, which ships with its single-header
   release, plus the few entry points that API lacks, implemented in chicken-simdutf.cpp over the C++
   API of the amalgamation. */

#include "simdutf_c.h"

#ifdef __cplusplus
extern "C"
{
#endif

    /* SIMDUTF_VERSION, as "9.4.0" */
    const char *chicken_simdutf_get_version(void);

    /* the kernel simdutf picked for this CPU at run time: "icelake", "haswell", "arm64", "fallback", ... */
    const char *chicken_simdutf_get_implementation(void);

    /* the UTF-16 code units that valid UTF-32 transcodes to: simdutf_c.h has no such estimator */
    size_t chicken_simdutf_utf16_length_from_utf32(const char32_t *input, size_t length);

#ifdef __cplusplus
}
#endif
