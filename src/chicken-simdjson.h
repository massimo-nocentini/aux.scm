
#include <chicken/chicken.h>

#ifdef __cplusplus
#include "simdjson.h"
#endif

#ifdef __cplusplus
extern "C"
{
#endif

    C_word parse_json(C_word C_k, const char *filename, C_word l, C_word p);
    // extern C_word c(C_word l);

#ifdef __cplusplus
}
#endif
