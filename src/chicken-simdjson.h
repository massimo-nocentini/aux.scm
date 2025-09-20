
#include <chicken/chicken.h>

#ifdef __cplusplus
#include "simdjson.h"
#endif

#ifdef __cplusplus
extern "C"
{
#endif

    void parse_json(C_word C_k, const char *filename);

#ifdef __cplusplus
}
#endif
