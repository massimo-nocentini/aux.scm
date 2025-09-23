
#include <chicken/chicken.h>

#ifdef __cplusplus
#include "simdjson.h"
#endif

#ifdef __cplusplus
extern "C"
{
#endif

    C_word chicken_simdjson_load(
        const char *filename,
        C_word callback_object,
        C_word callback_int64,
        C_word callback_vector,
        C_word callback_list,
        C_word callback_vector_set,
        C_word callback_list_finalize);

    C_word chicken_simdjson_load_ondemand(
        const char *filename,
        C_word callback_object,
        C_word callback_identity,
        C_word callback_vector,
        C_word callback_list,
        C_word callback_vector_set,
        C_word callback_list_finalize);

#ifdef __cplusplus
}
#endif
