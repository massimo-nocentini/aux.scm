
#include <chicken/chicken.h>

#ifdef __cplusplus
#include "simdjson.h"
#endif

#ifdef __cplusplus
extern "C"
{
#endif

    enum chicken_simdjson_type
    {
        CHICKEN_SIMDJSON_TYPE_UNKNOWN = 0,
        CHICKEN_SIMDJSON_TYPE_OBJECT,
        CHICKEN_SIMDJSON_TYPE_ARRAY,
        CHICKEN_SIMDJSON_TYPE_STRING,
        CHICKEN_SIMDJSON_TYPE_NULL,
        CHICKEN_SIMDJSON_TYPE_BOOLEAN,
        CHICKEN_SIMDJSON_TYPE_SIGNED_INTEGER,
        CHICKEN_SIMDJSON_TYPE_UNSIGNED_INTEGER,
        CHICKEN_SIMDJSON_TYPE_FLOATING_POINT_NUMBER
    };

    int chicken_simdjson_get_type(void *p);

    int64_t chicken_simdjson_get_signed_integer(void *p);
    uint64_t chicken_simdjson_get_unsigned_integer(void *p);
    double chicken_simdjson_get_floating_point_number(void *p);
    C_word chicken_simdjson_get_boolean(void *p);
    char * chicken_simdjson_get_string(void *p);
    size_t chicken_simdjson_get_array_count_elements(void *p);
    C_word chicken_simdjson_get_array(void *p, C_word mkvector, C_word callback);
    C_word chicken_simdjson_get_object(void *p, C_word mkvector, C_word callback);
    const char *chicken_simdjson_get_raw_json_string(void *p);

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

    C_word chicken_simdjson_load_ondemand_callback(
        const char *filename,
        C_word callback);

    C_word chicken_simdjson_parse_ondemand(
        const char *data,
        size_t length,
        C_word callback_object,
        C_word callback_identity,
        C_word callback_vector,
        C_word callback_list,
        C_word callback_vector_set,
        C_word callback_list_finalize);

    C_word chicken_simdjson_parse_ondemand_callback(
        const char *data,
        size_t length,
        C_word callback);

#ifdef __cplusplus
}
#endif
