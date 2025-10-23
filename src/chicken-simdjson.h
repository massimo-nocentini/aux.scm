
#include <chicken/chicken.h>

#ifdef __cplusplus
#include "simdjson.h"
#endif

#ifdef __cplusplus
extern "C"
{
#endif

    typedef enum chicken_simdjson_type_e
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
    } chicken_simdjson_type_t;

    typedef struct chicken_simdjson_dom_element_s chicken_simdjson_dom_element_t;

    typedef struct chicken_simdjson_many_s
    {
        size_t length;
        char **keys;
        chicken_simdjson_dom_element_t **elements;
    } chicken_simdjson_many_t;

    typedef union chicken_simdjson_dom_element_u
    {
        int64_t as_int64;
        uint64_t as_uint64;
        double as_double;
        int as_boolean;
        char *as_string;
        chicken_simdjson_many_t *as_array;
        chicken_simdjson_many_t *as_object;

    } chicken_simdjson_value_t;

    struct chicken_simdjson_dom_element_s
    {
        chicken_simdjson_value_t *value;
        chicken_simdjson_type_t type;
    };

    chicken_simdjson_type_t chicken_simdjson_get_type(chicken_simdjson_dom_element_t *element);

    int64_t chicken_simdjson_get_signed_integer(chicken_simdjson_dom_element_t *element);
    uint64_t chicken_simdjson_get_unsigned_integer(chicken_simdjson_dom_element_t *element);
    double chicken_simdjson_get_floating_point_number(chicken_simdjson_dom_element_t *element);
    C_word chicken_simdjson_get_boolean(chicken_simdjson_dom_element_t *element);
    char *chicken_simdjson_get_string(chicken_simdjson_dom_element_t *element);
    size_t chicken_simdjson_get_array_length(chicken_simdjson_dom_element_t *element);
    chicken_simdjson_dom_element_t * chicken_simdjson_get_array_ref(chicken_simdjson_dom_element_t *element, size_t i);
    size_t chicken_simdjson_get_object_length(chicken_simdjson_dom_element_t *element);
    char *chicken_simdjson_get_object_ref_key(chicken_simdjson_dom_element_t *element, size_t i);
    chicken_simdjson_dom_element_t * chicken_simdjson_get_object_ref_value(chicken_simdjson_dom_element_t *element, size_t i);
    
    chicken_simdjson_dom_element_t *chicken_simdjson_load_ondemand_callback(const char *filename);
    chicken_simdjson_dom_element_t *chicken_simdjson_parse_ondemand_callback(const char *data, size_t length);

#ifdef __cplusplus
}
#endif
