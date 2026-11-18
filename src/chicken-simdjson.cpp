
#include <chicken.h>
#include <limits.h>
#include <stdio.h>
#include <string.h>
#include "chicken-simdjson.h"
#include "simdjson.cpp"

using namespace simdjson;

// The binding uses no C++ exceptions: every simdjson call goes through the error codes of its
// simdjson_result, `.get(x)`, and the egg compiles simdjson with SIMDJSON_EXCEPTIONS=0 and
// -fno-exceptions.  So no exception can unwind through CHICKEN's C frames, and the object needs
// nothing of the C++ runtime but the allocation functions, which WebAssembly lacks: CHICKEN's page
// (a -sMAIN_MODULE=2 main module) exports libchicken and part of the C library only, and a side
// module importing anything else cannot be loaded.  There they are defined here, over malloc and
// free.  They are strong definitions: csc-wasm links a side module -Bsymbolic, binding its own
// symbols to itself, which does not hold for weak ones, imported instead; for that reason the egg
// also compiles this file -fvisibility=hidden, for the weak inline functions of simdjson.
#ifdef __EMSCRIPTEN__
#include <new>

void *operator new(size_t n) { return malloc(n ? n : 1); }
void *operator new[](size_t n) { return malloc(n ? n : 1); }
void *operator new(size_t n, const std::nothrow_t &) noexcept { return malloc(n ? n : 1); }
void *operator new[](size_t n, const std::nothrow_t &) noexcept { return malloc(n ? n : 1); }
void operator delete(void *p) noexcept { free(p); }
void operator delete[](void *p) noexcept { free(p); }
void operator delete(void *p, size_t) noexcept { free(p); }
void operator delete[](void *p, size_t) noexcept { free(p); }
namespace std
{
  const nothrow_t nothrow{};
}
extern "C" __attribute__((noreturn)) void __cxa_pure_virtual() { abort(); }
#endif

// A JSON string may hold \u0000, so its length is kept next to its bytes rather than implied by
// the terminating NUL, which is there for the c-string fast path of the getters below.
static chicken_simdjson_string_t *chicken_simdjson_string(const char *chars, size_t length)
{
  chicken_simdjson_string_t *res = (chicken_simdjson_string_t *)malloc(sizeof(chicken_simdjson_string_t) + length + 1);

  res->length = length;
  memcpy(res->chars, chars, length);
  res->chars[length] = '\0';

  return res;
}

// the bytes as a C string, or NULL when a NUL inside them would cut the C string short
static char *chicken_simdjson_c_string(chicken_simdjson_string_t *str)
{
  return memchr(str->chars, '\0', str->length) == NULL ? str->chars : NULL;
}

static chicken_simdjson_value_t *chicken_simdjson_value()
{
  return (chicken_simdjson_value_t *)malloc(sizeof(chicken_simdjson_value_t));
}

// On demand parsing finds malformed input only when the walk reaches it, so an error can come
// from anywhere below.  A node therefore gets its type only once its payload is in place, and
// child arrays are calloc'd, so that chicken_simdjson_free can release a half-built tree before
// the error is returned.  The walk answers the error, and the tree in *out on SUCCESS only.
//
// The walk recurses once per nesting level, and on demand parsing enforces no depth limit of its
// own: some 35000 nested arrays, a valid 70 kB document, would overflow the C stack and kill the
// process with SIGSEGV.  depth counts the arrays and objects enclosing element, and one more level
// than simdjson's DEFAULT_MAX_DEPTH, the limit of its DOM parser, answers DEPTH_ERROR.
//
// The walk starts from the ondemand::document itself, not from the ondemand::value it converts
// to: that conversion fails with SCALAR_DOCUMENT_AS_VALUE when the root is a scalar ("5", "true",
// "null", "\"s\""), which is valid JSON, and a document answers the same getters as a value.
template <typename V>
static error_code chicken_simdjson_visit_ondemand(V &element, size_t depth, chicken_simdjson_dom_element_t **out)
{
  chicken_simdjson_dom_element_t *res = (chicken_simdjson_dom_element_t *)malloc(sizeof(chicken_simdjson_dom_element_t));
  error_code err = SUCCESS;
  ondemand::json_type type;

  res->type = CHICKEN_SIMDJSON_TYPE_UNKNOWN;
  res->value = NULL;

  if ((err = element.type().get(type)))
    goto fail;

  switch (type)
  {
  case ondemand::json_type::array:
  {
    ondemand::array array;
    size_t length;

    if (depth >= DEFAULT_MAX_DEPTH)
    {
      err = DEPTH_ERROR;
      goto fail;
    }

    if ((err = element.get_array().get(array)) || (err = array.count_elements().get(length)))
      goto fail;

    res->value = chicken_simdjson_value();
    res->value->as_array = (chicken_simdjson_many_t *)malloc(sizeof(chicken_simdjson_many_t));
    res->value->as_array->length = length;
    res->value->as_array->keys = NULL;
    res->value->as_array->elements = (chicken_simdjson_dom_element_t **)calloc(length, sizeof(chicken_simdjson_dom_element_t *));
    res->type = CHICKEN_SIMDJSON_TYPE_ARRAY;

    size_t i = 0;
    for (auto child : array)
    {
      ondemand::value child_value;

      if ((err = std::move(child).get(child_value)))
        goto fail;

      // count_elements() only pre-scans the structure: on a malformed document ("[[}],1,2,3")
      // it can count fewer children than the loop meets before simdjson notices, and the
      // array must not be overrun
      if (i >= length)
      {
        err = TAPE_ERROR;
        goto fail;
      }

      if ((err = chicken_simdjson_visit_ondemand(child_value, depth + 1, &res->value->as_array->elements[i])))
        goto fail;

      i++;
    }

    // nor may a slot stay NULL for simdjson->scheme to dereference
    if (i != length)
    {
      err = TAPE_ERROR;
      goto fail;
    }

    break;
  }
  case ondemand::json_type::object:
  {
    ondemand::object obj;
    size_t length;

    if (depth >= DEFAULT_MAX_DEPTH)
    {
      err = DEPTH_ERROR;
      goto fail;
    }

    if ((err = element.get_object().get(obj)) || (err = obj.count_fields().get(length)))
      goto fail;

    res->value = chicken_simdjson_value();
    res->value->as_object = (chicken_simdjson_many_t *)malloc(sizeof(chicken_simdjson_many_t));
    res->value->as_object->length = length;
    res->value->as_object->keys = (chicken_simdjson_string_t **)calloc(length, sizeof(chicken_simdjson_string_t *));
    res->value->as_object->elements = (chicken_simdjson_dom_element_t **)calloc(length, sizeof(chicken_simdjson_dom_element_t *));
    res->type = CHICKEN_SIMDJSON_TYPE_OBJECT;

    size_t i = 0;
    for (auto field_result : obj)
    {
      ondemand::field field;
      std::string_view key;

      if ((err = std::move(field_result).get(field)))
        goto fail;

      // count_fields() pre-scans like count_elements() above
      if (i >= length)
      {
        err = TAPE_ERROR;
        goto fail;
      }

      if ((err = field.unescaped_key().get(key)))
        goto fail;

      res->value->as_object->keys[i] = chicken_simdjson_string(key.data(), key.size());

      if ((err = chicken_simdjson_visit_ondemand(field.value(), depth + 1, &res->value->as_object->elements[i])))
        goto fail;

      i++;
    }

    if (i != length)
    {
      err = TAPE_ERROR;
      goto fail;
    }

    break;
  }
  case ondemand::json_type::number:
  {
    ondemand::number_type number_type;

    if ((err = element.get_number_type().get(number_type)))
      goto fail;

    if (number_type == ondemand::number_type::signed_integer)
    {
      int64_t n;
      if ((err = element.get_int64().get(n)))
        goto fail;
      res->value = chicken_simdjson_value();
      res->value->as_int64 = n;
      res->type = CHICKEN_SIMDJSON_TYPE_SIGNED_INTEGER;
    }
    else if (number_type == ondemand::number_type::unsigned_integer)
    {
      uint64_t n;
      if ((err = element.get_uint64().get(n)))
        goto fail;
      res->value = chicken_simdjson_value();
      res->value->as_uint64 = n;
      res->type = CHICKEN_SIMDJSON_TYPE_UNSIGNED_INTEGER;
    }
    else if (number_type == ondemand::number_type::floating_point_number)
    {
      double n;
      if ((err = element.get_double().get(n)))
        goto fail;
      res->value = chicken_simdjson_value();
      res->value->as_double = n;
      res->type = CHICKEN_SIMDJSON_TYPE_FLOATING_POINT_NUMBER;
    }
    else
    {
      // a number_type::big_integer, beyond 64 bits: valid JSON, but no getter decodes it, and
      // a node of unknown type would only raise a vaguer error from the Scheme side
      err = BIGINT_ERROR;
      goto fail;
    }

    break;
  }
  case ondemand::json_type::string:
  {
    std::string_view str;

    if ((err = element.get_string().get(str)))
      goto fail;

    res->value = chicken_simdjson_value();
    res->value->as_string = chicken_simdjson_string(str.data(), str.size());
    res->type = CHICKEN_SIMDJSON_TYPE_STRING;

    break;
  }
  case ondemand::json_type::boolean:
  {
    bool b;

    if ((err = element.get_bool().get(b)))
      goto fail;

    res->value = chicken_simdjson_value();
    res->value->as_boolean = b ? 1 : 0;
    res->type = CHICKEN_SIMDJSON_TYPE_BOOLEAN;
    break;
  }
  case ondemand::json_type::null:
  {
    bool null;

    // type() looks at the first character only, is_null() checks the whole literal
    if ((err = element.is_null().get(null)))
      goto fail;

    if (!null)
    {
      err = N_ATOM_ERROR;
      goto fail;
    }

    res->type = CHICKEN_SIMDJSON_TYPE_NULL;
    break;
  }
  default:
    // json_type::unknown: the value starts with a character no JSON value starts with ("x",
    // "]", ",", "@"), which simdjson reports only by this type, not by an error of its own.
    err = TAPE_ERROR;
    goto fail;
  }

  *out = res;
  return SUCCESS;

fail:
  chicken_simdjson_free(res);
  return err;
}

// What a parse returns instead of a tree on an error.  Its message, the one simdjson_error::what()
// gives, crosses the FFI as data, and simdjson->scheme turns it into a Scheme error.
static chicken_simdjson_dom_element_t *chicken_simdjson_error(error_code err)
{
  const char *message = error_message(err);
  chicken_simdjson_dom_element_t *res = (chicken_simdjson_dom_element_t *)malloc(sizeof(chicken_simdjson_dom_element_t));

  res->value = chicken_simdjson_value();
  res->value->as_string = chicken_simdjson_string(message, strlen(message));
  res->type = CHICKEN_SIMDJSON_TYPE_ERROR;

  return res;
}

extern chicken_simdjson_type_t chicken_simdjson_get_type(chicken_simdjson_dom_element_t *element)
{
  return element->type;
}

extern char *chicken_simdjson_get_string(chicken_simdjson_dom_element_t *element)
{
  return chicken_simdjson_c_string(element->value->as_string);
}

extern size_t chicken_simdjson_get_string_length(chicken_simdjson_dom_element_t *element)
{
  return element->value->as_string->length;
}

extern void chicken_simdjson_get_string_copy(chicken_simdjson_dom_element_t *element, unsigned char *to)
{
  memcpy(to, element->value->as_string->chars, element->value->as_string->length);
}

extern int64_t chicken_simdjson_get_signed_integer(chicken_simdjson_dom_element_t *element)
{
  return element->value->as_int64;
}

extern uint64_t chicken_simdjson_get_unsigned_integer(chicken_simdjson_dom_element_t *element)
{
  return element->value->as_uint64;
}

extern double chicken_simdjson_get_floating_point_number(chicken_simdjson_dom_element_t *element)
{
  return element->value->as_double;
}

extern C_word chicken_simdjson_get_boolean(chicken_simdjson_dom_element_t *element)
{
  return element->value->as_boolean ? C_SCHEME_TRUE : C_SCHEME_FALSE;
}

extern size_t chicken_simdjson_get_array_length(chicken_simdjson_dom_element_t *element)
{
  return element->value->as_array->length;
}

extern chicken_simdjson_dom_element_t *chicken_simdjson_get_array_ref(chicken_simdjson_dom_element_t *element, size_t index)
{
  return element->value->as_array->elements[index];
}

extern size_t chicken_simdjson_get_object_length(chicken_simdjson_dom_element_t *element)
{
  return element->value->as_object->length;
}

extern char *chicken_simdjson_get_object_ref_key(chicken_simdjson_dom_element_t *element, size_t index)
{
  return chicken_simdjson_c_string(element->value->as_object->keys[index]);
}

extern size_t chicken_simdjson_get_object_ref_key_length(chicken_simdjson_dom_element_t *element, size_t index)
{
  return element->value->as_object->keys[index]->length;
}

extern void chicken_simdjson_get_object_ref_key_copy(chicken_simdjson_dom_element_t *element, size_t index, unsigned char *to)
{
  memcpy(to, element->value->as_object->keys[index]->chars, element->value->as_object->keys[index]->length);
}

extern chicken_simdjson_dom_element_t *chicken_simdjson_get_object_ref_value(chicken_simdjson_dom_element_t *element, size_t index)
{
  return element->value->as_object->elements[index];
}

// Walks the whole document.  On demand parsing looks past the root value only when asked: a
// scalar getter on the document does, but after an array or an object "[1][2]", "[1]]" or
// "{\"a\":1} {\"b\":2}" would silently decode as their first value.
static chicken_simdjson_dom_element_t *chicken_simdjson_visit_document(ondemand::parser &parser, padded_string &json)
{
  ondemand::document ddoc;
  chicken_simdjson_dom_element_t *res = NULL;
  error_code err;

  if ((err = parser.iterate(json).get(ddoc)) || (err = chicken_simdjson_visit_ondemand(ddoc, 0, &res)))
    return chicken_simdjson_error(err);

  if (!ddoc.at_end())
  {
    chicken_simdjson_free(res);
    return chicken_simdjson_error(TRAILING_CONTENT);
  }

  return res;
}

// padded_string::load(filename), whose errors it answers, but on a C string: that one copies its
// string_view into a std::string, for the NUL fopen needs, which is the only code here that would
// need libc++ (its std::string members are compiled into the library, not inline).
static error_code chicken_simdjson_load(const char *filename, padded_string &out)
{
  FILE *fp = fopen(filename, "rb");
  long llen;

  if (fp == NULL)
    return IO_ERROR;

  if (fseek(fp, 0, SEEK_END) < 0 || (llen = ftell(fp)) < 0 || llen == LONG_MAX)
  {
    fclose(fp);
    return IO_ERROR;
  }

  size_t len = (size_t)llen;
  padded_string s(len);

  if (s.data() == NULL)
  {
    fclose(fp);
    return MEMALLOC;
  }

  rewind(fp);
  size_t bytes_read = fread(s.data(), 1, len, fp);

  if (fclose(fp) != 0 || bytes_read != len)
    return IO_ERROR;

  out = std::move(s);
  return SUCCESS;
}

extern chicken_simdjson_dom_element_t *chicken_simdjson_load_ondemand_callback(const char *filename)
{
  ondemand::parser parser;
  padded_string json;
  error_code err;

  if ((err = chicken_simdjson_load(filename, json)))
    return chicken_simdjson_error(err);

  return chicken_simdjson_visit_document(parser, json);
}

extern chicken_simdjson_dom_element_t *chicken_simdjson_parse_ondemand_callback(const char *data, size_t length)
{
  ondemand::parser parser;
  padded_string json(data, length);

  return chicken_simdjson_visit_document(parser, json);
}

extern void chicken_simdjson_free(chicken_simdjson_dom_element_t *element)
{
  if (element == NULL)
    return;

  switch (element->type)
  {
  case CHICKEN_SIMDJSON_TYPE_ARRAY:
  {
    for (size_t i = 0; i < element->value->as_array->length; i++)
    {
      chicken_simdjson_free(element->value->as_array->elements[i]);
    }
    free(element->value->as_array->elements);
    free(element->value->as_array);
    break;
  }
  case CHICKEN_SIMDJSON_TYPE_OBJECT:
  {
    for (size_t i = 0; i < element->value->as_object->length; i++)
    {
      free(element->value->as_object->keys[i]);
      chicken_simdjson_free(element->value->as_object->elements[i]);
    }
    free(element->value->as_object->keys);
    free(element->value->as_object->elements);
    free(element->value->as_object);
    break;
  }
  case CHICKEN_SIMDJSON_TYPE_STRING:
  case CHICKEN_SIMDJSON_TYPE_ERROR:
  {
    free(element->value->as_string);
    break;
  }
  default:
    break;
  }

  free(element->value);
  free(element);
}

extern const char *chicken_simdjson_get_version()
{
  return SIMDJSON_VERSION;
}