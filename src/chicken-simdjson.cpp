
#include <chicken/chicken.h>
#include <string.h>
#include <iostream>
#include "chicken-simdjson.h"
#include "simdjson.cpp"

using namespace std;
using namespace simdjson;

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

// On demand parsing finds malformed input only when the walk reaches it, so a simdjson_error can
// be thrown from anywhere below.  A node therefore gets its type only once its payload is in
// place, and child arrays are calloc'd, so that chicken_simdjson_free can release a half-built
// tree before the exception is rethrown.
//
// The walk recurses once per nesting level, and on demand parsing enforces no depth limit of its
// own: some 35000 nested arrays, a valid 70 kB document, would overflow the C stack and kill the
// process with SIGSEGV.  depth counts the arrays and objects enclosing element, and one more level
// than simdjson's DEFAULT_MAX_DEPTH, the limit of its DOM parser, raises DEPTH_ERROR.
//
// The walk starts from the ondemand::document itself, not from the ondemand::value it converts
// to: that conversion throws SCALAR_DOCUMENT_AS_VALUE when the root is a scalar ("5", "true",
// "null", "\"s\""), which is valid JSON, and a document answers the same getters as a value.
template <typename V>
static chicken_simdjson_dom_element_t *chicken_simdjson_visit_ondemand(V &element, size_t depth)
{
  chicken_simdjson_dom_element_t *res = (chicken_simdjson_dom_element_t *)malloc(sizeof(chicken_simdjson_dom_element_t));

  res->type = CHICKEN_SIMDJSON_TYPE_UNKNOWN;
  res->value = NULL;

  try
  {
    switch (element.type())
    {
    case ondemand::json_type::array:
    {
      if (depth >= DEFAULT_MAX_DEPTH)
        throw simdjson_error(DEPTH_ERROR);

      auto array = element.get_array();
      size_t length = array.count_elements();

      res->value = (chicken_simdjson_value_t *)malloc(sizeof(chicken_simdjson_value_t));
      res->value->as_array = (chicken_simdjson_many_t *)malloc(sizeof(chicken_simdjson_many_t));
      res->value->as_array->length = length;
      res->value->as_array->keys = NULL;
      res->value->as_array->elements = (chicken_simdjson_dom_element_t **)calloc(length, sizeof(chicken_simdjson_dom_element_t *));
      res->type = CHICKEN_SIMDJSON_TYPE_ARRAY;

      size_t i = 0;
      for (auto child : array)
      {
        ondemand::value child_value = child.value();

        // count_elements() only pre-scans the structure: on a malformed document ("[[}],1,2,3")
        // it can count fewer children than the loop meets before simdjson notices, and the
        // array must not be overrun
        if (i >= length)
          throw simdjson_error(TAPE_ERROR);

        res->value->as_array->elements[i] = chicken_simdjson_visit_ondemand(child_value, depth + 1);

        i++;
      }

      // nor may a slot stay NULL for simdjson->scheme to dereference
      if (i != length)
        throw simdjson_error(TAPE_ERROR);

      break;
    }
    case ondemand::json_type::object:
    {
      if (depth >= DEFAULT_MAX_DEPTH)
        throw simdjson_error(DEPTH_ERROR);

      auto obj = element.get_object();
      size_t length = obj.count_fields();

      res->value = (chicken_simdjson_value_t *)malloc(sizeof(chicken_simdjson_value_t));
      res->value->as_object = (chicken_simdjson_many_t *)malloc(sizeof(chicken_simdjson_many_t));
      res->value->as_object->length = length;
      res->value->as_object->keys = (chicken_simdjson_string_t **)calloc(length, sizeof(chicken_simdjson_string_t *));
      res->value->as_object->elements = (chicken_simdjson_dom_element_t **)calloc(length, sizeof(chicken_simdjson_dom_element_t *));
      res->type = CHICKEN_SIMDJSON_TYPE_OBJECT;

      size_t i = 0;
      for (auto field : obj)
      {
        // count_fields() pre-scans like count_elements() above
        if (i >= length)
          throw simdjson_error(TAPE_ERROR);

        std::string_view key = field.unescaped_key();
        res->value->as_object->keys[i] = chicken_simdjson_string(key.data(), key.size());

        ondemand::value field_value = field.value();
        res->value->as_object->elements[i] = chicken_simdjson_visit_ondemand(field_value, depth + 1);

        i++;
      }

      if (i != length)
        throw simdjson_error(TAPE_ERROR);

      break;
    }
    case ondemand::json_type::number:
    {
      ondemand::number_type number_type = element.get_number_type();

      if (number_type == ondemand::number_type::signed_integer)
      {
        int64_t n = element.get_int64();
        res->value = (chicken_simdjson_value_t *)malloc(sizeof(chicken_simdjson_value_t));
        res->value->as_int64 = n;
        res->type = CHICKEN_SIMDJSON_TYPE_SIGNED_INTEGER;
      }
      else if (number_type == ondemand::number_type::unsigned_integer)
      {
        uint64_t n = element.get_uint64();
        res->value = (chicken_simdjson_value_t *)malloc(sizeof(chicken_simdjson_value_t));
        res->value->as_uint64 = n;
        res->type = CHICKEN_SIMDJSON_TYPE_UNSIGNED_INTEGER;
      }
      else if (number_type == ondemand::number_type::floating_point_number)
      {
        double n = element.get_double();
        res->value = (chicken_simdjson_value_t *)malloc(sizeof(chicken_simdjson_value_t));
        res->value->as_double = n;
        res->type = CHICKEN_SIMDJSON_TYPE_FLOATING_POINT_NUMBER;
      }
      else
        // a number_type::big_integer, beyond 64 bits: valid JSON, but no getter decodes it, and
        // a node of unknown type would only raise a vaguer error from the Scheme side
        throw simdjson_error(BIGINT_ERROR);

      break;
    }
    case ondemand::json_type::string:
    {

      std::string_view str = element.get_string();

      res->value = (chicken_simdjson_value_t *)malloc(sizeof(chicken_simdjson_value_t));
      res->value->as_string = chicken_simdjson_string(str.data(), str.size());
      res->type = CHICKEN_SIMDJSON_TYPE_STRING;

      break;
    }
    case ondemand::json_type::boolean:
    {

      bool b = element.get_bool();
      res->value = (chicken_simdjson_value_t *)malloc(sizeof(chicken_simdjson_value_t));
      res->value->as_boolean = b ? 1 : 0;
      res->type = CHICKEN_SIMDJSON_TYPE_BOOLEAN;
      break;
    }
    case ondemand::json_type::null:
    {
      // type() looks at the first character only, is_null() checks the whole literal
      if (!element.is_null())
        throw simdjson_error(N_ATOM_ERROR);

      res->type = CHICKEN_SIMDJSON_TYPE_NULL;
      break;
    }
    default:
      // json_type::unknown: the value starts with a character no JSON value starts with ("x",
      // "]", ",", "@"), which simdjson reports only by this type, not by an error of its own.
      throw simdjson_error(TAPE_ERROR);
    }
  }
  catch (...)
  {
    chicken_simdjson_free(res);
    throw;
  }

  return res;
}

// What a parse returns instead of a tree when simdjson throws.  The exception must not unwind
// through CHICKEN's C frames -- std::terminate would abort the whole process, and no Scheme
// handler could catch it -- so its message crosses the FFI as data, and simdjson->scheme turns
// it into a Scheme error.
static chicken_simdjson_dom_element_t *chicken_simdjson_error(const char *message)
{
  chicken_simdjson_dom_element_t *res = (chicken_simdjson_dom_element_t *)malloc(sizeof(chicken_simdjson_dom_element_t));

  res->value = (chicken_simdjson_value_t *)malloc(sizeof(chicken_simdjson_value_t));
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
static chicken_simdjson_dom_element_t *chicken_simdjson_visit_document(ondemand::document &ddoc)
{
  chicken_simdjson_dom_element_t *res = chicken_simdjson_visit_ondemand(ddoc, 0);

  if (!ddoc.at_end())
  {
    chicken_simdjson_free(res);
    throw simdjson_error(TRAILING_CONTENT);
  }

  return res;
}

extern chicken_simdjson_dom_element_t *chicken_simdjson_load_ondemand_callback(const char *filename)
{
  try
  {
    ondemand::parser parser;
    auto json = padded_string::load(filename);
    ondemand::document ddoc = parser.iterate(json);

    return chicken_simdjson_visit_document(ddoc);
  }
  catch (const std::exception &e)
  {
    return chicken_simdjson_error(e.what());
  }
}

extern chicken_simdjson_dom_element_t *chicken_simdjson_parse_ondemand_callback(const char *data, size_t length)
{
  try
  {
    ondemand::parser parser;
    simdjson::padded_string my_padded_data(data, length);
    ondemand::document ddoc = parser.iterate(my_padded_data);

    return chicken_simdjson_visit_document(ddoc);
  }
  catch (const std::exception &e)
  {
    return chicken_simdjson_error(e.what());
  }
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