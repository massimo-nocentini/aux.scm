
#include <chicken/chicken.h>
#include <string.h>
#include <iostream>
#include "chicken-simdjson.h"
#include "simdjson.cpp"

using namespace std;
using namespace simdjson;

chicken_simdjson_dom_element_t *chicken_simdjson_visit_ondemand(ondemand::value element)
{
  chicken_simdjson_dom_element_t *res = (chicken_simdjson_dom_element_t *)malloc(sizeof(chicken_simdjson_dom_element_t));

  res->type = CHICKEN_SIMDJSON_TYPE_UNKNOWN;
  res->value = NULL;

  switch (element.type())
  {
  case ondemand::json_type::array:
  {
    auto array = element.get_array();
    size_t length = array.count_elements();

    res->type = CHICKEN_SIMDJSON_TYPE_ARRAY;
    res->value = (chicken_simdjson_value_t *)malloc(sizeof(chicken_simdjson_value_t));
    res->value->as_array = (chicken_simdjson_many_t *)malloc(sizeof(chicken_simdjson_many_t));
    res->value->as_array->length = length;
    res->value->as_array->keys = NULL;
    res->value->as_array->elements = (chicken_simdjson_dom_element_t **)malloc(sizeof(chicken_simdjson_dom_element_t *) * length);

    size_t i = 0;
    for (auto child : array)
    {
      res->value->as_array->elements[i] = chicken_simdjson_visit_ondemand(child.value());

      i++;
    }

    break;
  }
  case ondemand::json_type::object:
  {

    auto obj = element.get_object();
    size_t length = obj.count_fields();

    res->type = CHICKEN_SIMDJSON_TYPE_OBJECT;
    res->value = (chicken_simdjson_value_t *)malloc(sizeof(chicken_simdjson_value_t));
    res->value->as_object = (chicken_simdjson_many_t *)malloc(sizeof(chicken_simdjson_many_t));
    res->value->as_object->length = length;
    res->value->as_object->keys = (char **)malloc(sizeof(char *) * length);
    res->value->as_object->elements = (chicken_simdjson_dom_element_t **)malloc(sizeof(chicken_simdjson_dom_element_t *) * length);

    size_t i = 0;
    for (auto field : obj)
    {

      auto raw = field.unescaped_key();
      size_t key_length = raw->length();
      res->value->as_object->keys[i] = (char *)malloc(key_length + 1);
      raw->copy(res->value->as_object->keys[i], key_length);
      res->value->as_object->keys[i][key_length] = '\0';

      res->value->as_object->elements[i] = chicken_simdjson_visit_ondemand(field.value());

      i++;
    }

    break;
  }
  case ondemand::json_type::number:
  {
    if (element.get_number_type() == ondemand::number_type::signed_integer)
    {
      res->type = CHICKEN_SIMDJSON_TYPE_SIGNED_INTEGER;
      res->value = (chicken_simdjson_value_t *)malloc(sizeof(chicken_simdjson_value_t));
      res->value->as_int64 = element.get_int64();
    }
    else if (element.get_number_type() == ondemand::number_type::unsigned_integer)
    {
      res->type = CHICKEN_SIMDJSON_TYPE_UNSIGNED_INTEGER;
      res->value = (chicken_simdjson_value_t *)malloc(sizeof(chicken_simdjson_value_t));
      res->value->as_uint64 = element.get_uint64();
    }
    else if (element.get_number_type() == ondemand::number_type::floating_point_number)
    {
      res->type = CHICKEN_SIMDJSON_TYPE_FLOATING_POINT_NUMBER;
      res->value = (chicken_simdjson_value_t *)malloc(sizeof(chicken_simdjson_value_t));
      res->value->as_double = element.get_double();
    }

    break;
  }
  case ondemand::json_type::string:
  {

    auto str = element.get_string();
    size_t length = str->size();

    res->type = CHICKEN_SIMDJSON_TYPE_STRING;
    res->value = (chicken_simdjson_value_t *)malloc(sizeof(chicken_simdjson_value_t));
    res->value->as_string = (char *)malloc(length + 1);
    str->copy(res->value->as_string, length);
    res->value->as_string[length] = '\0';

    break;
  }
  case ondemand::json_type::boolean:
  {

    res->type = CHICKEN_SIMDJSON_TYPE_BOOLEAN;
    res->value = (chicken_simdjson_value_t *)malloc(sizeof(chicken_simdjson_value_t));
    res->value->as_boolean = element.get_bool() ? 1 : 0;
    break;
  }
  case ondemand::json_type::null:
  {
    res->type = CHICKEN_SIMDJSON_TYPE_NULL;
    break;
  }
  default:
    break;
  }

  return res;
}

extern chicken_simdjson_type_t chicken_simdjson_get_type(chicken_simdjson_dom_element_t *element)
{
  return element->type;
}

extern char *chicken_simdjson_get_string(chicken_simdjson_dom_element_t *element)
{
  return element->value->as_string;
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
  return element->value->as_object->keys[index];
}

extern chicken_simdjson_dom_element_t *chicken_simdjson_get_object_ref_value(chicken_simdjson_dom_element_t *element, size_t index)
{
  return element->value->as_object->elements[index];
}

extern chicken_simdjson_dom_element_t *chicken_simdjson_load_ondemand_callback(const char *filename)
{
  ondemand::parser parser;
  auto json = padded_string::load(filename);
  ondemand::document ddoc = parser.iterate(json);

  chicken_simdjson_dom_element_t *doc = chicken_simdjson_visit_ondemand(ddoc);

  return doc;
}

extern chicken_simdjson_dom_element_t *chicken_simdjson_parse_ondemand_callback(const char *data, size_t length)
{
  ondemand::parser parser;
  simdjson::padded_string my_padded_data(data, length);
  ondemand::document ddoc = parser.iterate(my_padded_data);

  chicken_simdjson_dom_element_t *doc = chicken_simdjson_visit_ondemand(ddoc);

  return doc;
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