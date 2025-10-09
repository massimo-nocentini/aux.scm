
#include <chicken/chicken.h>
#include <string.h>
#include <iostream>
#include "chicken-simdjson.h"
#include "simdjson.h"

using namespace std;
using namespace simdjson;

C_word chicken_simdjson_visit(
    dom::element element,
    C_word callback_object,
    C_word callback_identity,
    C_word callback_vector,
    C_word callback_list,
    C_word callback_vector_set,
    C_word callback_list_finalize)
{
  C_word res = C_SCHEME_UNDEFINED;
  C_word tmp = C_SCHEME_UNDEFINED;
  C_word *ptr = NULL;

  switch (element.type())
  {
  case dom::element_type::ARRAY:
  {
    dom::array array = dom::array(element);
    size_t length = array.size();

    C_save(C_fix(length));
    res = C_callback(callback_vector, 1);

    size_t i = 0;
    for (dom::element child : array)
    {

      tmp = chicken_simdjson_visit(
          child,
          callback_object,
          callback_identity,
          callback_vector,
          callback_list,
          callback_vector_set,
          callback_list_finalize);

      C_save(tmp);
      C_save(C_fix(i));
      C_save(res);

      res = C_callback(callback_vector_set, 3);

      i++;
    }

    break;
  }
  case dom::element_type::OBJECT:
  {

    dom::object obj = dom::object(element);

    res = C_SCHEME_END_OF_LIST;

    for (dom::key_value_pair field : obj)
    {
      tmp = chicken_simdjson_visit(
          field.value,
          callback_object,
          callback_identity,
          callback_vector,
          callback_list,
          callback_vector_set,
          callback_list_finalize);

      size_t length = field.key.size();
      ptr = C_alloc(C_SIZEOF_INTERNED_SYMBOL(length));

      C_save(tmp);
      C_save(C_intern(&ptr, length, (char *)field.key.data()));
      tmp = C_callback(callback_object, 2);

      C_save(res);
      C_save(tmp);
      res = C_callback(callback_list, 2);
    }

    C_save(res);
    res = C_callback(callback_list_finalize, 1);

    break;
  }
  case dom::element_type::INT64:
  {
    res = C_fix(element.get_int64());
    C_save(res);
    res = C_callback(callback_identity, 1);
    break;
  }
  case dom::element_type::UINT64:
  {
    ptr = C_alloc(11); // taken from the upstream Chicken source code.
    res = C_uint64_to_num(&ptr, element.get_uint64());
    C_save(res);
    res = C_callback(callback_identity, 1);
    break;
  }
  case dom::element_type::DOUBLE:
  {
    ptr = C_alloc(C_SIZEOF_FLONUM);
    res = C_flonum(&ptr, element.get_double());
    C_save(res);
    res = C_callback(callback_identity, 1);
    break;
  }
  case dom::element_type::STRING:
  {
    const char *str = element.get_c_str();
    size_t length = element.get_string_length();
    ptr = C_alloc(C_SIZEOF_STRING(length));
    res = C_string(&ptr, length, (char *)str);
    C_save(res);
    res = C_callback(callback_identity, 1);
    break;
  }
  case dom::element_type::BOOL:
  {
    res = element.get_bool() ? C_SCHEME_TRUE : C_SCHEME_FALSE;
    break;
  }
  case dom::element_type::NULL_VALUE:
  {
    break;
  }
  default:
    break;
  }

  C_return(res);
}

extern C_word chicken_simdjson_load(
    const char *filename,
    C_word callback_object,
    C_word callback_identity,
    C_word callback_vector,
    C_word callback_list,
    C_word callback_vector_set,
    C_word callback_list_finalize)
{
  simdjson::dom::parser parser;
  simdjson::dom::element tweets = parser.load(filename);
  C_return(chicken_simdjson_visit(
      tweets,
      callback_object,
      callback_identity,
      callback_vector,
      callback_list,
      callback_vector_set,
      callback_list_finalize));
}

C_word chicken_simdjson_visit_ondemand(
    ondemand::value element,
    C_word callback_object,
    C_word callback_identity,
    C_word callback_vector,
    C_word callback_list,
    C_word callback_vector_set,
    C_word callback_list_finalize)
{
  C_word res = C_SCHEME_UNDEFINED;
  C_word tmp = C_SCHEME_UNDEFINED;
  C_word *ptr = NULL;

  switch (element.type())
  {
  case ondemand::json_type::array:
  {
    auto array = element.get_array();
    size_t length = array.count_elements();

    C_save(C_fix(length));
    res = C_callback(callback_vector, 1);

    size_t i = 0;
    for (auto child : array)
    {

      tmp = chicken_simdjson_visit_ondemand(
          child.value(),
          callback_object,
          callback_identity,
          callback_vector,
          callback_list,
          callback_vector_set,
          callback_list_finalize);

      C_save(tmp);
      C_save(C_fix(i));
      C_save(res);

      res = C_callback(callback_vector_set, 3);

      i++;
    }

    break;
  }
  case ondemand::json_type::object:
  {

    auto obj = element.get_object();

    res = C_SCHEME_END_OF_LIST;

    for (auto field : obj)
    {
      tmp = chicken_simdjson_visit_ondemand(
          field.value(),
          callback_object,
          callback_identity,
          callback_vector,
          callback_list,
          callback_vector_set,
          callback_list_finalize);

      auto raw = field.unescaped_key().value();
      size_t key_length = raw.length();
      ptr = C_alloc(C_SIZEOF_INTERNED_SYMBOL(key_length));

      C_save(tmp);
      C_save(C_intern(&ptr, key_length, (char *)raw.data()));
      tmp = C_callback(callback_object, 2);

      C_save(res);
      C_save(tmp);
      res = C_callback(callback_list, 2);
    }

    C_save(res);
    res = C_callback(callback_list_finalize, 1);

    break;
  }
  case ondemand::json_type::number:
  {
    if (element.get_number_type() == ondemand::number_type::signed_integer)
    {
      res = C_fix(element.get_int64());
      C_save(res);
      res = C_callback(callback_identity, 1);
    }
    else if (element.get_number_type() == ondemand::number_type::unsigned_integer)
    {
      ptr = C_alloc(11); // taken from the upstream Chicken source code.
      res = C_uint64_to_num(&ptr, element.get_uint64());

      C_save(res);
      res = C_callback(callback_identity, 1);
    }
    else if (element.get_number_type() == ondemand::number_type::floating_point_number)
    {
      ptr = C_alloc(C_SIZEOF_FLONUM);
      res = C_flonum(&ptr, element.get_double());

      C_save(res);
      res = C_callback(callback_identity, 1);
    }

    break;
  }
  case ondemand::json_type::string:
  {
    auto str = element.get_string().value();
    size_t length = str.size();
    ptr = C_alloc(C_SIZEOF_STRING(length));
    res = C_string(&ptr, length, (char *)str.data());

    C_save(res);
    res = C_callback(callback_identity, 1);
    break;
  }
  case ondemand::json_type::boolean:
  {
    res = element.get_bool() ? C_SCHEME_TRUE : C_SCHEME_FALSE;
    break;
  }
  case ondemand::json_type::null:
  {
    break;
  }
  default:
    break;
  }

  C_return(res);
}

/*
extern C_word chicken_simdjson_is_unknown(void *p)
{
  auto element = (ondemand::value *)p;
  return element->type() == ondemand::json_type::unknown
             ? C_SCHEME_TRUE
             : C_SCHEME_FALSE;
}

extern C_word chicken_simdjson_is_signed_integer(void *p)
{
  ondemand::value *element = static_cast<ondemand::value *>(p);
  return (element->type() == ondemand::json_type::number &&
          element->get_number_type() == ondemand::number_type::signed_integer)
             ? C_SCHEME_TRUE
             : C_SCHEME_FALSE;
}

extern C_word chicken_simdjson_is_unsigned_integer(void *p)
{
  ondemand::value *element = static_cast<ondemand::value *>(p);
  return (element->type() == ondemand::json_type::number &&
          element->get_number_type() == ondemand::number_type::unsigned_integer)
             ? C_SCHEME_TRUE
             : C_SCHEME_FALSE;
}

extern C_word chicken_simdjson_is_floating_point_number(void *p)
{
  ondemand::value *element = static_cast<ondemand::value *>(p);
  return (element->type() == ondemand::json_type::number &&
          element->get_number_type() == ondemand::number_type::floating_point_number)
             ? C_SCHEME_TRUE
             : C_SCHEME_FALSE;
}

extern C_word chicken_simdjson_is_boolean(void *p)
{
  ondemand::value *element = static_cast<ondemand::value *>(p);
  return element->type() == ondemand::json_type::boolean ? C_SCHEME_TRUE : C_SCHEME_FALSE;
}

extern C_word chicken_simdjson_is_string(void *p)
{
  ondemand::value *element = static_cast<ondemand::value *>(p);
  return element->type() == ondemand::json_type::string ? C_SCHEME_TRUE : C_SCHEME_FALSE;
}

extern C_word chicken_simdjson_is_null(void *p)
{
  ondemand::value *element = static_cast<ondemand::value *>(p);
  return element->type() == ondemand::json_type::null ? C_SCHEME_TRUE : C_SCHEME_FALSE;
}

extern C_word chicken_simdjson_is_array(void *p)
{
  ondemand::value *element = static_cast<ondemand::value *>(p);
  return element->type() == ondemand::json_type::array ? C_SCHEME_TRUE : C_SCHEME_FALSE;
}

extern C_word chicken_simdjson_is_object(void *p)
{
  ondemand::value *element = static_cast<ondemand::value *>(p);
  return element->type() == ondemand::json_type::object ? C_SCHEME_TRUE : C_SCHEME_FALSE;
}

*/

extern int chicken_simdjson_get_type(void *p)
{
  ondemand::value *element = static_cast<ondemand::value *>(p);

  switch (element->type())
  {
  case ondemand::json_type::array:
    return CHICKEN_SIMDJSON_TYPE_ARRAY;
  case ondemand::json_type::object:
    return CHICKEN_SIMDJSON_TYPE_OBJECT;
  case ondemand::json_type::string:
    return CHICKEN_SIMDJSON_TYPE_STRING;
  case ondemand::json_type::null:
    return CHICKEN_SIMDJSON_TYPE_NULL;
  case ondemand::json_type::boolean:
    return CHICKEN_SIMDJSON_TYPE_BOOLEAN;
  case ondemand::json_type::number:
    switch (element->get_number_type())
    {
    case ondemand::number_type::signed_integer:
      return CHICKEN_SIMDJSON_TYPE_SIGNED_INTEGER;
    case ondemand::number_type::unsigned_integer:
      return CHICKEN_SIMDJSON_TYPE_UNSIGNED_INTEGER;
    case ondemand::number_type::floating_point_number:
      return CHICKEN_SIMDJSON_TYPE_FLOATING_POINT_NUMBER;
    case ondemand::number_type::big_integer:
      return CHICKEN_SIMDJSON_TYPE_UNKNOWN;
    }
  case ondemand::json_type::unknown:
  default:
    break;
  };

  return CHICKEN_SIMDJSON_TYPE_UNKNOWN;
}

extern int64_t chicken_simdjson_get_signed_integer(void *p)
{
  ondemand::value *element = static_cast<ondemand::value *>(p);
  return element->get_int64();
}

extern uint64_t chicken_simdjson_get_unsigned_integer(void *p)
{
  ondemand::value *element = static_cast<ondemand::value *>(p);
  return element->get_uint64();
}

extern double chicken_simdjson_get_floating_point_number(void *p)
{
  ondemand::value *element = static_cast<ondemand::value *>(p);
  return element->get_double();
}

extern C_word chicken_simdjson_get_boolean(void *p)
{
  ondemand::value *element = static_cast<ondemand::value *>(p);
  return element->get_bool() ? C_SCHEME_TRUE : C_SCHEME_FALSE;
}

extern const char *chicken_simdjson_get_string(void *p)
{
  ondemand::value *element = static_cast<ondemand::value *>(p);
  return element->get_string()->data();
}

extern size_t chicken_simdjson_get_array_count_elements(void *p)
{
  ondemand::value *element = static_cast<ondemand::value *>(p);
  auto array = element->get_array();
  return array.count_elements();
}

extern C_word chicken_simdjson_get_array(void *p, C_word callback)
{
  ondemand::value *element = static_cast<ondemand::value *>(p);
  auto array = element->get_array();
  size_t n = array.count_elements();
  C_word *ptr = C_alloc(C_SIZEOF_VECTOR(n));
  C_word res = C_vector(&ptr, n);

  C_word v = C_SCHEME_UNDEFINED;
  ptr = C_alloc(C_SIZEOF_POINTER);

  size_t i = 0;
  for (auto child : array)
  {
    auto each = child.value();

    v = C_mpointer(&ptr, &each);

    C_save(v);
    C_save(C_fix(i));
    C_save(res);

    res = C_callback(callback, 3);

    i++;
  }

  return res;
}

extern C_word chicken_simdjson_get_object(void *p, C_word callback)
{
  ondemand::value *element = static_cast<ondemand::value *>(p);
  auto obj = element->get_object();

  C_word res = C_SCHEME_END_OF_LIST;

  C_word ckey = C_SCHEME_UNDEFINED;
  C_word cvalue = C_SCHEME_UNDEFINED;

  C_word *ptr_pointer = C_alloc(C_SIZEOF_POINTER);

  for (auto field : obj)
  {
    std:string_view each_key = field.escaped_key();


    // printf("key: %d %d\n", each_value.type(), each_value.get_int64());
    size_t length = each_key.length();
    char *copy = (char *)malloc(length + 1);
    memcpy(copy, each_key.data(), length);
    copy[length] = 0;
    C_word *ptr = C_alloc(C_SIZEOF_INTERNED_SYMBOL(length));
    ckey = C_intern(&ptr, length, copy);



    ondemand::value each_value = field.value();
    cvalue = C_mpointer(&ptr_pointer, &each_value);
    
    C_save(cvalue);
    C_save(ckey);
    C_save(res);

    res = C_callback(callback, 3);
  }

  return res;
}

extern const char *chicken_simdjson_get_raw_json_string(void *p)
{
  ondemand::value *element = (ondemand::value *)p;
  printf("chicken_simdjson_get_raw_json_string %p %s\n", element, element->get_raw_json_string().raw().value());
  return "";
}

extern C_word chicken_simdjson_load_ondemand(
    const char *filename,
    C_word callback_object,
    C_word callback_identity,
    C_word callback_vector,
    C_word callback_list,
    C_word callback_vector_set,
    C_word callback_list_finalize)
{
  ondemand::parser parser;
  auto json = padded_string::load(filename);
  ondemand::document doc = parser.iterate(json);
  C_return(chicken_simdjson_visit_ondemand(
      doc,
      callback_object,
      callback_identity,
      callback_vector,
      callback_list,
      callback_vector_set,
      callback_list_finalize));
}

extern C_word chicken_simdjson_parse_ondemand(
    const char *data,
    size_t length,
    C_word callback_object,
    C_word callback_identity,
    C_word callback_vector,
    C_word callback_list,
    C_word callback_vector_set,
    C_word callback_list_finalize)
{
  ondemand::parser parser;
  simdjson::padded_string my_padded_data(data, length);
  ondemand::document doc = parser.iterate(my_padded_data);
  C_return(chicken_simdjson_visit_ondemand(
      doc,
      callback_object,
      callback_identity,
      callback_vector,
      callback_list,
      callback_vector_set,
      callback_list_finalize));
}

extern C_word chicken_simdjson_parse_ondemand_callback(
    const char *data,
    size_t length,
    C_word callback)
{
  ondemand::parser parser;
  simdjson::padded_string my_padded_data(data, length);
  ondemand::document ddoc = parser.iterate(my_padded_data);

  ondemand::value doc = ddoc;

  C_word *ptr = C_alloc(C_SIZEOF_POINTER);
  C_word p = C_mpointer(&ptr, &doc);
  C_save(p);
  p = C_callback(callback, 1);
  C_return(p);
}