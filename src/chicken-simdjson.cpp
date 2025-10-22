
#include <chicken/chicken.h>
#include <string.h>
#include <iostream>
#include "chicken-simdjson.h"
#include "simdjson.cpp"

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
  return element->get_int64().value();
}

extern size_t chicken_simdjson_get_unsigned_integer(void *p)
{
  ondemand::value *element = static_cast<ondemand::value *>(p);
  return element->get_uint64().value();
}

extern double chicken_simdjson_get_floating_point_number(void *p)
{
  ondemand::value *element = static_cast<ondemand::value *>(p);
  return element->get_double().value();
}

extern C_word chicken_simdjson_get_boolean(void *p)
{
  ondemand::value *element = static_cast<ondemand::value *>(p);
  return element->get_bool() ? C_SCHEME_TRUE : C_SCHEME_FALSE;
}

extern void chicken_simdjson_get_string(void *p, C_word k)
{
  ondemand::value *element = static_cast<ondemand::value *>(p);
  std::string_view s = element->get_string();
  size_t size = s.size();

  char *res = (char *)malloc(size);
  s.copy(res, size);

  C_word *ptr = C_alloc(C_SIZEOF_STRING(size));
  C_word v = C_string(&ptr, size, res);

  free(res);

  C_kontinue(k, v);
}

extern void chicken_simdjson_get_array(void *p, C_word C_k)
{
  ondemand::value *element = static_cast<ondemand::value *>(p);
  ondemand::array array = element->get_array();
  size_t n = array.count_elements();

  C_word *ptr_obj = C_alloc(C_SIZEOF_POINTER);
  C_word vobj = C_mpointer(&ptr_obj, &array);

  C_word av[4] = {C_SCHEME_UNDEFINED, C_k, vobj, C_fix(n)};
  C_values(4, av);
}

extern void chicken_simdjson_get_array_begin(void *p, C_word C_k)
{
  ondemand::array *element = static_cast<ondemand::array *>(p);

  ondemand::array_iterator iter = element->begin();

  C_word *ptr_iter = C_alloc(C_SIZEOF_POINTER);

  C_kontinue(C_k, C_mpointer(&ptr_iter, &iter));
}

extern void chicken_simdjson_get_array_endp(void *p, void *i, C_word C_k)
{
  ondemand::array *element = static_cast<ondemand::array *>(p);
  ondemand::array_iterator *iter = static_cast<ondemand::array_iterator *>(i);

  C_kontinue(C_k, iter->operator==(element->end()) ? C_SCHEME_TRUE : C_SCHEME_FALSE);
}

extern void chicken_simdjson_get_array_each(void *i, C_word C_k)
{

  ondemand::array_iterator *iter = static_cast<ondemand::array_iterator *>(i);

  ondemand::value each = iter->operator*();

  C_word *ptr_pointer = C_alloc(C_SIZEOF_POINTER);

  C_kontinue(C_k, C_mpointer(&ptr_pointer, &each));
}

extern void chicken_simdjson_get_array_inc(void *i, C_word C_k)
{

  ondemand::array_iterator *iter = static_cast<ondemand::array_iterator *>(i);

  ondemand::array_iterator j = iter->operator++();

  C_word *ptr_pointer = C_alloc(C_SIZEOF_POINTER);

  C_kontinue(C_k, C_mpointer(&ptr_pointer, &j));
}

extern void chicken_simdjson_get_object(void *p, C_word C_k)
{
  ondemand::value *element = static_cast<ondemand::value *>(p);
  ondemand::object obj = element->get_object();

  C_word *ptr_obj = C_alloc(C_SIZEOF_POINTER);

  C_kontinue(C_k, C_mpointer(&ptr_obj, &obj));
}

extern void chicken_simdjson_get_object_begin(void *p, C_word C_k)
{
  ondemand::object *element = static_cast<ondemand::object *>(p);

  ondemand::object_iterator iter = element->begin();

  C_word *ptr_iter = C_alloc(C_SIZEOF_POINTER);

  C_kontinue(C_k, C_mpointer(&ptr_iter, &iter));
}

extern void chicken_simdjson_get_object_endp(void *p, void *i, C_word C_k)
{
  ondemand::object *element = static_cast<ondemand::object *>(p);
  ondemand::object_iterator *iter = static_cast<ondemand::object_iterator *>(i);

  C_kontinue(C_k, iter->operator==(element->end()) ? C_SCHEME_TRUE : C_SCHEME_FALSE);
}

extern void chicken_simdjson_get_object_each(void *i, C_word C_k)
{
  ondemand::object_iterator *iter = static_cast<ondemand::object_iterator *>(i);

  ondemand::field field = iter->operator*();

  std::string_view each_key = field.escaped_key();
  size_t length = each_key.length();

  char *cpy = (char *)malloc(length);
  each_key.copy(cpy, length);

  C_word *ptr = C_alloc(C_SIZEOF_INTERNED_SYMBOL(length));
  C_word key = C_intern(&ptr, length, cpy);

  free(cpy);

  ondemand::value &each_value = field.value();
  
  C_word *ptr_pointer = C_alloc(C_SIZEOF_POINTER);
  C_word value = C_mpointer(&ptr_pointer, &each_value);

  C_word av[4] = {C_SCHEME_UNDEFINED, C_k, key, value};
  C_values(4, av);
}

extern void chicken_simdjson_get_object_inc(void *i, C_word C_k)
{
  ondemand::object_iterator *iter = static_cast<ondemand::object_iterator *>(i);

  ondemand::object_iterator j = iter->operator++();

  C_word *ptr_pointer = C_alloc(C_SIZEOF_POINTER);

  C_kontinue(C_k, C_mpointer(&ptr_pointer, &j));
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

extern void chicken_simdjson_load_ondemand_callback(
    const char *filename,
    C_word cont)
{
  ondemand::parser parser;
  auto json = padded_string::load(filename);
  ondemand::document ddoc = parser.iterate(json);

  ondemand::value doc = ddoc; // !

  C_word *ptr = C_alloc(C_SIZEOF_POINTER);
  C_word p = C_mpointer(&ptr, &doc);

  C_kontinue(cont, p);
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

extern void chicken_simdjson_parse_ondemand_callback(
    const char *data,
    size_t length,
    C_word cont)
{
  ondemand::parser parser;
  simdjson::padded_string my_padded_data(data, length);
  ondemand::document ddoc = parser.iterate(my_padded_data);

  ondemand::value doc = ddoc; // !

  C_word *ptr = C_alloc(C_SIZEOF_POINTER);
  C_word p = C_mpointer(&ptr, &doc);

  C_kontinue(cont, p);
}