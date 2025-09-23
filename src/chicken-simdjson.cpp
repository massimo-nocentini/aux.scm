
#include <stdio.h>
#include <assert.h>
#include <chicken/chicken.h>
#include <timsort.h>
#include <stdlib.h>
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
  C_word tmp = C_SCHEME_UNDEFINED, another_tmp = C_SCHEME_UNDEFINED;
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
      another_tmp = chicken_simdjson_visit(
          field.value,
          callback_object,
          callback_identity,
          callback_vector,
          callback_list,
          callback_vector_set,
          callback_list_finalize);

      size_t length = field.key.size();
      ptr = C_alloc(C_SIZEOF_STRING(length));
      tmp = C_string(&ptr, length, (char *)field.key.data());

      C_save(another_tmp);
      C_save(tmp);
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
    res = C_SCHEME_UNDEFINED;
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
