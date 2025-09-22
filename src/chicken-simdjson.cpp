
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

C_word chicken_simdjson_visit(dom::element element, C_word callback_object, C_word callback_identity, C_word callback_vector)
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
    ptr = C_alloc(C_SIZEOF_VECTOR(length));
    res = C_vector(&ptr, length);
    C_save(res);
    res = C_callback(callback_identity, 1);

    int i = 0;
    for (dom::element child : array)
    {
      tmp = chicken_simdjson_visit(child, callback_object, callback_identity, callback_vector);
      // tmp = C_fix(i);

      C_save(tmp);
      C_save(C_fix(i));
      C_save(res);
      C_callback(callback_vector, 3);

      //   C_save(res);
      // res = C_callback(callback_identity, 1);

      i++;
    }

    break;
  }
  case dom::element_type::OBJECT:
  {

    dom::object obj = dom::object(element);
    size_t length = obj.size();

    res = C_SCHEME_END_OF_LIST;

    for (dom::key_value_pair field : obj)
    {
      another_tmp = chicken_simdjson_visit(field.value, callback_object, callback_identity, callback_vector);
      size_t length = field.key.size();
      ptr = C_alloc(C_SIZEOF_STRING(length));
      tmp = C_string(&ptr, length, (char *)field.key.data());
      C_save(another_tmp);
      C_save(tmp);
      tmp = C_callback(callback_object, 2);

      ptr = C_alloc(C_SIZEOF_PAIR);
      C_word w = C_a_pair(&ptr, tmp, res);
      C_save(w);
      res = C_callback(callback_identity, 1);
    }

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
    res = C_fix(element.get_uint64());
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

extern C_word chicken_simdjson_load(const char *filename, C_word callback_object, C_word callback_identity, C_word callback_vector)
{
  simdjson::dom::parser parser;
  simdjson::dom::element tweets = parser.load(filename);
  C_return(chicken_simdjson_visit(tweets, callback_object, callback_identity, callback_vector));
}
