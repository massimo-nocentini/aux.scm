
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

void print_json(dom::element element)
{
  switch (element.type())
  {
  case dom::element_type::ARRAY:
    cout << "[";
    for (dom::element child : dom::array(element))
    {
      print_json(child);
      cout << ",";
    }
    cout << "]";
    break;
  case dom::element_type::OBJECT:
    cout << "{";
    for (dom::key_value_pair field : dom::object(element))
    {
      cout << "\"" << field.key << "\": ";
      print_json(field.value);
    }
    cout << "}";
    break;
  case dom::element_type::INT64:
    cout << int64_t(element) << endl;
    break;
  case dom::element_type::UINT64:
    cout << uint64_t(element) << endl;
    break;
  case dom::element_type::DOUBLE:
    cout << double(element) << endl;
    break;
  case dom::element_type::STRING:
    cout << std::string_view(element) << endl;
    break;
  case dom::element_type::BOOL:
    cout << bool(element) << endl;
    break;
  case dom::element_type::NULL_VALUE:
    cout << "null" << endl;
    break;
  }
}

dom::element array_at(dom::element *element, size_t index)
{
  return dom::array(*element).at(index);
}

extern C_word f(const char *filename)
{
  const char *str = "hello world";
  int length = strlen(str);
  C_word *ptr = C_alloc(C_SIZEOF_STRING(length));

  C_word res = C_string(&ptr, length, (char *)str);
  return res;
}

extern void parse_json(C_word C_k, const char *filename)
{
  simdjson::dom::parser parser;
  simdjson::dom::element tweets = parser.load(filename);
  print_json(tweets);
  // const char *str = "hello world";
  // int length = strlen(str);
  // C_word *ptr = C_alloc(C_SIZEOF_STRING(length));

  // C_word res = C_string(&ptr, length, (char *)str);
  C_kontinue(C_k, f(filename));
}
