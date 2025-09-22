
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

C_word print_json(C_word C_k, dom::element element, C_word p)
{
  C_word res = C_SCHEME_UNDEFINED;
  C_word *ptr = NULL;

  switch (element.type())
  {
  // case dom::element_type::ARRAY:
  //   // cout << "[";
  //   // for (dom::element child : dom::array(element))
  //   // {
  //   //   print_json(child);
  //   //   cout << ",";
  //   // }
  //   // cout << "]";

  //   dom::array array = dom::array(element);
  //   size_t length = array.size();
  //   ptr = C_alloc(C_SIZEOF_PAIR);
  //   res = C_a_pair(&ptr, C_fix(length), C_SCHEME_END_OF_LIST);

  //   break;
  case dom::element_type::OBJECT:
    // cout << "{";
    // for (dom::key_value_pair field : dom::object(element))
    // {
    //   cout << "\"" << field.key << "\": ";
    //   print_json(field.value);
    // }
    // cout << "}";
    dom::object array = dom::object(element);
    size_t length = array.size();
    ptr = C_alloc(C_SIZEOF_PAIR);
    C_word w = C_a_pair(&ptr, C_fix(length), C_SCHEME_END_OF_LIST);
    C_save(w);
    res = C_callback(p, 1);
    break;
    // case dom::element_type::INT64:
    //   cout << int64_t(element) << endl;
    //   break;
    // case dom::element_type::UINT64:
    //   cout << uint64_t(element) << endl;
    //   break;
    // case dom::element_type::DOUBLE:
    //   cout << double(element) << endl;
    //   break;
    // case dom::element_type::STRING:
    //   const char *str = element.get_c_str();
    //   size_t length = element.get_string_length();
    //   C_word *ptr = C_alloc(C_SIZEOF_STRING(length));
    //   C_word res = C_string(&ptr, length, (char *)str);
    //   break;
    // case dom::element_type::BOOL:
    //   cout << bool(element) << endl;
    //   break;
    // case dom::element_type::NULL_VALUE:
    //   cout << "null" << endl;
    //   break;
  }

  // C_kontinue(C_k, res);
  printf("done\n");
  // C_word args[] = {C_SCHEME_UNDEFINED, C_k, res};
  C_return(res);
}

C_word f(const char *filename)
{
  const char *str = "hello world";
  int length = strlen(str);
  C_word *ptr = C_alloc(C_SIZEOF_STRING(length));

  C_word res = C_string(&ptr, length, (char *)str);
  return res;
}

C_word g(const char *filename)
{
  const char *str = "hello world";
  int length = strlen(str);
  C_word *ptr = C_alloc(C_SIZEOF_PAIR);
  C_word res = C_a_pair(&ptr, C_fix(length), C_SCHEME_END_OF_LIST);
  C_gc_protect(&ptr, 1);
  C_return(res);
}

extern C_word c_rec(C_word l);

extern C_word c(C_word l, C_word p)
{
  if (l == C_SCHEME_END_OF_LIST)
  {
    C_save(C_SCHEME_END_OF_LIST);
    C_return(C_callback(p, 1));
  }

  C_word cdr = c(C_i_cdr(l), p);
  C_word *ptr = C_alloc(C_SIZEOF_PAIR);
  C_word res = C_a_pair(&ptr, C_i_car(l), cdr);
  C_save(res);
  C_return(C_callback(p, 1));
}

extern C_word parse_json(C_word C_k, const char *filename, C_word l, C_word p)
{
  simdjson::dom::parser parser;
  simdjson::dom::element tweets = parser.load(filename);
  // C_return(print_json(C_k, &tweets, p));
  // C_kontinue(C_k, print_json(C_k, &tweets, p));
  // C_kontinue(C_k, print_json(C_k, tweets));
  // C_kontinue(C_k, g(filename));
  C_return(c(l, p));
}
