/* chicken-simdjson.cpp for WebAssembly.  csc-wasm compiles only *.c files as C (it reads any other
   source as Scheme), so aux.egg compiles chicken-simdjson.cpp through this file for the targets
   wasm32 and wasm64, with "-C -x -C c++": clang then reads it, and so what it includes, as C++. */
#include "chicken-simdjson.cpp"
