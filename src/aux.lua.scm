
(module (aux lua) *

  (import
    scheme 
    (chicken base) 
    (chicken foreign) 
    (chicken gc) 
    (chicken pretty-print)
    (aux base))

  #>

  #include "lauxlib.h"

  <#


  (define-record luaR state)

  (define luaL-newstate (foreign-lambda (c-pointer "lua_State") "luaL_newstate"))
  (define lua-close (foreign-lambda void "lua_close" (c-pointer "lua_State")))

  (define (lua-make-empty)
    (let* ((state (luaL-newstate))
           (state* (make-luaR state)))
      (set-finalizer! state* (o lua-close luaR-state))))

)

#|

(import (aux lua))

(define l (lua-make-empty))
(define ll l)
(define l 2)
ll
|#








