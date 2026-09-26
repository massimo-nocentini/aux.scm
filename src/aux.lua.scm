
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
  (define %lua-close (foreign-lambda void "lua_close" (c-pointer "lua_State")))

  ; closes the state of `L', a luaR, exactly once: the slot is cleared first, so a second
  ; close -- explicit, or the finalizer's at collection time -- does nothing.
  (define (lua-close L)
    (let1 (state (luaR-state L))
      (when state
        (luaR-state-set! L #f)
        (%lua-close state))))

  (define (lua-make-empty)
    (let1 (state (luaL-newstate))   ; #f is NULL: out of memory
      (unless state (error 'lua-make-empty "luaL_newstate cannot allocate a state"))
      (set-finalizer! (make-luaR state) lua-close)))

)

#|

(import (aux lua))

(define l (lua-make-empty))
(define ll l)
(define l 2)
ll
|#








