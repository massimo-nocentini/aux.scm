
; not in the Makefile `test` target: it needs the Lua headers and library, like (aux lua); run it
; with `make test-lua`.  One suite for the core (lua-suite) and one per section of the module.
(import scheme (chicken base) (chicken gc) (chicken condition) (chicken string)
        (only (chicken memory) pointer? pointer+ pointer-u32-set!)
        (only (chicken locative) make-weak-locative locative->object)
        (only (chicken bytevector) bytevector bytevector? bytevector-length make-bytevector
                               bytevector-u8-ref bytevector-u8-set!)
        (only (scheme base) call/cc string->utf8 utf8->string)
        (only srfi-1 filter last iota)
        (only (chicken file) create-temporary-file delete-file)
        (only (chicken file posix) duplicate-fileno file-open file-close fileno/stderr open/wronly open/creat open/trunc)
        (only (chicken io) read-string)
        (only (chicken port) call-with-output-string)
        (only (chicken time) current-process-milliseconds)
        (only (chicken eval) module-environment)
        (only srfi-18 make-thread thread-start! thread-join! thread-yield! thread-quantum-set! make-mutex mutex-lock! mutex-unlock!
                    current-thread thread-name uncaught-exception? uncaught-exception-reason)
        (aux unittest) (aux base) (aux lua)
        (aux category reader) (aux category monad reader))
;; the internals the white-box tests read, from the implementation module (never needed by user
;; code: (aux lua) is the public interface, and it is imported above, first, as it loads the code)
(import (only (aux lua internal)
              luaR-state luaR-u luaR-ref luaR-main make-luaR
              luaU-state luaU-callables luaU-close-pending luaU-anext luaU-unrefs luaU-unanchors
              %status %lua-finalize %pushlightuserdata-raw %in-window %getglobal/f %dbg-epoch-bump!
              %depth %pending-closes %pending-escape %lua-owner %lua-holds luaU-active
              %req-set!
              AUX_EARG AUX_EGLOBALS AUX_EINDEX AUX_ELIBRARY AUX_EMETATABLE AUX_ELOADING AUX_ERANGE AUX_ERDONLY AUX_ETHREAD AUX_ETYPE AUX_EUNDER))

;; ------------------------------------------------------------------------------------------
;; helpers
;; ------------------------------------------------------------------------------------------

(define (fresh) (let ((L (lua-make-empty))) ((luaL-openlibs) L) L))
(define ((register! name f) L) ((lua-pushcfunction f) L) ((lua-setglobal name) L))
(define (run! L code) ((lua-check (luaL-dostring code)) L))
(define (msg e) ((condition-property-accessor 'exn 'message) e))
;; numbers compared with eqv?: ⊦= accepts 3 for 3.0 and any difference below 1e-6, while Lua 5.5
;; keeps integers and floats apart (lua_Integer: an exact integer, lua_Number: a flonum)
(define (⊦eqv expected got) (⊦ eqv? expected got))

;; the Scheme value of the Lua value at idx (tests only)
(define (value-at L idx)
  (let ((t ((lua-type idx) L)))
    (cond ((= t LUA_TNUMBER) (let-values (((n ok?) ((lua-tointegerx idx) L))) (if ok? n ((lua-tonumber idx) L))))
          ((= t LUA_TSTRING) ((lua-tostring idx) L))
          ((= t LUA_TBOOLEAN) ((lua-toboolean idx) L))
          ((= t LUA_TNIL) 'nil)
          (else (lua-type->symbol t)))))
(define (global L name) ((lua-getglobal name) L) (begin1 (value-at L -1) ((lua-pop 1) L)))
;; (code who) of the (exn lua api) refusal thunk raises, or #f when it returns
(define (api-refusal thunk)
  (condition-case (begin (thunk) #f)
    (e (exn lua api) (list (get-condition-property e 'lua 'code) (get-condition-property e 'exn 'location)))))

(define (string-suffix? suffix s)
  (let ((n (string-length s)) (m (string-length suffix)))
    (and (>= n m) (string=? suffix (substring s (- n m) n)))))
(define (string-prefix? prefix s)
  (and (>= (string-length s) (string-length prefix)) (string=? prefix (substring s 0 (string-length prefix)))))
(define (string-prefix-before suffix s) (substring s 0 (- (string-length s) (string-length suffix))))

;; finalization: nothing here may capture a luaR (csi closures capture their whole frame)
(define (nop-callback C) 0)
(define (make-dropped-universe)
  (let ((L (fresh)))
    ((register! "f" nop-callback) L)
    ((lua-pushobject (list 'x)) L)
    ((lua-setglobal "o") L)
    (luaR-u L)))
;; a record whose printer runs printer-action when it is set (the text of a COND handle writes it)
(define-record printed x)
(define printer-action #f)
(set-record-printer! printed (λ (r out) (if printer-action (printer-action r out) (display "#<printed>" out))))
(define escape-k #f)
(define (escaping-gc C) (when escape-k (let ((k escape-k)) (set! escape-k #f) (k 'escaped-from-gc))) 0)
;; a dropped universe whose Scheme __gc escapes to escape-k when it is set
(define (make-escaping-universe)
  (let ((L (fresh)))
    ((register! "g" escaping-gc) L)
    (run! L "setmetatable({}, {__gc = g})")
    (luaR-u L)))
(define the-box #f)
(define the-u #f)
(define (drop-callback C) (set-car! the-box #f) (gc #t) (gc #t) ((lua-pushinteger 1) C) 1)
(define (run-dropping-universe)
  (set! the-box (list (fresh)))
  (set! the-u (luaR-u (car the-box)))
  ((register! "drop" drop-callback) (car the-box))
  ((luaL-dostring "R = drop()") (car the-box))
  the-u)
;; the trace buffer holds recent values: flush it with unrelated calls between collections.
;; lua-live-handles, like any binding, closes the universes whose close their finalizer deferred.
(define (flush-and-collect done?)
  (let loop ((i 0))
    (unless (or (done?) (= i 30))
      (for-each (λ (x) (list x x)) (iota 64))
      (gc #t)
      (lua-live-handles)
      (loop (+ i 1)))))

;; (exn lua api) refusals as a value, anything else raised
(define (api-outcome thunk) (condition-case (begin (thunk) 'ok) (e (exn lua api) 'refused)))
;; ((lua-resume from nargs) co) as a value: (status nres top-change message) where the message
;; (the value on top of co afterwards) is popped, or 'refused
(define (resume-answer from nargs co)
  (condition-case
    (let ((top ((lua-gettop) co)))
      (let-values (((st n) ((lua-resume from nargs) co)))
        (let ((m ((lua-tostring -1) co)) (d (- ((lua-gettop) co) (- top nargs))))
          ((lua-pop 1) co)
          (list st n d m))))
    (e (exn lua api) 'refused)))
(define (bytevector-prefix bv n) (let ((r (make-bytevector n))) (do ((i 0 (+ i 1))) ((= i n) r) (bytevector-u8-set! r i (bytevector-u8-ref bv i)))))
(define (bytevector-prefix-from bv k)              ; the bytes of bv from index k on
  (let ((r (make-bytevector (- (bytevector-length bv) k))))
    (do ((i k (+ i 1))) ((= i (bytevector-length bv)) r) (bytevector-u8-set! r (- i k) (bytevector-u8-ref bv i)))))

;; what C code writes on stderr (fd 2) while thunk runs
(define (with-stderr-captured thunk)
  (let* ((path (create-temporary-file)) (saved (duplicate-fileno fileno/stderr))
         (fd (file-open path (+ open/wronly open/creat open/trunc))))
    (duplicate-fileno fd fileno/stderr)
    (thunk)
    (duplicate-fileno saved fileno/stderr) (file-close saved) (file-close fd)
    (let ((s (with-input-from-file path (λ () (read-string))))) (delete-file path) (if (eof-object? s) "" s))))

;; a universe whose only callback captures its own main luaR (top level: nothing else is captured)
(define (capturing-universe)
  (let ((L (fresh)))
    ((register! "f" (λ (C) ((lua-getglobal "x") L) 1)) L)
    (run! L "x = string.rep('a', 100000) assert(f() == x)")
    (make-weak-locative L 0)))

;; a universe dropped at once, but for one of its thread luaRs
(define (universe-held-by-thread)
  (let* ((L (fresh)) (T ((lua-newthread) L)))
    ((lua-pop 1) L)
    (run! L "function add(a, b) return a + b end")
    T))

;; Lua -> Scheme -> lua-resume (from #f) -> Lua ... without end, each level 4 C calls deep: Lua's
;; C-call count must carry over from level to level
(define resume-depth 0)
(define resume-deepest #f)
(define (resume-next-level C)
  (set! resume-depth (+ resume-depth 1))
  (let ((T ((lua-newthread) C)))
    ((lua-pop 1) C)
    ((lua-getglobal "body") T)
    (let-values (((st n) ((lua-resume #f 0) T)))
      (when (and (not (= st LUA_OK)) (not resume-deepest)) (set! resume-deepest (list resume-depth ((lua-tostring -1) T))))
      ((lua-pushinteger st) C)
      1)))

;; every flavour of binding, applied to a closed state: each must be a procedure of one argument
;; that refuses with (exn lua api), never crashing
(define (all-curried-bindings)
  (list (lua-gettop) (lua-pushinteger 1) (lua-pushnumber 1.5) (lua-pushboolean #t) (lua-pushnil)
        (lua-pushvalue 1) (lua-type 1) (lua-settop 0) (lua-pop 0) (lua-tointegerx 1) (lua-tointeger 1)
        (lua-tonumberx 1) (lua-tonumber 1) (lua-toboolean 1) (lua-tolstring 1) (lua-tostring 1)
        (lua-tobytevector 1) (lua-isyieldable) (lua-pushlstring "a") (lua-pushstring "a")
        (lua-getfield 1 "a") (lua-setfield 1 "a") (lua-getglobal "a") (lua-setglobal "a") (luaL-openlibs)
        (lua-pcall 0 0 0) (lua-call 0 0) (luaL-loadbufferx "x" "n" #f) (luaL-loadbuffer "x" "n")
        (luaL-loadstring "x") (luaL-dostring "x") (lua-error) (luaL-error "x %d" 1) (luaL-argerror 1 "m")
        (luaL-typeerror 1 "t") (lua-yield 0) (lua-yieldk 0 (λ (st C) 0))
        (lua-pushcclosure (λ (C) 0) 0) (lua-pushcfunction (λ (C) 0)) (lua-pushobject 'x)
        (lua-toobject 1) (lua-toprocedure 1) (lua-toref 1) (lua-setwarnf 'collect) (lua-warnings)
        (lua-setmemorylimit 0)))

;; ------------------------------------------------------------------------------------------

(define-suite lua-suite

  ((test/make-empty _)
   (let1 (L (lua-make-empty))
     (⊨ (luaR? L))
     (⊨ (pointer? (luaR-state L)))
     (⊦= 'main (luaR-kind L))
     (⊦= 0 ((lua-gettop) L))))

  ; closing is idempotent, and the finalizer does not close a closed state again.
  ((test/close-twice _)
   (let1 (L (lua-make-empty))
     (lua-close L)
     (⊦= #f (luaR-state L))
     (lua-close L)
     (⊦= #f (luaR-state L))
     (set! L #f)
     (gc #t)))

  ;; ---------------------------------------------------------------- CORE

  ((test/core-currying _)
   (let ((L (lua-make-empty)))
     (lua-close L)
     (for-each (λ (m)
                 (⊨ (procedure? m))
                 (⊦raises (exn lua api) (m L))
                 (⊦raises (exn lua api) (m 'not-a-state)))
               (all-curried-bindings))
     ;; the few entries that are not curried (README, module header) answer directly
     (let ((L (fresh)))
       (⊦= "3 items" (lua-format "%d items" 3))                          ; the text, not an action
       (⊨ (string? (lua-format "%s" "x")))
       (⊦= (lua-format "%d|%s|%I" 3 "x" -7) ((lua-pushfstring "%d|%s|%I" 3 "x" -7) L))   ; the curried twin pushes it
       (⊦raises (exn) ((lua-format "%d" 1) L))                           ; a string is not a procedure of L
       (⊦= '(#f #f) (list (lua-ref? L) (lua-ref? 42)))
       ((lua-newtable) L)
       (let ((r ((lua-toref -1) L))) (⊦= #t (lua-ref? r)))
       (⊨ (lua-condition? (condition-case (run! L "error('x')") (e () e))))
       (⊦= '(#t main #f) (list (luaR? L) (luaR-kind L) (luaDebug? L)))
       (⊦= 'nil (lua-type->symbol LUA_TNIL))
       (⊨ (fixnum? (lua-upvalueindex 1)))
       (lua-close L))))

  ((test/core-monad _)
   (let ((L (fresh)))
     (define (global-int name)
       (do/monad (lua-getglobal name) (← v (lua-tointeger -1)) (lua-pop 1) ,v))
     (define sum-xy
       (do/monad
         (← x (global-int "x"))
         (← y (global-int "y"))
         (lua-pushinteger (+ x y))
         (lua-setglobal "z")
         (global-int "z")))
     (run! L "x, y = 40, 2")
     (⊦= 42 (sum-xy L))
     (⊦= 0 ((lua-gettop) L))
     ; a reader bind keeps only the first of several values: wrap lua-tointegerx to keep isnum.
     (define ((all-values m) L) (call-with-values (λ () (m L)) list))
     ((lua-pushnumber 3.5) L)
     (⊦= '(0) ((do/monad (← r (lua-tointegerx -1)) (return (list r))) L))
     (⊦= '(0 #f) ((do/monad (← r (all-values (lua-tointegerx -1))) (return r)) L))
     ((lua-pop 1) L)
     (lua-close L)))

  ((test/core-push-read _)
   (let ((L (fresh)))
     ((lua-pushinteger 9223372036854775807) L)
     ((lua-pushinteger -9223372036854775808) L)
     ((lua-pushnumber 1.5) L)
     ((lua-pushnumber 3) L)                       ; exact -> inexact
     ((lua-pushboolean #t) L)
     ((lua-pushboolean #f) L)
     ((lua-pushnil) L)
     ((lua-pushlstring "a\x0;b") L)
     ((lua-pushlstring (bytevector 104 255 105)) L)
     ((lua-pushstring #f) L)
     ((lua-pushstring "") L)
     (⊦= 11 ((lua-gettop) L))
     (⊦= 9223372036854775807 ((lua-tointeger 1) L))
     (⊦= -9223372036854775808 ((lua-tointeger 2) L))
     (⊦= 1.5 ((lua-tonumber 3) L))
     (⊦= LUA_TNUMBER ((lua-type 4) L))
     (let-values (((n ok?) ((lua-tointegerx 4) L))) (⊦= 3 n) (⊨ ok?))       ; 3.0 has an integer representation
     (let-values (((n ok?) ((lua-tointegerx 3) L))) (⊦= 0 n) (⊭ ok?))
     (⊨ ((lua-toboolean 5) L))
     (⊭ ((lua-toboolean 6) L))
     (⊦= LUA_TNIL ((lua-type 7) L))
     (⊦= "a\x0;b" ((lua-tostring 8) L))
     (⊦= 3 (string-length ((lua-tostring 8) L)))
     (⊦= (bytevector 104 255 105) ((lua-tostring 9) L))       ; invalid UTF-8 stays bytes
     (⊦= (bytevector 104 255 105) ((lua-tobytevector 9) L))
     (⊦= LUA_TNIL ((lua-type 10) L))
     (⊦= "" ((lua-tostring 11) L))
     ;; numbers read as Lua writes them, the slot is not converted
     (⊦= "9223372036854775807" ((lua-tostring 1) L))
     (⊦= "1.5" ((lua-tolstring 3) L))
     (⊦= "3.0" ((lua-tolstring 4) L))
     (⊦= LUA_TNUMBER ((lua-type 4) L))
     (⊦= #f ((lua-tostring 5) L))
     ;; beyond the top: none / defaults, without calling Lua
     (⊦= LUA_TNONE ((lua-type 12) L))
     (⊭ ((lua-toboolean 99) L))
     (⊦= #f ((lua-tostring 99) L))
     (let-values (((n ok?) ((lua-tointegerx 99) L))) (⊦= 0 n) (⊭ ok?))
     ((lua-settop 0) L)
     (⊦= 0 ((lua-gettop) L))
     ((lua-settop 3) L)
     (⊦= 3 ((lua-gettop) L))
     (⊦= LUA_TNIL ((lua-type -1) L))
     ((lua-pushvalue 1) L)
     ((lua-pop 4) L)
     (⊦= 0 ((lua-gettop) L))
     (⊦= 'string (lua-type->symbol LUA_TSTRING))
     (⊦= 'none (lua-type->symbol LUA_TNONE))
     (lua-close L)))

  ((test/core-api-refusals _)
   (let ((L (fresh)))
     (⊦raises (exn lua api) ((lua-getfield 5 "x") L))             ; above the top
     (⊦raises (exn lua api) ((lua-getfield 0 "x") L))             ; 0 is never an index
     (⊦raises (exn lua api) ((lua-getfield -1 "x") L))            ; empty stack
     (⊦raises (exn lua api) ((lua-type 0) L))
     (⊦raises (exn lua api) ((lua-type -1) L))
     (⊦raises (exn lua api) ((lua-pushvalue 1) L))
     (⊦raises (exn lua api) ((lua-pop 1) L))
     (⊦raises (exn lua api) ((lua-settop -2) L))
     (⊦raises (exn lua api) ((lua-pcall 0 0 0) L))                ; no function
     (⊦raises (exn lua api) ((lua-setglobal "x") L))              ; no value
     (⊦raises (exn lua api) ((lua-setfield 1 "x") L))
     (⊦raises (exn lua api) ((lua-pushvalue 4294967296) L))       ; int would truncate
     (⊦raises (exn lua api) ((lua-pushlstring 42) L))
     (⊦raises (exn lua api) ((luaL-loadbufferx "x" "na\x0;me" #f) L))
     (⊦raises (exn lua api) ((lua-pushcclosure 42 0) L))
     (⊦raises (exn lua api) ((lua-pushcclosure (λ (C) 0) 1) L))   ; no upvalue on the stack
     (⊦raises (exn lua api) ((lua-pushcclosure (λ (C) 0) 300) L))
     (⊦raises (exn lua api) (lua-upvalueindex 0))
     ;; an upvalue pseudo-index is read only while a C function runs (Lua asserts it): never at base level
     (⊦raises (exn lua api) ((lua-pushvalue (lua-upvalueindex 1)) L))
     (⊦raises (exn lua api) ((lua-type (lua-upvalueindex 1)) L))
     (⊦raises (exn lua api) ((lua-toboolean (lua-upvalueindex 1)) L))
     (⊦raises (exn lua api) ((lua-getfield (lua-upvalueindex 1) "k") L))
     (⊦raises (exn lua api) ((lua-setwarnf 'loud) L))
     (⊦raises (exn) ((lua-pushinteger 9223372036854775808) L))    ; integer64 range, before C runs
     ((lua-pushinteger 1) L)
     (⊦raises (exn lua api) ((lua-pcall 0 0 2) L))                ; msgh above the function
     (⊦raises (exn lua api) ((lua-pop 2) L))
     (⊦= 1 ((lua-gettop) L))
     ((lua-pop 1) L)
     (⊦raises (exn lua api) ((lua-error) L))                      ; no error value
     (⊦= 0 ((lua-gettop) L))
     (let ((c (condition-case ((lua-getfield 3 "x") L) (e (exn lua api) e))))
       (⊦= 'api (lua-condition-status c))
       (⊨ (lua-condition? c)))
     (lua-close L)))

  ((test/core-getset _)
   (let ((L (fresh)))
     (run! L "t = {a = 1, ['k\\0z'] = 2}")
     ((lua-getglobal "t") L)
     (⊦= LUA_TNUMBER ((lua-getfield -1 "a") L))
     (⊦= 1 ((lua-tointeger -1) L))
     ((lua-pop 1) L)
     (⊦= LUA_TNUMBER ((lua-getfield 1 "k\x0;z") L))                 ; NUL-safe keys
     (⊦= 2 ((lua-tointeger -1) L))
     ((lua-pop 1) L)
     ((lua-pushstring "v") L)
     ((lua-setfield 1 "b") L)
     (⊦= 1 ((lua-gettop) L))
     (⊦= LUA_TNIL ((lua-getfield 1 "missing") L))
     ((lua-pop 2) L)
     (⊦= "v" (begin (run! L "B = t.b") (global L "B")))
     (⊦= LUA_TNIL ((lua-getglobal "nope") L))
     ((lua-pop 1) L)
     ((lua-pushinteger 7) L)
     ((lua-setglobal "g\x0;x") L)
     (⊦= 7 (global L "g\x0;x"))
     ;; Lua errors raised by protected bindings, with the stack effect of C
     (run! L "bad = setmetatable({}, {__index = function(t, k) error('no field ' .. k) end})")
     ((lua-getglobal "bad") L)
     (let ((e (condition-case ((lua-getfield -1 "zz") L) (e (exn lua) e))))
       (⊦= 'errrun (lua-condition-status e))
       (⊨ (string-suffix? "no field zz" (msg e)))
       (⊦= 'lua-getfield ((condition-property-accessor 'exn 'location) e)))
     (⊦= 1 ((lua-gettop) L))
     ((lua-pushinteger 3) L)
     ((lua-setfield -2 "zz") L)                                     ; __index only: a plain set is fine
     (run! L "ro = setmetatable({}, {__newindex = function() error('read only', 0) end})")
     ((lua-getglobal "ro") L)
     ((lua-pushinteger 3) L)
     (⊦= "read only" (condition-case ((lua-setfield -2 "k") L) (e (exn lua) (msg e))))
     (⊦= 2 ((lua-gettop) L))                                        ; the value was consumed
     ((lua-settop 0) L)
     (lua-close L)))

  ((test/core-callbacks-basic _)
   (let ((L (fresh)))
     ((register! "add" (λ (C) ((lua-pushinteger (+ ((lua-tointeger 1) C) ((lua-tointeger 2) C))) C) 1)) L)
     (run! L "R = add(40, 2)")
     (⊦= 42 (global L "R"))
     ;; upvalues
     ((lua-pushinteger 10) L)
     ((lua-pushstring "twenty") L)
     ((lua-pushcclosure (λ (C) ((lua-pushvalue (lua-upvalueindex 2)) C) ((lua-pushvalue (lua-upvalueindex 1)) C) 2) 2) L)
     ((lua-setglobal "up") L)
     (⊦= 0 ((lua-gettop) L))
     (run! L "A, B = up()")
     (⊦= "twenty" (global L "A"))
     (⊦= 10 (global L "B"))
     ;; answers: a count within the stack, else a Lua error
     ((register! "bad1" (λ (C) 'oops)) L)
     ((register! "bad2" (λ (C) 5)) L)
     (run! L "local ok, e = pcall(bad1); E1 = e; ok, e = pcall(bad2); E2 = e")
     (⊨ (string-suffix? "must answer a result count" (global L "E1")))
     (⊨ (string-suffix? "answered more results than it pushed" (global L "E2")))
     ;; recovery of the procedure and of objects, identity kept
     (let ((f (λ (C) 0)) (obj (vector 1 2 3)))
       ((lua-pushcfunction f) L)
       (⊦= f ((lua-toprocedure -1) L))
       (⊦= #f ((lua-toobject -1) L))
       ((lua-pushobject obj) L)
       (⊨ (eq? obj ((lua-toobject -1) L)))
       (⊦= #f ((lua-toprocedure -1) L))
       ((lua-setglobal "OBJ") L)
       (run! L "S = tostring(OBJ); T = type(OBJ)")
       (⊦= "<scheme object>" (global L "S"))
       (⊦= "userdata" (global L "T"))
       ((lua-getglobal "OBJ") L)
       (⊨ (eq? obj ((lua-toobject -1) L)))
       ((lua-settop 0) L))
     (⊦= #f ((lua-toprocedure 5) L))
     (lua-close L)))

  ;; a result count crosses the C int return of the dispatcher: a fixnum above INT_MAX must never
  ;; be truncated into one of the internal answer codes (2^32-1 was "raise the value on top",
  ;; 2^32-7 a stale escape, 2^32-5 a yield, 2^32 a count of 0...): it is a bad answer, whatever
  ;; its low 32 bits, from a lua_CFunction and from a lua-yieldk continuation alike
  ((test/core-callback-answer-range _)
   (let ((L (fresh))
         (bad (list (expt 2 32) (- (expt 2 32) 1) (- (expt 2 32) 3) (- (expt 2 32) 4) (- (expt 2 32) 5)
                    (- (expt 2 32) 6) (- (expt 2 32) 7) (- (expt 2 32) 8) (- (expt 2 32) 11) (+ (expt 2 32) 1)
                    2147483648 (expt 2 61) (- (expt 2 62) 1) (- (expt 2 62)) (expt 2 70) -1 1.0)))
     (for-each
       (λ (n)
         ((register! "f" (λ (C) n)) L)
         ((register! "yk" (λ (C) ((lua-yieldk 0 (λ (st C) n)) C))) L)
         (run! L "local ok, e = pcall(f); OK, E = ok, e
                  local co = coroutine.wrap(function() return f() end)
                  ok, e = pcall(co); COK, CE = ok, e
                  co = coroutine.wrap(function() yk(); return 'unreached' end)
                  co(); ok, e = pcall(co); KOK, KE = ok, e")
         (⊦= (list n #f #f #f) (list n (global L "OK") (global L "COK") (global L "KOK")))
         (⊨ (string-suffix? "must answer a result count" (global L "E")))
         (⊨ (string-suffix? "must answer a result count" (global L "CE")))
         (⊨ (string-suffix? "must answer a result count" (global L "KE")))
         ;; uncaught by Lua: the same Lua error, never a raise of a stale value
         (⊦= "errrun" (condition-case (begin (run! L "f()") 'no-raise)
                        (e (exn lua) (symbol->string (lua-condition-status e)))))
         (⊦= 0 ((lua-gettop) L)))
       bad)
     ;; INT_MAX itself crosses unchanged: more results than the frame holds
     ((register! "f" (λ (C) 2147483647)) L)
     (run! L "local ok, e = pcall(f); E = e")
     (⊨ (string-suffix? "answered more results than it pushed" (global L "E")))
     ;; the counts that fit still work, in both places
     ((register! "f" (λ (C) ((lua-pushinteger 7) C) ((lua-pushinteger 8) C) 2)) L)
     ((register! "yk" (λ (C) ((lua-yieldk 0 (λ (st C) ((lua-pushinteger 9) C) 1)) C))) L)
     (run! L "A, B = f(); local co = coroutine.wrap(function() return yk() end); co(); K = co()")
     (⊦= '(7 8 9) (list (global L "A") (global L "B") (global L "K")))
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)))

  ;; a condition shaped like the requests of luaL-error / luaL-argerror / luaL-typeerror, made by
  ;; user code with a text that is not a string or a bytevector (or an argerror arg that is not a
  ;; C int): an ordinary Scheme condition, raised back as itself (its text was read as a block
  ;; pointer: a wild read).  Well-formed ones are requests, as the bindings' own.
  ((test/core-forged-requests _)
   (let ((L (fresh)))
     (⊦= #f (%req-set! 42))
     (⊦= #f (%req-set! 'sym))
     (⊦= #f (%req-set! '(1 2)))
     (⊦= #f (%req-set! 1.5))
     (⊦= #f (%req-set! #f))
     (⊦= #f (%req-set! (vector 1 2)))
     (⊦= #t (%req-set! "text"))
     (⊦= #t (%req-set! (bytevector 1 0 2)))
     (for-each
       (λ (c)
         ((register! "f" (λ (C) (abort c))) L)
         (⊨ (eq? c (condition-case (run! L "f()") (e () e))))
         (run! L "local ok, e = pcall(f); T = type(e)")
         (⊦= "userdata" (global L "T"))                                ; a Scheme condition in Lua
         (⊦= 0 ((lua-gettop) L)))
       (list (make-property-condition 'lua 'request 'argerror 'text 42 'arg 1)
             (make-property-condition 'lua 'request 'lerror 'text 'sym)
             (make-property-condition 'lua 'request 'runerror 'text '(1 2))
             (make-property-condition 'lua 'request 'typeerror 'text 1.5 'arg 1)
             (make-property-condition 'lua 'request 'lerror 'text (vector 1 2))
             (make-property-condition 'lua 'request 'argerror 'text "x" 'arg (expt 2 80))
             (make-property-condition 'lua 'request 'argerror 'text "x" 'arg 4294967297)
             (make-property-condition 'lua 'request 'typeerror 'text "x" 'arg "one")
             (make-property-condition 'lua 'request 'argerror 'text "x")))  ; no arg at all
     ;; well formed: C's messages
     ((register! "f" (λ (C) (abort (make-property-condition 'lua 'request 'argerror 'text "boom" 'arg 1)))) L)
     (run! L "local ok, e = pcall(f); E = e")
     (⊨ (string-suffix? "bad argument #1 to 'f' (boom)" (global L "E")))
     ((register! "f" (λ (C) (abort (make-property-condition 'lua 'request 'lerror 'text (bytevector 104 105) 'arg 'ignored)))) L)
     (run! L "local ok, e = pcall(f); E = e")
     (⊦= "hi" (global L "E"))
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)))

  ;; Lua code with the debug library cannot make a Scheme closure run with the wrong upvalue
  ;; count, nor pass a foreign userdata off as a Scheme object
  ((test/core-forged-handles _)
   (let ((L (fresh)))
     ((register! "F1" (λ (C) ((lua-pushstring "F1 ran") C) 1)) L)
     ((lua-pushinteger 1) L) ((lua-pushinteger 2) L)
     ((lua-pushcclosure (λ (C) 0) 2) L)
     ((lua-setglobal "F2") L)
     ((lua-pushobject 'obj) L)
     ((lua-setglobal "OBJ") L)
     (run! L "local _, h2 = debug.getupvalue(F2, 1)
              debug.setupvalue(F1, 1, h2)
              local ok, e = pcall(F1) E1 = e
              debug.setupvalue(F1, 1, {})
              ok, e = pcall(F1) E2 = e
              local f = io.tmpfile()
              debug.setmetatable(f, debug.getmetatable(OBJ))
              FAKE = f
              M = getmetatable(OBJ)")
     (⊨ (string-suffix? "released or forged Scheme procedure" (global L "E1")))
     (⊨ (string-suffix? "released or forged Scheme procedure" (global L "E2")))
     (⊦= #f (global L "M"))                                          ; __metatable hides it
     ((lua-getglobal "FAKE") L)
     (⊦= #f ((lua-toobject -1) L))
     ((lua-pop 1) L)
     (run! L "FAKE = nil collectgarbage() collectgarbage()")
     (⊦= 'obj (begin ((lua-getglobal "OBJ") L) (begin1 ((lua-toobject -1) L) ((lua-pop 1) L))))
     (lua-close L)
     (⊦= 0 (lua-live-handles (luaR-u L)))))

  ;; the __tostring of the handle metatable is reachable (lua-getmetatable ignores __metatable, as
  ;; debug.getmetatable does) and callable with anything: what is not shaped as a handle is refused
  ;; with luaL_checkudata's message, never read as a userdata
  ((test/core-handle-tostring-checks-its-argument _)
   (let ((L (lua-make-empty)))                                      ; the bound API only, no library
     ((lua-pushobject (vector 1 2)) L)                               ; 1
     (⊨ ((lua-getmetatable 1) L))
     (⊦= LUA_TFUNCTION ((lua-getfield -1 "__tostring") L))
     ((lua-remove -2) L)                                             ; 1 obj, 2 __tostring
     (let ((call (λ (nargs push)                                      ; (status text) of __tostring(...)
                   ((lua-pushvalue 2) L) (push)
                   (let ((st ((lua-pcall nargs 1 0) L)))
                     (begin1 (list st ((lua-tostring -1) L)) ((lua-pop 1) L)))))
           (refused (λ (got) (list LUA_ERRRUN (string-append "bad argument #1 to '?' (aux.scheme expected, got " got ")")))))
       (⊦= (list LUA_OK "<scheme object>") (call 1 (λ () ((lua-pushvalue 1) L))))
       (⊦= (refused "number") (call 1 (λ () ((lua-pushinteger 42) L))))
       (⊦= (refused "nil") (call 1 (λ () ((lua-pushnil) L))))
       (⊦= (refused "no value") (call 0 void))
       (⊦= (refused "table") (call 1 (λ () ((lua-newtable) L))))
       (⊦= (refused "string") (call 1 (λ () ((lua-pushstring "s") L))))
       (⊦= (refused "boolean") (call 1 (λ () ((lua-pushboolean #t) L))))
       (⊦= (refused "light userdata") (call 1 (λ () ((lua-pushlightuserdata #f) L))))
       (⊦= (refused "userdata") (call 1 (λ () ((lua-newuserdatauv 8 1) L))))           ; another size
       (⊦= (refused "userdata") (call 1 (λ ()                          ; a handle's size, a text, no magic
                                          (let ((p ((lua-newuserdatauv 16 1) L)))
                                            (for-each (λ (k) (pointer-u32-set! (pointer+ p k) 0)) '(0 4 8 12))
                                            ((lua-pushstring "not a handle") L) ((lua-setiuservalue -2 1) L)))))
       (⊦= (refused "function") (call 1 (λ () ((lua-pushvalue 2) L)))))
     (⊦= 2 ((lua-gettop) L))
     (lua-close L))
   ;; from Lua, through the debug library
   (let ((L (fresh)))
     ((lua-pushobject 'x) L)
     ((lua-setglobal "OBJ") L)
     (run! L "local f = debug.getmetatable(OBJ).__tostring
              S = f(OBJ)
              local ok, e = pcall(f, 42) E1 = e
              ok, e = pcall(f, {}) E2 = e
              ok, e = pcall(f, io.stdout) E3 = e")
     (⊦= "<scheme object>" (global L "S"))
     (⊦= "bad argument #1 to '?' (aux.scheme expected, got number)" (global L "E1"))
     (⊦= "bad argument #1 to '?' (aux.scheme expected, got table)" (global L "E2"))
     (⊦= "bad argument #1 to '?' (aux.scheme expected, got FILE*)" (global L "E3"))
     (lua-close L)))

  ;; The handle metatable lives in the registry, which the bound API (lua-rawset on
  ;; LUA_REGISTRYINDEX) and Lua (debug.getregistry) can write.  A handle is never given an entry of
  ;; another type as its metatable (lua_setmetatable's undefined behaviour): the binding raises
  ;; instead, and every handle works again once the entry is restored.
  ((test/core-handle-metatable-replaced _)
   (let* ((L (lua-make-empty))                                       ; the bound API only, no library
          (replaced "aux.lua: the registry entry of the Scheme handle metatable is not a table")
          (outcome (λ (thunk) (condition-case (begin (thunk) 'ok) (e (exn lua) (msg e)))))
          (key #f)                                                    ; the key, a light userdata's pointer
          (replace! (λ (C) ((lua-pushlightuserdata key) C) ((lua-pushinteger 42) C) ((lua-rawset LUA_REGISTRYINDEX) C)))
          (breaker (λ (C) (replace! C) (error "scheme boom" 1)))     ; replaces the entry, then raises
          (T (let ((T ((lua-newthread) L)))                          ; replaces it, then yields with a k
               ((lua-pop 1) L)
               ((lua-pushcfunction (λ (C) (replace! C) ((lua-yieldk 0 (λ (st C) 0)) C))) T)
               T))
          (find-key (λ ()                                             ; pushes the metatable's key, if found
                      ((lua-pushnil) L)
                      (let loop ()
                        (and ((lua-next LUA_REGISTRYINDEX) L)
                             (let ((hit (and (= LUA_TLIGHTUSERDATA ((lua-type -2) L)) (= LUA_TTABLE ((lua-type -1) L))
                                             (begin ((lua-getfield -1 "__name") L)
                                                    (begin1 (equal? "aux.scheme" ((lua-tostring -1) L)) ((lua-pop 1) L))))))
                               ((lua-pop 1) L)
                               (or hit (loop))))))))
     ((lua-pushobject 'old) L)                                       ; 1: a handle made before
     ((lua-pushcfunction breaker) L)                                 ; 2: a Scheme closure made before
     (⊨ (find-key))                                                  ; 3: the key
     (set! key ((lua-touserdata 3) L))
     ((lua-pushvalue 3) L) ((lua-rawget LUA_REGISTRYINDEX) L)        ; 4: the metatable, kept
     ;; the Scheme error of a callback that replaced the entry cannot become a handle: the refusal is
     ;; the error value
     ((lua-pushvalue 2) L)
     (⊦= LUA_ERRRUN ((lua-pcall 0 0 0) L))
     (⊦= replaced ((lua-tostring -1) L))
     ((lua-pop 1) L)
     (⊦= LUA_TNUMBER (begin ((lua-pushvalue 3) L) ((lua-rawget LUA_REGISTRYINDEX) L) (begin1 ((lua-type -1) L) ((lua-pop 1) L))))
     ;; restored; a yield with a Scheme continuation, by a callback that replaced the entry, cannot
     ;; anchor the continuation: the refusal is raised in the coroutine
     ((lua-pushvalue 3) L) ((lua-pushvalue 4) L) ((lua-rawset LUA_REGISTRYINDEX) L)
     (let-values (((st n) ((lua-resume L 0) T))) (⊦= LUA_ERRRUN st))
     (⊦= replaced ((lua-tostring -1) T))
     ;; replaced (still): nothing makes a handle
     (⊦= replaced (outcome (λ () ((lua-pushobject 'new) L))))
     (⊦= replaced (outcome (λ () ((lua-pushcfunction (λ (C) 0)) L))))
     (⊦= replaced (outcome (λ () ((lua-pushinteger 7) L) ((lua-pushcclosure (λ (C) 0) 1) L))))
     (⊦= replaced (outcome (λ () ((lua-sethook (λ (ar C) 0) LUA_MASKCOUNT 1000) L))))
     (⊦= replaced (outcome (λ () ((luaL-setfuncs (list (cons "f" (λ (C) 0))) 0) L))))
     (⊦= 4 ((lua-gettop) L))                                         ; nothing left behind
     ;; a Scheme closure made before is not recognized meanwhile: refused, never misread
     ((lua-pushvalue 2) L)
     (⊦= LUA_ERRRUN ((lua-pcall 0 0 0) L))
     (⊨ (string-suffix? "released or forged Scheme procedure" ((lua-tostring -1) L)))
     ((lua-pop 1) L)
     ((lua-settop 4) L)
     (⊦= #f ((lua-toobject 1) L))                                    ; no longer recognized, never misread
     ;; restored: everything works again, the old handles included
     ((lua-pushvalue 3) L) ((lua-pushvalue 4) L) ((lua-rawset LUA_REGISTRYINDEX) L)
     (⊦= 'old ((lua-toobject 1) L))
     ((lua-pushobject 'new) L)
     (⊦= 'new ((lua-toobject -1) L))
     ((lua-pushcfunction (λ (C) (error "scheme boom" 2))) L)
     (⊦= LUA_ERRRUN ((lua-pcall 0 1 0) L))
     (⊦= "scheme boom" (msg ((lua-toobject -1) L)))
     ((lua-settop 0) L)
     (lua-close L)
     (⊦= 0 (lua-live-handles (luaR-u L))))
   ;; from Lua
   (let ((L (fresh)))
     (run! L "local r = debug.getregistry()
              for k, v in pairs(r) do
                if type(v) == 'table' and rawget(v, '__name') == 'aux.scheme' then K, V = k, v end
              end
              r[K] = 42")
     (⊦= "aux.lua: the registry entry of the Scheme handle metatable is not a table"
         (condition-case (begin ((lua-pushobject 'x) L) 'ok) (e (exn lua) (msg e))))
     (run! L "debug.getregistry()[K] = V")
     ((lua-pushobject 'x) L)
     (⊦= 'x ((lua-toobject -1) L))
     (lua-close L)))

  ((test/core-views _)
   (let ((L (fresh)) (saved #f) (kind #f) (refusal #f) (self-close #f))
     ((register! "f" (λ (C)
                       (set! saved C)
                       (set! kind (luaR-kind C))
                       (set! refusal (condition-case (begin (lua-close C) 'closed) (e (exn lua api) 'refused)))
                       (set! self-close (condition-case (begin (lua-close L) 'closed) (e (exn lua api) 'refused)))
                       ((lua-pushinteger ((lua-gettop) C)) C)
                       1))
      L)
     (run! L "N = f(1, 2, 3)")
     (⊦= 3 (global L "N"))
     (⊦= 'view kind)
     (⊦= 'refused refusal)
     (⊦= 'refused self-close)
     (⊦= #f (luaR-state saved))                                     ; invalidated on exit
     (⊦raises (exn lua api) ((lua-gettop) saved))
     (⊦raises (exn lua api) ((lua-pushinteger 1) saved))
     (⊨ (pointer? (luaR-state L)))                                  ; the refused close changed nothing
     (run! L "N = f()")
     (⊦= 0 (global L "N"))
     (lua-close L)
     (⊦raises (exn lua api) ((lua-gettop) L))))

  ((test/core-finalizer _)
   (let* ((L (fresh)) (u (luaR-u L)) (seen #f))
     ;; the finalizer, run while a binding of the universe is active, defers the close
     ((register! "f" (λ (C) (%lua-finalize L) (set! seen (luaU-close-pending u)) 0)) L)
     (run! L "f()")
     (⊦= #t seen)
     (⊨ (pointer? (luaU-state u)))
     (⊨ (pointer? (luaR-state L)))
     (%lua-finalize L)                                              ; not running: closes now
     (⊦= #f (luaU-state u))
     (⊦= #f (luaR-state L))
     (⊦= 0 (lua-live-handles u))
     (%lua-finalize L)))                                            ; and only once

  ;; states dropped with live callbacks and objects are closed by their finalizer.  The callbacks
  ;; are top-level procedures: a closure that captured the luaR would keep it alive (the handle
  ;; table keeps a procedure given to Lua until Lua collects it or its universe is closed).
  ((test/core-finalizer-gc _)
   (let ((us (let loop ((i 0) (acc '())) (if (= i 10) acc (loop (+ i 1) (cons (make-dropped-universe) acc))))))
     (flush-and-collect (λ () (null? (filter luaU-state us))))
     (⊦= '() (filter luaU-state us))
     (for-each (λ (u) (⊦= 0 (lua-live-handles u)) (⊦= 0 (luaU-callables u))) us)))

  ;; a finalizer that fires while a callback of the universe drops the last reference to it:
  ;; the close is deferred to a later collection
  ((test/core-finalizer-deferred-gc _)
   (let ((u (run-dropping-universe)))
     (flush-and-collect (λ () (not (luaU-state u))))
     (⊦= #f (luaU-state u))
     (⊦= 0 (lua-live-handles u))))

  ((test/core-handles-baseline _)
   (let* ((L (fresh)) (u (luaR-u L)) (base (lua-live-handles L)))
     (run! L "T = {}")
     ((lua-getglobal "T") L)
     (do ((i 0 (+ i 1))) ((= i 500))
       ((lua-pushcfunction (λ (C) ((lua-pushinteger i) C) 1)) L)
       ((lua-setfield 1 (string-append "f" (number->string i))) L)
       ((lua-pushobject (cons i i)) L)
       ((lua-setfield 1 (string-append "o" (number->string i))) L))
     ((lua-pop 1) L)
     (⊦= (+ base 1000) (lua-live-handles L))
     (⊦= 500 (luaU-callables u))
     (run! L "R = T.f123()")
     (⊦= 123 (global L "R"))
     (run! L "T = nil; collectgarbage(); collectgarbage()")
     (⊦= base (lua-live-handles L))
     (⊦= 0 (luaU-callables u))                                      ; the fast path is chosen again
     (let ((all (lua-live-handles)))
       (lua-close L)
       (⊦= 0 (lua-live-handles u))
       (⊦= (- all base) (lua-live-handles)))))

  ((test/core-close-with-live-callbacks _)
   (let* ((L (fresh)) (u (luaR-u L)) (base (lua-live-handles L)) (gc-calls 0))
     (do ((i 0 (+ i 1))) ((= i 100))
       ((register! (string-append "f" (number->string i)) (λ (C) 0)) L))
     ;; __gc metamethods that call Scheme while lua_close runs
     ((register! "at_gc" (λ (C) (set! gc-calls (+ gc-calls 1)) (gc #t) 0)) L)
     (run! L "for i = 1, 20 do setmetatable({}, {__gc = function() at_gc() end}) end
              keep = {} for i = 1, 20 do keep[i] = setmetatable({}, {__gc = function() at_gc() end}) end")
     (⊦= (+ base 101) (lua-live-handles L))
     (let ((all (lua-live-handles)))
       (lua-close L)
       (⊨ (>= gc-calls 20))
       (⊦= #f (luaU-state u))
       (⊦= 0 (luaU-callables u))
       (⊦= 0 (lua-live-handles u))
       (⊦= (- all 101) (lua-live-handles)))
     (gc #t)))

  ;; J1: a Scheme callback that uses a FAST (non-reentrant) stub whose Lua code reaches Scheme gets
  ;; a clean Lua error instead of a callback inside a non-safe foreign call
  ((test/core-j1-reentrancy _)
   (let ((L (fresh)) (result #f))
     ((register! "idx" (λ (C) (gc #t) ((lua-pushinteger 7) C) 1)) L)
     (run! L "setmetatable(_G, {__index = function(t, k) return idx() end})")
     (⊦= 7 (global L "missing"))                                    ; the safe path reaches Scheme
     ((register! "p1" (λ (C)
                        (let* ((keep (list (make-string 3 #\a) (vector 1 2 3) (list 1 2 3 4 5)))
                               (r (%getglobal/f #f (luaR-state C) "missing2")))
                          (set! result (list (and (<= r -1000) (> r -1100))
                                             ((lua-tostring -1) C)))
                          ((lua-pop 1) C)
                          (gc #t)
                          (⊦= (list "aaa" (vector 1 2 3) (list 1 2 3 4 5)) keep)
                          0)))
      L)
     (run! L "p1()")
     (⊨ (car result))
     (⊨ (string-suffix? "Scheme procedure called from a non-reentrant binding" (cadr result)))
     (gc #t)
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)))

  ;; J2: argerror / typeerror / luaL_error raised in the callback's frame, byte-identical to C
  ((test/core-tail-errors _)
   (let ((L (fresh)))
     ((register! "f" (λ (C) ((luaL-typeerror 2 "number") C))) L)
     ((register! "g" (λ (C) ((luaL-error "boom %d %s" 42 "x") C))) L)
     ((register! "h" (λ (C) ((luaL-argerror 1 "custom") C))) L)
     ;; one call per line: line 1 is C's own message, the others ours
     (run! L "local ok, e = pcall(function() local r = string.rep('x', nil); return r end); B = e
              ok, e = pcall(function() local r = f(1, nil); return r end); A = e
              ok, e = pcall(function() local r = g(); return r end); C = e
              ok, e = pcall(function() local r = h(true); return r end); D = e")
     (let* ((b (global L "B"))
            (b-tail "1: bad argument #2 to 'rep' (number expected, got nil)")
            (chunk (string-prefix-before b-tail b)))                ; [string "..."]:
       (⊨ (string-suffix? b-tail b))
       (⊦= (string-append chunk "2: bad argument #2 to 'f' (number expected, got nil)") (global L "A"))
       (⊦= (string-append chunk "3: boom 42 x") (global L "C"))
       (⊦= (string-append chunk "4: bad argument #1 to 'h' (custom)") (global L "D")))
     ;; the same requests caught in Scheme, or at top level: the message without position
     (⊦= "bad argument #2 (x)" (condition-case ((luaL-argerror 2 "x") L) (e (exn lua) (msg e))))
     ((lua-pushnil) L)
     (⊦= "bad argument #1 (number expected, got nil)" (condition-case ((luaL-typeerror 1 "number") L) (e (exn lua) (msg e))))
     (run! L "NAMED = setmetatable({}, {__name = 'MyType'})")
     ((lua-getglobal "NAMED") L)
     (⊦= "bad argument #2 (number expected, got MyType)" (condition-case ((luaL-typeerror 2 "number") L) (e (exn lua) (msg e))))
     (⊦= "bad argument #3 (number expected, got no value)" (condition-case ((luaL-typeerror 3 "number") L) (e (exn lua) (msg e))))
     (⊦= "boom 7" (condition-case ((luaL-error "boom %d" 7) L) (e (exn lua) (msg e))))
     (⊦= 'lerror (condition-case ((luaL-error "x") L) (e (exn lua) (get-condition-property e 'lua 'request))))
     ;; a typeerror request names the argument as it is when the callback answers: an argument just
     ;; above the top is "no value", a negative one is counted from the callback's own top; lauxlib
     ;; calls an argument number <= 0 an "extra argument"
     ((register! "t2" (λ (C) ((luaL-typeerror 2 "number") C))) L)
     ((register! "tneg" (λ (C) ((luaL-typeerror -1 "number") C))) L)
     (run! L "local ok, e = pcall(t2, 1); T2 = e
              ok, e = pcall(tneg, 1, 'two'); TNEG = e
              ok, e = pcall(string.rep); SREP = e")
     (⊦= "bad argument #2 to 't2' (number expected, got no value)" (global L "T2"))
     (⊦= "bad extra argument #-1 to 'tneg' (number expected, got string)" (global L "TNEG"))
     (⊦= "bad argument #1 to 'string.rep' (string expected, got no value)" (global L "SREP"))  ; C's own wording
     ;; a callback catching its own request in Scheme goes on normally
     ((register! "k" (λ (C) (condition-case ((luaL-error "inner") C) (e (exn lua) ((lua-pushstring (msg e)) C) 1)))) L)
     (run! L "K = k()")
     (⊦= "inner" (global L "K"))
     ((lua-settop 0) L)
     (lua-close L)))

  ;; The `value' of luaL-error / luaL-argerror / luaL-typeerror (the module header's Error rule): the
  ;; string C raises.  At top level it is the message (luaL_where gives "", and luaL_argerror has no
  ;; frame to name); inside a callback, caught in Scheme, it is exactly what Lua receives when the
  ;; same condition goes uncaught: the caller's position, "to 'name'", a method's "bad self", an
  ;; extra argument, a name found in package.loaded.  Where no op can run, the text without position.
  ((test/core-tail-error-values _)
   (let ((L (fresh)) (seen #f))
     (define (value-of thunk) (condition-case (begin (thunk) 'no-raise) (e (exn lua) (list (lua-condition-value e) (msg e)))))
     ;; top level: value = message
     (⊦= '("boom 7" "boom 7") (value-of (λ () ((luaL-error "boom %d" 7) L))))
     (⊦= '("bad argument #2 (x)" "bad argument #2 (x)") (value-of (λ () ((luaL-argerror 2 "x") L))))
     (⊦= '("bad argument #3 (number expected, got no value)" "bad argument #3 (number expected, got no value)")
         (value-of (λ () ((luaL-typeerror 3 "number") L))))
     (⊦= '("stack overflow (too)" "stack overflow (too)") (value-of (λ () ((luaL-checkstack 100000000 "too") L))))
     ((lua-pushstring "s") L)
     (⊦= '("bad argument #1 (number expected, got string)" "bad argument #1 (number expected, got string)")
         (value-of (λ () ((luaL-checkinteger 1) L))))
     ((lua-settop 0) L)
     ;; C reads the extramsg with %s: the value stops at a NUL, as Lua's would
     (⊦= "bad argument #1 (ab)" (car (value-of (λ () ((luaL-argerror 1 "ab\x0;cd") L)))))
     ;; luaL-error's fmt and %s arguments are C strings (luaL_error stops at their first NUL):
     ;; a NUL in either is refused before anything is raised, as in every C string
     (⊦= (list AUX_EARG 'luaL-error) (api-refusal (λ () ((luaL-error "a%sb" "\x0;") L))))
     (⊦= (list AUX_EARG 'luaL-error) (api-refusal (λ () ((luaL-error "a\x0;b") L))))
     (⊦= (list AUX_EARG 'luaL-error) (api-refusal (λ () ((luaL-error "[%s]" "x\x0;y") L))))
     (⊦= (list AUX_EARG 'luaL-error) (api-refusal (λ () ((luaL-error "[%s]" 42) L))))   ; %s is a const char *
     (⊦= "a\x0;b" (car (value-of (λ () ((luaL-error "a%cb" 0) L)))))     ; %c 0: a NUL byte, as C writes it
     ;; inside a callback: caught (value recorded), then raised again uncaught: Lua's view
     (define ((catching thunk) C)
       (condition-case (thunk C) (e (exn lua) (set! seen (lua-condition-value e)) (abort e))))
     ((register! "E" (catching (λ (C) ((luaL-error "raised %d" 3) C)))) L)
     ((register! "A" (catching (λ (C) ((luaL-argerror 1 "custom") C)))) L)
     ((register! "A2" (catching (λ (C) ((luaL-argerror 2 "second") C)))) L)
     ((register! "TE" (catching (λ (C) ((luaL-typeerror 1 "widget") C)))) L)
     ((register! "CI" (catching (λ (C) ((luaL-checkinteger 1) C)))) L)
     ((register! "CT" (catching (λ (C) ((luaL-checktype 1 LUA_TTABLE) C)))) L)
     ((register! "LIBFN" (catching (λ (C) ((luaL-argerror 1 "custom") C)))) L)
     ;; (a function reachable under two names would get either: lauxlib takes the first lua_next finds)
     (run! L "LIB = {fn = LIBFN} LIBFN = nil package.loaded.mylib = LIB
              OBJ = {m = A, m2 = A2}
              function via(src) local f = load(src) local ok, e = pcall(f) return e end")
     (for-each
       (λ (src)
         (set! seen 'unset)
         ((lua-getglobal "via") L)
         ((lua-pushstring src) L)
         ((lua-call 1 1) L)
         (let ((lua-view ((lua-tostring -1) L)))
           ((lua-pop 1) L)
           (⊨ (string? seen))
           (⊦= lua-view seen)))
       '("local r = E() return r"                          ; the caller's position
         "local r = A(1) return r"                         ; to 'A'
         "local r = OBJ:m() return r"                      ; calling 'm' on bad self
         "local r = OBJ:m2(1) return r"                    ; a method's argument #1
         "local ok, e = pcall(A) error(e, 0)"              ; no name from code: _G.A -> 'A'
         "local ok, e = pcall(LIB.fn) error(e, 0)"         ; no name from code: found in package.loaded
         "local r = TE({}) return r"
         "local r = CI('x') return r"
         "local r = CT(1) return r"
         "local c = setmetatable({}, {__call = A}) local r = c(1) return r"))  ; an extra argument
     ;; the texts themselves, for two of them
     (run! L "function via2(f, ...) local ok, e = pcall(f, ...) return e end")
     ((lua-getglobal "via2") L) ((lua-getglobal "A") L) ((lua-call 1 1) L)
     (⊦= "bad argument #1 to 'A' (custom)" ((lua-tostring -1) L))
     (⊦= "bad argument #1 to 'A' (custom)" seen)
     ((lua-pop 1) L)
     ((lua-getglobal "via2") L) ((lua-getglobal "LIB") L) ((lua-getfield -1 "fn") L) ((lua-remove -2) L) ((lua-call 1 1) L)
     (⊦= "bad argument #1 to 'mylib.fn' (custom)" ((lua-tostring -1) L))
     (⊦= "bad argument #1 to 'mylib.fn' (custom)" seen)
     ((lua-pop 1) L)
     ((luaL-loadstring "local c = setmetatable({}, {__call = A}) local r = c(1) return r") L)
     (⊦= LUA_ERRRUN ((lua-pcall 0 0 0) L))
     (⊨ (string-suffix? ":1: bad extra argument #1 to 'c' (custom)" ((lua-tostring -1) L)))
     (⊦= ((lua-tostring -1) L) seen)
     ((lua-pop 1) L)
     ;; a suspended thread: no op runs there, the value is the text without position
     (let ((T ((lua-newthread) L)))
       ((lua-pop 1) L)
       ((luaL-loadstring "coroutine.yield()") T)
       (⊦= LUA_YIELD (let-values (((st n) ((lua-resume L 0) T))) st))
       (⊦= '("later" "later") (value-of (λ () ((luaL-error "later") T))))
       (⊦= '("bad argument #1 (x)" "bad argument #1 (x)") (value-of (λ () ((luaL-argerror 1 "x") T)))))
     ((lua-settop 0) L)
     (lua-close L)))

  ;; the same requests from a callback (or a hook) that filled its frame up to the reserved top
  ;; (lua-settop grows it with lua_checkstack): the C side makes room before it pushes the message
  ;; (the API does not check a push: an ASan + LUA_USE_APICHECK build aborts without it).  At the
  ;; stack size limit, where the stack cannot grow, the callback's own top values are dropped.
  ((test/core-tail-errors-full-frame _)
   (let ((L (fresh)))
     (define (full C) ((lua-settop 60) C))
     ((register! "g" (λ (C) (full C) ((luaL-error "boom %d" 42) C))) L)
     ((register! "h" (λ (C) (full C) ((luaL-argerror 1 "custom") C))) L)
     ((register! "f" (λ (C) (full C) ((luaL-typeerror 2 "number") C))) L)
     ((register! "n" (λ (C) (full C) 61)) L)                               ; more results than pushed
     ;; called by pcall (a C function): luaL_where has no position to add
     (run! L "local function msg(f, ...) local ok, e = pcall(f, ...) return e end
              G = msg(g) H = msg(h, true) F = msg(f, 1, nil) N = msg(n)
              G2 = select(2, pcall(function() g() end))")
     (⊦= "boom 42" (global L "G"))
     (⊦= "bad argument #1 to 'h' (custom)" (global L "H"))
     (⊦= "bad argument #2 to 'f' (number expected, got nil)" (global L "F"))
     (⊦= "aux.lua: callback answered more results than it pushed" (global L "N"))
     (⊨ (string-suffix? ":3: boom 42" (global L "G2")))
     ;; a hook, armed by a callback inside the protected function: its own values above the hooked
     ;; function's registers
     (let ((armed #f))
       ((register! "arm" (λ (C) (set! armed #t) 0)) L)
       ((lua-sethook (λ (H event ar)
                       (when armed
                         (set! armed #f)
                         ((lua-settop (+ ((lua-gettop) H) 40)) H)
                         ((luaL-error "from hook") H))
                       0)
                     '(count) 1) L)
       (run! L "HK = select(2, pcall(function() arm() local x = 1 return x end))")
       ((lua-sethook #f 0 0) L)
       (⊦= "from hook" (global L "HK")))                    ; level 1 of a hook: pcall, no position
     ;; at the stack size limit
     (define (largest-grant C)                                 ; the largest n lua_checkstack grants
       (let loop ((lo 0) (hi 1000000))
         (if (< lo (- hi 1))
           (let ((mid (quotient (+ lo hi) 2))) (if ((lua-checkstack mid) C) (loop mid hi) (loop lo mid)))
           lo)))
     ((register! "lim" (λ (C)
                         (let fill ()
                           (let ((n (largest-grant C)))
                             (unless (= n 0) ((lua-settop (+ ((lua-gettop) C) n)) C) (fill))))
                         (⊭ ((lua-checkstack 1) C))
                         ((luaL-error "at the limit") C))) L)
     (run! L "LIM = select(2, pcall(function() lim() end))")
     (⊨ (string-suffix? ": at the limit" (global L "LIM")))
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)))

  ((test/core-format _)
   (⊦= "1 s 1.0 5 A \x20AC; % %q (null)" (lua-format "%d %s %f %I %c %U %% %q %s" 1 "s" 1.0 5 65 #x20AC #f))
   (⊦= "0.1 1e+100 -inf 3.14159265358979 1.0 0.30000000000000004" (lua-format "%f %f %f %f %f %f" 0.1 1e100 -inf.0 3.14159265358979 1 (+ 0.1 0.2)))
   (⊦= "100%" (lua-format "100%"))
   ;; fmt and %s are C strings, read up to a NUL by lua_pushfstring: a NUL in either is refused
   ;; (C would drop the rest of the text), and %s takes only a string, a bytevector or #f (NULL)
   (⊦= (list AUX_EARG 'lua-format) (api-refusal (λ () (lua-format "a\x0;b"))))
   (⊦= (list AUX_EARG 'lua-format) (api-refusal (λ () (lua-format (bytevector 97 0 98)))))
   (⊦= (list AUX_EARG 'lua-format) (api-refusal (λ () (lua-format "[%s]" "x\x0;y"))))
   (⊦= (list AUX_EARG 'lua-format) (api-refusal (λ () (lua-format "[%s]" (bytevector 120 0)))))
   (⊦= (list AUX_EARG 'lua-format) (api-refusal (λ () (lua-format "[%s]" 42))))
   (⊦= (list AUX_EARG 'lua-format) (api-refusal (λ () (lua-format "[%s]" 'sym))))
   (⊦= (list AUX_EARG 'lua-format) (api-refusal (λ () (lua-format 42))))
   (⊦= "[x][(null)]" (lua-format "[%s][%s]" (bytevector 120) #f))
   (⊦raises (exn lua api) (lua-format "%d"))
   (⊦raises (exn lua api) (lua-format "%d" 1.5))
   ;; %d reads a C int, %I a lua_Integer, %c an int: C can never print a value outside those
   ;; ranges, so they are refused with AUX_ERANGE, as lua-pushinteger / lua-createtable refuse them
   (⊦= "2147483647 -2147483648" (lua-format "%d %d" 2147483647 -2147483648))
   (⊦= "9223372036854775807 -9223372036854775808" (lua-format "%I %I" LUA_MAXINTEGER LUA_MININTEGER))
   (let ((code-of (λ (thunk)
                    (let ((e (condition-case (begin (thunk) #f) (e (exn lua api) e))))
                      (and e (list (get-condition-property e 'lua 'code) (get-condition-property e 'exn 'location)))))))
     (for-each (λ (d n) (⊦= (list AUX_ERANGE 'lua-format) (code-of (λ () (lua-format d n)))))
               (list "%d" "%d" "%d" "%I" "%I" "%I" "%c" "%c")
               (list (expt 2 40) 2147483648 -2147483649 (expt 2 70) (+ 1 LUA_MAXINTEGER) (- LUA_MININTEGER 1) (expt 2 40) (expt 2 70)))
     (⊦= (list AUX_ERANGE 'lua-pushfstring) (code-of (λ () (let ((L (lua-make-empty))) (dynamic-wind void (λ () ((lua-pushfstring "%I" (expt 2 63)) L)) (λ () (lua-close L)))))))
     (⊦= (list AUX_ERANGE 'luaL-error) (code-of (λ () (let ((L (lua-make-empty))) (dynamic-wind void (λ () ((luaL-error "%d" (expt 2 31)) L)) (λ () (lua-close L)))))))))

  ((test/core-condition-identity _)
   (let ((L (fresh)) (c (make-property-condition 'mine 'x 1)))
     ((register! "thrower" (λ (C) (abort c))) L)
     ((register! "errer" (λ (C) (error "bad thing" 1 "two"))) L)
     ((register! "raiser" (λ (C) (abort 42))) L)
     ;; uncaught by Lua: the original object, identity kept (luaL-dostring returns the status, lua-check raises)
     (⊨ (eq? c (condition-case (run! L "thrower()") (e () e))))
     (⊦= 42 (condition-case (run! L "raiser()") (e () e)))
     ((lua-getglobal "thrower") L)
     (⊨ (eq? c (condition-case ((lua-call 0 0) L) (e () e))))
     (⊦= 0 ((lua-gettop) L))
     ;; the status of lua-pcall, with the condition as a userdata on top
     ((lua-getglobal "thrower") L)
     (⊦= LUA_ERRRUN ((lua-pcall 0 0 0) L))
     (⊨ (eq? c ((lua-toobject -1) L)))
     ((lua-pop 1) L)
     ;; seen by Lua
     (run! L "local ok, e = pcall(errer); M = tostring(e); T = type(e)
              ok, e = pcall(raiser); N = tostring(e)
              ok, e = pcall(thrower); E = e")
     (⊦= "bad thing: 1 \"two\"" (global L "M"))
     (⊦= "userdata" (global L "T"))
     (⊦= "42" (global L "N"))
     ((lua-getglobal "E") L)
     (⊨ (eq? c ((lua-toobject -1) L)))
     ((lua-pop 1) L)
     ;; a Scheme error caught by an outer Scheme handler through two Lua layers
     ((register! "outer" (λ (C) ((lua-getglobal "errer") C) ((lua-call 0 0) C) 0)) L)
     (⊦= "bad thing" (condition-case (run! L "outer()") (e (exn) (msg e))))
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)))

  ((test/core-lua-error-values _)
   (let ((L (fresh)))
     ((register! "g" (λ (C) ((lua-pushvalue 1) C) ((lua-error) C))) L)
     (run! L "local t = {} local ok, e = pcall(g, t) R = rawequal(t, e)
              ok, e = pcall(g, 'str') S = e
              ok, e = pcall(g, 12) N = e
              ok, e = pcall(g, nil) local ok2, e2 = pcall(error, nil) Z = (e == e2)")
     (⊦= #t (global L "R"))                                         ; table identity kept through Scheme
     (⊦= "str" (global L "S"))
     (⊦= 12 (global L "N"))
     (⊦= #t (global L "Z"))                                         ; 5.5: "<no error object>", as C
     ;; Lua error values seen in Scheme
     (let ((e (condition-case (run! L "error({code = 7})") (e (exn lua) e))))
       (⊨ (lua-ref? (lua-condition-value e)))
       (⊨ (string-prefix? "table: " (msg e)))
       ((lua-pushref (lua-condition-value e)) L)
       (⊦= LUA_TNUMBER ((lua-getfield -1 "code") L))
       (⊦= 7 ((lua-tointeger -1) L))
       ((lua-settop 0) L))
     (let ((e (condition-case (run! L "error(setmetatable({}, {__tostring = function() return 'custom' end}))") (e (exn lua) e))))
       (⊦= "custom" (msg e)))
     (let ((e (condition-case (run! L "error(nil)") (e (exn lua) e))))
       (⊦= "<no error object>" (msg e))
       (⊦= "<no error object>" (lua-condition-value e)))
     (let ((e (condition-case (run! L "error(true)") (e (exn lua) e))))
       (⊦= "true" (msg e))
       (⊦= #t (lua-condition-value e)))
     (let ((e (condition-case (run! L "error(42)") (e (exn lua) e))))
       (⊦= "42" (msg e))
       (⊦= 42 (lua-condition-value e)))
     (let ((e (condition-case (run! L "error('plain', 0)") (e (exn lua) e))))
       (⊦= "plain" (msg e))
       (⊦= "plain" (lua-condition-value e))
       (⊦= 'errrun (lua-condition-status e))
       (⊦= LUA_ERRRUN (get-condition-property e 'lua 'code)))
     ;; top level lua-error: the value becomes the condition's value
     (run! L "T0 = {}")
     ((lua-getglobal "T0") L)
     (let ((v (condition-case ((lua-error) L) (e (exn lua) (lua-condition-value e)))))
       (⊦= 0 ((lua-gettop) L))
       ((lua-pushref v) L)
       ((lua-setglobal "T1") L)
       (run! L "SAME = rawequal(T0, T1)")
       (⊦= #t (global L "SAME")))
     ;; a value re-raised from a callback keeps its Lua identity
     ((register! "rethrow" (λ (C) (condition-case (run! C "error(T0)") (e (exn lua) (abort e))))) L)
     (run! L "local ok, e = pcall(rethrow) SAME2 = rawequal(e, T0)")
     (⊦= #t (global L "SAME2"))
     ;; lua-toref / lua-pushref
     ((lua-getglobal "T0") L)
     (let ((v ((lua-toref -1) L)))
       ((lua-pop 1) L)
       ((lua-pushref v) L)
       ((lua-setglobal "T2") L)
       (run! L "SAME3 = rawequal(T0, T2)")
       (⊦= #t (global L "SAME3"))
       (let ((other (fresh)))
         (⊦raises (exn lua api) ((lua-pushref v) other))           ; another universe
         (lua-close other)))
     (lua-close L)))

  ((test/core-syntax-and-status _)
   (let ((L (fresh)))
     (⊦= LUA_ERRSYNTAX ((luaL-loadstring "x = = 1") L))
     (⊦= 1 ((lua-gettop) L))
     (⊨ (string? ((lua-tostring -1) L)))
     ((lua-pop 1) L)
     (⊦= 'errsyntax (condition-case ((lua-check (luaL-loadstring "x = = 1")) L) (e (exn lua) (lua-condition-status e))))
     (⊦= 0 ((lua-gettop) L))
     (⊦= LUA_OK ((luaL-loadbufferx "return 1 + 1" "=chunk" "t") L))
     ((lua-call 0 1) L)
     (⊦= 2 ((lua-tointeger -1) L))
     ((lua-pop 1) L)
     (⊦= LUA_OK ((luaL-loadbufferx (string->utf8 "return 'b\\0v'") #f #f) L))
     ((lua-call 0 1) L)
     (⊦= "b\x0;v" ((lua-tostring -1) L))
     ((lua-pop 1) L)
     (⊦= LUA_ERRRUN ((luaL-dostring "error('x')") L))
     ((lua-pop 1) L)
     (⊦= LUA_OK ((luaL-dostring "return 1, 2, 3") L))
     (⊦= 3 ((lua-gettop) L))
     ((lua-settop 0) L)
     ;; message handlers: Lua and Scheme
     (run! L "function H(m) return 'H:' .. m end")
     ((lua-getglobal "H") L)
     (⊦= LUA_OK ((luaL-loadstring "error('e', 0)") L))
     (⊦= LUA_ERRRUN ((lua-pcall 0 0 1) L))
     (⊦= "H:e" ((lua-tostring -1) L))
     ((lua-settop 0) L)
     ((lua-pushcfunction (λ (C) ((lua-pushstring (string-append "S:" ((lua-tostring 1) C))) C) 1)) L)
     (⊦= LUA_OK ((luaL-loadstring "error('e', 0)") L))
     (⊦= LUA_ERRRUN ((lua-pcall 0 0 -2) L))
     (⊦= "S:e" ((lua-tostring -1) L))
     ((lua-settop 0) L)
     (lua-close L)))

  ((test/core-escape-simple _)
   (let ((L (fresh)) (k #f))
     ((register! "esc" (λ (C) (k 'escaped))) L)
     (⊦= 'escaped (call/cc (λ (kk) (set! k kk) (run! L "esc()") 'not-escaped)))
     (⊦= 0 ((lua-gettop) L))
     (⊦= #f %pending-escape)
     (⊦= 0 %depth)
     ;; through lua-pcall (a status binding): the escape still completes
     ((lua-getglobal "esc") L)
     (⊦= 'escaped (call/cc (λ (kk) (set! k kk) ((lua-pcall 0 0 0) L) 'not-escaped)))
     ((lua-settop 0) L)
     ;; through a Lua <close> variable: it runs
     (⊦= 'escaped (call/cc (λ (kk) (set! k kk)
                             (run! L "closed = false do local x <close> = setmetatable({}, {__close = function() closed = true end}); esc() end")
                             'not-escaped)))
     (⊦= #t (global L "closed"))
     ;; through a coroutine
     (⊦= 'escaped (call/cc (λ (kk) (set! k kk) (run! L "local co = coroutine.wrap(function() esc() end); co()") 'not-escaped)))
     (⊦= 0 ((lua-gettop) L))
     ;; while an error value is converted: its __tostring (Scheme) escapes, and the escape wins
     (⊦= 'escaped (call/cc (λ (kk) (set! k kk)
                             (run! L "error(setmetatable({}, {__tostring = function() return esc() end}))")
                             'not-escaped)))
     (⊦= 0 ((lua-gettop) L))
     (⊦= #f %pending-escape)
     ;; Scheme dynamic-wind around the binding runs once each way
     (let ((log '()))
       (⊦= 'escaped (call/cc (λ (kk) (set! k kk)
                               (dynamic-wind (λ () (set! log (cons 'in log)))
                                             (λ () (run! L "esc()") 'not-escaped)
                                             (λ () (set! log (cons 'out log)))))))
       (⊦= '(out in) log))
     (lua-close L)))

  ;; the text of the COND handle a raised Scheme object becomes (a condition's message and written
  ;; arguments, or the object's ->string) runs user code, a record printer, still inside the
  ;; callback: an escape from it completes as any escape of the callback -- through Lua's C frames
  ;; by returning, the state left idle and closable, Lua's C-call count unwound -- and a raise
  ;; from it gives a fallback text
  ((test/core-escape-from-condition-text _)
   (let ((L (fresh)) (k #f))
     (define (escape-to-k r out) (k 'escaped-from-printer))
     (define (text-of fname) ((lua-getglobal "T") L) ((lua-getglobal fname) L) ((lua-call 1 1) L)
                             (begin1 ((lua-tostring -1) L) ((lua-pop 1) L)))
     ((register! "f" (λ (C) (error "boom" (make-printed 1)))) L)
     ((register! "g" (λ (C) (abort (make-printed 2)))) L)
     (run! L "function T(h) local ok, e = pcall(h) return tostring(e) end")
     (⊦= "boom: #<printed>" (text-of "f"))
     (⊦= "#<printed>" (text-of "g"))
     (set! printer-action escape-to-k)
     (⊦= 'escaped-from-printer (call/cc (λ (kk) (set! k kk) (run! L "return f()") 'not-escaped)))
     ;; a continuation that left through Lua's C frames would leave the binding running for ever
     ;; (lua-close refused) and its frames on the C stack: nothing more is tried then
     (⊦= 0 (luaU-active (luaR-u L)))
     (when (= 0 (luaU-active (luaR-u L)))
       (⊦= 'escaped-from-printer (call/cc (λ (kk) (set! k kk) (run! L "return g()") 'not-escaped)))
       (⊦= 'escaped-from-printer (call/cc (λ (kk) (set! k kk) (text-of "f") 'not-escaped)))  ; a Lua pcall cannot keep it
       ((lua-settop 0) L)                                              ; what the escaped lua-call left
       (⊦= #f %pending-escape)
       (⊦= 0 %depth)
       (⊦= 0 (luaU-active (luaR-u L)))
       ;; 300 of them: a frame left behind by each would overflow Lua's C stack count (~200)
       (do ((i 0 (+ i 1))) ((= i 300))
         (⊦= i (call/cc (λ (kk) (set! k (λ (_) (kk i))) (run! L "f()") 'no))))
       ;; from a hook
       ((lua-sethook (λ (H ev ar) (error "hook boom" (make-printed 3))) '(line) 0) L)
       (⊦= 'escaped-from-printer (call/cc (λ (kk) (set! k kk) (run! L "local x = 1") 'not-escaped)))
       ((lua-sethook #f 0 0) L)
       ;; a printer that raises: CHICKEN's writer shows its error in the text; the error reaches Lua
       (set! printer-action (λ (r out) (error "printer failed")))
       (⊨ (string-prefix? "boom: #<Error in printer" (text-of "f")))
       (⊨ (string-prefix? "#<Error in printer" (text-of "g")))
       (set! printer-action #f)
       (⊦= "boom: #<printed>" (text-of "f"))
       (⊦= 0 ((lua-gettop) L))
       (⊦= 0 (luaU-active (luaR-u L)))
       (lua-close L)                                                   ; not refused: nothing is running
       (⊦= #f (luaR-state L)))))

  ;; the text of a COND handle is written under CHICKEN's print-length limit (as print-error-message
  ;; writes an error): it is computed for every raised condition, even one Lua catches and never
  ;; reads, so a circular argument must not make it endless.  A string message is kept whole.
  ((test/core-condition-text-bounded _)
   (let ((L (fresh)) (circ (let ((l (list 1 2 3))) (set-cdr! (cddr l) l) l)))
     (define (text-of fname) ((lua-getglobal "T") L) ((lua-getglobal fname) L) ((lua-call 1 1) L)
                             (begin1 ((lua-tostring -1) L) ((lua-pop 1) L)))
     ;; cut at the print-length limit, about 4096 characters: not far past it, and not much shorter
     (define (bounded? t) (and (string-suffix? "..." t) (<= 4000 (string-length t) 4200)))
     ((register! "circ" (λ (C) (error "boom" circ))) L)                      ; a circular argument
     ((register! "circargs" (λ (C) (abort (make-composite-condition           ; a circular argument list
                                              (make-property-condition 'exn 'message "args" 'arguments circ)))))
      L)
     ((register! "circobj" (λ (C) (abort circ))) L)                           ; a circular non-condition
     ((register! "circmsg" (λ (C) (abort (make-property-condition 'exn 'message circ 'arguments '())))) L)
     ((register! "big" (λ (C) (error "big" (make-string 100000 #\a)))) L)
     ((register! "longmsg" (λ (C) (error (make-string 6000 #\m)))) L)
     (run! L "function T(h) local ok, e = pcall(h) return tostring(e) end")
     (let ((t (text-of "circ")))
       (⊨ (string-prefix? "boom: (1 2 3 1 2 3 1 2 3" t))
       (⊨ (bounded? t)))
     (let ((t (text-of "circargs")))
       (⊨ (string-prefix? "args: 1 2 3 1 2 3" t))
       (⊨ (bounded? t)))
     (let ((t (text-of "circobj")))
       (⊨ (string-prefix? "(1 2 3 1 2 3" t))
       (⊨ (bounded? t)))
     (⊨ (bounded? (text-of "circmsg")))
     (let ((t (text-of "big")))
       (⊨ (string-prefix? "big: \"aaaa" t))
       (⊨ (bounded? t)))
     (⊦= (make-string 6000 #\m) (text-of "longmsg"))                   ; a string message, whole
     ;; the same raise that Lua never reads: pcall's status only
     (run! L "OK = pcall(circ)")
     (⊦= #f (global L "OK"))
     ;; a Scheme handler inside the callback sees the condition itself, untouched
     ((register! "caught" (λ (C) ((lua-pushboolean (handle-exceptions e (eq? circ (car ((condition-property-accessor 'exn 'arguments) e)))
                                                     (error "boom" circ)))
                                   C)
                                 1)) L)
     (run! L "CAUGHT = caught()")
     (⊦= #t (global L "CAUGHT"))
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)))

  ((test/core-escape-nested _)
   (let ((L (fresh)) (k #f) (after-inner '()))
     ((register! "f1" (λ (C) (run! C "f2()") (set! after-inner (cons 'f1 after-inner)) 0)) L)
     ((register! "f2" (λ (C) (run! C "f3()") (set! after-inner (cons 'f2 after-inner)) 0)) L)
     ((register! "f3" (λ (C) (k 'deep))) L)
     (⊦= 'deep (call/cc (λ (kk) (set! k kk) (run! L "f1()") 'no)))
     (⊦= '() after-inner)                                            ; nothing after the escape ran
     (⊦= 0 ((lua-gettop) L))
     (⊦= 0 %depth)
     ;; a target inside an outer callback: the callback goes on and answers normally
     ((register! "g1" (λ (C)
                        (let ((r (call/cc (λ (kk) (set! k kk) (run! C "g2()") 'no))))
                          ((lua-pushstring (symbol->string r)) C)
                          1)))
      L)
     ((register! "g2" (λ (C) (run! C "g3()") 0)) L)
     ((register! "g3" (λ (C) (k 'caught-in-g1))) L)
     (run! L "R = g1()")
     (⊦= "caught-in-g1" (global L "R"))
     (⊦= 0 %depth)
     (⊦= #f %pending-escape)
     ;; 1000 escapes: no leak of handles, stack or depth
     (let ((base (lua-live-handles)))
       (do ((i 0 (+ i 1))) ((= i 1000))
         (⊦= i (call/cc (λ (kk) (set! k (λ (_) (kk i))) (run! L "f3()") 'no))))
       (⊦= 0 ((lua-gettop) L))
       (run! L "collectgarbage()")
       (⊦= base (lua-live-handles)))
     (lua-close L)))

  ;; a Lua pcall that catches the escape sentinel cannot cancel the escape: Lua goes on, later
  ;; callbacks are refused, and the escape completes when the binding returns
  ((test/core-escape-swallowed _)
   (let ((L (fresh)) (k #f) (count 0))
     ((register! "esc" (λ (C) (k 'escaped))) L)
     ((register! "cnt" (λ (C) (set! count (+ count 1)) 0)) L)
     (⊦= 'escaped (call/cc (λ (kk) (set! k kk) (run! L "reached = false; pcall(esc); reached = true; pcall(cnt); cnt()") 'no)))
     (⊦= #t (global L "reached"))
     (⊦= 0 count)
     (run! L "cnt()")                                               ; accepted again afterwards
     (⊦= 1 count)
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)))

  ((test/core-escape-reentry-refused _)
   (let ((L (fresh)) (saved #f) (entries 0))
     ((register! "f" (λ (C) (call/cc (λ (kk) (set! saved kk))) (set! entries (+ entries 1)) 0)) L)
     ;; the refusal is raised by the rewind, so it reaches the handlers the capture and the
     ;; resumption share: this condition-case encloses both
     (⊦= 'refused (condition-case (begin (run! L "f()")
                                         (let ((s saved)) (set! saved #f) (when s (s #f)))
                                         'reentered)
                    (e (exn) 'refused)))
     (⊦= 1 entries)
     (run! L "f()")
     (⊦= 2 entries)
     (lua-close L)))

  ((test/core-nested-callbacks _)
   (let ((L (fresh)))
     ;; f(n) = n == 0 ? 0 : 1 + f(n-1), each level a Scheme -> Lua -> Scheme round trip
     ((register! "f" (λ (C)
                       (let ((n ((lua-tointeger 1) C)))
                         (if (= n 0)
                           ((lua-pushinteger 0) C)
                           (begin
                             ((lua-getglobal "f") C)
                             ((lua-pushinteger (- n 1)) C)
                             ((lua-call 1 1) C)
                             ((lua-pushinteger (+ 1 ((lua-tointeger -1) C))) C)))
                         1)))
      L)
     (run! L "R = f(60)")
     (⊦= 60 (global L "R"))
     (⊦= 0 %depth)
     ;; an error from the deepest level unwinds every level as a Lua error, then a Scheme one
     ((register! "d" (λ (C)
                       (let ((n ((lua-tointeger 1) C)))
                         (when (= n 0) (error "bottom reached"))
                         ((lua-getglobal "d") C) ((lua-pushinteger (- n 1)) C) ((lua-call 1 0) C) 0)))
      L)
     (⊦= "bottom reached" (condition-case (run! L "d(20)") (e (exn) (msg e))))
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)))

  ((test/core-many-callbacks-gc _)
   (let* ((base (lua-live-handles)) (L (fresh)) (u (luaR-u L)) (calls 0))
     (do ((i 0 (+ i 1))) ((= i 2000))
       ((register! "cb" (λ (C) ((lua-pushinteger i) C) 1)) L))
     (run! L "collectgarbage(); collectgarbage()")
     (⊦= (+ base 1) (lua-live-handles))
     (⊦= 1 (luaU-callables u))
     (run! L "R = cb()")
     (⊦= 1999 (global L "R"))
     ((register! "tick" (λ (C) (set! calls (+ calls 1)) (when (= 0 (modulo calls 1000)) (gc #t))
                          ((lua-pushinteger (* 2 ((lua-tointeger 1) C))) C) 1))
      L)
     (run! L "local s = 0 for i = 1, 20000 do s = s + tick(i) end S = s")
     (⊦= 20000 calls)
     (⊦= (* 20000 20001) (global L "S"))
     (lua-close L)
     (⊦= base (lua-live-handles))))

  ;; operands of protected bindings are copied before Lua runs: a callback forcing major
  ;; collections cannot make a binding read moved Scheme memory
  ((test/core-gc-stress _)
   ;; every callback return is a minor collection (it moves nursery objects, the fresh keys
   ;; included); every 50th callback also forces a major one
   (let* ((L (fresh)) (seen '()) (calls 0)
          (collect (λ () (set! calls (+ calls 1)) (if (= 0 (modulo calls 50)) (gc #t) (gc #f)))))
     ((register! "sink" (λ (C) (collect) (set! seen (cons ((lua-tostring 2) C) seen)) 0)) L)
     ((register! "src" (λ (C) (collect) ((lua-pushstring (string-append "v:" ((lua-tostring 2) C))) C) 1)) L)
     (run! L "W = setmetatable({}, {__newindex = function(t, k, v) sink(t, k) end})
              R = setmetatable({}, {__index = function(t, k) return src(t, k) end})")
     ((lua-getglobal "W") L)
     ((lua-getglobal "R") L)
     (do ((i 0 (+ i 1))) ((= i 3000))
       (let ((k (string-append "key-" (number->string i))))
         ((lua-pushinteger i) L)
         ((lua-setfield 1 k) L)
         ((lua-getfield 2 k) L)
         (⊦= (string-append "v:" k) ((lua-tostring -1) L))
         ((lua-pop 1) L)))
     (⊦= 3000 (length seen))
     (⊦= "key-2999" (car seen))
     (⊦= "key-0" (last seen))
     (⊦= 2 ((lua-gettop) L))
     (lua-close L)))

  ;; A Lua __gc finalizer runs inside any allocating API call, and debug.setlocal reaches the slots of
  ;; the C frame below it (level 2): the op frame of a binding, with the values its body has just made.
  ;; A body that then applies a typed call to them (lua_setiuservalue, lua_rawset, lua_setmetatable)
  ;; re-checks its slots first: the binding either succeeds, with the right result, or raises "a slot
  ;; ... was rewritten"; it never misapplies the call (a segmentation fault with a release liblua).
  ;; The finalizer below rewrites every "(C temporary)" of a C frame at level 2 and re-arms itself for
  ;; the next cycle.  Nothing is parsed while it runs unrestricted: Lua's own parser (like
  ;; string.format's buffer) has the same exposure, which only keeping the debug library from untrusted
  ;; code closes (see the README); for lua-load it spares the frames holding no thread (the parser's).
  ;; WIPE replaces the module's private registry tables (thread anchors, hooks, continuations) with 42,
  ;; so that the next binding makes them anew: those allocations are exposed too.
  ((test/core-finalizer-rewrites-op-slots _)
   (let* ((L (fresh)) (u (luaR-u L))
          (rewritten "aux.lua: a slot of a protected operation was rewritten (debug.setlocal from a finalizer)")
          (others '()) (refusals 0)
          (tally (λ (label n thunk)                   ; thunk n times; a refusal is counted, any other error kept
                   (do ((i 0 (+ i 1))) ((= i n))
                     (condition-case (thunk i)
                       (e (exn lua) (if (equal? rewritten (msg e))
                                      (set! refusals (+ refusals 1))
                                      (set! others (cons (list label (msg e)) others)))))
                     ((lua-settop 0) L))))
          (set-flag! (λ (name b) ((lua-pushboolean b) L) ((lua-setglobal name) L)))
          (wipe! (λ () ((lua-getglobal "WIPE") L) ((lua-check (lua-pcall 0 0 0)) L)))
          (f (λ (C) 0))
          (raiser (λ (C) (error "scheme boom" 1)))
          (yielder (λ (C) ((lua-yieldk 0 (λ (st C) 0)) C)))
          (hook (λ (ar C) (void)))
          (reader (λ () (let ((n 0)) (λ () (set! n (+ n 1)) (and (< n 5) "local x = {} "))))))
     (run! L "COS = {}
              for i = 1, 64 do COS[i] = coroutine.create(print) end
              HITS = 0
              function WIPE()
                local r, ks = debug.getregistry(), {}
                for k, v in next, r do
                  if type(k) == 'userdata' and not (type(v) == 'table' and rawget(v, '__name') == 'aux.scheme') then
                    ks[#ks + 1] = k
                  end
                end
                for i = 1, #ks do r[ks[i]] = 42 end
              end
              local mt = {}
              function mt.__gc()
                if not ARMED then return end
                local info = debug.getinfo(2, 'S')
                if info and info.what == 'C' then
                  local slots, thread, i = {}, false, 1
                  while true do
                    local name, v = debug.getlocal(2, i)
                    if not name then break end
                    if name == '(C temporary)' then slots[#slots + 1] = i; thread = thread or type(v) == 'thread' end
                    i = i + 1
                  end
                  if thread or not ONLY_THREADS then
                    for j = 1, #slots do debug.setlocal(2, slots[j], 42) end
                    HITS = HITS + #slots
                  end
                end
                setmetatable({}, mt)
              end
              for i = 1, 200 do setmetatable({}, mt) end
              ARMED = true")
     (tally 'pushobject 600 (λ (i) (let ((o (vector i))) ((lua-pushobject o) L) (⊨ (eq? o ((lua-toobject -1) L))))))
     (tally 'pushcclosure 600 (λ (i) ((lua-pushinteger i) L) ((lua-pushcclosure f 1) L) (⊨ (eq? f ((lua-toprocedure -1) L)))))
     (tally 'callback-raise 600
            (λ (i) ((lua-pushcfunction raiser) L)
                   (⊦= LUA_ERRRUN ((lua-pcall 0 1 0) L))
                   (let ((v (or ((lua-toobject -1) L) ((lua-tostring -1) L))))   ; the condition, or the refusal
                     (⊨ (or (equal? rewritten v) (and (condition? v) (equal? "scheme boom" (msg v))))))))
     (let ((resume-yielder
             (λ (i) (let ((T ((lua-newthread) L)))
                      ((lua-pushcfunction yielder) T)
                      (let-values (((st n) ((lua-resume L 0) T)))
                        (⊨ (or (= st LUA_YIELD) (and (= st LUA_ERRRUN) (equal? rewritten ((lua-tostring -1) T)))))))))
           (hook-on-off
             (λ (i) (let ((T ((lua-newthread) L)))
                      ((lua-sethook hook LUA_MASKCOUNT 1000) T)
                      (⊦= LUA_MASKCOUNT ((lua-gethookmask) T))
                      ((lua-sethook #f 0 0) T)))))
       (tally 'yieldk 400 resume-yielder)
       (tally 'sethook 400 hook-on-off)
       ;; the private tables made anew each time
       (tally 'wipe-newthread 400 (λ (i) (wipe!) (let ((T ((lua-newthread) L))) (⊦= 0 ((lua-gettop) T)) (⊦= LUA_TTHREAD ((lua-type -1) L)))))
       (tally 'wipe-tothread 400 (λ (i) (wipe!) ((lua-getglobal "COS") L) ((lua-geti -1 (+ 1 (modulo i 64))) L)
                                        (⊦= LUA_OK ((lua-status) ((lua-tothread -1) L)))))
       (tally 'wipe-yieldk 300 (λ (i) (wipe!) (resume-yielder i)))
       (tally 'wipe-sethook 300 (λ (i) (wipe!) (hook-on-off i))))
     ;; lua-load with a reader: its parser thread, pinned while the frame of lua-load holds it
     (set-flag! "ONLY_THREADS" #t)
     (tally 'load-reader 300 (λ (i) (let ((st ((lua-load (reader) "=r" "t") L)))
                                      (⊨ (if (= st LUA_OK) (= LUA_TFUNCTION ((lua-type -1) L)) (equal? rewritten ((lua-tostring -1) L)))))))
     (set-flag! "ARMED" #f)
     ((lua-gc 'collect) L) ((lua-gc 'collect) L)
     (⊦= '() others)
     (⊨ (> (global L "HITS") 0))
     (⊨ (> refusals 0))
     ;; the universe is intact, and no failed binding left a handle behind
     ((lua-pushobject 'after) L)
     (⊦= 'after ((lua-toobject -1) L))
     (let ((T ((lua-newthread) L)))
       ((lua-pushcfunction yielder) T)
       (let-values (((st n) ((lua-resume L 0) T))) (⊦= LUA_YIELD st)))
     ((lua-settop 0) L)
     (lua-close L)
     (⊦= 0 (lua-live-handles u))))

  ((test/core-yield _)
   (let ((L (fresh)) (kstatus #f) (base (lua-live-handles)))
     ((register! "y" (λ (C) ((lua-pushinteger (+ 1 ((lua-tointeger 1) C))) C) ((lua-yield 1) C))) L)
     (run! L "local co = coroutine.wrap(function(a) local r = y(a) return r * 10 end)
              A = co(3) B = co(5)")
     (⊦= 4 (global L "A"))
     (⊦= 50 (global L "B"))
     ((register! "yk" (λ (C)
                        ((lua-pushinteger (+ 1 ((lua-tointeger 1) C))) C)
                        ((lua-yieldk 1 (λ (status C)
                                         (set! kstatus status)
                                         ((lua-pushinteger (* 100 ((lua-tointeger -1) C))) C)
                                         1))
                         C)))
      L)
     (run! L "local co = coroutine.wrap(function(a) local r = yk(a) return r * 10 end)
              A = co(3) B = co(5)")
     (⊦= 4 (global L "A"))
     (⊦= 5000 (global L "B"))
     (⊦= LUA_YIELD kstatus)
     ;; an abandoned coroutine releases its continuation
     (run! L "local co = coroutine.wrap(function(a) return yk(a) end) co(1) co = nil collectgarbage() collectgarbage()")
     ;; yielding where Lua cannot: an ordinary Lua error
     ((register! "y0" (λ (C) ((lua-yield 0) C))) L)
     (run! L "local ok, e = pcall(y0) E = e")
     (⊨ (string-suffix? "attempt to yield from outside a coroutine" (global L "E")))
     ;; ... and C's other message on any thread but the main one: a coroutine whose callback runs
     ;; under a C call without continuation (table.sort's comparator), as Lua sees it and as
     ;; Scheme catches it
     (run! L "local co = coroutine.wrap(function() table.sort({3, 2, 1}, function(a, b) y0() return a < b end) end)
              local ok, e = pcall(co) E = e")
     (⊦= "attempt to yield across a C-call boundary" (global L "E"))
     (let ((seen '()))
       ((register! "y1" (λ (C) (set! seen (cons (condition-case ((lua-yield 0) C) (e (exn lua) (list (msg e) (get-condition-property e 'lua 'value))))
                                                seen))
                           0))
        L)
       (run! L "y1() coroutine.wrap(function() table.sort({3, 2, 1}, function(a, b) y1() return a < b end) end)()")
       (⊦= (list "attempt to yield across a C-call boundary" "attempt to yield across a C-call boundary") (car seen))
       (⊦= (list "attempt to yield from outside a coroutine" "attempt to yield from outside a coroutine") (last seen)))
     (run! L "collectgarbage()")
     (⊦= (+ base 4) (lua-live-handles))                              ; y, yk, y0, y1
     (lua-close L)))

;; a coroutine suspended by lua-yieldk looks as in C: its stack holds what the function left (its
  ;; argument and the yielded value), nothing else, and the resumer may pop, clear or move those
  ;; values before it resumes; the continuation sees the frame as C's would
  ((test/core-yieldk-stack _)
   (let* ((L (fresh)) (base (lua-live-handles L)) (seen '())
          (types (λ (S) (map (λ (i) (lua-type->symbol ((lua-type i) S))) (iota ((lua-gettop) S) 1))))
          (k (λ (st K) (set! seen (cons (types K) seen)) ((lua-pushstring "after") K) 1)))
     ((register! "y" (λ (C) ((lua-pushinteger 7) C) ((lua-yieldk 1 k) C))) L)
     (run! L "function body() local r = y('arg') return 'done', r end")
     (define (start)
       (let ((co ((lua-newthread) L)))
         ((lua-pop 1) L)
         ((lua-getglobal "body") co)
         (let-values (((st n) ((lua-resume #f 0) co)))
           (⊦= LUA_YIELD st) (⊦= 1 n)
           (⊦= '(string number) (types co))                            ; not a hidden slot in between
           (⊦eqv 7 ((lua-tointeger -1) co))
           (⊦= "arg" ((lua-tostring 1) co)))
         co))
     (define (finish co nargs)
       (let-values (((st n) ((lua-resume #f nargs) co)))
         (⊦= LUA_OK st) (⊦= 2 n)
         (⊦= '("done" "after") (list ((lua-tostring -2) co) ((lua-tostring -1) co)))))
     ;; as the manual says: the yielded value popped, the resume value pushed
     (let ((co (start))) ((lua-pop 1) co) ((lua-pushstring "R") co) (finish co 1))
     (⊦= '(string string) (car seen))
     ;; everything cleared first
     (let ((co (start))) ((lua-settop 0) co) ((lua-pushstring "R") co) (finish co 1))
     (⊦= '(string) (car seen))
     ;; everything moved to another thread, then resumed with no value
     (let ((co (start)))
       ((lua-xmove L ((lua-gettop) co)) co)
       (⊦= '(string number) (types L))
       ((lua-settop 0) L)
       (finish co 0))
     (⊦= '() (car seen))
     ;; resumed as it is
     (let ((co (start))) (finish co 0))
     (⊦= '(string number) (car seen))
     ;; every continuation ran and was released; an abandoned one goes with its thread
     (let ((co (start)))
       (run! L "collectgarbage() collectgarbage()")                 ; the consumed continuations go
       (⊦= (+ base 2) (lua-live-handles L))                         ; y and the pending continuation
       (⊨ (luaR? co)))
     (let loop ((i 0))                                               ; the thread luaR, then the thread
       (unless (or (= (+ base 1) (lua-live-handles L)) (= i 20))
         (for-each (λ (x) (list x x)) (iota 64))
         (gc #t)
         (run! L "collectgarbage() collectgarbage()")
         (loop (+ i 1))))
     (⊦= (+ base 1) (lua-live-handles L))                           ; y only
     (⊦= 4 (length seen))
     (lua-close L)))

  ((test/core-memory-limit _)
   (let ((L (fresh)))
     (run! L "M = collectgarbage('count')")
     ((lua-setmemorylimit (+ (inexact->exact (floor (* 1024 (global L "M")))) 200000)) L)
     (let ((e (condition-case ((lua-pushlstring (make-string 1000000 #\a)) L) (e (exn lua) e))))
       (⊦= 'errmem (lua-condition-status e))
       (⊦= "not enough memory" (msg e)))
     (⊦= 0 ((lua-gettop) L))
     (⊦= LUA_ERRMEM ((luaL-dostring "S = string.rep('x', 1000000)") L))
     ((lua-pop 1) L)
     ((lua-setmemorylimit 0) L)
     ((lua-pushlstring (make-string 1000000 #\a)) L)
     (⊦= 1000000 (bytevector-length ((lua-tobytevector -1) L)))
     ((lua-pop 1) L)
     ;; the limit is a size_t: anything else is an (exn lua api) raised before C runs (never
     ;; CHICKEN's own FFI error)
     (for-each (λ (b) (let ((e (condition-case (begin ((lua-setmemorylimit b) L) #f) (e (exn lua api) e) (e () 'other))))
                        (⊦= (list 'api AUX_ERANGE) (and (condition? e) (list (lua-condition-status e) (get-condition-property e 'lua 'code))))))
               (list (expt 2 64) (expt 2 100) -1 1.5 'a))
     ((lua-setmemorylimit 18446744073709551615) L)                ; SIZE_MAX: as good as unlimited
     ((lua-pushlstring (make-string 1000 #\a)) L)
     ((lua-pop 1) L)
     ((lua-setmemorylimit 0) L)
     (lua-close L)))

  ((test/core-warnings _)
   (let ((L (fresh)))
     ((lua-setwarnf 'collect) L)
     (run! L "warn('@on') warn('a', 'b', 'c') warn('@off') warn('hidden') warn('@on') warn('x')")
     (⊦= '("abc" "x") ((lua-warnings) L))
     (⊦= '() ((lua-warnings) L))
     ((lua-setwarnf #f) L)
     (run! L "warn('dropped')")
     (⊦= '() ((lua-warnings) L))
     ;; an error in a __gc becomes a warning, never a crash
     ((lua-setwarnf 'collect) L)
     (run! L "setmetatable({}, {__gc = function() error('in gc', 0) end}) collectgarbage()")
     (⊨ (pair? ((lua-warnings) L)))
     ;; 'stderr: lauxlib's writer (the default), nothing is collected
     ((lua-setwarnf 'stderr) L)
     (⊦= "Lua warning: from scheme\nLua warning: from lua, continued\n"
         (with-stderr-captured
           (λ ()
             ((lua-warning "from scheme" #f) L)
             (run! L "warn('from lua, ', 'continued') warn('@off') warn('hidden') warn('@on')"))))
     (⊦= '() ((lua-warnings) L))
     (lua-close L)))

  ;; I6 / J5: pushes onto a suspended or dead coroutine run on the helper thread; lowering its top
  ;; never runs code.  A thread that died inside a Lua function cannot be lowered (its pending
  ;; <close> would run outside the API's contract: lua_settop asserts CIST_TBC); lua-closethread
  ;; (S4) unwinds it.  The thread luaR is built by hand here (S4 provides lua-tothread); the
  ;; coroutines stay anchored in Lua globals.
  ((test/core-non-ok-threads _)
   (let* ((L (fresh)) (ptr #f)
          (co (λ () (make-luaR ptr (luaR-u L) 'thread #f -1 0 #f))))
     ((register! "grab" (λ (C) (set! ptr (luaR-state C)) 0)) L)
     (run! L "closed = 0
              CO = coroutine.create(function()
                local x <close> = setmetatable({}, {__close = function() closed = closed + 1 end})
                grab()
                coroutine.yield(1)
                local n = nil; n = n + 1                  -- dies in the Lua frame that holds x
              end)
              assert(coroutine.resume(CO))")
     (let ((T (co)))
       (⊦= LUA_YIELD (%status ptr))
       (let ((top ((lua-gettop) T)))
         ((lua-pushstring "on a suspended thread") T)                ; helper path
         (⊦= (+ top 1) ((lua-gettop) T))
         (⊦= "on a suspended thread" ((lua-tostring -1) T))
         (⊦= LUA_TNIL ((lua-getglobal "nothing") T))                 ; P* on the helper too
         ((lua-pop 2) T)                                             ; a yield from a C frame: lowering is fine
         (⊦= top ((lua-gettop) T)))
       (⊦raises (exn lua api) ((lua-setglobal "x") T))              ; plain P ops refuse a non-OK thread
       (run! L "local ok, e = coroutine.resume(CO) E = e")
       (⊨ (string-suffix? "attempt to perform arithmetic on a nil value (local 'n')" (global L "E")))
       (⊦= 0 (global L "closed"))                                    ; a dead coroutine keeps its <close> pending
       (let ((top ((lua-gettop) T)))
         (⊦raises (exn lua api) ((lua-settop 0) T))                  ; dead in a Lua frame: refused
         (⊦raises (exn lua api) ((lua-pop 1) T))
         (⊦= top ((lua-gettop) T))
         ((lua-pushinteger 5) T)                                     ; growing is fine
         (⊦= (+ top 1) ((lua-gettop) T)))
       (⊦= 0 (global L "closed")))
     ;; dead inside a C function (error): its frame holds nothing to close, lowering is fine
     (run! L "CO2 = coroutine.create(function()
                local x <close> = setmetatable({}, {__close = function() closed = closed + 10 end})
                grab()
                error('died', 0)
              end)
              coroutine.resume(CO2)")
     (let ((T (co)))
       (⊨ (> ((lua-gettop) T) 0))
       (⊦= "died" ((lua-tostring -1) T))
       ((lua-settop 0) T)
       (⊦= 0 ((lua-gettop) T)))
     (⊦= 0 (global L "closed"))
     (⊦= 0 ((lua-gettop) L))
     (gc #t)
     (lua-close L)))

  ;; a thread luaR whose anchor Lua code (or the bound API) removed is refused, never used as a
  ;; dangling pointer
  ((test/core-thread-anchor-removed _)
   (let* ((L (fresh)) (T ((lua-newthread) L)))
     ((lua-pop 1) L)
     (run! L "co = coroutine.create(function() coroutine.yield(1) end) coroutine.resume(co)")
     ((lua-getglobal "co") L)
     (let ((C2 ((lua-tothread -1) L)))
       ((lua-pop 1) L)
       ;; the anchor is not a registry ref: releasing luaR-ref there does not touch it
       (⊦raises (exn lua) ((luaL-unref LUA_REGISTRYINDEX (luaR-ref T)) L))
       (run! L "collectgarbage() collectgarbage()")
       ((lua-pushinteger 1) T)
       (⊦= 1 ((lua-tointeger -1) T))
       ((lua-settop 0) T)
       ;; Lua code clears the private anchor table, and the threads are collected
       (run! L "co = nil
                for k, v in pairs(debug.getregistry()) do
                  if type(k) == 'userdata' and type(v) == 'table' then
                    for i, x in pairs(v) do if type(x) == 'thread' then v[i] = nil end end
                  end
                end
                collectgarbage() collectgarbage()")
       (⊦raises (exn lua api) ((lua-pushinteger 1) T))
       (⊦raises (exn lua api) ((lua-gettop) T))
       (⊦raises (exn lua api) ((lua-status) C2))
       (⊦raises (exn lua api) ((lua-resume #f 0) C2))
       (⊦raises (exn lua api) ((lua-resume C2 0) T))
       (⊦raises (exn lua api) ((lua-xmove T 0) L))
       ;; new threads are anchored again
       (let ((T2 ((lua-newthread) L)))
         ((lua-pop 1) L)
         ((lua-pushinteger 2) T2)
         (⊦= 2 ((lua-tointeger -1) T2))
         ;; the anchor table itself replaced: its luaRs are refused, a new one is made
         (run! L "local r = debug.getregistry()
                  for k, v in pairs(r) do
                    if type(k) == 'userdata' and type(v) == 'table' then
                      local threads = false
                      for _, x in pairs(v) do if type(x) == 'thread' then threads = true end end
                      if threads then r[k] = 42 end
                    end
                  end")
         (⊦raises (exn lua api) ((lua-gettop) T2))
         (let ((T3 ((lua-newthread) L)))
           ((lua-pop 1) L)
           (⊦= 0 ((lua-gettop) T3)))))
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)))

  ;; P* pushes onto a suspended thread run on the main thread: no helper thread Lua code could remove
  ((test/core-pushes-onto-suspended-thread _)
   (let ((L (fresh)))
     (run! L "co = coroutine.create(function(a) local x = a coroutine.yield(1) end) coroutine.resume(co, 5)")
     ((lua-getglobal "co") L)
     (let ((T ((lua-tothread -1) L)))
       ((lua-pop 1) L)
       (run! L "local r = debug.getregistry()
                for k, v in pairs(r) do if type(k) == 'userdata' and type(v) == 'thread' then r[k] = nil end end
                collectgarbage() collectgarbage()")
       (⊦= LUA_YIELD ((lua-status) T))
       (let ((top ((lua-gettop) T)))
         ((lua-pushstring "abc") T)
         ((lua-createtable 2 2) T)
         (let ((T3 ((lua-newthread) T)))
           (⊦= (+ top 3) ((lua-gettop) T))
           (⊦= "abc" ((lua-tostring (+ top 1)) T))
           (⊨ (luaR? ((lua-tothread -1) T)))
           ((lua-pushinteger 3) T3)
           (⊦= 3 ((lua-tointeger -1) T3)))
         ((lua-getglobal "print") T)
         (let ((d ((lua-getinfo ">L" #f) T))) (⊨ (luaDebug? d)))       ; 'L' allocates: on the main thread
         (⊦= LUA_TNIL ((lua-type -1) T))                                 ; a C function has no lines
         ((lua-settop top) T))
       (⊦= #f ((lua-gethook) T))
       (⊦= LUA_OK ((lua-gc 'collect) T)))
     (lua-close L)))

  ;; a thread luaR keeps its universe open; the universe closes once the thread luaR is gone too
  ((test/core-thread-keeps-universe _)
   (let* ((T (universe-held-by-thread)) (u (luaR-u T)))
     (⊨ (luaR? (luaR-main T)))
     (flush-and-collect (λ () #f))                                       ; the main luaR is only held by T
     (⊨ (pointer? (luaU-state u)))
     ((lua-getglobal "add") T) ((lua-pushinteger 40) T) ((lua-pushinteger 2) T)
     ((lua-call 2 1) T)
     (⊦= 42 ((lua-tointeger -1) T))
     ((lua-pop 1) T)
     (set! T #f)
     (flush-and-collect (λ () (not (luaU-state u))))
     (⊦= #f (luaU-state u))))

  ;; the finalizer of a main luaR never closes a universe whose pointer a binding has just read:
  ;; it defers to the next binding (and never re-registers itself: (gc #t) would loop for ever)
  ((test/core-finalizer-busy _)
   (let* ((La (fresh)) (L (fresh)) (u (luaR-u L)) (L2 (fresh)))
     ;; a binding that raised no longer uses its pointer: a finalizer then closes at once
     (run! La "function boom() error('boom') end")
     ((lua-getglobal "boom") La)
     (⊦raises (exn lua) ((lua-call 0 0) La))
     (%lua-finalize La)
     (⊦= #f (luaU-state (luaR-u La)))
     ;; a binding read L's pointer (its stub is yet to run), and L became garbage meanwhile
     (%in-window L)
     (%lua-finalize L)
     (⊨ (pointer? (luaU-state u)))
     (⊦= #t (luaU-close-pending u))
     (⊨ (pair? (memq L %pending-closes)))
     (gc #t)                                                              ; returns
     (⊨ (pointer? (luaU-state u)))
     ((lua-gettop) L2)                                                    ; the next binding closes it
     (⊦= #f (luaU-state u))
     (⊦raises (exn lua api) ((lua-gettop) L))
     (⊨ (pointer? (luaU-state (luaR-u L2))))
     ;; deferred inside a callback of the universe (it is running): closed after it
     (let* ((L3 (fresh)) (u3 (luaR-u L3)))
       ((register! "fin" (λ (C) (%lua-finalize L3) (gc #t) 0)) L3)
       (run! L3 "fin()")
       (⊨ (pointer? (luaU-state u3)))
       ((lua-gettop) L2)
       (⊦= #f (luaU-state u3)))
     (lua-close L2)))

  ;; the finalizer of a main luaR runs inside CHICKEN's finalizer loop, which a continuation must
  ;; not leave (the loop would stay marked as running: no finalizer would ever run again, every
  ;; (gc #t) would raise "re-entry from finalizer thread").  A universe holding Scheme callables
  ;; (a __gc among them may escape) is therefore only marked there, and closed at the next binding
  ;; entry, where an escape of its __gc leaves that binding as usual; the universes not closed yet
  ;; stay pending for the next one.
  ((test/core-finalizer-escape _)
   (let* ((u1 (make-escaping-universe)) (u2 (make-escaping-universe)) (us (list u1 u2)))
     (let loop ((i 0))                                             ; no binding runs meanwhile
       (unless (or (= i 30) (and (luaU-close-pending u1) (luaU-close-pending u2)))
         (for-each (λ (x) (list x x)) (iota 64))
         (gc #t)
         (loop (+ i 1))))
     (⊦= '(#t #t) (map luaU-close-pending us))
     (⊦= 2 (length (filter luaU-state us)))                        ; marked, not closed
     ;; the escape is a Lua error in __gc for Lua, which lua_close turns into a warning (the universe
     ;; writes warnings to stderr, as luaL_newstate's): captured here, with the continuation inside
     (let* ((got #f)
            (err (with-stderr-captured
                   (λ () (set! got (call/cc (λ (k) (set! escape-k k) (lua-live-handles) 'no-escape)))))))
       (⊦= 'escaped-from-gc got)
       (⊦= "Lua warning: error in __gc (error object is not a string)\n" err))
     (⊦= 1 (length (filter luaU-state us)))                        ; the escape left the other pending
     (⊦= "" (with-stderr-captured lua-live-handles))              ; the next binding entry closes it
     (⊦= '() (filter luaU-state us))
     (for-each (λ (u) (⊦= 0 (lua-live-handles u))) us)
     ;; the finalizer loop is intact: (gc #t) returns and plain finalizers still run
     (let ((ran #f))
       (set-finalizer! (make-vector 3 0) (λ (x) (set! ran #t)))
       (let loop ((i 0)) (unless (or ran (= i 30)) (for-each (λ (x) (list x x)) (iota 64)) (gc #t) (loop (+ i 1))))
       (⊦= #t ran))
     ;; a universe without callables is still closed by its finalizer, at once
     (let ((w (let ((L (fresh))) (run! L "X = setmetatable({}, {__gc = function() end})") (luaR-u L))))
       (let loop ((i 0)) (unless (or (not (luaU-state w)) (= i 30)) (for-each (λ (x) (list x x)) (iota 64)) (gc #t) (loop (+ i 1))))
       (⊦= #f (luaU-state w))
       (⊦= #f (luaU-close-pending w)))))

  ;; universes whose callbacks capture their own luaR are collected (the Scheme objects they hand
  ;; to Lua live in a table of the universe, not in a global root)
  ((test/core-capturing-callbacks-collected _)
   (let* ((base (lua-live-handles)) (ws (map (λ (i) (capturing-universe)) (iota 20))))
     (flush-and-collect (λ () (null? (filter locative->object ws))))
     (⊦= '() (filter locative->object ws))
     (⊨ (<= (lua-live-handles) base))))

  ;; lua_pcallk's precondition: the message handler is a function
  ((test/core-pcall-msgh _)
   (let ((L (fresh)))
     ((lua-pushinteger 3) L)
     ((luaL-loadstring "error('boom')") L)
     (⊦raises (exn lua api) ((lua-pcall 0 0 1) L))
     (⊦= 2 ((lua-gettop) L))                                             ; nothing consumed
     ((lua-settop 0) L)
     ((luaL-loadstring "return function(m) return 'handled: ' .. m end") L)
     ((lua-call 0 1) L)
     ((luaL-loadstring "error('boom', 0)") L)
     (⊦= LUA_ERRRUN ((lua-pcall 0 0 1) L))
     (⊦= "handled: boom" ((lua-tostring -1) L))
     ((lua-settop 0) L)
     (⊦raises (exn lua api) ((lua-check (λ (L) LUA_ERRRUN)) L))          ; nothing to raise
     (lua-close L)))

  ;; a Lua call hook sees the trampoline of every protected binding: rewriting its slots, or
  ;; calling it again later, must not reach C with unchecked operands
;; the trampoline of a protected binding is the module's own C function, not a call the program
  ;; made: as in C, where lua_rawset fires no hook, no call or return hook sees it enter or leave.
  ;; Lua code can still reach it from a metamethod the op runs, and calling it later is refused.
  ((test/core-trampoline _)
   (let ((L (fresh)))
     (run! L "SEEN = 0 debug.sethook(function(ev) if debug.getinfo(2, 'S').what == 'C' then SEEN = SEEN + 1 end end, 'cr')")
     ((lua-pushinteger 0) L) ((lua-setglobal "SEEN") L)
     ((lua-newtable) L)
     ((lua-pushstring "k") L)
     ((lua-pushstring "v") L)
     ((lua-rawset 1) L)
     ((lua-getfield 1 "k") L)
     (⊦= "v" ((lua-tostring -1) L))
     (⊦= 0 (global L "SEEN"))                                     ; three trampolines, no event
     ((lua-settop 0) L)
     (run! L "debug.sethook()
              T = setmetatable({}, {__index = function(t, k) STOLEN = debug.getinfo(2, 'f').func return 1 end})")
     ((lua-getglobal "T") L)
     ((lua-getfield -1 "x") L)                                     ; the metamethod's caller: the trampoline
     ((lua-settop 0) L)
     (⊦= 'function (global L "STOLEN"))
     (⊨ (string-suffix? "called out of its context"
                        (condition-case (run! L "STOLEN()") (e (exn lua) (msg e)))))
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)))

  ;; a return hook that raises cannot make a committed op look failed (the binding would release
  ;; what Lua holds, and a reused handle would then resolve to another Scheme object): pushes that
  ;; store a handle, refs and threads all succeed under it, and every handle keeps its identity
  ((test/core-return-hook-commit _)
   (let* ((L (fresh)) (base (lua-live-handles L))
          (F (λ (C) ((lua-pushstring "I am F") C) 1))
          (G (λ (C) ((lua-pushstring "I am G") C) 1)))
     (run! L "ARMED = false debug.sethook(function() if ARMED then error('return hook') end end, 'r')")
     ((lua-pushboolean #t) L) ((lua-setglobal "ARMED") L)          ; from now on every hooked return raises
     ((lua-pushcfunction F) L)                                     ; 1
     ((lua-pushobject 'X) L)                                       ; 2
     ((lua-pushobject 'R) L)
     (let* ((ref ((luaL-ref LUA_REGISTRYINDEX) L))
            (T ((lua-newthread) L))                                 ; 3
            (v (begin ((lua-newtable) L) ((lua-toref -1) L))))     ; 4
       ((lua-pushboolean #f) L) ((lua-setglobal "ARMED") L)
       (⊦= 4 ((lua-gettop) L))
       (⊨ (luaR? T))
       (⊨ (lua-ref? v))
       (⊦= (+ base 3) (lua-live-handles L))                        ; F, X and R
       ;; the handles are Lua's: collections free nothing, new handles take new numbers
       ((lua-gc 'collect) L) ((lua-gc 'collect) L)
       (⊦= (+ base 3) (lua-live-handles L))
       ((lua-pushcfunction G) L) ((lua-setglobal "g") L)
       ((lua-pushobject 'Y) L)
       ((lua-gc 'collect) L) ((lua-gc 'collect) L)
       (⊨ (eq? F ((lua-toprocedure 1) L)))
       (⊦= 'X ((lua-toobject 2) L))
       (⊦= 'Y ((lua-toobject -1) L))
       ((lua-pop 1) L)
       (⊦= LUA_TUSERDATA ((lua-rawgeti LUA_REGISTRYINDEX ref) L))
       (⊦= 'R ((lua-toobject -1) L))
       ((lua-pop 1) L)
       ((lua-pushvalue 1) L) ((lua-call 0 1) L)
       (⊦= "I am F" ((lua-tostring -1) L))
       ((lua-pop 1) L)
       (run! L "G = g()")
       (⊦= "I am G" (global L "G"))
       ((luaL-unref LUA_REGISTRYINDEX ref) L))
     ((lua-settop 0) L)
     (run! L "debug.sethook() g = nil collectgarbage() collectgarbage()")
     (⊦= base (lua-live-handles L))
     (lua-close L)))

  ;; the same with a Scheme return hook, in a state without any library, and a Scheme hook set with
  ;; a return mask does not fire on the return of lua-sethook's own trampoline
  ((test/core-scheme-return-hook-commit _)
   (let* ((L (lua-make-empty)) (armed #f) (fired 0)
          (hook (λ (H ev ar) (when (eq? ev 'return) (set! fired (+ fired 1)) (when armed (error "hook says no"))) 0)))
     ((lua-sethook hook '(return) 0) L)
     (⊦= 0 fired)
     (set! armed #t)
     ((lua-pushobject 'X) L)
     ((lua-pushobject 'Y) L)
     (set! armed #f)
     (⊦= 0 fired)
     (⊦= 3 (lua-live-handles L))                                   ; the hook, X and Y
     ((lua-gc 'collect) L) ((lua-gc 'collect) L)
     (do ((i 0 (+ i 1))) ((= i 4)) ((lua-pushobject (list 'Z i)) L))
     (⊦= '(X Y) (list ((lua-toobject 1) L) ((lua-toobject 2) L)))
     ;; a hook that yields on call and return events, set on a thread from its own callback: the
     ;; trampoline's return is not an event, so lua-sethook itself does not raise
     (let ((T ((lua-newthread) L)) (outcome #f))
       ((lua-pushcfunction (λ (C)
                             (set! outcome (condition-case (begin ((lua-sethook (λ (H ev ar) ((lua-yield 0) H)) '(call return) 0) C) 'set)
                                             (e (exn lua) (msg e))))
                             ((lua-sethook #f 0 0) C)
                             0))
        T)
       (let-values (((st n) ((lua-resume #f 0) T))) (⊦= LUA_OK st))
       (⊦= 'set outcome))
     ((lua-sethook #f 0 0) L)
     ((lua-settop 0) L)
     ((lua-gc 'collect) L) ((lua-gc 'collect) L)
     (⊦= 0 (lua-live-handles L))
     (lua-close L)))

  ;; luaL-newstate: a main luaR made by luaL_newstate, as lua-make-empty (no library opened);
  ;; before the port it answered the bare lua_State pointer
  ((test/core-luaL-newstate _)
   (let ((L (luaL-newstate)))
     (⊨ (luaR? L))
     (⊦= 'main (luaR-kind L))
     ((lua-getglobal "print") L)
     (⊦= LUA_TNIL ((lua-type -1) L))                               ; no library
     ((lua-pop 1) L)
     ((luaL-openlibs) L)
     (run! L "X = string.rep('ab', 3)")
     (⊦= "ababab" (global L "X"))
     ((lua-setmemorylimit 1) L)                                    ; the module's allocator takes over
     (⊦raises (exn lua) ((lua-pushlstring (make-string 100000 #\a)) L))
     ((lua-setmemorylimit 0) L)
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)
     (⊦raises (exn lua api) ((lua-gettop) L))))

  ;; (aux lua) exports the bindings, the constants, the condition accessors, lua-live-handles and
  ;; read-only record predicates: no raw stub (it would call the API unchecked), constructor or
  ;; mutator (they could forge the pointers, floors and handles the checks rely on)
  ((test/core-public-interface _)
   (let* ((env (module-environment '(aux lua)))
          (bound? (λ (name) (condition-case (begin (eval name env) #t) (e () #f))))
          (exports (call-with-values (λ () (##sys#module-exports (##sys#find-module 'aux.lua))) (λ (e v s) (map car v))))
          (public? (λ (name)
                     (let ((s (symbol->string name)))
                       (or (memq name '(luaR? luaR-kind luaDebug?))
                           (and (not (string-suffix? "-set!" s))
                                (not (string-prefix? "lua-yield-request" s))
                                (or (string-prefix? "lua-" s) (string-prefix? "luaL-" s) (string-prefix? "luaopen-" s)
                                    (string-prefix? "LUA_" s) (string-prefix? "LUAL_" s))))))))
     (⊨ (> (length exports) 300))
     (⊦= '() (filter (λ (n) (not (public? n))) exports))
     (⊦= '() (filter (λ (n) (not (bound? n))) '(lua-gettop luaL-newstate luaopen-base LUA_OK lua-check luaR? luaDebug?)))
     (⊦= '() (filter bound? '(%rawlen-raw %pushinteger-raw %type-raw %gettop %L luaR-state luaR-state-set!
                              luaR-floor-set! luaR-u make-luaR make-luaU luaU-objects luaU-callables-set!
                              luaDebug-ptr luaDebug-ptr-set! make-luaDebug make-luaV luaV-ref-set!
                              AUX_EINDEX AUX_K_PROC aux_lua_dispatch aux_lua_kdispatch
                              make-lua-yield-request lua-yield-request-k-set! %handle-free!)))
     ;; what the raw stubs would do is refused by the bindings
     (let ((L (lua-make-empty)))
       (⊦raises (exn lua api) ((lua-rawlen -100000000) L))
       (lua-close L))))

  ;; lua-ref? answers #t for a luaV only: never for a number, a string, a luaR, a luaDebug, a
  ;; Scheme object handed to Lua (as it comes back), a condition, #f or a procedure
  ((test/ref-predicate _)
   (let ((L (fresh)))
     ((lua-newtable) L)
     (let ((v ((lua-toref -1) L)))
       (⊨ (lua-ref? v))
       ((lua-pushobject (list 'obj)) L)
       ((lua-getglobal "print") L)
       (let ((d ((lua-getinfo ">S" #f) L))
             (o ((lua-toobject -1) L))
             (T ((lua-newthread) L)))
         (⊨ (luaDebug? d))
         (for-each (λ (x) (⊭ (lua-ref? x)))
                   (list 1 1.5 "s" (string->utf8 "s") 'sym #f #t '() (list v) (vector v) L T d o
                         (make-property-condition 'lua) (λ (C) 0) luaopen-base (void))))
       ((lua-settop 0) L)
       ((lua-pushref v) L)
       (⊦= LUA_TTABLE ((lua-type -1) L)))
     (lua-close L)))

  ;; lua-pushinteger / lua-pushnumber refuse a bad argument before Lua runs with an (exn lua api),
  ;; as every lua_Integer / lua_Number argument (lua-seti, luaL-optinteger, luaL-optnumber, ...)
  ((test/push-number-refusals _)
   (let ((L (lua-make-empty)))
     (define (api-props thunk)
       (let ((e (condition-case (begin (thunk) #f) (e (exn lua api) e))))
         (and e (lua-condition? e)
              (list (lua-condition-status e) (get-condition-property e 'lua 'code) (get-condition-property e 'exn 'location)))))
     (for-each (λ (n) (⊦= (list 'api AUX_ERANGE 'lua-pushinteger) (api-props (λ () ((lua-pushinteger n) L)))))
               (list 9223372036854775808 -9223372036854775809 (expt 2 100) 1.5 2.0 +inf.0 'a "1" #f))
     (for-each (λ (x) (⊦= (list 'api AUX_EARG 'lua-pushnumber) (api-props (λ () ((lua-pushnumber x) L)))))
               (list 'a "1.5" #f (list 1) (void)))
     (⊦= 0 ((lua-gettop) L))
     ;; the lua_Integer range, fixnums and bignums alike
     (for-each (λ (n) ((lua-pushinteger n) L) (⊦eqv n ((lua-tointeger -1) L)) (⊨ ((lua-isinteger -1) L)))
               (list 0 -1 9223372036854775807 -9223372036854775808 4611686018427387904))
     ;; any real: a float in Lua
     (for-each (λ (x y) ((lua-pushnumber x) L) (⊦eqv y ((lua-tonumber -1) L)) (⊭ ((lua-isinteger -1) L)))
               (list 3 1/4 (expt 2 70) -0.5) (list 3.0 0.25 (exact->inexact (expt 2 70)) -0.5))
     ((lua-settop 0) L)
     (lua-close L)
     ;; inside a callback a handler of (exn lua api) catches it, and Lua never sees it
     (let ((L (fresh)))
       ((register! "f" (λ (C) ((lua-pushboolean (condition-case (begin ((lua-pushinteger 1.5) C) #f) (e (exn lua api) #t))) C) 1)) L)
       (run! L "R = f()")
       (⊦= #t (global L "R"))
       (lua-close L))))

  ;; I12: srfi-18 threads, each with a universe of its own, whose callbacks switch threads (an
  ;; explicit thread-yield!, or a tiny timeslice): one thread at a time runs Lua with callbacks, so
  ;; every answer reaches its own Lua call, and every universe ends idle and closable
  ((test/core-threads-serialized _)
   (define (worker id iters quantum)
     (let ((t (make-thread
                (λ ()
                  (let ((L (fresh)) (sum 0))
                    ((register! "f" (λ (C)
                                      (thread-yield!)
                                      (let loop ((i 0) (acc '())) (when (< i 300) (loop (+ i 1) (cons i acc))))  ; a few timeslices
                                      ((lua-pushinteger (+ id ((lua-tointeger 1) C))) C) 1)) L)
                    (do ((i 0 (+ i 1))) ((= i iters))
                      (run! L "local co = coroutine.wrap(function(n) local s = 0 for i = 1, 3 do s = s + f(n) coroutine.yield(s) end end)
                               X = co(1) + co() + f(0)")
                      (set! sum (+ sum (global L "X"))))
                    (let ((idle (= 0 (luaU-active (luaR-u L)))))
                      (lua-close L)                                           ; not refused: nothing is left running
                      (list id sum idle (luaR-state L)))))
                id)))
       (when quantum (thread-quantum-set! t quantum))
       t))
   (let ((ts (list (worker 1 30 #f) (worker 2 40 50) (worker 3 25 10))))
     (for-each thread-start! ts)
     ;; f(n) answers id + n: X = (id+1) + ((id+1) + (id+1)) + id, per iteration
     (⊦= (map (λ (id n) (list id (* n (+ (* 3 (+ id 1)) id)) #t #f)) '(1 2 3) '(30 40 25))
         (map thread-join! ts)))
   (⊦= '(#f 0) (list %lua-owner %lua-holds))
   ;; a callback that waits for another thread which wants Lua in turn: the waiter raises instead
   ;; of waiting for ever (the callback joins it, or waits for a mutex it holds)
   (let ((L (fresh)) (L2 (fresh)) (m (make-mutex)) (flag #f))
     ((register! "g" (λ (C) ((lua-pushinteger 7) C) 1)) L2)              ; L2 takes the safe path
     (define (wants-lua) (condition-case (begin (run! L2 "Y = g()") 'ran)
                           (e (exn lua api) (list 'refused (get-condition-property e 'lua 'code)))))
     (define joiner (make-thread wants-lua))
     (define locker (make-thread (λ () (mutex-lock! m) (let loop () (unless flag (thread-yield!) (loop)))
                                   (begin1 (wants-lua) (mutex-unlock! m)))))
     ((register! "j" (λ (C) (thread-start! joiner) ((lua-pushboolean (equal? (thread-join! joiner) (list 'refused AUX_ETHREAD))) C) 1)) L)
     ((register! "k" (λ (C) (set! flag #t) (mutex-lock! m) (mutex-unlock! m) ((lua-pushboolean #t) C) 1)) L)
     (run! L "J = j()")
     (⊦= #t (global L "J"))
     (thread-start! locker)
     (thread-yield!)                                                     ; the locker takes m
     (run! L "K = k()")
     (⊦= #t (global L "K"))
     (⊦= (list 'refused AUX_ETHREAD) (thread-join! locker))
     ;; once the owner is out of Lua, the other thread runs Lua normally
     (⊦= 'ran (thread-join! (thread-start! (make-thread wants-lua))))
     (⊦= 7 (global L2 "Y"))
     (⊦= '(#f 0) (list %lua-owner %lua-holds))
     (lua-close L)
     (lua-close L2)))

  ;; I12: a binding waits for the lock BEFORE it reads its state.  While it waits, the owner
  ;; (thread A, inside a callback of V) may close that state, or return from the callback whose
  ;; view the waiter holds (then the coroutine behind it may be collected): the waiter then
  ;; refuses its luaR, and never runs its stub on the freed or stale lua_State
  ((test/core-threads-wait-before-read _)
   (define closed "closed, invalidated or not a lua state")
   ;; B's outcome of (op T) begun while A holds the lock in a callback of V; A then closes T
   (define (race T op)
     (let ((V (fresh)) (in-hold #f) (waiting #f))
       ((register! "hold" (λ (C)
                            (set! in-hold #t)
                            (let loop () (unless waiting (thread-yield!) (loop)))
                            (do ((i 0 (+ i 1))) ((= i 10)) (thread-yield!))   ; B is in its wait, if it waits
                            (lua-close T)
                            0)) V)
       (let ((A (make-thread (λ () (run! V "hold()") 'a-done)))
             (B (make-thread (λ ()
                               (let loop () (unless in-hold (thread-yield!) (loop)))
                               (set! waiting #t)
                               (condition-case (begin (op T) 'ran)
                                 (e (exn lua api) (list 'refused (msg e))))))))
         (thread-start! A) (thread-start! B)
         (let ((r (thread-join! B)))
           (⊦= 'a-done (thread-join! A))
           (lua-close V)
           r))))
   (define (reader) (let ((done #f)) (λ () (if done #f (begin (set! done #t) "return 1")))))
   ;; (name kind op).  kind 'p: a binding that may run Lua code, which waits while the state holds
   ;; a Scheme procedure; 'adds: one that gives the state a Scheme procedure before its stub runs,
   ;; which always waits; 'd: a direct binding (no Lua code runs, no callback), which never waits
   (define ops
     (list (list 'getglobal 'p (λ (L) ((lua-getglobal "print") L)))
           (list 'dostring 'p (λ (L) ((lua-check (luaL-dostring "X = 1")) L)))
           (list 'pushcfunction 'adds (λ (L) ((lua-pushcfunction nop-callback) L)))
           (list 'sethook 'adds (λ (L) ((lua-sethook (λ (H e ar) 0) '(count) 1000) L)))
           (list 'load-reader 'adds (λ (L) ((lua-load (reader) "=r" #f) L)))
           (list 'settop 'd (λ (L) ((lua-settop 0) L)))))
   (let ((live (lua-live-handles)))
     (for-each
       (λ (o)
         (let ((name (car o)) (kind (cadr o)) (op (caddr o)))
           ;; a state with a Scheme procedure: every binding but a direct one waits, then refuses
           ;; the closed state
           (let ((T (fresh)))
             ((register! "g" nop-callback) T)
             (⊦= (list name (if (eq? kind 'd) 'ran (list 'refused closed))) (list name (race T op))))
           ;; a state without one: only the bindings that add one wait; the others run at once
           (⊦= (list name (if (eq? kind 'adds) (list 'refused closed) 'ran)) (list name (race (fresh) op)))
           (⊦= '(#f 0) (list %lua-owner %lua-holds))
           (⊦= live (lua-live-handles))))
       ops))
   ;; the view of a callback, handed to B: B's binding waits until the callback has returned,
   ;; then refuses the view (the old code pushed onto the thread the view had named)
   (for-each
     (λ (chunk)
       (let ((U (fresh)) (view #f) (waiting #f))
         ((register! "cb" (λ (C)
                            (set! view C)
                            (let loop () (unless waiting (thread-yield!) (loop)))
                            (do ((i 0 (+ i 1))) ((= i 10)) (thread-yield!))
                            0)) U)
         (let ((A (make-thread (λ () (run! U chunk) 'a-done)))
               (B (make-thread (λ ()
                                 (let loop () (unless view (thread-yield!) (loop)))
                                 (set! waiting #t)
                                 (condition-case (begin ((lua-getglobal "print") view) 'pushed)
                                   (e (exn lua api) (list 'refused (msg e))))))))
           (thread-start! A) (thread-start! B)
           (⊦= (list 'refused closed) (thread-join! B))
           (⊦= 'a-done (thread-join! A))
           (⊦= 0 ((lua-gettop) U))
           (⊦= '(#f 0) (list %lua-owner %lua-holds))
           (lua-close U))))
     (list "cb()"
           "local co = coroutine.create(function() cb() end) coroutine.resume(co) co = nil
            collectgarbage() collectgarbage()
            local t = {} for i = 1, 300 do t[i] = coroutine.create(print) end t = nil collectgarbage()")))

  ;; A Lua error raised by a binding inside a callback crosses the callback as a condition and is
  ;; raised again once the callback has returned (no longjmp crosses CHICKEN frames): its value
  ;; and status are kept, but an enclosing message handler runs at the callback's frame, so a
  ;; traceback there misses the frames that raised.  A lua-pcall with a handler of the callback's
  ;; own runs it at the error point, as C does.
  ((test/core-callback-error-under-handler _)
   (let ((L (fresh)))
     (run! L "E = {} function inner() error('deep') end function inner_e() error(E) end")
     ((register! "cb" (λ (C) ((lua-getglobal "inner") C) ((lua-call 0 0) C) 0)) L)
     ((register! "cb_e" (λ (C) ((lua-getglobal "inner_e") C) ((lua-call 0 0) C) 0)) L)
     ((register! "cb_own" (λ (C)
                            ((lua-getglobal "debug") C) ((lua-getfield -1 "traceback") C)   ; 1: debug, 2: traceback
                            ((lua-getglobal "inner") C)
                            (⊦= LUA_ERRRUN ((lua-pcall 0 0 2) C))
                            ((lua-error) C))) L)
     (run! L "ok, TB = xpcall(function() cb() end, debug.traceback)
              okE, SAME = pcall(cb_e) SAME = SAME == E
              ok2, TB2 = pcall(cb_own)")
     (define (has? tb s) (and (substring-index s tb) #t))
     (let ((tb (global L "TB")) (tb2 (global L "TB2")))
       (⊨ (has? tb "deep"))
       (⊨ (has? tb "in global 'cb'"))
       (⊭ (has? tb "'inner'"))                 ; the frames that raised are gone
       (⊭ (has? tb "in global 'error'"))
       (⊨ (has? tb2 "deep"))
       (⊨ (has? tb2 "in function 'inner'"))    ; the callback's own handler sees them
       (⊨ (has? tb2 "in global 'error'"))
       (⊨ (has? tb2 "'cb_own'")))
     (⊦= #t (global L "SAME"))                             ; the error value itself comes back
     (lua-close L)))
  ;; I6, normal threads.  While a coroutine's Scheme callback runs, the thread that resumed it is a
  ;; NORMAL one: status LUA_OK, waiting in a C function (coroutine.resume's luaB_coresume, whose
  ;; slot 1 is the running coroutine, and maybe its only reference).  Every binding that would
  ;; lower, move or overwrite that frame's slots is refused (AUX_EINDEX), whatever luaR of the
  ;; thread it is given: the main luaR, a 'thread luaR from lua-tothread, a still-live view of an
  ;; outer callback.  (The old code let ((lua-settop 0) L) drop the running coroutine: the next
  ;; full collection freed it under its own callback, a use-after-free.)  Values pushed onto such
  ;; a thread can be popped again, down to the frame's height; a Scheme closure's frame (an outer
  ;; callback that called lua-resume) has no floor: its slots are the caller's.
  ((test/core-normal-thread-floor _)
   (define (code-of thunk)
     (condition-case (begin (thunk) 'ran) (e (exn lua api) (get-condition-property e 'lua 'code))))
   ;; the ops that would lower, move or overwrite slot 1 of T (C: the inner callback's view): the
   ;; refusal code of each ('ran if it ran); a value an op pushed first is popped again
   (define (pushed-7 T thunk) ((lua-pushinteger 7) T) (begin1 (code-of thunk) ((lua-pop 1) T)))
   (define ops
     (list (list 'settop (λ (T C) (code-of (λ () ((lua-settop 0) T)))))
           (list 'pop (λ (T C) (code-of (λ () ((lua-pop 1) T)))))
           (list 'remove (λ (T C) (code-of (λ () ((lua-remove 1) T)))))
           (list 'replace (λ (T C) (pushed-7 T (λ () ((lua-replace 1) T)))))
           (list 'copy (λ (T C) (pushed-7 T (λ () ((lua-copy 2 1) T)))))
           (list 'xmove (λ (T C) (code-of (λ () ((lua-xmove C 1) T)))))
           (list 'rotate (λ (T C) (pushed-7 T (λ () ((lua-rotate 1 1) T)))))
           (list 'insert (λ (T C) (pushed-7 T (λ () ((lua-insert 1) T)))))
           (list 'setglobal (λ (T C) (code-of (λ () ((lua-setglobal "x") T)))))        ; P ops consuming slot 1
           (list 'concat (λ (T C) (code-of (λ () ((lua-concat 1) T)))))
           (list 'error (λ (T C) (code-of (λ () ((lua-error) T)))))))
   ;; (name code top-after type1-after) for op on the target of kind: 'main (the main luaR), 'thread
   ;; (lua-tothread of the resumer), 'view (the resumer's own callback view, still live)
   (define (run-op kind name op)
     (let ((L (fresh)) (out #f) (outer-view #f))
       ((register! "cbO" (λ (C) (set! outer-view C) ((lua-getglobal "runinner") C) ((lua-call 0 0) C) 0)) L)
       ((register! "cb" (λ (C)
                          (let* ((T (case kind
                                      ((main) L)
                                      ((thread) ((lua-tothread 1) C))       ; the resumer, passed as argument
                                      ((view) outer-view)))
                                 (c (op T C)))
                            (set! out (list name c ((lua-gettop) T) ((lua-type 1) T)))
                            ((lua-gc LUA_GCCOLLECT) L) ((lua-gc LUA_GCCOLLECT) L)   ; the running coroutine is kept
                            0))) L)
       (run! L "function runinner()
                  local me = coroutine.running()
                  local ok, v = coroutine.resume(coroutine.create(function() cb(me) collectgarbage()
                    local t = {} for i = 1, 200 do t[i] = {} end return 'inner done' end))
                  R = tostring(ok) .. ' ' .. tostring(v)
                end")
       (case kind
         ((main) (run! L "runinner()"))
         (else (run! L "local co = coroutine.create(function() cbO() end) assert(coroutine.resume(co))")))
       (let ((r (global L "R")))
         (⊦= 0 ((lua-gettop) L))
         (lua-close L)
         (list out r))))
   (for-each
     (λ (kind)
       (for-each
         (λ (o)
           (let ((expected (if (eq? (car o) 'error) AUX_EUNDER AUX_EINDEX)))
             (⊦= (list (list kind (car o) expected 1 LUA_TTHREAD) "true inner done")
                 (let ((r (run-op kind (car o) (cadr o)))) (cons (cons kind (car r)) (cdr r))))))
         ops))
     '(main thread view))
   ;; above the floor: pushes, pops, growing and shrinking back down to the frame's height; a P op
   ;; on the normal thread runs and leaves its result there
   (let ((L (fresh)) (out '()))
     ((register! "cb" (λ (C)
                        ((lua-pushinteger 7) L) ((lua-pushstring "x") L)
                        (set! out (cons ((lua-gettop) L) out))
                        ((lua-pop 1) L)
                        ((lua-settop 5) L) ((lua-settop 2) L)
                        ((lua-getglobal "print") L) ((lua-remove 2) L)       ; 7 removed: above the floor
                        (set! out (cons (list ((lua-gettop) L) ((lua-type 2) L)) out))
                        ((lua-settop 1) L)                                 ; the floor itself
                        (set! out (cons (code-of (λ () ((lua-settop 0) L))) out))
                        0)) L)
     (run! L "R = coroutine.resume(coroutine.create(function() cb() end))")
     (⊦= (list 3 (list 2 LUA_TFUNCTION) AUX_EINDEX) (reverse out))
     (⊦= #t (global L "R"))
     ;; coroutine.wrap: the resumer's frame keeps nothing (auxwrap moved every argument)
     ((register! "cbw" (λ (C)
                         (set! out (list ((lua-gettop) L)))
                         ((lua-pushinteger 1) L) ((lua-settop 0) L)
                         (set! out (cons ((lua-gettop) L) out))
                         0)) L)
     (run! L "coroutine.wrap(function() cbw() end)()")
     (⊦= '(0 0) out)
     (⊦= 0 ((lua-gettop) L))
     (lua-close L))
   ;; a Scheme closure's frame has no floor: an outer callback, running in a coroutine, that
   ;; resumed another one itself (lua-resume, which runs lua_resume in a protected frame of the
   ;; main thread) lets the inner one pop what it pushed, as C would.  The main thread's frame of
   ;; that resume holds the resumed thread: it has one.
   (let ((L (fresh)) (outer #f) (seen #f))
     ((register! "inner" (λ (C)
                           (set! seen (list ((lua-gettop) outer) ((lua-gettop) L)
                                            (code-of (λ () ((lua-settop 0) L)))))
                           ((lua-settop 0) outer) 0)) L)
     ((register! "outer" (λ (C)
                           (set! outer C)
                           ((lua-pushinteger 1) C) ((lua-pushinteger 2) C)
                           (let ((co ((lua-newthread) C)))
                             ((lua-getglobal "inner") co)
                             (let-values (((st n) ((lua-resume C 0) co))) (⊦= LUA_OK st)))
                           ((lua-pushinteger ((lua-gettop) C)) C) 1)) L)
     (run! L "R = coroutine.wrap(function() return outer() end)()")
     (⊦= (list 3 1 AUX_EINDEX) seen)                                      ; 1 2 and the thread
     (⊦= 0 (global L "R"))
     (lua-close L)))

  ;; A callback body ends at most once, in the srfi-18 thread that entered it (%guard).  A
  ;; continuation captured in a running callback and invoked by another thread is refused: by the
  ;; rewind for a thread from outside the callback; for a thread created inside it (which inherits
  ;; its dynamic extent, so nothing is rewound) at the end of the body, which never returns to C
  ;; from there.  The callback's own thread gets its answer.  (The old code ran the body in both
  ;; threads: the second to end aborted the process, "callback returned twice", and the first to
  ;; end returned the other thread's Lua call.)
  ((test/core-callback-thread-owner _)
   (define refused "a Lua callback was resumed from another Scheme thread while it runs")
   (define (outcome t)
     (handle-exceptions e
       (if (uncaught-exception? e) (list 'died (msg (uncaught-exception-reason e))) (abort e))
       (thread-join! t)))
   ;; inside: B is created by the callback body; mode: what the body does when run again in B
   (define (race inside mode)
     (let ((L (fresh)) (K #f) (B #f) (out #f) (captured #f) (b-done #f) (log '()))
       (define (spawn!)
         (set! B (make-thread (λ () (call/cc (λ (k) (set! out k) (K #t)))) 'B))
         (thread-start! B))
       ((register! "cb" (λ (C)
                          (let ((again (call/cc (λ (k) (set! K k) #f))))
                            (set! log (cons (list (if again 'again 'body) (thread-name (current-thread))) log))
                            (cond
                              ((not again)
                               (when inside (spawn!))
                               (set! captured #t)
                               (let loop ((i 0)) (unless (or b-done (= i 3000)) (thread-yield!) (loop (+ i 1))))
                               ((lua-pushinteger 5) C) 1)
                              ((eq? mode 'raise) (error "boom in B"))
                              ((eq? mode 'escape) (out 'escaped))
                              (else 1))))) L)
       (let ((A (make-thread (λ () (run! L "R = cb()") (list (global L "R") (thread-name (current-thread)))) 'A)))
         (thread-start! A)
         (let loop () (unless captured (thread-yield!) (loop)))
         (unless inside (spawn!))
         (let* ((b (outcome B)) (a (begin (set! b-done #t) (thread-join! A))))
           (⊦= '(#f 0) (list %lua-owner %lua-holds))
           (⊦= 0 %depth)
           (⊦= 0 ((lua-gettop) L))
           (lua-close L)
           (list a b (reverse log))))))
   (let ((warnings (enable-warnings)))
     (enable-warnings #f)                                          ; B's uncaught errors
     (⊦= (list '(5 A) (list 'died refused) '((body A))) (race #f 'value))
     (⊦= (list '(5 A) (list 'died refused) '((body A) (again B))) (race #t 'value))
     (let ((r (race #t 'raise)))                                   ; B's own error: it never reaches Lua
       (⊦= '(5 A) (car r))
       (⊦= 'died (car (cadr r)))
       (⊦= '((body A) (again B)) (caddr r)))
     (⊦= (list '(5 A) 'escaped '((body A) (again B))) (race #t 'escape))
     (enable-warnings warnings)))
  )

(unittest/✓ lua-suite)

;; ------------------------------------------------------------------------------------------
;; S1: stack manipulation, type queries and constants
;; ------------------------------------------------------------------------------------------

(define-suite lua-stack-suite

  ((test/s1-currying _)
   (let ((L (lua-make-empty)) (live (lua-make-empty)))
     (lua-close L)
     (for-each (λ (m)
                 (⊨ (procedure? m))
                 (⊦raises (exn lua api) (m L))
                 (⊦raises (exn lua api) (m 'not-a-state)))
               (list (lua-absindex 1) (lua-rotate 1 1) (lua-insert 1) (lua-copy 1 2) (lua-remove 1)
                     (lua-replace 1) (lua-checkstack 1) (lua-xmove live 1) (lua-typename 0)
                     (lua-isnumber 1) (lua-isstring 1) (lua-iscfunction 1) (lua-isinteger 1)
                     (lua-isuserdata 1) (lua-isfunction 1) (lua-istable 1) (lua-islightuserdata 1)
                     (lua-isnil 1) (lua-isboolean 1) (lua-isthread 1) (lua-isnone 1) (lua-isnoneornil 1)))
     ;; a closed destination is refused too, and nothing moves
     ((lua-pushinteger 1) live)
     (⊦raises (exn lua api) ((lua-xmove L 1) live))
     (⊦= 1 ((lua-gettop) live))
     (lua-close live)))

  ((test/s1-constants _)
   (let ((L (fresh)))
     (⊦= 505 LUA_VERSION_NUM)
     (⊦= 50501 LUA_VERSION_RELEASE_NUM)
     (⊦= '(5 5 1) (list LUA_VERSION_MAJOR_N LUA_VERSION_MINOR_N LUA_VERSION_RELEASE_N))
     (⊦= "Lua 5.5" LUA_VERSION)
     (⊦= (string-append LUA_VERSION "." LUA_VERSION_RELEASE) LUA_RELEASE)
     (⊦= '("5" "5" "1") (list LUA_VERSION_MAJOR LUA_VERSION_MINOR LUA_VERSION_RELEASE))
     (⊨ (string-prefix? LUA_RELEASE LUA_COPYRIGHT))
     (⊨ (string? LUA_AUTHORS))
     (⊦= LUA_VERSION (global L "_VERSION"))
     (⊦= 20 LUA_MINSTACK)
     (⊦= 3 LUA_RIDX_LAST)
     (⊦= 9223372036854775807 LUA_MAXINTEGER)
     (⊦= -9223372036854775808 LUA_MININTEGER)
     (run! L "MAXI, MINI = math.maxinteger, math.mininteger")
     (⊦= LUA_MAXINTEGER (global L "MAXI"))
     (⊦= LUA_MININTEGER (global L "MINI"))
     (⊦= (iota 14) (list LUA_OPADD LUA_OPSUB LUA_OPMUL LUA_OPMOD LUA_OPPOW LUA_OPDIV LUA_OPIDIV
                         LUA_OPBAND LUA_OPBOR LUA_OPBXOR LUA_OPSHL LUA_OPSHR LUA_OPUNM LUA_OPBNOT))
     (⊦= '(0 1 2) (list LUA_OPEQ LUA_OPLT LUA_OPLE))
     (⊦= (iota 10) (list LUA_GCSTOP LUA_GCRESTART LUA_GCCOLLECT LUA_GCCOUNT LUA_GCCOUNTB LUA_GCSTEP
                         LUA_GCISRUNNING LUA_GCGEN LUA_GCINC LUA_GCPARAM))
     (⊦= (iota 6) (list LUA_GCPMINORMUL LUA_GCPMAJORMINOR LUA_GCPMINORMAJOR LUA_GCPPAUSE LUA_GCPSTEPMUL
                        LUA_GCPSTEPSIZE))
     (⊦= 6 LUA_GCPN)
     (⊦= (iota 5) (list LUA_HOOKCALL LUA_HOOKRET LUA_HOOKLINE LUA_HOOKCOUNT LUA_HOOKTAILCALL))
     (⊦= '(1 2 4 8) (list LUA_MASKCALL LUA_MASKRET LUA_MASKLINE LUA_MASKCOUNT))
     (⊦= 64 LUA_N2SBUFFSZ)
     (⊦= '("_G" "_LOADED" "_PRELOAD" "FILE*") (list LUA_GNAME LUA_LOADED_TABLE LUA_PRELOAD_TABLE LUA_FILEHANDLE))
     (⊦= (map (λ (i) (expt 2 i)) (iota 10))
         (list LUA_GLIBK LUA_LOADLIBK LUA_COLIBK LUA_DBLIBK LUA_IOLIBK LUA_MATHLIBK LUA_OSLIBK
               LUA_STRLIBK LUA_TABLIBK LUA_UTF8LIBK))
     (⊦= '("package" "coroutine" "debug" "io" "math" "os" "string" "table" "utf8")
         (list LUA_LOADLIBNAME LUA_COLIBNAME LUA_DBLIBNAME LUA_IOLIBNAME LUA_MATHLIBNAME LUA_OSLIBNAME
               LUA_STRLIBNAME LUA_TABLIBNAME LUA_UTF8LIBNAME))
     ;; the running Lua agrees: registry layout, loaded library names, the file handle metatable
     ((register! "reg" (λ (C) ((lua-pushvalue LUA_REGISTRYINDEX) C) 1)) L)
     (run! L (string-append
               "local r = reg()
                OK1 = r['" LUA_LOADED_TABLE "'] == package.loaded and r['" LUA_PRELOAD_TABLE "'] == package.preload
                      and r[" (number->string LUA_RIDX_GLOBALS) "] == _G
                      and r[" (number->string LUA_RIDX_MAINTHREAD) "] == coroutine.running()
                      and getmetatable(io.stdout).__name == '" LUA_FILEHANDLE "'"))
     (⊨ (global L "OK1"))
     (for-each (λ (name)
                 (run! L (string-append "OK2 = package.loaded['" name "'] ~= nil and package.loaded['" name "'] == _G['" name "']"))
                 (⊨ (global L "OK2")))
               (list LUA_GNAME LUA_LOADLIBNAME LUA_COLIBNAME LUA_DBLIBNAME LUA_IOLIBNAME LUA_MATHLIBNAME
                     LUA_OSLIBNAME LUA_STRLIBNAME LUA_TABLIBNAME LUA_UTF8LIBNAME))
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)))

  ((test/s1-absindex _)
   (let ((L (fresh)))
     (for-each (λ (i) ((lua-pushinteger i) L)) '(10 20 30))
     (⊦= '(3 2 1) (map (λ (i) ((lua-absindex i) L)) '(-1 -2 -3)))
     (⊦= '(1 2 3) (map (λ (i) ((lua-absindex i) L)) '(1 2 3)))
     (⊦= LUA_REGISTRYINDEX ((lua-absindex LUA_REGISTRYINDEX) L))     ; pseudo-indices unchanged
     (⊦= (lua-upvalueindex 3) ((lua-absindex (lua-upvalueindex 3)) L))
     ;; a positive index is acceptable above the top too, and answered unchanged, as C (lua-type
     ;; accepts the same index: LUA_TNONE); a negative one must be a slot
     (⊦= '(4 100) (map (λ (i) ((lua-absindex i) L)) '(4 100)))
     (⊦= LUA_TNONE ((lua-type 4) L))
     (for-each (λ (i) (⊦raises (exn lua api) ((lua-absindex i) L))) (list 0 -4 -100 4294967296 'one 1.0))
     ;; inside a callback: relative to the callback's own frame
     ((register! "abs" (λ (C) ((lua-pushinteger ((lua-absindex -1) C)) C) 1)) L)
     (run! L "A0 = abs(7, 8, 9)")
     (⊦= 3 (global L "A0"))
     (⊦= 3 ((lua-gettop) L))
     ((lua-settop 0) L)
     (lua-close L)))

  ;; the manual's examples, one binding at a time, against a list model
  ((test/s1-rotate-insert-remove-replace-copy _)
   (let ((L (fresh)))
     (define (stack) (map (λ (i) ((lua-tointeger i) L)) (iota ((lua-gettop) L) 1)))
     (for-each (λ (i) ((lua-pushinteger i) L)) '(1 2 3 4 5))
     ((lua-rotate 2 1) L)   (⊦= '(1 5 2 3 4) (stack))
     ((lua-rotate 2 -1) L)  (⊦= '(1 2 3 4 5) (stack))
     ((lua-rotate 1 2) L)   (⊦= '(4 5 1 2 3) (stack))
     ((lua-rotate -5 -2) L) (⊦= '(1 2 3 4 5) (stack))
     ((lua-rotate 1 5) L)   (⊦= '(1 2 3 4 5) (stack))       ; |n| = the segment length: identity
     ((lua-rotate 1 -5) L)  (⊦= '(1 2 3 4 5) (stack))
     ((lua-rotate -1 1) L)  (⊦= '(1 2 3 4 5) (stack))       ; a one-slot segment
     ((lua-rotate 3 0) L)   (⊦= '(1 2 3 4 5) (stack))
     ((lua-insert 1) L)     (⊦= '(5 1 2 3 4) (stack))
     ((lua-insert -1) L)    (⊦= '(5 1 2 3 4) (stack))       ; inserting the top at the top
     ((lua-insert -3) L)    (⊦= '(5 1 4 2 3) (stack))
     ((lua-remove 1) L)     (⊦= '(1 4 2 3) (stack))
     ((lua-remove -2) L)    (⊦= '(1 4 3) (stack))
     ((lua-remove -1) L)    (⊦= '(1 4) (stack))
     ((lua-pushinteger 9) L)
     ((lua-replace 1) L)    (⊦= '(9 4) (stack))
     ((lua-pushinteger 7) L)
     ((lua-replace -2) L)   (⊦= '(9 7) (stack))              ; -2 counted before the pop
     ((lua-replace -1) L)   (⊦= '(9) (stack))                ; onto itself: just a pop
     ((lua-pushinteger 3) L)
     ((lua-copy 1 2) L)     (⊦= '(9 9) (stack))
     ((lua-copy -1 -1) L)   (⊦= '(9 9) (stack))
     ((lua-pushinteger 3) L)
     ((lua-copy -1 1) L)    (⊦= '(3 9 3) (stack))
     ;; copy from pseudo-indices
     ((lua-copy LUA_REGISTRYINDEX 2) L)
     (⊨ ((lua-istable 2) L))
     (⊦= '(3 3) (list ((lua-tointeger 1) L) ((lua-tointeger 3) L)))
     ;; values of any type keep their identity when moved
     ((lua-settop 0) L)
     (run! L "T1, T2 = {}, {}")
     ((lua-getglobal "T1") L) ((lua-getglobal "T2") L) ((lua-pushstring "s\x0;z") L)
     ((lua-rotate 1 1) L)
     (⊦= "s\x0;z" ((lua-tostring 1) L))
     ((lua-pushvalue 2) L) ((lua-setglobal "X1") L)
     ((lua-pushvalue 3) L) ((lua-setglobal "X2") L)
     (run! L "IDOK = rawequal(X1, T1) and rawequal(X2, T2)")
     (⊨ (global L "IDOK"))
     ((lua-settop 0) L)
     (lua-close L)))

  ;; 600 random operations against a Scheme list model (deterministic LCG)
  ((test/s1-stack-model _)
   (let ((L (fresh)) (model '()) (seed 20260927) (next 100) (mismatches '()))
     (define (rand n) (set! seed (modulo (+ (* seed 1103515245) 12345) 2147483648)) (modulo (quotient seed 65536) n))
     (define (stack) (map (λ (i) ((lua-tointeger i) L)) (iota ((lua-gettop) L) 1)))
     (define (ltake l k) (if (= k 0) '() (cons (car l) (ltake (cdr l) (- k 1)))))
     (define (len) (length model))
     (define (abs-of i) (if (> i 0) i (+ (len) i 1)))
     (define (pick) (let ((a (+ 1 (rand (len))))) (if (= 0 (rand 2)) a (- a (len) 1))))
     (define (at a) (list-ref model (- a 1)))
     (define (set-at a v) (set! model (append (ltake model (- a 1)) (list v) (list-tail model a))))
     (define (m-rotate a n)
       (let* ((pre (ltake model (- a 1))) (seg (list-tail model (- a 1))) (k (modulo n (length seg))))
         (set! model (append pre (list-tail seg (- (length seg) k)) (ltake seg (- (length seg) k))))))
     (define (m-remove a) (set! model (append (ltake model (- a 1)) (list-tail model a))))
     (define (m-pop) (set! model (ltake model (- (len) 1))))
     (do ((step 0 (+ step 1))) ((= step 600))
       (let ((op (cond ((< (len) 3) 0) ((> (len) 12) 7) (else (rand 9)))))
         (case op
           ((0) ((lua-pushinteger next) L) (set! model (append model (list next))) (set! next (+ next 1)))
           ((1) (let ((i (pick))) ((lua-pushvalue i) L) (set! model (append model (list (at (abs-of i)))))))
           ((2) (let* ((i (pick)) (a (abs-of i)) (w (+ (- (len) a) 1)) (n (- (rand (+ (* 2 w) 1)) w)))
                  ((lua-rotate i n) L) (m-rotate a n)))
           ((3) (let ((i (pick))) ((lua-insert i) L) (m-rotate (abs-of i) 1)))
           ((4) (let ((i (pick))) ((lua-remove i) L) (m-remove (abs-of i))))
           ((5) (let* ((i (pick)) (a (abs-of i)) (v (at (len)))) ((lua-replace i) L) (set-at a v) (m-pop)))
           ((6) (let* ((f (pick)) (t (pick)) (v (at (abs-of f)))) ((lua-copy f t) L) (set-at (abs-of t) v)))
           ((7) (let ((k (+ 1 (rand 3)))) ((lua-pop k) L) (do ((j 0 (+ j 1))) ((= j k)) (m-pop))))
           (else (let ((i (pick))) (unless (= (abs-of i) ((lua-absindex i) L)) (set! mismatches (cons (list step 'absindex i) mismatches))))))
         (unless (equal? model (stack))
           (set! mismatches (cons (list step op model (stack)) mismatches)))))
     (⊦= '() mismatches)
     (⊦= (len) ((lua-gettop) L))
     ((lua-settop 0) L)
     (lua-close L)))

  ((test/s1-refusals _)
   (let ((L (fresh)) (other (fresh)))
     (define (stack) (map (λ (i) ((lua-tointeger i) L)) (iota ((lua-gettop) L) 1)))
     (define (code-of thunk)
       (condition-case (begin (thunk) 'no-raise)
         (e (exn lua api) (list (lua-condition-status e) (get-condition-property e 'lua 'code)
                                ((condition-property-accessor 'exn 'location) e)))))
     (define (refused? code who thunk) (⊦= (list 'api code who) (code-of thunk)))
     (⊦= (list 'api AUX_EUNDER 'lua-replace) (code-of (λ () ((lua-replace 1) L))))   ; empty stack
     (for-each (λ (i) ((lua-pushinteger i) L)) '(1 2 3 4 5))
     (for-each
       (λ (i)
         (if (> i 0) (⊦= i ((lua-absindex i) L)) (refused? AUX_EINDEX 'lua-absindex (λ () ((lua-absindex i) L))))
         (refused? AUX_EINDEX 'lua-rotate (λ () ((lua-rotate i 0) L)))
         (refused? AUX_EINDEX 'lua-insert (λ () ((lua-insert i) L)))
         (refused? AUX_EINDEX 'lua-remove (λ () ((lua-remove i) L)))
         (refused? AUX_EINDEX 'lua-replace (λ () ((lua-replace i) L)))
         (refused? AUX_EINDEX 'lua-copy (λ () ((lua-copy i 1) L)))
         (refused? AUX_EINDEX 'lua-copy (λ () ((lua-copy 1 i) L)))
         (refused? AUX_EINDEX 'lua-isnumber (λ () ((lua-isnumber (if (> i 0) (- i) i)) L)))
         (refused? AUX_EINDEX 'lua-isnil (λ () ((lua-isnil (if (> i 0) (- i) i)) L))))
       (list 0 6 -6 7 -100))
     ;; pseudo-indices: never moved; the registry is never written; upvalues only inside callbacks
     (for-each
       (λ (i)
         (refused? AUX_EINDEX 'lua-rotate (λ () ((lua-rotate i 1) L)))
         (refused? AUX_EINDEX 'lua-insert (λ () ((lua-insert i) L)))
         (refused? AUX_EINDEX 'lua-remove (λ () ((lua-remove i) L)))
         (refused? AUX_EINDEX 'lua-replace (λ () ((lua-replace i) L)))
         (refused? AUX_EINDEX 'lua-copy (λ () ((lua-copy 1 i) L))))
       (list LUA_REGISTRYINDEX (lua-upvalueindex 1) (lua-upvalueindex 2) (- LUA_REGISTRYINDEX 1)))
     ;; at base level there is no running C function whose upvalues could be read
     (refused? AUX_EINDEX 'lua-copy (λ () ((lua-copy (lua-upvalueindex 1) 1) L)))
     (refused? AUX_EINDEX 'lua-isnone (λ () ((lua-isnone (lua-upvalueindex 1)) L)))
     (⊨ ((lua-istable LUA_REGISTRYINDEX) L))                           ; the registry is readable
     ;; rotate counts
     (refused? AUX_ERANGE 'lua-rotate (λ () ((lua-rotate 1 6) L)))
     (refused? AUX_ERANGE 'lua-rotate (λ () ((lua-rotate 1 -6) L)))
     (refused? AUX_ERANGE 'lua-rotate (λ () ((lua-rotate -2 3) L)))
     (refused? AUX_ERANGE 'lua-rotate (λ () ((lua-rotate 5 -2) L)))
     (refused? AUX_ERANGE 'lua-rotate (λ () ((lua-rotate 1 -2147483648) L)))   ; no overflow of |n|
     (refused? AUX_ERANGE 'lua-rotate (λ () ((lua-rotate 1 2147483648) L)))    ; not an int
     (refused? AUX_ERANGE 'lua-remove (λ () ((lua-remove 4294967297) L)))      ; would truncate to 1
     ;; checkstack, xmove and typename arguments
     (refused? AUX_ERANGE 'lua-checkstack (λ () ((lua-checkstack -1) L)))
     (let ((T ((lua-newthread) L)))
       ((lua-pop 1) L)                                        ; T stays anchored by its luaR
       (refused? AUX_EUNDER 'lua-xmove (λ () ((lua-xmove T 6) L)))
       (refused? AUX_EUNDER 'lua-xmove (λ () ((lua-xmove T -1) L))))
     ;; a thread onto itself is a no-op for ANY n, as C (lapi.c: `from == to' returns before its
     ;; api_checkpop), even through two luaRs of the same thread
     ((lua-xmove L 6) L)
     ((lua-xmove L -1) L)
     ((lua-xmove L 2147483647) L)
     (let ((M (begin ((lua-pushthread) L) (begin1 ((lua-tothread -1) L) ((lua-pop 1) L)))))
       (⊦= 'thread (luaR-kind M))
       ((lua-xmove M 9) L)
       ((lua-xmove L 9) M))
     (refused? AUX_EARG 'lua-xmove (λ () ((lua-xmove other 1) L)))             ; another universe
     (refused? AUX_ERANGE 'lua-typename (λ () ((lua-typename 9) L)))
     (refused? AUX_ERANGE 'lua-typename (λ () ((lua-typename -2) L)))
     (refused? AUX_ERANGE 'lua-typename (λ () ((lua-typename 'nil) L)))
     (⊦raises (exn lua api) ((lua-copy 1 "2") L))
     ;; nothing was touched
     (⊦= '(1 2 3 4 5) (stack))
     (⊦= 0 ((lua-gettop) other))
     ((lua-settop 0) L)
     (lua-close other)
     (lua-close L)))

  ((test/s1-checkstack _)
   (let ((L (fresh)))
     ;; an allocation refused by the memory limit: #f, never an error, and the state recovers
     (run! L "collectgarbage() M = collectgarbage('count')")
     ((lua-setmemorylimit (+ (inexact->exact (floor (* 1024 (global L "M")))) 100000)) L)
     (⊭ ((lua-checkstack 200000) L))
     ((lua-setmemorylimit 0) L)
     (⊨ ((lua-checkstack 200000) L))
     (⊨ ((lua-checkstack 0) L))
     (⊨ ((lua-checkstack LUA_MINSTACK) L))
     (⊨ ((lua-checkstack 5000) L))
     (⊦= 0 ((lua-gettop) L))
     (do ((i 0 (+ i 1))) ((= i 5000)) ((lua-pushinteger i) L))
     (⊦= 4999 ((lua-tointeger -1) L))
     (⊦= 5000 ((lua-gettop) L))
     ;; beyond LUAI_MAXSTACK (1000000 slots): #f, never an error
     (⊭ ((lua-checkstack 2000000) L))
     (⊭ ((lua-checkstack 2147483647) L))
     (⊦= 5000 ((lua-gettop) L))
     (⊨ ((lua-checkstack 10) L))
     ((lua-settop 0) L)
     (run! L "X = 1")
     (⊦= 1 (global L "X"))
     ;; inside a callback: the callback's frame grows
     ((register! "grow" (λ (C) ((lua-pushboolean ((lua-checkstack 10000) C)) C) 1)) L)
     (run! L "G = grow()")
     (⊨ (global L "G"))
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)))

  ((test/s1-typename _)
   (let ((L (lua-make-empty)))
     (⊦= '("no value" "nil" "boolean" "userdata" "number" "string" "table" "function" "userdata" "thread")
         (map (λ (t) ((lua-typename t) L)) (iota 10 -1)))
     (⊦= "table" ((lua-typename LUA_TTABLE) L))
     (⊦= "no value" ((lua-typename ((lua-type 1) L)) L))
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)))

  ((test/s1-is-predicates _)
   (let ((L (fresh)))
     (run! L "local up = 1
              LF = function() return up end
              UD = io.stdout
              TH = coroutine.create(print)
              LU = debug.upvalueid(LF, 1)
              TB = {}")
     ((lua-pushnil) L)                                   ;  1 nil
     ((lua-pushboolean #f) L)                            ;  2 false
     ((lua-pushboolean #t) L)                            ;  3 true
     ((lua-pushinteger 42) L)                            ;  4 integer
     ((lua-pushnumber 1.5) L)                            ;  5 float
     ((lua-pushnumber 3) L)                              ;  6 float with an integral value
     ((lua-pushstring "12") L)                           ;  7 numeric string
     ((lua-pushstring " 0x10 ") L)                       ;  8 numeric string, hex and spaces
     ((lua-pushstring "abc") L)                          ;  9 string
     ((lua-getglobal "TB") L)                            ; 10 table
     ((lua-getglobal "LF") L)                            ; 11 Lua function
     ((lua-getglobal "print") L)                         ; 12 C function
     ((lua-pushcfunction nop-callback) L)                ; 13 Scheme closure (a C closure)
     ((lua-getglobal "UD") L)                            ; 14 full userdata
     ((lua-getglobal "LU") L)                            ; 15 light userdata
     ((lua-getglobal "TH") L)                            ; 16 thread
     ((lua-pushobject 'scheme) L)                        ; 17 Scheme object (a full userdata)
     (let ((expected
             ;; idx        1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18=none
             `((,lua-isnumber        #f #f #f #t #t #t #t #t #f #f #f #f #f #f #f #f #f #f)
               (,lua-isstring        #f #f #f #t #t #t #t #t #t #f #f #f #f #f #f #f #f #f)
               (,lua-iscfunction     #f #f #f #f #f #f #f #f #f #f #f #t #t #f #f #f #f #f)
               (,lua-isinteger       #f #f #f #t #f #f #f #f #f #f #f #f #f #f #f #f #f #f)
               (,lua-isuserdata      #f #f #f #f #f #f #f #f #f #f #f #f #f #t #t #f #t #f)
               (,lua-isfunction      #f #f #f #f #f #f #f #f #f #f #t #t #t #f #f #f #f #f)
               (,lua-istable         #f #f #f #f #f #f #f #f #f #t #f #f #f #f #f #f #f #f)
               (,lua-islightuserdata #f #f #f #f #f #f #f #f #f #f #f #f #f #f #t #f #f #f)
               (,lua-isnil           #t #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)
               (,lua-isboolean       #f #t #t #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)
               (,lua-isthread        #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #t #f #f)
               (,lua-isnone          #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #t)
               (,lua-isnoneornil     #t #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #t))))
       (for-each (λ (row)
                   (⊦= (cdr row) (map (λ (i) (((car row) i) L)) (iota 18 1)))
                   ;; negative indices agree (18 is none: only positive)
                   (⊦= (reverse (cdr (reverse (cdr row)))) (map (λ (i) (((car row) (- i 18)) L)) (iota 17 1))))
                 expected))
     ;; isnumber does not convert the slot, isinteger does not look inside strings
     (⊦= LUA_TSTRING ((lua-type 7) L))
     (⊭ ((lua-isinteger 7) L))
     (⊦= 17 ((lua-gettop) L))
     ((lua-settop 0) L)
     (lua-close L)))

  ;; upvalues of the running Scheme closure: read, written (copy / replace), and every write
  ;; outside the closure's own upvalues refused
  ((test/s1-callback-upvalues _)
   (let ((L (fresh)) (refusals '()))
     (define (try tag thunk)
       (set! refusals (cons (condition-case (begin (thunk) (list tag 'done)) (e (exn lua api) (list tag 'refused))) refusals)))
     ((lua-pushinteger 0) L)
     ((lua-pushstring "tag") L)
     ((lua-pushcclosure
        (λ (C)
          ;; upvalue 1 := upvalue 1 + 1, answered
          ((lua-pushvalue (lua-upvalueindex 1)) C)
          ((lua-pushinteger (+ 1 ((lua-tointeger -1) C))) C)
          ((lua-replace (lua-upvalueindex 1)) C)                ; pops the new count
          ((lua-copy (lua-upvalueindex 1) -1) C)                ; the old count is overwritten
          (when (null? refusals)
            (try 'is (λ () (unless (and ((lua-isinteger (lua-upvalueindex 1)) C) ((lua-isstring (lua-upvalueindex 2)) C)
                                        ((lua-isnone (lua-upvalueindex 3)) C) (not ((lua-isnone (lua-upvalueindex 2)) C)))
                             (error "is* on upvalues"))))
            (try 'abs (λ () (unless (= (lua-upvalueindex 2) ((lua-absindex (lua-upvalueindex 2)) C)) (error "absindex"))))
            (try 'beyond-nup (λ () ((lua-copy -1 (lua-upvalueindex 3)) C)))
            (try 'handle (λ () ((lua-copy -1 (- LUA_REGISTRYINDEX 1)) C)))
            (try 'registry (λ () ((lua-copy -1 LUA_REGISTRYINDEX) C)))
            (try 'replace-beyond (λ () ((lua-pushnil) C) ((lua-replace (lua-upvalueindex 3)) C)))
            ((lua-settop 1) C))
          1)
        2) L)
     ((lua-setglobal "counter") L)
     (run! L "A, B, C3 = counter(), counter(), counter()")
     (⊦= '(1 2 3) (list (global L "A") (global L "B") (global L "C3")))
     (⊦= '((is done) (abs done) (beyond-nup refused) (handle refused) (registry refused) (replace-beyond refused))
         (reverse refusals))
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)))

  ;; the upvalue limits.  (lua-upvalueindex i) is C's lua_upvalueindex(i + 1) (raw upvalue 1 is the
  ;; closure's handle), so user upvalues 1..255 reach C's 256, which the manual (4.2) makes an
  ;; acceptable index even beyond the closure's upvalues: LUA_TNONE, lua-isnone #t (255 was
  ;; refused).  A Scheme closure takes at most 254 upvalues, one fewer than a C closure's 255.
  ((test/s1-upvalue-limits _)
   (let ((L (fresh)) (seen #f))
     (⊦raises (exn lua api) (lua-upvalueindex 256))
     (⊦raises (exn lua api) (lua-upvalueindex 0))
     (⊦= (- LUA_REGISTRYINDEX 256) (lua-upvalueindex 255))
     ((lua-pushinteger 7) L)
     ((lua-pushcclosure
        (λ (C)
          (set! seen (list ((lua-type (lua-upvalueindex 1)) C) ((lua-type (lua-upvalueindex 2)) C)
                           ((lua-type (lua-upvalueindex 255)) C) ((lua-isnone (lua-upvalueindex 255)) C)
                           ((lua-isnoneornil (lua-upvalueindex 255)) C)
                           (condition-case (begin ((lua-copy 1 (lua-upvalueindex 255)) C) 'written)
                             (e (exn lua api) 'refused))))       ; readable, but not one of its upvalues
          0)
        1) L)
     ((lua-setglobal "f") L)
     (run! L "f(1)")
     (⊦= (list LUA_TNUMBER LUA_TNONE LUA_TNONE #t #t 'refused) seen)
     ;; 254 upvalues: the last one is (lua-upvalueindex 254); 255 are refused, a C closure's limit
     (do ((i 1 (+ i 1))) ((> i 254)) ((lua-pushinteger i) L))
     ((lua-pushcclosure (λ (C) ((lua-pushvalue (lua-upvalueindex 254)) C) 1) 254) L)
     ((lua-setglobal "g") L)
     (run! L "G = g()")
     (⊦= 254 (global L "G"))
     (do ((i 1 (+ i 1))) ((> i 255)) ((lua-pushinteger i) L))
     (⊦= AUX_ERANGE (condition-case ((lua-pushcclosure (λ (C) 0) 255) L)
                      (e (exn lua api) (get-condition-property e 'lua 'code))))
     (⊦= 255 ((lua-gettop) L))                                          ; nothing consumed
     ((lua-pushcclosure luaopen-base 255) L)                              ; a C function takes 255
     (⊦= 1 ((lua-gettop) L))
     (⊨ ((lua-iscfunction 1) L))
     ((lua-settop 0) L)
     (lua-close L)))

  ;; a view kept past a nested call must not write the upvalues of whatever C function is running
  ;; on its thread now (here coroutine.wrap's closure, whose upvalue 2 does not exist: Lua would
  ;; hand out its global nil object)
  ((test/s1-stale-view-upvalue-write _)
   (let ((L (fresh)) (saved #f) (outcome '()))
     ((register! "inner" (λ (C)
                           (set! outcome (cons (condition-case (begin ((lua-copy LUA_REGISTRYINDEX (lua-upvalueindex 1)) saved) 'written)
                                                 (e (exn lua api) 'refused))
                                               outcome))
                           0)) L)
     ((lua-pushinteger 7) L)
     ((lua-pushinteger 8) L)
     ((lua-pushcclosure (λ (C)
                          (set! saved C)
                          ((lua-check (luaL-dostring "coroutine.wrap(function() inner() end)()")) C)
                          ;; back in its own frame the same write is fine
                          ((lua-pushinteger 70) C)
                          ((lua-replace (lua-upvalueindex 1)) C)
                          ((lua-pushvalue (lua-upvalueindex 1)) C)
                          1)
                        2) L)
     ((lua-setglobal "outer") L)
     (run! L "R = outer()")
     (⊦= 70 (global L "R"))
     (⊦= '(refused) outcome)
     (run! L "NIL_OK = (type(nil) == 'nil') and rawequal(nil, nil) and select('#', nil) == 1")
     (⊨ (global L "NIL_OK"))
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)))

  ((test/s1-xmove _)
   (let* ((L (fresh)) (ptr #f) (other (fresh)))
     (define (stack S) (map (λ (i) (value-at S i)) (iota ((lua-gettop) S) 1)))
     ((register! "grab" (λ (C) (set! ptr (luaR-state C)) 0)) L)
     (run! L "CO = coroutine.create(function() grab() local a, b, c = coroutine.yield('y') R = a .. b .. c end)
              assert(coroutine.resume(CO))")
     (let ((T (make-luaR ptr (luaR-u L) 'thread #f -1 0 #f)))
       (⊦= LUA_YIELD (%status ptr))
       (let ((t0 ((lua-gettop) T)))
         (for-each (λ (i) ((lua-pushinteger i) L)) '(1 2 3))
         ((lua-xmove T 2) L)                                   ; onto a suspended thread
         (⊦= '(1) (stack L))
         (⊦= (+ t0 2) ((lua-gettop) T))
         (⊦= '(2 3) (list ((lua-tointeger -2) T) ((lua-tointeger -1) T)))
         ((lua-xmove L 1) T)
         (⊦= '(1 3) (stack L))
         (⊦= (+ t0 1) ((lua-gettop) T))
         ;; slots of a thread suspended in a C function can be moved and removed
         ((lua-pushinteger 4) T)
         ((lua-insert -2) T)
         (⊦= '(4 2) (list ((lua-tointeger -2) T) ((lua-tointeger -1) T)))
         ((lua-rotate -2 1) T)
         ((lua-remove -1) T)
         ((lua-pushinteger 5) T)
         ((lua-replace -2) T)
         (⊦= (+ t0 1) ((lua-gettop) T))
         (⊦= 5 ((lua-tointeger -1) T))
         ;; no-ops: n = 0, and a thread onto itself
         ((lua-xmove T 0) L)
         ((lua-xmove L 2) L)
         (⊦= '(1 3) (stack L))
         ;; refusals leave both stacks alone
         (⊦raises (exn lua api) ((lua-xmove T 3) L))
         (⊦raises (exn lua api) ((lua-xmove T -1) L))
         (⊦raises (exn lua api) ((lua-xmove other 1) L))
         (⊦raises (exn lua api) ((lua-xmove L 1) other))
         (⊦= '(1 3) (stack L))
         (⊦= 0 ((lua-gettop) other))
         ;; the coroutine goes on: the extra value stays in the frame of yield and is dropped
         (run! L "assert(coroutine.resume(CO, 'a', 'b', 'c'))")
         (⊦= "abc" (global L "R"))
         (⊦= "dead" (begin (run! L "S = coroutine.status(CO)") (global L "S")))))
     ;; from a view inside a coroutine to the main thread and back
     ((lua-settop 0) L)
     ((register! "mv" (λ (C)
                        (let ((before ((lua-gettop) L)))
                          ((lua-xmove L 2) C)
                          (unless (= (+ before 2) ((lua-gettop) L)) (error "xmove to main"))
                          ((lua-xmove C 2) L)
                          2))) L)
     (run! L "X, Y = coroutine.wrap(function() return mv(4, 5) end)()")
     (⊦= '(4 5) (list (global L "X") (global L "Y")))
     (⊦= 0 ((lua-gettop) L))
     (lua-close other)
     (lua-close L)))

  ;; J5: a thread that died inside a Lua function keeps its <close> pending; S1
  ;; never moves or lowers its slots (a to-be-closed slot may be among them), only its error value
  ;; can be moved out.  A thread that died inside a C function can be edited freely.
  ((test/s1-dead-threads _)
   (let* ((L (fresh)) (ptr #f) (mk (λ () (make-luaR ptr (luaR-u L) 'thread #f -1 0 #f))))
     (define (code-of thunk)
       (condition-case (begin (thunk) 'no-raise) (e (exn lua api) (get-condition-property e 'lua 'code))))
     ((register! "grab" (λ (C) (set! ptr (luaR-state C)) 0)) L)
     (run! L "closed = 0
              CO = coroutine.create(function()
                local x <close> = setmetatable({}, {__close = function() closed = closed + 1 end})
                grab()
                local n = nil; n = n + 1
              end)
              OK = coroutine.resume(CO)")
     (⊭ (global L "OK"))
     (let* ((T (mk)) (top ((lua-gettop) T)))
       (⊨ (string-suffix? "attempt to perform arithmetic on a nil value (local 'n')" ((lua-tostring -1) T)))
       (⊦= AUX_ETHREAD (code-of (λ () ((lua-rotate 1 1) T))))
       (⊦= AUX_ETHREAD (code-of (λ () ((lua-insert -1) T))))
       (⊦= AUX_ETHREAD (code-of (λ () ((lua-remove -1) T))))
       (⊦= AUX_ETHREAD (code-of (λ () ((lua-replace -1) T))))
       (⊦= AUX_ETHREAD (code-of (λ () ((lua-copy -1 1) T))))
       (⊦= AUX_ETHREAD (code-of (λ () ((lua-xmove L 2) T))))
       (⊦= top ((lua-gettop) T))
       (⊨ ((lua-isstring -1) T))                                  ; queries are fine
       ;; Lua's coroutine.resume has moved the error value away: what the thread holds is its
       ;; frame's (no binding can tell Lua's copy of the message from a register), so nothing
       ;; moves; a value pushed since does, and nothing below it
       (⊦= AUX_ETHREAD (code-of (λ () ((lua-xmove L 1) T))))
       ((lua-pushinteger 5) T)
       ((lua-xmove L 1) T)
       (⊦= 5 ((lua-tointeger -1) L))
       ((lua-pop 1) L)
       (⊦= AUX_ETHREAD (code-of (λ () ((lua-xmove L 1) T))))
       (⊦= top ((lua-gettop) T))
       (⊦= 0 (global L "closed")))
     ;; closing it runs the pending __close: the thread was left consistent
     (run! L "coroutine.close(CO)")
     (⊦= 1 (global L "closed"))
     ;; dead inside error(): its C frame holds nothing to close
     (run! L "CO2 = coroutine.create(function()
                local x <close> = setmetatable({}, {__close = function() closed = closed + 10 end})
                grab()
                error('died', 0)
              end)
              coroutine.resume(CO2)")
     (let ((T (mk)))
       (⊦= '(1 "died") (list ((lua-gettop) T) ((lua-tostring 1) T)))
       ((lua-pushinteger 1) T)
       ((lua-pushinteger 2) T)
       ((lua-rotate 1 1) T)
       ((lua-insert 1) T)
       (⊦= '(1 2 "died") (list ((lua-tointeger 1) T) ((lua-tointeger 2) T) ((lua-tostring 3) T)))
       ((lua-remove 1) T)
       ((lua-pushinteger 9) T)
       ((lua-replace 1) T)
       ((lua-copy 1 2) T)
       (⊦= '(9 9) (list ((lua-tointeger 1) T) ((lua-tointeger 2) T)))
       ((lua-xmove L 2) T)
       (⊦= 0 ((lua-gettop) T))
       (⊦= '(9 9) (list ((lua-tointeger 1) L) ((lua-tointeger 2) L)))
       ((lua-pop 2) L))
     (⊦= 1 (global L "closed"))
     (run! L "coroutine.close(CO2)")
     (⊦= 11 (global L "closed"))
     (⊦= 0 ((lua-gettop) L))
     (gc #t)
     (lua-close L)))

  ;; a thread lua-resume left dead in a Lua frame: lua-xmove takes the error value lua-resume
  ;; pushed, or a value pushed since, one at a time, and never goes below the height at its death:
  ;; repeated moves cannot drain the frame's registers (its pending to-be-closed variable among
  ;; them), whose __close lua-closethread then runs.  C (with API checks) forbids those moves too.
  ((test/s1-dead-thread-no-drain _)
   (let* ((L (fresh)) (T ((lua-newthread) L)))
     (define (code-of thunk)
       (condition-case (begin (thunk) 'no-raise) (e (exn lua api) (get-condition-property e 'lua 'code))))
     ((lua-setglobal "CO") L)
     (run! L "closed = 0")
     ((luaL-loadstring "local x <close> = setmetatable({}, {__close = function() closed = closed + 1 end})
                         local y = nil; local z = y + 1") T)
     (let-values (((st n) ((lua-resume L 0) T))) (⊦= LUA_ERRRUN st))
     (let ((top ((lua-gettop) T)))
       (⊨ (> top 2))
       ((lua-xmove L 1) T)                                        ; the error value
       (⊨ (string-suffix? "(local 'y')" ((lua-tostring -1) L)))
       ((lua-pop 1) L)
       (⊦= (- top 1) ((lua-gettop) T))
       (⊦= AUX_ETHREAD (code-of (λ () ((lua-xmove L 1) T))))       ; and nothing below it, ever
       (⊦= AUX_ETHREAD (code-of (λ () ((lua-xmove L 1) T))))
       ;; values pushed since move, down to the same height
       ((lua-pushinteger 1) T) ((lua-pushinteger 2) T) ((lua-pushinteger 3) T)
       (⊦= AUX_ETHREAD (code-of (λ () ((lua-xmove L 4) T))))
       ((lua-xmove L 1) T) ((lua-xmove L 2) T)
       (⊦= '(3 1 2) (list ((lua-tointeger 1) L) ((lua-tointeger 2) L) ((lua-tointeger 3) L)))
       ((lua-settop 0) L)
       (⊦= AUX_ETHREAD (code-of (λ () ((lua-xmove L 1) T))))
       (⊦= (- top 1) ((lua-gettop) T))
       ;; another luaR of the thread, the first one dropped: the same limit
       ((lua-getglobal "CO") L)
       (let ((T2 ((lua-tothread -1) L)))
         ((lua-pop 1) L)
         (set! T #f) (gc #t) (gc #t)
         ((lua-gc 'collect) L)                                    ; a drain point: T's anchor goes
         (⊦= AUX_ETHREAD (code-of (λ () ((lua-xmove L 1) T2))))
         ;; later pushes land above the frame: closing the thread runs the pending __close
         ((lua-pushinteger 7) T2) ((lua-pushinteger 8) T2)
         (⊦= 0 (global L "closed"))
         (⊦= LUA_ERRRUN ((lua-closethread L) T2))
         (⊦= 1 (global L "closed"))))
     (⊦= 0 ((lua-gettop) L))
     (gc #t)
     (lua-close L)))

  ;; a coroutine suspended by a yielding count hook: its current frame is the hooked Lua
  ;; function, and slots 1..top of its luaR are that function's registers (a numeric for loop's
  ;; state and control variable among them).  No binding writes, moves or lowers them -- lua-copy
  ;; included -- while pushes (run on the main thread and moved over) and reads work; once
  ;; resumed, the loop runs to its end as Lua defines it.
  ((test/s1-hook-suspended-thread _)
   (let* ((L (fresh)) (T ((lua-newthread) L)))
     (define (code-of thunk)
       (condition-case (begin (thunk) 'no-raise) (e (exn lua api) (get-condition-property e 'lua 'code))))
     ((lua-pop 1) L)
     ((luaL-loadstring "local s = 0 for i = 1, 1000000, 65536 do s = s + 1 if type(i) ~= 'number' then return 'corrupted' end end return s") T)
     ((lua-sethook (λ (H ev ar) ((lua-yield 0) H)) '(count) 7) T)
     (let-values (((st n) ((lua-resume #f 0) T))) (⊦= LUA_YIELD st))
     ((lua-sethook #f 0 0) T)
     (let* ((ar ((lua-getstack 0) T))
            (i (let loop ((n 1)) (if (equal? "i" ((lua-getlocal ar n) T)) n (loop (+ n 1)))))   ; each pushes (no pop: refused)
            (top ((lua-gettop) T)))
       (⊨ (<= i top))
       ((lua-newtable) T)                                          ; a push: fine
       (⊦= (+ top 1) ((lua-gettop) T))
       (⊦= AUX_ETHREAD (code-of (λ () ((lua-copy -1 i) T))))
       (⊦= AUX_ETHREAD (code-of (λ () ((lua-copy -1 1) T))))
       (⊦= AUX_ETHREAD (code-of (λ () ((lua-replace i) T))))
       (⊦= AUX_ETHREAD (code-of (λ () ((lua-setlocal ar i) T))))
       (⊦= AUX_ETHREAD (code-of (λ () ((lua-rotate i 1) T))))
       (⊦= AUX_ETHREAD (code-of (λ () ((lua-settop (- top 1)) T))))
       (⊨ ((lua-isinteger i) T))                                   ; reads are fine
       (⊦= AUX_ETHREAD (code-of (λ () ((lua-copy i -1) T))))      ; any slot of that frame, as lua-replace
       (⊦= (+ top 1) ((lua-gettop) T))
       (⊦= LUA_TTABLE ((lua-type -1) T)))
     (let-values (((st n) ((lua-resume #f 0) T))) (⊦= LUA_OK st))
     (⊦= 16 ((lua-tointeger -1) T))
     (lua-close L)))

  ;; lua-error and lua-check pop the value they raise: on a thread suspended by a hook, or dead by an
  ;; error raised in Lua code, the top is a register of that Lua frame (a pending <close> variable:
  ;; lua_settop would run its __close unprotected, a panic), so both refuse as lua-pop does, before
  ;; reading or popping anything -- except the error value a lua-resume run by lua-check's m pushed.
  ((test/s1-error-check-frozen-thread _)
   (let ((L (fresh)))
     (define (code-of thunk)
       (condition-case (begin (thunk) 'no-raise) (e (exn lua api) (get-condition-property e 'lua 'code))))
     (define (refused-twice T)                                      ; both refuse, and nothing moves
       (let ((top ((lua-gettop) T)))
         (do ((k 0 (+ k 1))) ((= k 3))
           (⊦= AUX_ETHREAD (code-of (λ () ((lua-error) T))))
           (⊦= AUX_ETHREAD (code-of (λ () ((lua-check (λ (T) LUA_ERRRUN)) T))))
           (⊦= AUX_ETHREAD (code-of (λ () ((lua-check (λ (T) (values LUA_ERRMEM 1))) T)))))
         (⊦= top ((lua-gettop) T))))
     (define (thread-of code)
       (let ((T ((lua-newthread) L))) ((lua-pop 1) L) ((luaL-loadstring code) T) T))
     (define (resume T) (let-values (((st n) ((lua-resume L 0) T))) st))
     (run! L "CLOSED = 0")
     ;; (1) a hook yield after the <close> variable exists; its __close raises when it runs
     (let ((T (thread-of "local x <close> = setmetatable({}, {__close = function() CLOSED = CLOSED + 1 end})
                          local s = 0 for i = 1, 100 do s = s + i end return s")))
       ((lua-sethook (λ (H e ar) ((lua-yield 0) H)) LUA_MASKCOUNT 20) T)
       (⊦= LUA_YIELD (resume T))
       (⊦= LUA_YIELD ((lua-status) T))
       (refused-twice T)
       (⊦= 0 (global L "CLOSED"))                                   ; no __close ran
       ((lua-sethook #f 0 0) T)
       (⊦= LUA_OK (resume T))
       (⊦= 5050 ((lua-tointeger -1) T))                              ; the frame was intact
       (⊦= 1 (global L "CLOSED")))
     ;; (2) a hook yield before it exists (count 5): the registers stay the function's
     (let ((T (thread-of "local s = 0 for i = 1, 100 do s = s + i end
                          local x <close> = setmetatable({}, {__close = function() CLOSED = CLOSED + 1 end})
                          return s")))
       ((lua-sethook (λ (H e ar) ((lua-yield 0) H)) LUA_MASKCOUNT 5) T)
       (⊦= LUA_YIELD (resume T))
       (refused-twice T)
       ((lua-sethook #f 0 0) T)
       (⊦= LUA_OK (resume T))
       (⊦= 5050 ((lua-tointeger -1) T))
       (⊦= 2 (global L "CLOSED")))
     ;; (3) dead in a Lua frame with a pending <close> whose __close raises: lua-check of the
     ;; resume raises its error value (a Lua error, not a refusal); after it, the registers are refused
     (let ((T (thread-of "local x <close> = setmetatable({}, {__close = function() CLOSED = CLOSED + 1 error('boom') end})
                          local y = nil local z = y + 1")))
       (let ((e (condition-case ((lua-check (lua-resume L 0)) T) (e (exn lua) e))))
         (⊦= 'errrun (lua-condition-status e))
         (⊨ (and (substring-index "perform arithmetic on a nil value" (lua-condition-value e)) #t)))
       (⊦= LUA_ERRRUN ((lua-status) T))
       (refused-twice T)
       (⊦= 2 (global L "CLOSED"))                                   ; the __close is still pending
       ;; resuming the dead thread: the message lua_resume pushes is raised as well
       (let ((e (condition-case ((lua-check (lua-resume L 0)) T) (e (exn lua) e))))
         (⊦= "cannot resume dead coroutine" (lua-condition-value e)))
       (refused-twice T)
       ;; lua-closethread runs the __close protected and resets the thread: its error is raisable
       (let ((e (condition-case ((lua-check (lua-closethread L)) T) (e (exn lua) e))))
         (⊦= 'errrun (lua-condition-status e))
         (⊨ (and (substring-index "boom" (lua-condition-value e)) #t)))
       (⊦= 3 (global L "CLOSED"))
       (⊦= LUA_OK ((lua-status) T)))
     ;; (4) a plain lua-resume leaves the error value: raising it later is refused (reads and
     ;; lua-xmove of the error value still work); so is a lua-check whose m did not resume
     (let ((T (thread-of "local y = nil local z = y + 1")))
       (⊦= LUA_ERRRUN (resume T))
       (refused-twice T)
       (⊨ (and (substring-index "perform arithmetic" ((lua-tostring -1) T)) #t))
       ((lua-xmove L 1) T)
       (⊨ (and (substring-index "perform arithmetic" ((lua-tostring -1) L)) #t))
       ((lua-pop 1) L))
     ;; (5) a lua-check whose m moved the resume's error value away: the new top is not it
     (let ((T (thread-of "local y = nil local z = y + 1")))
       (⊦= AUX_ETHREAD (code-of (λ () ((lua-check (λ (T) (let ((st (resume T))) ((lua-xmove L 1) T) st))) T))))
       ((lua-pop 1) L))
     ;; (6) a hook-yielded thread that dies on resume under lua-check: raised
     (let ((T (thread-of "local s = 0 for i = 1, 100 do s = s + i end local y = nil return y + s")))
       ((lua-sethook (λ (H e ar) ((lua-yield 0) H)) LUA_MASKCOUNT 20) T)
       (⊦= LUA_YIELD (resume T))
       ((lua-sethook #f 0 0) T)
       (let ((e (condition-case ((lua-check (lua-resume L 0)) T) (e (exn lua) e))))
         (⊦= 'errrun (lua-condition-status e))
         (⊨ (and (substring-index "perform arithmetic" (lua-condition-value e)) #t)))
       (refused-twice T))
     ;; an OK thread and a thread dead in a C frame (error()) raise as before
     (let ((T (thread-of "error('in C', 0)")))
       (⊦= LUA_ERRRUN (resume T))
       (⊦raises (exn lua) ((lua-error) T))
       ((lua-pushstring "again") T)
       (⊦= "again" (lua-condition-value (condition-case ((lua-check (λ (T) LUA_ERRRUN)) T) (e (exn lua) e)))))
     (⊦= 0 ((lua-gettop) L))
     (gc #t)
     (lua-close L)))

  ;; hook views (S5) cannot move or lower slots at or below their floor (the hooked function's
  ;; registers); a hand-built one checks the rule
  ((test/s1-hook-floor _)
   (let* ((L (fresh)) (H (make-luaR (luaR-state L) (luaR-u L) 'hook #f -1 2 #f)) (ptr #f))
     (define (stack) (map (λ (i) ((lua-tointeger i) L)) (iota ((lua-gettop) L) 1)))
     (for-each (λ (i) ((lua-pushinteger i) L)) '(1 2 3 4))
     (⊦raises (exn lua api) ((lua-rotate 1 1) H))
     (⊦raises (exn lua api) ((lua-rotate 2 1) H))
     (⊦raises (exn lua api) ((lua-insert -3) H))
     (⊦raises (exn lua api) ((lua-remove 2) H))
     (⊦= '(1 2 3 4) (stack))
     ((lua-rotate 3 1) H)  (⊦= '(1 2 4 3) (stack))
     ((lua-insert 3) H)    (⊦= '(1 2 3 4) (stack))
     ;; the registers below the floor are not written either (a for loop's state is typed)
     (⊦raises (exn lua api) ((lua-copy 4 1) H))
     (⊦raises (exn lua api) ((lua-copy 4 -3) H))
     ((lua-copy 1 3) H)    (⊦= '(1 2 1 4) (stack))                ; reading them is fine
     ((lua-remove 3) H)    (⊦= '(1 2 4) (stack))
     (⊦raises (exn lua api) ((lua-replace 2) H))                  ; a register
     ((lua-pushinteger 9) L)
     ((lua-replace 3) H)   (⊦= '(1 2 9) (stack))                  ; top 4 -> 3, above the floor
     ((lua-replace 3) H)   (⊦= '(1 2) (stack))                    ; top 3 -> 2 = floor
     (⊦raises (exn lua api) ((lua-replace 1) H))                  ; would go below the floor
     (⊦raises (exn lua api) ((lua-remove -1) H))
     (⊦= '(1 2) (stack))
     ;; xmove from a hook view: at most down to the floor
     ((register! "grab" (λ (C) (set! ptr (luaR-state C)) 0)) L)
     (run! L "CO = coroutine.create(function() grab() coroutine.yield() end) assert(coroutine.resume(CO))")
     (let ((HT (make-luaR ptr (luaR-u L) 'hook #f -1 2 #f)) (T (make-luaR ptr (luaR-u L) 'thread #f -1 0 #f)))
       ((lua-settop 0) T)
       (for-each (λ (i) ((lua-pushinteger i) T)) '(1 2 3 4))
       (⊦raises (exn lua api) ((lua-xmove L 3) HT))
       ((lua-xmove L 2) HT)
       (⊦= 2 ((lua-gettop) T))
       ((lua-settop 0) T))
     (⊦= '(1 2 3 4) (stack))
     ((lua-settop 0) L)
     (lua-close L)))

  ;; S1 bindings are reader-monad actions
  ((test/s1-monad _)
   (let ((L (fresh)))
     (define swap (lua-insert -2))
     (define (dup) (lua-pushvalue -1))
     (define program
       (do/monad
         (lua-pushinteger 1)
         (lua-pushinteger 2)
         swap
         (dup)
         (lua-rotate 1 1)
         (← n (lua-gettop))
         (← a (lua-tointeger 1))
         (← b (lua-tointeger 2))
         (← c (lua-tointeger 3))
         (← table? (lua-istable LUA_REGISTRYINDEX))
         (lua-settop 0)
         ,(list n a b c table?)))
     (⊦= '(3 1 2 1 #t) (program L))
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)))

  ;; LUA_SIGNATURE and LUA_VERSUFFIX
  ((test/s1-signature-constants _)
   (let ((L (fresh)))
     (⊦= "\x1b;Lua" LUA_SIGNATURE)
     (⊦= "_5_5" LUA_VERSUFFIX)
     (⊦= (string-append "_" LUA_VERSION_MAJOR "_" LUA_VERSION_MINOR) LUA_VERSUFFIX)
     ((luaL-loadstring "return 1") L)
     (let ((bin ((lua-dump #f) L)))
       ((lua-pop 1) L)
       (⊦= (string->utf8 LUA_SIGNATURE) (bytevector-prefix bin 4))             ; a binary chunk starts with it
       (⊦= LUA_ERRSYNTAX ((lua-load bin "=b" "t") L))                  ; text only: refused
       (⊨ (string-suffix? "attempt to load a binary chunk (mode is 't')" ((lua-tostring -1) L)))
       ((lua-pop 1) L))
     (run! L (string-append "V = os.getenv('LUA_INIT" LUA_VERSUFFIX "') == os.getenv('LUA_INIT" LUA_VERSUFFIX "')"))
     (⊦= #t (global L "V"))
     (lua-close L)))

  ;; every type code and its symbol; other values are refused
  ((test/s1-type-symbols _)
   (let ((L (fresh)))
     (⊦= 9 LUA_NUMTYPES)
     (⊦= '(none nil boolean lightuserdata number string table function userdata thread)
         (map lua-type->symbol (iota (+ LUA_NUMTYPES 1) -1)))
     (⊦= (list LUA_TNONE LUA_TNIL LUA_TBOOLEAN LUA_TLIGHTUSERDATA LUA_TNUMBER LUA_TSTRING LUA_TTABLE LUA_TFUNCTION LUA_TUSERDATA LUA_TTHREAD)
         (iota (+ LUA_NUMTYPES 1) -1))
     (⊦= '("no value" "nil" "boolean" "userdata" "number" "string" "table" "function" "userdata" "thread")
         (map (λ (t) ((lua-typename t) L)) (iota (+ LUA_NUMTYPES 1) -1)))
     (⊦raises (exn lua api) (lua-type->symbol LUA_NUMTYPES))
     (⊦raises (exn lua api) (lua-type->symbol -2))
     (⊦raises (exn lua api) (lua-type->symbol 'string))
     (lua-close L)))
  )

(unittest/✓ lua-stack-suite)

;; ------------------------------------------------------------------------------------------
;; S2: reading and pushing values, operators
;; ------------------------------------------------------------------------------------------

(define-suite lua-push-to-suite

  ;; every S2 binding is curried: a procedure of the state only, refusing a closed or non state
  ((test/s2-currying _)
   (let ((L (lua-make-empty)))
     (lua-close L)
     (for-each (λ (m)
                 (⊨ (procedure? m))
                 (⊦raises (exn lua api) (m L))
                 (⊦raises (exn lua api) (m 'not-a-state)))
               (list (lua-rawlen 1) (lua-tocfunction 1) (lua-touserdata 1) (lua-tothread 1) (lua-topointer 1)
                     (lua-arith 0) (lua-rawequal 1 2) (lua-compare 1 2 0) (lua-pushfstring "%d" 1)
                     (lua-pushliteral "x") (lua-pushlightuserdata #f) (lua-pushthread) (lua-concat 2)
                     (lua-len 1) (lua-numbertocstring 1) (lua-stringtonumber "1")
                     (lua-strlen 1) (lua-objlen 1) (lua-equal 1 2) (lua-lessthan 1 2)))))

  ((test/s2-numbers-round-trip _)
   (let ((L (fresh)))
     ((lua-pushinteger 9223372036854775807) L)
     ((lua-pushinteger -9223372036854775808) L)
     ((lua-pushnumber +nan.0) L)
     ((lua-pushnumber +inf.0) L)
     ((lua-pushnumber -inf.0) L)
     ((lua-pushnumber -0.0) L)
     ((lua-pushnumber 1/3) L)                                         ; exact -> inexact
     ((lua-pushnumber 3) L)
     ((lua-pushinteger 0) L)
     (⊦= 9 ((lua-gettop) L))
     (⊦eqv 9223372036854775807 ((lua-tointeger 1) L))
     (⊦eqv -9223372036854775808 ((lua-tointeger 2) L))
     (⊨ (nan? ((lua-tonumber 3) L)))
     (⊦eqv +inf.0 ((lua-tonumber 4) L))
     (⊦eqv -inf.0 ((lua-tonumber 5) L))
     (⊨ (eqv? -0.0 ((lua-tonumber 6) L)))
     (⊭ (eqv? 0.0 ((lua-tonumber 6) L)))
     (⊦eqv (exact->inexact 1/3) ((lua-tonumber 7) L))                ; a double: no precision lost
     (⊦eqv 3.0 ((lua-tonumber 8) L))
     (⊦eqv 0.0 ((lua-tonumber 9) L))                                  ; an integer, read as a float
     (⊦= "3.0" ((lua-numbertocstring 8) L))                         ; a float, not an integer
     (⊦= "0" ((lua-numbertocstring 9) L))
     ;; conversions and their ok? flag
     (let-values (((x ok?) ((lua-tonumberx 1) L))) (⊦eqv 9223372036854775807.0 x) (⊨ ok?))
     (let-values (((n ok?) ((lua-tointegerx 4) L))) (⊦= 0 n) (⊭ ok?))    ; inf has no integer representation
     (let-values (((n ok?) ((lua-tointegerx 3) L))) (⊦= 0 n) (⊭ ok?))    ; nor has nan
     (let-values (((n ok?) ((lua-tointegerx 6) L))) (⊦= 0 n) (⊨ ok?))    ; -0.0 has
     ((lua-pushstring "10") L)
     ((lua-pushstring " 0x10 ") L)
     ((lua-pushstring "abc") L)
     ((lua-pushnumber 2.5) L)
     (let-values (((n ok?) ((lua-tointegerx 10) L))) (⊦= 10 n) (⊨ ok?))  ; strings convert, as C
     (let-values (((x ok?) ((lua-tonumberx 11) L))) (⊦eqv 16.0 x) (⊨ ok?))
     (let-values (((x ok?) ((lua-tonumberx 12) L))) (⊦eqv 0.0 x) (⊭ ok?))
     (let-values (((n ok?) ((lua-tointegerx 13) L))) (⊦eqv 0 n) (⊭ ok?))
     (let-values (((n ok?) ((lua-tointegerx 10) L))) (⊦eqv 10 n) (⊨ ok?))
     (⊦eqv 0 ((lua-tointeger 12) L))                                 ; 0 when not convertible
     (⊦eqv 0.0 ((lua-tonumber 12) L))
     (⊦= LUA_TSTRING ((lua-type 10) L))                              ; the slots are not converted
     (⊦= 13 ((lua-gettop) L))
     (⊦raises (exn) ((lua-pushinteger 9223372036854775808) L))
     (⊦raises (exn) ((lua-pushinteger -9223372036854775809) L))
     (⊦raises (exn lua api) ((lua-tonumberx 0) L))
     (⊦raises (exn lua api) ((lua-tointegerx -14) L))
     (⊦= 13 ((lua-gettop) L))
     ((lua-settop 0) L)
     (lua-close L)))

  ((test/s2-booleans-nil-lightuserdata _)
   (let ((L (fresh)))
     ((lua-pushboolean #t) L)
     ((lua-pushboolean #f) L)
     ;; b is C's int used as a boolean: only #t / #f, since Scheme truthiness would make 0 true,
     ;; the opposite of C's lua_pushboolean(L, 0).  Refused before anything is pushed.
     (for-each (λ (b) (⊦= (list AUX_EARG 'lua-pushboolean) (api-refusal (λ () ((lua-pushboolean b) L)))))
               (list 0 1 'anything "" '()))
     (⊦= 2 ((lua-gettop) L))
     ((lua-pushboolean #t) L)
     ((lua-pushnil) L)
     ((lua-pushinteger 0) L)
     (⊨ ((lua-toboolean 1) L))
     (⊭ ((lua-toboolean 2) L))
     (⊨ ((lua-toboolean 3) L))
     (⊭ ((lua-toboolean 4) L))
     (⊨ ((lua-toboolean 5) L))                                       ; Lua truthiness: 0 is true
     (⊭ ((lua-toboolean 6) L))                                       ; none
     (⊦= LUA_TBOOLEAN ((lua-type 3) L))
     ((lua-settop 0) L)
     ;; light userdata: identity through Lua, NULL as #f
     (let ((p (luaR-state L)))
       ((lua-pushlightuserdata p) L)
       ((lua-pushlightuserdata #f) L)
       (⊦= LUA_TLIGHTUSERDATA ((lua-type 1) L))
       (⊦= LUA_TLIGHTUSERDATA ((lua-type 2) L))
       (⊦= p ((lua-touserdata 1) L))
       (⊦= p ((lua-topointer 1) L))
       (⊦= #f ((lua-touserdata 2) L))
       (⊨ ((lua-rawequal 1 1) L))
       (⊭ ((lua-rawequal 1 2) L))
       ((lua-pushvalue 1) L)
       ((lua-setglobal "LU") L)
       (run! L "LT = type(LU)")
       (⊦= "userdata" (global L "LT"))
       ((lua-getglobal "LU") L)
       (⊨ ((lua-rawequal 1 -1) L))
       (⊦raises (exn lua api) ((lua-pushlightuserdata 42) L))
       (⊦= 3 ((lua-gettop) L)))
     ((lua-settop 0) L)
     (lua-close L)))

  ((test/s2-strings _)
   (let ((L (fresh)))
     ((lua-pushlstring "a\x0;b\x0;") L)
     ((lua-pushlstring "") L)
     ((lua-pushlstring (bytevector 0 255 1)) L)
     ((lua-pushstring "caf\xe9;") L)                                  ; UTF-8 in and out
     ((lua-pushliteral "lit") L)
     ((lua-pushliteral #f) L)                                        ; as lua_pushstring(NULL)
     (⊦= 6 ((lua-gettop) L))
     (⊦= "a\x0;b\x0;" ((lua-tolstring 1) L))
     (⊦= 4 ((lua-rawlen 1) L))
     (⊦= "" ((lua-tostring 2) L))
     (⊦= 0 ((lua-rawlen 2) L))
     (⊦= (bytevector 0 255 1) ((lua-tostring 3) L))                 ; invalid UTF-8: bytes
     (⊦= (bytevector 0 255 1) ((lua-tobytevector 3) L))
     (⊦= "caf\xe9;" ((lua-tostring 4) L))
     (⊦= 5 ((lua-rawlen 4) L))                                       ; bytes, not characters
     (⊦= (string->utf8 "caf\xe9;") ((lua-tobytevector 4) L))
     (⊦= "lit" ((lua-tostring 5) L))
     (⊦= LUA_TNIL ((lua-type 6) L))
     (⊦= #f ((lua-tostring 6) L))
     (⊦= #f ((lua-tobytevector 6) L))
     ;; Lua sees the same bytes
     ((lua-pushvalue 1) L)
     ((lua-setglobal "S1") L)
     (run! L "N1 = #S1; B = S1:byte(2); C = S1:byte(4)")
     (⊦= 4 (global L "N1"))
     (⊦= 0 (global L "B"))
     (⊦= 0 (global L "C"))
     ;; numbers read as strings, the slot left a number (safe for lua_next)
     ((lua-pushinteger 42) L)
     ((lua-pushnumber 0.1) L)
     (⊦= "42" ((lua-tolstring 7) L))
     (⊦= "0.1" ((lua-tostring 8) L))
     (⊦= LUA_TNUMBER ((lua-type 7) L))
     (⊦= LUA_TNUMBER ((lua-type 8) L))
     (⊦= "42" ((luaL-checklstring 7) L))                            ; the same for luaL-checklstring
     (⊦= LUA_TNUMBER ((lua-type 7) L))
     (⊦= 0 ((lua-rawlen 7) L))                                       ; not a string
     ;; a 20 MB string, in and out
     (let ((big (make-string (* 20 1024 1024) #\z)))
       (string-set! big 12345 #\x0)
       ((lua-pushlstring big) L)
       (⊦= (* 20 1024 1024) ((lua-rawlen -1) L))
       (let ((bv ((lua-tobytevector -1) L)))
         (⊦= (* 20 1024 1024) (bytevector-length bv))
         (⊦= 0 (bytevector-u8-ref bv 12345))
         (⊦= 122 (bytevector-u8-ref bv (- (* 20 1024 1024) 1))))
       (⊦= big ((lua-tostring -1) L))
       ((lua-pop 1) L))
     (⊦= 8 ((lua-gettop) L))
     ((lua-settop 0) L)
     (lua-close L)))

  ((test/s2-numbertocstring _)
   (let ((L (fresh)))
     (run! L "local V = {1, -0.0, 1e100, 0.1, 2^63, math.pi, 1/0, -1/0, math.mininteger, 3.0, 1e15, 1e16, -7, 2^-1074}
              N, S = {}, {}
              for i, v in ipairs(V) do N['k' .. i] = v; S['k' .. i] = tostring(v) end
              COUNT = #V")
     ((lua-getglobal "N") L)
     ((lua-getglobal "S") L)
     (do ((i 1 (+ i 1))) ((> i (global L "COUNT")))
       (let ((k (string-append "k" (number->string i))))
         ((lua-getfield 1 k) L)
         ((lua-getfield 2 k) L)
         (⊦= ((lua-tostring -1) L) ((lua-numbertocstring -2) L))
         ((lua-pop 2) L)))
     ;; not numbers: #f, even for a string holding a numeral
     ((lua-pushstring "12") L)
     (⊦= #f ((lua-numbertocstring -1) L))
     (⊦= LUA_TSTRING ((lua-type -1) L))
     (⊦= #f ((lua-numbertocstring 99) L))                            ; above the top
     (⊦raises (exn lua api) ((lua-numbertocstring 0) L))
     (⊦= 3 ((lua-gettop) L))
     ((lua-settop 0) L)
     (lua-close L)))

  ((test/s2-stringtonumber _)
   (let ((L (fresh)))
     (⊦= 3 ((lua-stringtonumber "42") L))                            ; strlen + 1
     (⊦= 1 ((lua-gettop) L))
     (let-values (((n ok?) ((lua-tointegerx -1) L))) (⊦= 42 n) (⊨ ok?))
     (⊦= 6 ((lua-stringtonumber "0x1p4") L))
     (⊦= 16.0 ((lua-tonumber -1) L))
     (⊦= "16.0" ((lua-numbertocstring -1) L))                        ; a float
     (⊦= 7 ((lua-stringtonumber "  12  ") L))
     (⊦= 12 ((lua-tointeger -1) L))
     (⊦= 4 ((lua-stringtonumber "1e2") L))
     (⊦= 100.0 ((lua-tonumber -1) L))
     (⊦= 2 ((lua-stringtonumber (bytevector 55)) L))                 ; a bytevector "7"
     (⊦= 7 ((lua-tointeger -1) L))
     (⊦= 5 ((lua-gettop) L))
     ;; failures push nothing
     (⊦= 0 ((lua-stringtonumber "abc") L))
     (⊦= 0 ((lua-stringtonumber "1e") L))
     (⊦= 0 ((lua-stringtonumber "") L))
     (⊦= 0 ((lua-stringtonumber "1\x0;2") L))                        ; C would read "1": refused
     (⊦= 0 ((lua-stringtonumber "9223372036854775808x") L))
     (⊦= 5 ((lua-gettop) L))
     (⊦raises (exn lua api) ((lua-stringtonumber 12) L))
     ;; Lua's own conversion agrees
     (⊦= 20 ((lua-stringtonumber "9223372036854775808") L))           ; overflows to a float
     (⊦= "9.2233720368547758e+18" ((lua-numbertocstring -1) L))  ; as 5.5 prints it
     ((lua-settop 0) L)
     (lua-close L)))

  ((test/s2-pushfstring _)
   (let ((L (fresh)))
     (⊦= "42|str|1.5|-7|A|\x20AC;|%|%q|0.1" ((lua-pushfstring "%d|%s|%f|%I|%c|%U|%%|%q|%f" 42 "str" 1.5 -7 65 #x20AC 0.1) L))
     (⊦= 1 ((lua-gettop) L))
     (⊦= "42|str|1.5|-7|A|\x20AC;|%|%q|0.1" ((lua-tostring -1) L))
     (⊦= "(null) 9223372036854775807 1e+100 3.0" ((lua-pushfstring "%s %I %f %f" #f 9223372036854775807 1e100 3) L))
     (⊦= "a\x0;b" ((lua-pushfstring "a%cb" 0) L))                  ; NUL bytes survive
     (⊦= 3 ((lua-rawlen -1) L))
     (⊦= (bytevector 120 255) ((lua-pushfstring "x%s" (bytevector 255)) L))   ; not UTF-8: bytes
     (⊦= "plain" ((lua-pushfstring "plain") L))
     (⊦= 5 ((lua-gettop) L))
     ;; %p formats a pointer as Lua's string.format('%p') does
     (let ((p (luaR-state L)))
       ((lua-pushlightuserdata p) L)
       ((lua-setglobal "LU") L)
       (run! L "P = string.format('%p', LU)")
       (⊦= (global L "P") ((lua-pushfstring "%p" p) L)))
     ;; refusals push nothing
     (⊦raises (exn lua api) ((lua-pushfstring "%d") L))
     (⊦raises (exn lua api) ((lua-pushfstring "%d" "x") L))
     (⊦raises (exn lua api) ((lua-pushfstring "%d" (expt 2 40)) L))     ; not a C int
     (⊦raises (exn lua api) ((lua-pushfstring "%I" (expt 2 70)) L))     ; not a lua_Integer
     (⊦raises (exn lua api) ((lua-pushfstring "ok %d then %I" 1 (expt 2 63)) L))
     (⊦raises (exn lua api) ((lua-pushfstring 42) L))
     ;; a NUL in fmt or in a %s argument (C strings: C would stop at it), a %s that is no string
     (⊦= (list AUX_EARG 'lua-pushfstring) (api-refusal (λ () ((lua-pushfstring "a\x0;b") L))))
     (⊦= (list AUX_EARG 'lua-pushfstring) (api-refusal (λ () ((lua-pushfstring "[%s]" "x\x0;y") L))))
     (⊦= (list AUX_EARG 'lua-pushfstring) (api-refusal (λ () ((lua-pushfstring "[%s]" 42) L))))
     (⊦= 6 ((lua-gettop) L))
     ((lua-settop 0) L)
     (lua-close L)))

  ((test/s2-pointers _)
   (let ((L (fresh)))
     (run! L "T = {} F = function() end CO = coroutine.create(F) FILE = io.tmpfile()")
     ((lua-getglobal "T") L)
     ((lua-getglobal "F") L)
     ((lua-getglobal "CO") L)
     ((lua-getglobal "FILE") L)
     ((lua-getglobal "tostring") L)                                  ; a light C function
     ((lua-pushstring "s") L)
     ((lua-pushinteger 1) L)
     (⊨ (pointer? ((lua-topointer 1) L)))
     (⊨ (pointer? ((lua-topointer 2) L)))
     (⊨ (pointer? ((lua-topointer 3) L)))
     (⊨ (pointer? ((lua-topointer 4) L)))
     (⊨ (pointer? ((lua-topointer 6) L)))                            ; strings are objects
     (⊦= #f ((lua-topointer 7) L))
     (⊦= #f ((lua-topointer 99) L))
     (⊭ (equal? ((lua-topointer 1) L) ((lua-topointer 2) L)))
     ((lua-getglobal "T") L)
     (⊦= ((lua-topointer 1) L) ((lua-topointer -1) L))              ; identity
     ((lua-pop 1) L)
     ;; userdata: a full userdata's block, the same as its topointer
     (⊨ (pointer? ((lua-touserdata 4) L)))
     (⊦= ((lua-topointer 4) L) ((lua-touserdata 4) L))
     (⊦= #f ((lua-touserdata 1) L))
     (⊦= #f ((lua-touserdata 7) L))
     ;; C functions: a pointer that works when pushed back
     (⊦= #f ((lua-tocfunction 2) L))                                 ; a Lua function
     (⊦= #f ((lua-tocfunction 1) L))
     (let ((f ((lua-tocfunction 5) L)))
       (⊨ (pointer? f))
       (⊦= f ((lua-topointer 5) L))                                  ; a light C function is its pointer
       ((lua-pushcfunction f) L)
       ((lua-setglobal "ts2") L)
       (run! L "R = ts2(12.5)")
       (⊦= "12.5" (global L "R")))
     ;; a Scheme closure: the trampoline's pointer; lua-toprocedure gives the procedure
     (let ((proc (λ (C) 0)))
       ((lua-pushcfunction proc) L)
       (⊨ (pointer? ((lua-tocfunction -1) L)))
       (⊦= proc ((lua-toprocedure -1) L))
       ((lua-pop 1) L))
     (⊦raises (exn lua api) ((lua-tocfunction 0) L))
     (⊦raises (exn lua api) ((lua-touserdata -8) L))
     (⊦raises (exn lua api) ((lua-topointer (- LUA_REGISTRYINDEX 300)) L))
     (⊦= 7 ((lua-gettop) L))
     ((lua-settop 0) L)
     (lua-close L)))

  ;; A c-pointer handed to Lua as a lua_CFunction (lua-pushcfunction, lua-pushcclosure,
  ;; lua-register, luaL-setfuncs, luaL-newlib, luaL-newlibtable, luaL-requiref) must be a C function
  ;; the module can vouch for: a luaopen-* opener, or what lua-tocfunction read from a LIGHT C
  ;; function.  Lua would call any other as code: a table's or a userdata's address (SIGSEGV), or a C
  ;; closure's function (io.lines' iterator) pushed without the upvalues it reads (a NULL deref).
  ;; Refused with (exn lua api), before Lua runs; nothing is pushed.
  ((test/s2-cfunction-pointers _)
   (let ((L (fresh)) (L2 (fresh)))
     (define (code-of thunk) (condition-case (begin (thunk) 'no-raise) (e (exn lua api) (get-condition-property e 'exn 'location))))
     ((lua-newtable) L)                                                 ; 1: a table
     ((lua-newuserdatauv 64 0) L)                                       ; 2: a full userdata
     (run! L "IT = io.lines('/dev/null') GM = string.gmatch('a', 'a')")
     ((lua-getglobal "IT") L)                                           ; 3: a C closure (upvalues)
     ((lua-getglobal "GM") L)                                           ; 4: another
     ((lua-getglobal "print") L)                                        ; 5: a light C function
     (let ((table-ptr ((lua-topointer 1) L))
           (ud-ptr ((lua-touserdata 2) L))
           (new-ud-ptr (begin1 ((lua-newuserdatauv 16 0) L) ((lua-pop 1) L)))
           (closure-fn ((lua-tocfunction 3) L))
           (closure-fn2 ((lua-tocfunction 4) L))
           (light ((lua-tocfunction 5) L)))
       (for-each (λ (p) (⊨ (pointer? p))) (list table-ptr ud-ptr new-ud-ptr closure-fn closure-fn2 light))
       (for-each (λ (p)
                   (⊦= 'lua-pushcclosure (code-of (λ () ((lua-pushcfunction p) L))))
                   (⊦= 'lua-pushcclosure (code-of (λ () ((lua-pushcclosure p 0) L))))
                   ((lua-pushinteger 1) L)
                   (⊦= 'lua-pushcclosure (code-of (λ () ((lua-pushcclosure p 1) L))))  ; with an upvalue too
                   ((lua-pop 1) L)
                   (⊦= 'lua-pushcclosure (code-of (λ () ((lua-register "g" p) L))))
                   (⊦= 'luaL-setfuncs (code-of (λ () ((luaL-setfuncs `(("ok" . ,nop-callback) ("g" . ,p)) 0) L))))
                   (⊦= 'luaL-newlib (code-of (λ () ((luaL-newlib `(("g" . ,p))) L))))
                   (⊦= 'luaL-newlibtable (code-of (λ () ((luaL-newlibtable `(("g" . ,p))) L))))
                   (⊦= 'luaL-requiref (code-of (λ () ((luaL-requiref "m" p #f) L)))))
                 (list table-ptr ud-ptr new-ud-ptr closure-fn closure-fn2))
       (⊦= 5 ((lua-gettop) L))                                          ; nothing pushed, nothing registered
       (⊦= LUA_TNIL ((lua-getglobal "g") L))
       ((lua-pop 1) L)
       (⊦= LUA_TNIL (begin ((luaL-getsubtable LUA_REGISTRYINDEX "_LOADED") L) (begin1 ((lua-getfield -1 "m") L) ((lua-pop 2) L))))
       ;; a light C function is vouched once lua-tocfunction has read it: pushed back (with or
       ;; without upvalues, which it ignores), in this universe or another one
       ((lua-pushcfunction light) L)
       ((lua-setglobal "P1") L)
       ((lua-pushstring "up") L2)
       ((lua-pushcclosure light 1) L2)
       ((lua-setglobal "P2") L2)
       (run! L "R1 = type(P1) == 'function' and P1 == print")
       (⊦= #t (global L "R1"))
       (run! L2 "R2 = type(P2)")
       (⊦= "function" (global L2 "R2"))
       ((lua-settop 5) L)
       ;; the openers are vouched from the start
       ((lua-pushcfunction luaopen-math) L2)
       ((lua-setglobal "OPENMATH") L2)
       (run! L2 "PI = OPENMATH().pi")
       (⊦= 3.141592653589793 (global L2 "PI"))
       ;; a Scheme procedure's trampoline is a C closure (its handle is an upvalue): not vouched
       ((lua-pushcfunction nop-callback) L)
       (let ((tramp ((lua-tocfunction -1) L)))
         ((lua-pop 1) L)
         (⊦= 'lua-pushcclosure (code-of (λ () ((lua-pushcfunction tramp) L))))))
     ;; the refused calls left both universes usable and closable
     (run! L "X = pcall(IT) ~= nil")
     (⊦= #t (global L "X"))
     ((lua-settop 0) L)
     (lua-close L)
     (lua-close L2)
     (⊦= #f (luaR-state L))
     (⊦= #f (luaR-state L2))))

  ((test/s2-rawlen _)
   (let ((L (fresh)))
     (run! L "T = {1, 2, 3, nil, 5} M = setmetatable({1, 2}, {__len = function() return 99 end}) U = io.tmpfile()")
     ((lua-getglobal "T") L)
     ((lua-getglobal "M") L)
     ((lua-pushobject 'x) L)                                         ; a full userdata of the module
     ((lua-pushinteger 12345) L)
     (⊨ (and (memv ((lua-rawlen 1) L) '(3 5)) #t))                            ; a border, as C
     (⊦= 2 ((lua-rawlen 2) L))                                       ; no __len
     (⊨ (> ((lua-rawlen 3) L) 0))                                    ; the userdata block size
     (⊦= 0 ((lua-rawlen 4) L))
     (⊦= 0 ((lua-rawlen 5) L))                                       ; above the top
     (⊦raises (exn lua api) ((lua-rawlen 0) L))
     (⊦= 4 ((lua-gettop) L))
     ((lua-settop 0) L)
     (lua-close L)))

  ((test/s2-pushthread _)
   (let ((L (fresh)) (in-co #f))
     (⊨ ((lua-pushthread) L))                                        ; the main thread
     (⊦= LUA_TTHREAD ((lua-type -1) L))
     ((lua-setglobal "MAINT") L)
     (run! L "SAME = (MAINT == select(1, coroutine.running()))")
     (⊦= #t (global L "SAME"))
     ((register! "pt" (λ (C) (set! in-co ((lua-pushthread) C)) 1)) L)
     (run! L "local co = coroutine.create(function() return pt() == coroutine.running() end)
              local ok, r = coroutine.resume(co) R = r")
     (⊦= #t (global L "R"))
     (⊦= #f in-co)
     (run! L "R2 = (pt() == MAINT)")
     (⊦= #t (global L "R2"))
     (⊦= #t in-co)
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)))

  ((test/s2-tothread _)
   (let ((L (fresh)))
     (run! L "CO = coroutine.create(function(a) local b = coroutine.yield(a + 1) return b * 2 end)")
     ((lua-getglobal "CO") L)
     (let ((T ((lua-tothread -1) L)))
       (⊨ (luaR? T))
       (⊦= 'thread (luaR-kind T))
       (⊨ (pointer? (luaR-state T)))
       (⊦= 1 ((lua-gettop) T))                                       ; not started: just its function
       (⊦= #f ((lua-pushthread) T))                                  ; not the main thread
       (⊦= LUA_TTHREAD ((lua-type -1) T))
       ((lua-pop 1) T)
       ;; the same thread as Lua's value
       ((lua-pushthread) T)
       ((lua-setglobal "CO_AGAIN") T)
       (run! L "SAME = rawequal(CO, CO_AGAIN) CO_AGAIN = nil")
       (⊦= #t (global L "SAME"))
       ;; start it and let it yield: a suspended thread
       (run! L "local ok, v = coroutine.resume(CO, 41) V = v")
       (⊦= 42 (global L "V"))
       (⊦= LUA_YIELD (%status (luaR-state T)))
       ;; P* pushes onto the suspended thread run on the helper
       (let ((top ((lua-gettop) T)))
         ((lua-pushlstring "x\x0;y") T)
         (⊦= "x\x0;y" ((lua-tostring -1) T))
         (⊦= "v=7" ((lua-pushfstring "v=%d" 7) T))
         (⊦= (+ top 2) ((lua-gettop) T))
         ;; lua-tothread from a suspended thread anchors through the helper too
         ((lua-pushthread) T)
         (let ((T2 ((lua-tothread -1) T)))
           (⊦= (luaR-state T) (luaR-state T2))
           (⊦= 'thread (luaR-kind T2)))
         ((lua-pop 3) T)
         (⊦= top ((lua-gettop) T)))
       ;; plain P ops refuse a non-OK thread
       ((lua-pushinteger 1) T)
       ((lua-pushinteger 2) T)
       (⊦raises (exn lua api) ((lua-arith 0) T))
       (⊦raises (exn lua api) ((lua-compare -1 -2 1) T))
       (⊦raises (exn lua api) ((lua-concat 2) T))
       ((lua-pop 2) T)
       ;; the luaR keeps the thread alive after Lua dropped it
       (run! L "CO = nil collectgarbage() collectgarbage()")
       (⊦= LUA_YIELD (%status (luaR-state T)))
       ((lua-pushinteger 5) T)
       (⊦= 5 ((lua-tointeger -1) T))
       ((lua-pop 1) T))
     ;; not a thread, above the top, invalid
     ((lua-pushinteger 1) L)
     (⊦= #f ((lua-tothread -1) L))
     (⊦= #f ((lua-tothread 99) L))
     (⊦raises (exn lua api) ((lua-tothread 0) L))
     (⊦raises (exn lua api) ((lua-tothread -5) L))
     ;; the main thread itself: a 'thread luaR that cannot close the universe
     ((lua-pushthread) L)
     (let ((M ((lua-tothread -1) L)))
       (⊦= (luaR-state L) (luaR-state M))
       (⊦raises (exn lua api) (lua-close M)))
     (⊦= 3 ((lua-gettop) L))
     ((lua-settop 0) L)
     ;; closing the universe invalidates every thread luaR
     (run! L "CO3 = coroutine.create(print)")
     ((lua-getglobal "CO3") L)
     (let ((T3 ((lua-tothread -1) L)))
       (lua-close L)
       (⊦raises (exn lua api) ((lua-gettop) T3)))))

  ;; a thread luaR's registry anchor keeps the thread alive, and is released once the luaR is collected
  ((test/s2-tothread-release _)
   (let ((L (fresh)))
     (define (count!) (run! L "collectgarbage() collectgarbage() N = 0 for _ in pairs(W) do N = N + 1 end") (global L "N"))
     (run! L "W = setmetatable({}, {__mode = 'k'}) CO = coroutine.create(print) W[CO] = true")
     ((lua-getglobal "CO") L)
     (let ((T ((lua-tothread -1) L)))
       ((lua-pop 1) L)
       (run! L "CO = nil")
       (⊦= 1 (count!))                                               ; only T holds it
       (⊦= 1 ((lua-gettop) T)))
     (flush-and-collect (λ () (pair? (luaU-unrefs (luaR-u L)))))
     (⊦= 0 (count!))                                                 ; the anchor was released
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)))

  ((test/s2-rawequal-compare _)
   (let ((L (fresh)) (OPEQ 0) (OPLT 1) (OPLE 2))
     (run! L "local mt = {__eq = function(a, b) return a.v == b.v end,
                         __lt = function(a, b) return a.v < b.v end,
                         __le = function(a, b) return a.v <= b.v end}
              A = setmetatable({v = 1}, mt) B = setmetatable({v = 1}, mt) C = setmetatable({v = 2}, mt)")
     ((lua-getglobal "A") L)
     ((lua-getglobal "B") L)
     ((lua-getglobal "C") L)
     ((lua-pushinteger 1) L)
     ((lua-pushnumber 1.0) L)
     ((lua-pushstring "a") L)
     ((lua-pushstring "b") L)
     ;; rawequal ignores __eq, compare honours it
     (⊭ ((lua-rawequal 1 2) L))
     (⊨ ((lua-compare 1 2 OPEQ) L))
     (⊨ ((lua-rawequal 1 1) L))
     (⊭ ((lua-compare 1 3 OPEQ) L))
     (⊨ ((lua-compare 1 3 OPLT) L))
     (⊭ ((lua-compare 3 1 OPLT) L))
     (⊨ ((lua-compare 1 2 OPLE) L))
     (⊭ ((lua-compare 3 2 OPLE) L))
     ;; numbers and strings, as Lua
     (⊨ ((lua-rawequal 4 5) L))                                      ; 1 == 1.0
     (⊨ ((lua-compare 4 5 OPEQ) L))
     (⊨ ((lua-compare 6 7 OPLT) L))
     (⊨ ((lua-compare -2 -1 OPLE) L))
     (⊭ ((lua-compare -1 -2 OPLE) L))
     (⊨ ((lua-compare LUA_REGISTRYINDEX LUA_REGISTRYINDEX OPEQ) L)) ; pseudo-indices are readable
     ;; above the top: false without calling Lua (C semantics)
     (⊭ ((lua-rawequal 1 99) L))
     (⊭ ((lua-compare 99 99 OPEQ) L))
     (⊦= 7 ((lua-gettop) L))
     ;; errors: comparing a number with a string, a raising __lt
     (let ((e (condition-case ((lua-compare 4 6 OPLT) L) (e (exn lua) e))))
       (⊦= 'errrun (lua-condition-status e))
       (⊦= "attempt to compare number with string" (msg e)))
     (run! L "BAD = setmetatable({}, {__lt = function() error('no order', 0) end})")
     ((lua-getglobal "BAD") L)
     (⊦= "no order" (condition-case ((lua-compare -1 -1 OPLT) L) (e (exn lua) (msg e))))
     (⊦= 8 ((lua-gettop) L))                                         ; nothing consumed
     (⊨ ((lua-compare 1 2 OPEQ) L))                                  ; the state is still usable
     ;; refusals
     (⊦raises (exn lua api) ((lua-compare 1 2 3) L))
     (⊦raises (exn lua api) ((lua-compare 1 2 -1) L))
     (⊦raises (exn lua api) ((lua-compare 0 1 OPEQ) L))
     (⊦raises (exn lua api) ((lua-compare 1 -9 OPEQ) L))
     (⊦raises (exn lua api) ((lua-rawequal 0 1) L))
     (⊦raises (exn lua api) ((lua-rawequal 1 -9) L))
     (⊦= 8 ((lua-gettop) L))
     ((lua-settop 0) L)
     (lua-close L)))

  ;; luaconf.h's compatibility macros, which Lua 5.5 always defines: lua_strlen and lua_objlen are
  ;; lua_rawlen, lua_equal and lua_lessthan are lua_compare with LUA_OPEQ and LUA_OPLT.  Same
  ;; answers, same errors and refusals, reported under their own names.
  ((test/s2-luaconf-compat _)
   (let ((L (fresh)))
     (run! L "local mt = {__eq = function(a, b) return a.v == b.v end, __lt = function(a, b) return a.v < b.v end,
                         __len = function() return 99 end}
              A = setmetatable({v = 1, 10, 20}, mt) B = setmetatable({v = 1}, mt) C = setmetatable({v = 2}, mt)")
     (for-each (λ (g) ((lua-getglobal g) L)) '("A" "B" "C"))
     ((lua-pushstring "h\x0;llo") L)
     ((lua-pushinteger 1) L)
     ((lua-pushstring "x") L)
     (for-each (λ (i) (⊦= ((lua-rawlen i) L) ((lua-strlen i) L)) (⊦= ((lua-rawlen i) L) ((lua-objlen i) L)))
               '(1 2 3 4 5 6 -1 99))
     (⊦= '(2 5 0 0) (map (λ (i) ((lua-objlen i) L)) '(1 4 5 99)))       ; raw: no __len; NUL counted
     (for-each (λ (i1 i2)
                 (⊦= ((lua-compare i1 i2 LUA_OPEQ) L) ((lua-equal i1 i2) L))
                 (⊦= ((lua-compare i1 i2 LUA_OPLT) L) ((lua-lessthan i1 i2) L)))
               '(1 1 3 4 99)
               '(2 3 1 4 1))
     (⊨ ((lua-equal 1 2) L))                                            ; __eq
     (⊨ ((lua-lessthan 1 3) L))                                         ; __lt
     (⊭ ((lua-lessthan 3 1) L))
     (⊭ ((lua-equal 1 99) L))                                           ; above the top: false, as C
     (let ((e (condition-case ((lua-lessthan 5 6) L) (e (exn lua) e))))  ; 1 < "x"
       (⊦= 'errrun (lua-condition-status e))
       (⊦= "attempt to compare number with string" (msg e)))
     (⊦= (list AUX_EINDEX 'lua-strlen) (api-refusal (λ () ((lua-strlen 0) L))))
     (⊦= (list AUX_EINDEX 'lua-objlen) (api-refusal (λ () ((lua-objlen -99) L))))
     (⊦= (list AUX_EINDEX 'lua-equal) (api-refusal (λ () ((lua-equal 0 1) L))))
     (⊦= (list AUX_EINDEX 'lua-lessthan) (api-refusal (λ () ((lua-lessthan 1 -99) L))))
     (⊦raises (exn lua api) ((lua-equal 1 4294967297) L))              ; not a C int
     (⊦= 6 ((lua-gettop) L))
     ((lua-settop 0) L)
     (lua-close L)))

  ;; Scheme metamethods: compare (on the safe path) calls back into Scheme, errors come back
  ((test/s2-compare-scheme-metamethods _)
   (let ((L (fresh)) (calls '()) (OPEQ 0) (OPLT 1))
     (define (field C idx) ((lua-getfield idx "v") C) (begin1 ((lua-tointeger -1) C) ((lua-pop 1) C)))
     (run! L "MT = {}")
     ((lua-getglobal "MT") L)
     ((lua-pushcfunction (λ (C) (set! calls (cons 'lt calls))
                            ((lua-pushboolean (< (field C 1) (field C 2))) C) 1)) L)
     ((lua-setfield -2 "__lt") L)
     ((lua-pushcfunction (λ (C) (set! calls (cons 'eq calls))
                            ((lua-pushboolean (= (field C 1) (field C 2))) C) 1)) L)
     ((lua-setfield -2 "__eq") L)
     ((lua-pushcfunction (λ (C) (set! calls (cons 'le calls)) (abort 'scheme-le))) L)
     ((lua-setfield -2 "__le") L)
     ((lua-pop 1) L)
     (run! L "X = setmetatable({v = 1}, MT) Y = setmetatable({v = 2}, MT) Z = setmetatable({v = 1}, MT)")
     ((lua-getglobal "X") L)
     ((lua-getglobal "Y") L)
     ((lua-getglobal "Z") L)
     (⊨ ((lua-compare 1 2 OPLT) L))
     (⊭ ((lua-compare 2 1 OPLT) L))
     (⊨ ((lua-compare 1 3 OPEQ) L))
     (⊭ ((lua-compare 1 2 OPEQ) L))
     (⊦= '(eq eq lt lt) calls)
     ;; the Scheme object raised by __le comes back as itself
     (⊦= 'scheme-le (condition-case ((lua-compare 1 2 2) L) (e () e)))
     (⊦= 3 ((lua-gettop) L))
     (⊨ ((lua-compare 1 2 OPLT) L))
     ((lua-settop 0) L)
     (lua-close L)))

  ((test/s2-arith _)
   (let ((L (fresh))
         (ops '((0 . "+") (1 . "-") (2 . "*") (3 . "%") (4 . "^") (5 . "/") (6 . "//")
                (7 . "&") (8 . "|") (9 . "~") (10 . "<<") (11 . ">>"))))
     (define ((push-num x) L) (if (exact? x) ((lua-pushinteger x) L) ((lua-pushnumber x) L)))
     (define (set-meta! L mt field f) ((lua-getglobal mt) L) ((lua-pushcfunction f) L) ((lua-setfield -2 field) L) ((lua-pop 1) L))
     ;; every binary operator on integers and on floats, against Lua's own result
     (for-each
       (λ (operands)
         (for-each
           (λ (op)
             (let ((a (car operands)) (b (cdr operands)))
               (unless (and (member (cdr op) '("&" "|" "~" "<<" ">>")) (or (inexact? a) (inexact? b)))
                 (run! L (string-append "R = (" (number->string a) ") " (cdr op) " (" (number->string b) ")"))
                 ((push-num a) L)
                 ((push-num b) L)
                 ((lua-arith (car op)) L)
                 (⊦= 1 ((lua-gettop) L))
                 ((lua-getglobal "R") L)
                 (⊨ ((lua-rawequal -1 -2) L))
                 (⊦= ((lua-numbertocstring -1) L) ((lua-numbertocstring -2) L))   ; same subtype too
                 ((lua-pop 2) L))))
           ops))
       '((7 . 3) (-7 . 2) (7.5 . 2) (-7.5 . 2.0) (1 . 64)))
     ;; unary operators take one operand
     ((lua-pushinteger 7) L)
     ((lua-arith 12) L)
     (⊦= -7 ((lua-tointeger -1) L))
     ((lua-arith 13) L)
     (⊦= 6 ((lua-tointeger -1) L))                                   ; ~(-7)
     ((lua-pushnumber 2.5) L)
     ((lua-arith 12) L)
     (⊦= -2.5 ((lua-tonumber -1) L))
     (⊦= 2 ((lua-gettop) L))
     ((lua-settop 0) L)
     ;; strings are coerced, metamethods run
     ((lua-pushstring "10") L)
     ((lua-pushinteger 5) L)
     ((lua-arith 0) L)
     (⊦= 15 ((lua-tointeger -1) L))
     (run! L "VM = {__add = function(a, b) return 'added' end, __unm = function(a) return 'negated' end} V = setmetatable({}, VM)")
     ((lua-getglobal "V") L)
     ((lua-pushinteger 1) L)
     ((lua-arith 0) L)
     (⊦= "added" ((lua-tostring -1) L))
     ((lua-getglobal "V") L)
     ((lua-arith 12) L)
     (⊦= "negated" ((lua-tostring -1) L))
     (⊦= 3 ((lua-gettop) L))
     ;; a Scheme __mul
     (set-meta! L "VM" "__mul" (λ (C) ((lua-pushstring "scheme-mul") C) 1))
     ((lua-getglobal "V") L)
     ((lua-pushinteger 2) L)
     ((lua-arith 2) L)
     (⊦= "scheme-mul" ((lua-tostring -1) L))
     ((lua-settop 0) L)
     ;; errors consume the operands: integer division and modulo by zero, bad operands
     ((lua-pushinteger 99) L)
     ((lua-pushinteger 1) L)
     ((lua-pushinteger 0) L)
     (let ((e (condition-case ((lua-arith 6) L) (e (exn lua) e))))
       (⊦= 'errrun (lua-condition-status e))
       (⊨ (string-suffix? "attempt to divide by zero" (msg e)))
       (⊦= 'lua-arith ((condition-property-accessor 'exn 'location) e)))
     (⊦= 1 ((lua-gettop) L))                                         ; -2
     ((lua-pushinteger 1) L)
     ((lua-pushinteger 0) L)
     (⊨ (string-suffix? "attempt to perform 'n%0'" (condition-case ((lua-arith 3) L) (e (exn lua) (msg e)))))
     (⊦= 1 ((lua-gettop) L))
     ((lua-pushnumber 1.0) L)
     ((lua-pushinteger 0) L)
     ((lua-arith 6) L)                                               ; float // 0 is fine
     (⊦= +inf.0 ((lua-tonumber -1) L))
     ((lua-pop 1) L)
     ((lua-pushboolean #t) L)
     ((lua-pushinteger 1) L)
     (⊨ (string-suffix? "attempt to perform arithmetic on a boolean value" (condition-case ((lua-arith 0) L) (e (exn lua) (msg e)))))
     ((lua-pushnumber 1.5) L)
     (⊨ (string-suffix? "number has no integer representation" (condition-case ((lua-arith 13) L) (e (exn lua) (msg e)))))
     (⊦= 1 ((lua-gettop) L))                                         ; -1 for a unary op
     (⊦= 99 ((lua-tointeger 1) L))                                   ; usable afterwards
     ;; refusals: bad op, not enough operands
     (⊦raises (exn lua api) ((lua-arith 14) L))
     (⊦raises (exn lua api) ((lua-arith -1) L))
     (⊦raises (exn lua api) ((lua-arith 0) L))                       ; one value only
     ((lua-settop 0) L)
     (⊦raises (exn lua api) ((lua-arith 12) L))
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)))

  ((test/s2-concat _)
   (let ((L (fresh)))
     (define (set-meta! L mt field f) ((lua-getglobal mt) L) ((lua-pushcfunction f) L) ((lua-setfield -2 field) L) ((lua-pop 1) L))
     ((lua-pushstring "a") L)
     ((lua-pushinteger 1) L)
     ((lua-pushnumber 2.5) L)
     ((lua-pushlstring "\x0;z") L)
     ((lua-concat 4) L)
     (⊦= 1 ((lua-gettop) L))
     (⊦= "a12.5\x0;z" ((lua-tostring -1) L))
     ((lua-concat 1) L)                                               ; one value: unchanged
     (⊦= "a12.5\x0;z" ((lua-tostring -1) L))
     ((lua-pushinteger 3) L)
     ((lua-concat 1) L)
     (⊦= LUA_TNUMBER ((lua-type -1) L))                             ; not even converted
     ((lua-concat 0) L)                                               ; none: ""
     (⊦= "" ((lua-tostring -1) L))
     (⊦= 3 ((lua-gettop) L))
     ((lua-settop 0) L)
     ;; __concat, in Lua and in Scheme
     (run! L "CC = setmetatable({}, {__concat = function(a, b) return 'cat' end})")
     ((lua-pushstring "x") L)
     ((lua-getglobal "CC") L)
     ((lua-concat 2) L)
     (⊦= "cat" ((lua-tostring -1) L))
     (run! L "SCM = {} SC = setmetatable({}, SCM)")
     (set-meta! L "SCM" "__concat" (λ (C) ((lua-pushfstring "<%s>" ((lua-tostring 1) C)) C) 1))
     ((lua-pushstring "p") L)
     ((lua-getglobal "SC") L)
     ((lua-concat 2) L)
     (⊦= "<p>" ((lua-tostring -1) L))
     (⊦= 2 ((lua-gettop) L))
     ;; errors consume the n operands
     ((lua-pushstring "a") L)
     ((lua-pushboolean #t) L)
     ((lua-pushstring "b") L)
     (let ((e (condition-case ((lua-concat 3) L) (e (exn lua) e))))
       (⊨ (string-suffix? "attempt to concatenate a boolean value" (msg e))))
     (⊦= 2 ((lua-gettop) L))
     ;; out of memory
     (run! L "M = collectgarbage('count')")
     ((lua-pushlstring (make-string 600000 #\a)) L)
     ((lua-pushlstring (make-string 600000 #\b)) L)
     ((lua-setmemorylimit (+ (inexact->exact (floor (* 1024 (global L "M")))) 1600000)) L)
     (let ((e (condition-case ((lua-concat 2) L) (e (exn lua) e))))
       (⊦= 'errmem (lua-condition-status e)))
     (⊦= 2 ((lua-gettop) L))
     ((lua-setmemorylimit 0) L)
     (⊦raises (exn lua api) ((lua-concat -1) L))
     (⊦raises (exn lua api) ((lua-concat 3) L))                       ; only 2 values
     (⊦= 2 ((lua-gettop) L))
     ((lua-settop 0) L)
     (lua-close L)))

  ((test/s2-len _)
   (let ((L (fresh)))
     (define (set-meta! L mt field f) ((lua-getglobal mt) L) ((lua-pushcfunction f) L) ((lua-setfield -2 field) L) ((lua-pop 1) L))
     (run! L "T = {1, 2, 3} M = setmetatable({}, {__len = function() return 42 end}) SM = {} S = setmetatable({}, SM)")
     ((lua-getglobal "T") L)
     ((lua-len 1) L)
     (⊦= 3 ((lua-tointeger -1) L))
     ((lua-pushlstring "ab\x0;") L)
     ((lua-len -1) L)
     (⊦= 3 ((lua-tointeger -1) L))
     ((lua-getglobal "M") L)
     ((lua-len -1) L)
     (⊦= 42 ((lua-tointeger -1) L))
     (⊦= 0 ((lua-rawlen -2) L))                                      ; rawlen ignores __len
     ;; a Scheme __len
     (set-meta! L "SM" "__len" (λ (C) ((lua-pushstring "long") C) 1))
     ((lua-getglobal "S") L)
     ((lua-len -1) L)
     (⊦= "long" ((lua-tostring -1) L))
     (⊦= 8 ((lua-gettop) L))
     ;; an error leaves the stack as it was
     ((lua-pushinteger 5) L)
     (let ((e (condition-case ((lua-len -1) L) (e (exn lua) e))))
       (⊦= 'errrun (lua-condition-status e))
       (⊨ (string-suffix? "attempt to get length of a number value" (msg e))))
     (⊦= 9 ((lua-gettop) L))
     ((lua-len 1) L)                                                 ; still usable
     (⊦= 3 ((lua-tointeger -1) L))
     (⊦raises (exn lua api) ((lua-len 0) L))
     (⊦raises (exn lua api) ((lua-len 99) L))
     (⊦= 10 ((lua-gettop) L))
     ((lua-settop 0) L)
     (lua-close L)))

  ;; P* pushes fail with errmem when the memory limit is reached, then the state recovers
  ((test/s2-errmem-pushes _)
   (let ((L (fresh)))
     (run! L "M = collectgarbage('count')")
     ((lua-setmemorylimit (+ (inexact->exact (floor (* 1024 (global L "M")))) 100000)) L)
     (let ((e (condition-case ((lua-pushfstring "%s" (make-string 500000 #\q)) L) (e (exn lua) e))))
       (⊦= 'errmem (lua-condition-status e))
       (⊦= "not enough memory" (msg e)))
     (⊦= 0 ((lua-gettop) L))
     ((lua-setmemorylimit 0) L)
     (⊦= 500000 (string-length ((lua-pushfstring "%s" (make-string 500000 #\q)) L)))
     (⊦= 500000 ((lua-rawlen -1) L))
     ((lua-pop 1) L)
     (lua-close L)))
  )

(unittest/✓ lua-push-to-suite)

;; ------------------------------------------------------------------------------------------
;; S3: tables, userdata, metatables, globals
;; ------------------------------------------------------------------------------------------

(define-suite lua-table-suite

  ;; every S3 binding is curried with the state last, and refuses a closed state or a non-state
  ((test/s3-currying _)
   (let ((L (lua-make-empty)))
     (lua-close L)
     (for-each (λ (m)
                 (⊨ (procedure? m))
                 (⊦raises (exn lua api) (m L))
                 (⊦raises (exn lua api) (m 'not-a-state)))
               (list (lua-gettable 1) (lua-geti 1 1) (lua-rawget 1) (lua-rawgeti 1 1) (lua-rawgetp 1 #f)
                     (lua-createtable 0 0) (lua-newtable) (lua-newuserdatauv 8 1) (lua-newuserdata 8)
                     (lua-getmetatable 1) (lua-getiuservalue 1 1) (lua-getuservalue 1)
                     (lua-settable 1) (lua-seti 1 1) (lua-rawset 1) (lua-rawseti 1 1) (lua-rawsetp 1 #f)
                     (lua-setmetatable 1) (lua-setiuservalue 1 1) (lua-setuservalue 1)
                     (lua-next 1) (lua-pushglobaltable) (lua-register "f" (λ (C) 0))))
     (⊨ (eq? (lua-newtable) (lua-newtable)))                        ; nullary: preallocated closures
     (⊨ (eq? (lua-pushglobaltable) (lua-pushglobaltable)))))

  ((test/s3-gettable-settable _)
   (let ((L (fresh)))
     (run! L "t = {10, 20, x = 'ex', [true] = 'yes', [2.5] = 'float'}")
     ((lua-getglobal "t") L)
     ((lua-pushstring "x") L)
     (⊦= LUA_TSTRING ((lua-gettable 1) L))                          ; the key is replaced by the value
     (⊦= 2 ((lua-gettop) L))
     (⊦= "ex" ((lua-tostring -1) L))
     ((lua-pop 1) L)
     ((lua-pushboolean #t) L)
     (⊦= LUA_TSTRING ((lua-gettable -2) L))
     (⊦= "yes" ((lua-tostring -1) L))
     ((lua-pop 1) L)
     ((lua-pushnumber 2.5) L)
     (⊦= "float" (begin ((lua-gettable 1) L) (begin1 ((lua-tostring -1) L) ((lua-pop 1) L))))
     ((lua-pushnumber 2.0) L)                                       ; a float key with an integer value is key 2
     (⊦= 20 (begin ((lua-gettable 1) L) (begin1 ((lua-tointeger -1) L) ((lua-pop 1) L))))
     ((lua-pushstring "missing") L)
     (⊦= LUA_TNIL ((lua-gettable 1) L))
     ((lua-pop 1) L)
     ;; NUL bytes in keys, both ways
     ((lua-pushlstring "k\x0;z") L)
     ((lua-pushinteger 5) L)
     ((lua-settable 1) L)                                           ; key and value popped
     (⊦= 1 ((lua-gettop) L))
     (run! L "NUL = t['k\\0z'] ; SHORT = t['k']")
     (⊦= 5 (global L "NUL"))
     (⊦= 'nil (global L "SHORT"))
     ((lua-pushlstring (bytevector 107 0 122)) L)
     (⊦= LUA_TNUMBER ((lua-gettable 1) L))
     (⊦= 5 ((lua-tointeger -1) L))
     ((lua-pop 1) L)
     ;; geti / seti, including the lua_Integer extremes
     (⊦= LUA_TNUMBER ((lua-geti 1 1) L))
     (⊦= 10 ((lua-tointeger -1) L))
     (⊦= LUA_TNIL ((lua-geti -2 3) L))
     ((lua-pop 2) L)
     ((lua-pushinteger 30) L)
     ((lua-seti 1 3) L)
     ((lua-pushstring "max") L)
     ((lua-seti 1 9223372036854775807) L)
     ((lua-pushstring "min") L)
     ((lua-seti -2 -9223372036854775808) L)
     (⊦= 1 ((lua-gettop) L))
     (run! L "N = #t; MAX = t[math.maxinteger]; MIN = t[math.mininteger]")
     (⊦= 3 (global L "N"))
     (⊦= "max" (global L "MAX"))
     (⊦= "min" (global L "MIN"))
     (⊦= "min" (begin ((lua-geti 1 -9223372036854775808) L) (begin1 ((lua-tostring -1) L) ((lua-pop 1) L))))
     ;; the registry through its pseudo-index: the globals table
     (⊦= LUA_TTABLE ((lua-geti LUA_REGISTRYINDEX LUA_RIDX_GLOBALS) L))
     ((lua-pushstring "via registry") L)
     ((lua-setfield -2 "REG") L)
     ((lua-pop 1) L)
     (⊦= "via registry" (global L "REG"))
     ;; api refusals: nothing ran, nothing was consumed
     (⊦raises (exn lua api) ((lua-gettable 0) L))
     (⊦raises (exn lua api) ((lua-gettable 2) L))                  ; above the top
     (⊦raises (exn lua api) ((lua-gettable (lua-upvalueindex 1)) L))  ; no Scheme closure is running
     (⊦raises (exn lua api) ((lua-geti 1 9223372036854775808) L))  ; not a lua_Integer
     (⊦raises (exn lua api) ((lua-geti 1 1.0) L))
     (⊦raises (exn lua api) ((lua-seti 1 'x) L))
     ((lua-pushnil) L)
     ((lua-settop 1) L)
     (⊦raises (exn lua api) ((lua-settable 1) L))                  ; only the table: no key, no value
     (⊦= 1 ((lua-gettop) L))
     ((lua-pushvalue 1) L)
     ((lua-pushinteger 1) L)
     ((lua-settable 1) L)                                           ; t[t] = 1: the key may be t itself (C)
     (⊦= 1 ((lua-gettop) L))
     ((lua-pushvalue 1) L)
     (⊦= 1 (begin ((lua-rawget 1) L) (begin1 ((lua-tointeger -1) L) ((lua-pop 1) L))))
     ((lua-settop 0) L)
     (⊦raises (exn lua api) ((lua-gettable 1) L))                  ; empty stack
     (⊦raises (exn lua api) ((lua-gettable LUA_REGISTRYINDEX) L))  ; the registry, but no key
     (⊦raises (exn lua api) ((lua-seti LUA_REGISTRYINDEX 1) L))
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)))

  ;; Lua errors of the protected bindings: (exn lua) with Lua's message, the stack effect of C,
  ;; and a state that keeps working
  ((test/s3-errors _)
   (let ((L (fresh)))
     ((lua-pushinteger 42) L)
     ((lua-pushstring "k") L)
     (let ((e (condition-case ((lua-gettable 1) L) (e (exn lua) e))))
       (⊨ (lua-condition? e))
       (⊦= 'errrun (lua-condition-status e))
       (⊦= "attempt to index a number value" (msg e))
       (⊦= 'lua-gettable ((condition-property-accessor 'exn 'location) e))
       (⊦= '(1) ((condition-property-accessor 'exn 'arguments) e)))
     (⊦= 1 ((lua-gettop) L))                                        ; the key was consumed
     (⊦= "attempt to index a number value" (condition-case ((lua-geti 1 1) L) (e (exn lua) (msg e))))
     (⊦= 1 ((lua-gettop) L))
     ((lua-pushstring "k") L)
     ((lua-pushstring "v") L)
     (⊦= "attempt to index a number value" (condition-case ((lua-settable 1) L) (e (exn lua) (msg e))))
     (⊦= 1 ((lua-gettop) L))                                        ; key and value consumed
     ((lua-pushstring "v") L)
     (⊦= "attempt to index a number value" (condition-case ((lua-seti 1 7) L) (e (exn lua) (msg e))))
     (⊦= 1 ((lua-gettop) L))
     ;; nil / NaN keys: settable raises through luaV_finishset, rawset through luaH_set
     ((lua-newtable) L)
     ((lua-pushnil) L)
     ((lua-pushinteger 1) L)
     (let ((e (condition-case ((lua-settable 2) L) (e (exn lua) e))))
       (⊦= "table index is nil" (msg e))
       (⊦= "table index is nil" (lua-condition-value e)))
     (⊦= 2 ((lua-gettop) L))
     ((lua-pushnil) L)
     ((lua-pushinteger 1) L)
     (⊦= "table index is nil" (condition-case ((lua-rawset 2) L) (e (exn lua) (msg e))))
     (⊦= 2 ((lua-gettop) L))
     ((lua-pushnumber +nan.0) L)
     ((lua-pushinteger 1) L)
     (⊦= "table index is NaN" (condition-case ((lua-rawset -3) L) (e (exn lua) (msg e))))
     (⊦= 2 ((lua-gettop) L))
     ((lua-pushnumber +nan.0) L)
     ((lua-pushinteger 1) L)
     (⊦= "table index is NaN" (condition-case ((lua-settable 2) L) (e (exn lua) (msg e))))
     (⊦= 2 ((lua-gettop) L))
     ;; reading with a nil key is fine
     ((lua-pushnil) L)
     (⊦= LUA_TNIL ((lua-gettable 2) L))
     ((lua-pop 1) L)
     ;; the state is intact: the table still works
     ((lua-pushstring "ok") L)
     ((lua-seti 2 1) L)
     (⊦= "ok" (begin ((lua-rawgeti 2 1) L) (begin1 ((lua-tostring -1) L) ((lua-pop 1) L))))
     (⊦= 2 ((lua-gettop) L))
     ;; out of memory while growing a table
     (run! L "M = collectgarbage('count')")
     ((lua-setmemorylimit (+ (inexact->exact (floor (* 1024 (global L "M")))) 100000)) L)
     (⊦= 'errmem (condition-case ((lua-createtable 1000000 0) L) (e (exn lua) (lua-condition-status e))))
     (⊦= 2 ((lua-gettop) L))
     ((lua-pushstring "same slot") L)
     ((lua-seti 2 1) L)                                             ; an existing slot: no allocation
     (⊦= "same slot" (begin ((lua-geti 2 1) L) (begin1 ((lua-tostring -1) L) ((lua-pop 1) L))))
     (⊦= 2 ((lua-gettop) L))
     ((lua-setmemorylimit 0) L)
     ((lua-createtable 1000000 0) L)
     (⊦= LUA_TTABLE ((lua-type -1) L))
     ((lua-settop 0) L)
     (lua-close L)))

  ((test/s3-raw _)
   (let ((L (fresh)))
     ;; a table whose metamethods all fail: the raw bindings never run them
     (run! L "t = setmetatable({}, {__index = function() error('__index ran', 0) end,
                                    __newindex = function() error('__newindex ran', 0) end})")
     ((lua-getglobal "t") L)
     ((lua-pushstring "a") L)
     ((lua-pushinteger 1) L)
     ((lua-rawset 1) L)
     (⊦= 1 ((lua-gettop) L))
     ((lua-pushstring "two") L)
     ((lua-rawseti 1 2) L)
     ((lua-pushstring "a") L)
     (⊦= LUA_TNUMBER ((lua-rawget 1) L))                           ; the key is replaced by the value
     (⊦= 2 ((lua-gettop) L))
     (⊦= 1 ((lua-tointeger -1) L))
     ((lua-pop 1) L)
     ((lua-pushstring "zz") L)
     (⊦= LUA_TNIL ((lua-rawget 1) L))                              ; __index not run
     ((lua-pop 1) L)
     (⊦= LUA_TSTRING ((lua-rawgeti 1 2) L))
     (⊦= "two" ((lua-tostring -1) L))
     (⊦= LUA_TNIL ((lua-rawgeti -2 99) L))
     ((lua-pop 2) L)
     (⊦= "__index ran" (condition-case ((lua-geti 1 99) L) (e (exn lua) (msg e))))
     ((lua-pushinteger 3) L)
     (⊦= "__newindex ran" (condition-case ((lua-seti 1 99) L) (e (exn lua) (msg e))))
     (⊦= 1 ((lua-gettop) L))
     ;; light userdata keys: a pointer, and NULL
     (let ((ptr ((lua-newuserdata 16) L)))
       (⊨ (pointer? ptr))
       ((lua-pop 1) L)
       ((lua-pushstring "by pointer") L)
       ((lua-rawsetp 1 ptr) L)
       ((lua-pushstring "by NULL") L)
       ((lua-rawsetp 1 #f) L)
       (⊦= 1 ((lua-gettop) L))
       (⊦= LUA_TSTRING ((lua-rawgetp 1 ptr) L))
       (⊦= "by pointer" ((lua-tostring -1) L))
       (⊦= LUA_TSTRING ((lua-rawgetp 1 #f) L))
       (⊦= "by NULL" ((lua-tostring -1) L))
       ((lua-pop 2) L)
       ;; the same key through rawget: a light userdata pushed from Scheme
       (%pushlightuserdata-raw (luaR-state L) ptr)                  ; S2's lua-pushlightuserdata
       (⊦= LUA_TSTRING ((lua-rawget 1) L))
       (⊦= "by pointer" ((lua-tostring -1) L))
       ((lua-pop 1) L)
       ;; the registry takes light userdata keys too (the C idiom for private registry slots)
       ((lua-pushstring "private") L)
       ((lua-rawsetp LUA_REGISTRYINDEX ptr) L)
       (⊦= LUA_TSTRING ((lua-rawgetp LUA_REGISTRYINDEX ptr) L))
       (⊦= "private" ((lua-tostring -1) L))
       ((lua-pop 1) L)
       ((lua-pushnil) L)
       ((lua-rawsetp LUA_REGISTRYINDEX ptr) L)
       (⊦= LUA_TNIL ((lua-rawgetp LUA_REGISTRYINDEX ptr) L))
       ((lua-pop 1) L))
     (⊦= LUA_TTABLE ((lua-rawgeti LUA_REGISTRYINDEX LUA_RIDX_GLOBALS) L))
     ((lua-pop 1) L)
     ;; raw operations need a table: refused before Lua runs, nothing consumed
     ((lua-pushinteger 5) L)
     ((lua-pushstring "k") L)
     (let ((e (condition-case ((lua-rawget 2) L) (e (exn lua api) e))))
       (⊦= 'api (lua-condition-status e))
       (⊦= AUX_ETYPE (get-condition-property e 'lua 'code)))
     (⊦raises (exn lua api) ((lua-rawget -1) L))
     (⊦raises (exn lua api) ((lua-rawgeti 2 1) L))
     (⊦raises (exn lua api) ((lua-rawgetp 2 #f) L))
     (⊦raises (exn lua api) ((lua-rawseti 2 1) L))
     (⊦raises (exn lua api) ((lua-rawsetp 2 #f) L))
     (⊦raises (exn lua api) ((lua-rawset 2) L))
     (⊦raises (exn lua api) ((lua-rawgetp 1 'not-a-pointer) L))
     (⊦raises (exn lua api) ((lua-rawgeti 1 (expt 2 63)) L))
     (⊦= 3 ((lua-gettop) L))
     ((lua-settop 1) L)
     (⊦raises (exn lua api) ((lua-rawset 1) L))                    ; t alone: no key, no value
     (⊦= LUA_TNIL ((lua-rawget LUA_REGISTRYINDEX) L))              ; t is the key: registry[t] is nil
     (⊦= 1 ((lua-gettop) L))
     ((lua-settop 0) L)
     (⊦raises (exn lua api) ((lua-rawget LUA_REGISTRYINDEX) L))    ; no key at all is refused
     (⊦raises (exn lua api) ((lua-rawgeti 1 1) L))
     (⊦raises (exn lua api) ((lua-rawgeti (lua-upvalueindex 1) 1) L))
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)))

  ((test/s3-next _)
   (let ((L (fresh)))
     ;; every (key . value) of the table at idx, as Scheme values, through lua-next
     (define (pairs idx)
       ((lua-pushnil) L)
       (let loop ((acc '()) (i (if (and (< idx 0) (> idx LUA_REGISTRYINDEX)) (- idx 1) idx)))   ; the key is above t
         (if ((lua-next i) L)
           (let ((kv (cons (value-at L -2) (value-at L -1))))
             ((lua-pop 1) L)
             (loop (cons kv acc) i))
           acc)))
     (run! L "t = {10, 20, a = 'x', b = true, [2.5] = 'f'}")
     ((lua-getglobal "t") L)
     (let ((r (pairs 1)))
       (⊦= 5 (length r))
       (for-each (λ (kv) (⊨ (and (member kv r) #t)))
                 '((1 . 10) (2 . 20) ("a" . "x") ("b" . #t) (2.5 . "f"))))
     (⊦= 1 ((lua-gettop) L))                                        ; #f popped the key, pushed nothing
     ;; negative and pseudo indices
     ((lua-pushnil) L)
     (⊨ ((lua-next -2) L))
     (⊦= 3 ((lua-gettop) L))
     ((lua-settop 1) L)
     ((lua-newtable) L)
     (⊦= '() (pairs -1))
     (⊦= 2 ((lua-gettop) L))
     ((lua-pop 1) L)
     ;; assigning to existing fields while traversing is allowed
     ((lua-pushnil) L)
     (let loop ()
       (when ((lua-next 1) L)
         ((lua-pop 1) L)
         ((lua-pushvalue -1) L)
         ((lua-pushstring "changed") L)
         ((lua-settable 1) L)
         (loop)))
     (⊦= 5 (length (pairs 1)))
     (⊦= '() (filter (λ (kv) (not (equal? "changed" (cdr kv)))) (pairs 1)))
     ;; __pairs is not honoured, next is raw
     (run! L "p = setmetatable({only = 1}, {__pairs = function() error('no') end,
                                           __index = function() error('no') end})")
     ((lua-getglobal "p") L)
     (⊦= '(("only" . 1)) (pairs -1))
     ((lua-pop 1) L)
     ;; a key that is not in the table: Lua's error, the key consumed
     ((lua-pushstring "not a key") L)
     (let ((e (condition-case ((lua-next 1) L) (e (exn lua) e))))
       (⊦= "invalid key to 'next'" (msg e))
       (⊦= 'errrun (lua-condition-status e)))
     (⊦= 1 ((lua-gettop) L))
     ;; refusals
     ((lua-pushinteger 1) L)
     ((lua-pushnil) L)
     (⊦raises (exn lua api) ((lua-next 2) L))                      ; not a table
     (⊦= 3 ((lua-gettop) L))
     ((lua-settop 1) L)
     (⊦raises (exn lua api) ((lua-next 5) L))
     (⊦raises (exn lua api) ((lua-next (lua-upvalueindex 1)) L))
     (⊦= 1 ((lua-gettop) L))
     ;; the table as its own key (C semantics): not a key of itself
     ((lua-pushvalue 1) L)
     (⊦= "invalid key to 'next'" (condition-case ((lua-next 1) L) (e (exn lua) (msg e))))
     (⊦= 1 ((lua-gettop) L))
     ;; the registry can be traversed too
     ((lua-pushnil) L)
     (⊨ ((lua-next LUA_REGISTRYINDEX) L))
     (⊦= 3 ((lua-gettop) L))
     ((lua-settop 0) L)
     (lua-close L)))

  ((test/s3-constructors _)
   (let ((L (fresh)))
     ((lua-createtable 4 2) L)
     ((lua-newtable) L)
     (⊦= LUA_TTABLE ((lua-type 1) L))
     (⊦= LUA_TTABLE ((lua-type 2) L))
     (⊦= 2 ((lua-gettop) L))
     ((lua-setglobal "B") L)
     ((lua-setglobal "A") L)
     (run! L "A[1] = 1 A.x = 2 B[1] = 3 SAME = (A == B)")
     (⊦= #f (global L "SAME"))
     (⊦raises (exn lua api) ((lua-createtable -1 0) L))
     (⊦raises (exn lua api) ((lua-createtable 0 -1) L))
     (⊦raises (exn lua api) ((lua-createtable 4294967296 0) L))
     (⊦= "table overflow" (condition-case ((lua-createtable 0 2147483647) L) (e (exn lua) (msg e))))
     (⊦= 0 ((lua-gettop) L))
     ;; userdata: Lua-owned blocks with user values
     (let ((p ((lua-newuserdatauv 24 2) L)))
       (⊨ (pointer? p))
       (⊦= LUA_TUSERDATA ((lua-type -1) L))
       ((lua-setglobal "U") L)
       (run! L "P = string.format('%p', U) T = type(U)")
       (⊦= (lua-format "%p" p) (global L "P"))                     ; the address Lua reports for U
       (⊦= "userdata" (global L "T"))
       ;; the block is writable over its whole size
       (let ((u8-set! (eval '(begin (import (only (chicken memory) pointer-u8-set!)) pointer-u8-set!)))
             (u8-ref (eval '(begin (import (only (chicken memory) pointer-u8-ref)) pointer-u8-ref)))
             (off (eval '(begin (import (only (chicken memory) pointer+)) pointer+))))
         (for-each (λ (i) (u8-set! (off p i) (* 3 i))) (iota 24))
         (⊦= (map (λ (i) (* 3 i)) (iota 24)) (map (λ (i) (u8-ref (off p i))) (iota 24)))))
     (let ((p ((lua-newuserdatauv 0 0) L)))                         ; empty block, no user values
       (⊨ (pointer? p))
       (⊦= LUA_TNONE ((lua-getiuservalue -1 1) L))
       (⊦= LUA_TNIL ((lua-type -1) L))                              ; nil pushed for a missing value
       ((lua-pop 2) L))
     (let ((p ((lua-newuserdata 8) L)) (q ((lua-newuserdata 8) L)))
       (⊦≠ (lua-format "%p" p) (lua-format "%p" q))
       ((lua-pop 2) L))
     ((lua-newuserdatauv 8 32766) L)                                ; the largest count lua.h accepts
     ((lua-pop 1) L)
     (⊦raises (exn lua api) ((lua-newuserdatauv 8 32767) L))
     (⊦raises (exn lua api) ((lua-newuserdatauv 8 -1) L))
     (⊦raises (exn lua api) ((lua-newuserdatauv -1 1) L))
     (⊦raises (exn lua api) ((lua-newuserdatauv (expt 2 64) 1) L))
     (⊦raises (exn lua api) ((lua-newuserdatauv 1.5 1) L))
     (⊦= "memory allocation error: block too big"
         (condition-case ((lua-newuserdatauv (expt 2 63) 0) L) (e (exn lua) (msg e))))
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)))

  ((test/s3-uservalues _)
   (let ((L (fresh)))
     ((lua-newuserdatauv 8 2) L)
     ((lua-pushstring "first") L)
     (⊨ ((lua-setiuservalue -2 1) L))
     ((lua-newtable) L)
     (⊨ ((lua-setiuservalue 1 2) L))
     ((lua-pushinteger 3) L)
     (⊭ ((lua-setiuservalue 1 3) L))                                 ; no third value: popped anyway
     ((lua-pushinteger 0) L)
     (⊭ ((lua-setiuservalue 1 0) L))
     (⊦= 1 ((lua-gettop) L))
     (⊦= LUA_TSTRING ((lua-getiuservalue 1 1) L))
     (⊦= "first" ((lua-tostring -1) L))
     (⊦= LUA_TTABLE ((lua-getiuservalue 1 2) L))
     (⊦= LUA_TNONE ((lua-getiuservalue 1 3) L))
     (⊦= LUA_TNONE ((lua-getiuservalue 1 -1) L))
     (⊦= 5 ((lua-gettop) L))
     ((lua-settop 1) L)
     ((lua-setglobal "U") L)
     (run! L "A, B = debug.getuservalue(U, 1) C = debug.getuservalue(U, 2) D, E = debug.getuservalue(U, 3)")
     (⊦= "first" (global L "A"))
     (⊦= #t (global L "B"))
     (⊦= 'table (global L "C"))
     (⊦= 'nil (global L "E"))                                      ; no third value
     ;; the one-value macros
     ((lua-newuserdata 4) L)
     (⊦= LUA_TNIL ((lua-getuservalue 1) L))
     ((lua-pop 1) L)
     ((lua-pushstring "uv") L)
     (⊨ ((lua-setuservalue 1) L))
     (⊦= LUA_TSTRING ((lua-getuservalue -1) L))
     (⊦= "uv" ((lua-tostring -1) L))
     ((lua-settop 1) L)
     ;; only full userdata: tables, light userdata, bad indices are refused, nothing popped
     ((lua-newtable) L)
     (%pushlightuserdata-raw (luaR-state L) #f)
     ((lua-pushinteger 1) L)
     (⊦raises (exn lua api) ((lua-getiuservalue 2 1) L))
     (⊦raises (exn lua api) ((lua-getiuservalue 3 1) L))
     (⊦raises (exn lua api) ((lua-setiuservalue 2 1) L))
     (⊦raises (exn lua api) ((lua-setuservalue 3) L))
     (⊦raises (exn lua api) ((lua-getuservalue 9) L))
     (⊦raises (exn lua api) ((lua-getiuservalue 1 4294967296) L))
     (⊦= 4 ((lua-gettop) L))
     ((lua-settop 1) L)
     (⊨ ((lua-setuservalue 1) L))                                   ; its own user value (C semantics)
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)))

  ((test/s3-metatables _)
   (let ((L (fresh)))
     ((lua-newtable) L)                                             ; 1: the object
     ((lua-newtable) L)                                             ; 2: its metatable
     ((lua-pushstring "M") L)
     ((lua-setfield -2 "__name") L)
     ((lua-pushboolean #f) L)
     ((lua-setfield -2 "__metatable") L)                            ; hides it from Lua, not from C
     ((lua-setmetatable 1) L)
     (⊦= 1 ((lua-gettop) L))
     (⊨ ((lua-getmetatable 1) L))
     (⊦= 2 ((lua-gettop) L))
     (⊦= "M" (begin ((lua-getfield -1 "__name") L) (begin1 ((lua-tostring -1) L) ((lua-pop 2) L))))
     ((lua-pushvalue 1) L)
     ((lua-setglobal "OBJ") L)
     (run! L "HIDDEN = getmetatable(OBJ)")
     (⊦= #f (global L "HIDDEN"))
     ;; nil removes it
     ((lua-pushnil) L)
     ((lua-setmetatable -2) L)
     (⊭ ((lua-getmetatable 1) L))
     (⊦= 1 ((lua-gettop) L))                                        ; #f: nothing pushed
     ;; the top must be a table or nil: refused, not popped
     ((lua-pushinteger 3) L)
     (let ((e (condition-case ((lua-setmetatable 1) L) (e (exn lua api) e))))
       (⊦= AUX_ETYPE (get-condition-property e 'lua 'code)))
     (⊦= 2 ((lua-gettop) L))
     ((lua-settop 1) L)
     (⊦raises (exn lua api) ((lua-setmetatable 5) L))
     (⊦raises (exn lua api) ((lua-setmetatable 0) L))
     (⊦raises (exn lua api) ((lua-setmetatable (lua-upvalueindex 1)) L))
     ;; a table as its own metatable
     ((lua-pushvalue 1) L)
     ((lua-setmetatable 1) L)
     (⊨ ((lua-getmetatable 1) L))
     ((lua-pop 1) L)
     ;; getmetatable: a positive index above the top is none (#f); an invalid one is refused
     (⊭ ((lua-getmetatable 7) L))
     (⊦raises (exn lua api) ((lua-getmetatable 0) L))
     (⊦raises (exn lua api) ((lua-getmetatable -3) L))
     (⊦raises (exn lua api) ((lua-getmetatable (lua-upvalueindex 1)) L))
     (⊭ ((lua-getmetatable LUA_REGISTRYINDEX) L))
     ;; strings share the string library's metatable
     ((lua-pushstring "s") L)
     (⊨ ((lua-getmetatable -1) L))
     (⊦= LUA_TTABLE ((lua-getfield -1 "__index") L))
     ((lua-settop 1) L)
     ;; a metatable with __gc, set from Scheme, registers the finalizer
     (run! L "GCED = 0")
     ((lua-newtable) L)
     ((lua-newtable) L)
     (run! L "function FIN() GCED = GCED + 1 end")
     ((lua-getglobal "FIN") L)
     ((lua-setfield -2 "__gc") L)
     ((lua-setmetatable -2) L)
     ((lua-pop 1) L)
     (run! L "collectgarbage() collectgarbage()")
     (⊦= 1 (global L "GCED"))
     ;; for other types, the metatable of the whole type (C semantics)
     ((lua-pushinteger 5) L)
     (run! L "NMT = {__index = function(n, k) return k .. n end}")
     ((lua-getglobal "NMT") L)
     ((lua-setmetatable -2) L)
     (⊦= 2 ((lua-gettop) L))
     (run! L "N = (7).x")
     (⊦= "x7" (global L "N"))
     ((lua-pushnil) L)
     ((lua-setmetatable 2) L)
     (⊨ (string-suffix? "attempt to index a number value" (condition-case (run! L "N = (7).x") (e (exn lua) (msg e)))))
     ;; userdata
     ((lua-settop 1) L)
     ((lua-newuserdata 8) L)
     (run! L "UMT = {__index = function(u, k) return 'u.' .. k end}")
     ((lua-getglobal "UMT") L)
     ((lua-setmetatable -2) L)
     ((lua-setglobal "UD") L)
     (run! L "R = UD.field")
     (⊦= "u.field" (global L "R"))
     (⊦= 1 ((lua-gettop) L))
     ((lua-pop 1) L)
     (lua-close L)))

  ;; The metatables the module owns cannot be changed through the bound API (Lua code cannot either,
  ;; without the debug library: getmetatable of a handle is false, setmetatable takes tables only):
  ;; - a Scheme handle's (a lua-pushobject object, a COND handle, a closure's upvalue 1): its __gc
  ;;   releases the Scheme object, so another one leaked it until lua-close, and FILE*'s made liolib
  ;;   take the handle's bytes for an LStream and call a garbage closef (SIGSEGV);
  ;; - the registry's: a __mode let the collector clear registry[LUA_RIDX_GLOBALS], which Lua's own
  ;;   load / lua_getglobal then read as garbage (removing one with nil is allowed);
  ;; - the handle metatable itself is given to nothing else, nor stored in the registry under a
  ;;   string key (luaL-setmetatable of that name would give it; one Lua code stored there with the
  ;;   debug library is refused by luaL-setmetatable itself).
  ;; Refused with (exn lua api) code AUX_EMETATABLE, nothing consumed; luaL-setmetatable too, whether
  ;; its stub or its op (where registry.tname is known) refuses.
  ((test/s3-owned-metatables _)
   (let* ((L (fresh)) (base (lua-live-handles L)))
     (define (code-of thunk)
       (condition-case (begin (thunk) 'no-raise) (e (exn lua api) (get-condition-property e 'lua 'code))))
     ((lua-pushobject (vector 1 2 3)) L)                                ; 1: an object handle
     ((lua-pushcfunction nop-callback) L)                               ; 2: a Scheme closure
     (⊦= "" ((lua-getupvalue 2 1) L))                                   ; 3: its handle, upvalue 1
     ((lua-register "raiser" (λ (C) (error "cond"))) L)
     (run! L "local ok, e = pcall(raiser) COND = e")
     ((lua-getglobal "COND") L)                                         ; 4: a COND handle
     (⊦= LUA_TUSERDATA ((lua-type 4) L))
     (for-each
       (λ (idx)
         ((lua-newtable) L)
         (⊦= AUX_EMETATABLE (code-of (λ () ((lua-setmetatable idx) L))))
         (⊦= 5 ((lua-gettop) L))                                        ; not consumed
         ((lua-pop 1) L)
         ((lua-pushnil) L)
         (⊦= AUX_EMETATABLE (code-of (λ () ((lua-setmetatable idx) L))))
         ((lua-pop 1) L)
         ((luaL-getmetatable "FILE*") L)
         (⊦= AUX_EMETATABLE (code-of (λ () ((lua-setmetatable idx) L))))
         ((lua-pop 1) L)
         ((lua-pushvalue idx) L)
         (⊦= AUX_EMETATABLE (code-of (λ () ((luaL-setmetatable "FILE*") L))))
         ((lua-pop 1) L))
       '(1 3 4))
     (⊦= 4 ((lua-gettop) L))
     ;; the forged FILE* of the review: io.close refuses the handle
     ((lua-pushvalue 1) L)
     ((lua-setglobal "H") L)
     (run! L "local ok, e = pcall(io.close, H) CLOSE = e")
     (⊨ (string-suffix? "(FILE* expected, got aux.scheme)" (global L "CLOSE")))
     ;; the handle metatable given to another value
     (⊨ ((lua-getmetatable 1) L))                                        ; 5: raw, as C
     ((lua-newtable) L)
     ((lua-pushvalue 5) L)
     (⊦= AUX_EMETATABLE (code-of (λ () ((lua-setmetatable -2) L))))
     ((lua-settop 5) L)
     ((lua-newuserdatauv 16 0) L)
     ((lua-pushvalue 5) L)
     (⊦= AUX_EMETATABLE (code-of (λ () ((lua-setmetatable -2) L))))
     ((lua-settop 5) L)
     ;; nor stored in the registry under a name, which luaL-setmetatable (luaL-checkudata, ...) reads
     (⊦= AUX_EMETATABLE (code-of (λ () ((lua-setfield LUA_REGISTRYINDEX "HMT") L))))
     (⊦= 5 ((lua-gettop) L))                                            ; not consumed
     ((lua-pushstring "HMT") L)
     ((lua-pushvalue 5) L)
     (⊦= AUX_EMETATABLE (code-of (λ () ((lua-rawset LUA_REGISTRYINDEX) L))))
     (⊦= AUX_EMETATABLE (code-of (λ () ((lua-settable LUA_REGISTRYINDEX) L))))
     ((lua-pushvalue LUA_REGISTRYINDEX) L) ((lua-insert -3) L)          ; a copy of the registry
     (⊦= AUX_EMETATABLE (code-of (λ () ((lua-rawset -3) L))))
     (⊦= 8 ((lua-gettop) L))
     ((lua-settop 5) L)
     ((lua-pushglobaltable) L)                                           ; 6: the globals
     ((lua-pushvalue LUA_REGISTRYINDEX) L)                               ; the globals being the registry
     ((lua-rawseti LUA_REGISTRYINDEX LUA_RIDX_GLOBALS) L)
     ((lua-pushglobaltable) L) (⊨ ((lua-rawequal -1 LUA_REGISTRYINDEX) L)) ((lua-pop 1) L)
     ((lua-pushvalue 5) L)
     (⊦= AUX_EMETATABLE (code-of (λ () ((lua-setglobal "HMT") L))))
     (⊦= 7 ((lua-gettop) L))
     ((lua-pushvalue 6) L)                                               ; restored
     ((lua-rawseti LUA_REGISTRYINDEX LUA_RIDX_GLOBALS) L)
     ((lua-settop 5) L)
     (⊦= LUA_TNIL ((lua-getfield LUA_REGISTRYINDEX "HMT") L))
     ((lua-settop 5) L)
     ;; Lua code with the debug library can store it: luaL-setmetatable of that name is refused all
     ;; the same, in its op, with the same condition
     (run! L "debug.getregistry().HMT = debug.getmetatable(H)")
     ((lua-newtable) L)
     (⊦= AUX_EMETATABLE (code-of (λ () ((luaL-setmetatable "HMT") L))))
     (⊭ ((lua-getmetatable -1) L))
     ((lua-settop 4) L)
     (run! L "debug.getregistry().HMT = nil")
     ;; the registry, by its pseudo-index or a copy
     ((lua-newtable) L)
     ((lua-pushstring "v") L)
     ((lua-setfield -2 "__mode") L)
     (⊦= AUX_EMETATABLE (code-of (λ () ((lua-setmetatable LUA_REGISTRYINDEX) L))))
     (⊦= 5 ((lua-gettop) L))
     ((lua-pushvalue LUA_REGISTRYINDEX) L)                                ; 6: a copy of the registry
     ((lua-pushvalue 5) L)                                                ; 7: the weak table
     (⊦= AUX_EMETATABLE (code-of (λ () ((lua-setmetatable -2) L))))
     ((lua-settop 4) L)
     ((lua-pushvalue LUA_REGISTRYINDEX) L)
     (⊦= AUX_EMETATABLE (code-of (λ () ((luaL-setmetatable "FILE*") L))))
     (⊦= LUA_TNIL ((luaL-getmetatable "NOSUCH") L)) ((lua-pop 1) L)
     ((luaL-setmetatable "NOSUCH") L)                                     ; nil, as in C: allowed
     (⊭ ((lua-getmetatable -1) L))
     ((lua-pop 1) L)
     ((lua-pushnil) L)
     ((lua-setmetatable LUA_REGISTRYINDEX) L)                              ; nil: allowed
     (⊭ ((lua-getmetatable LUA_REGISTRYINDEX) L))
     ((lua-gc 'collect) L) ((lua-gc 'collect) L)
     (⊦= LUA_TTABLE ((lua-rawgeti LUA_REGISTRYINDEX LUA_RIDX_GLOBALS) L))   ; the globals survive
     ((lua-pop 1) L)
     ;; other userdata are unaffected, the handle-sized ones too
     ((lua-newuserdatauv 16 0) L)
     (⊦= #t ((luaL-newmetatable "Plain") L))
     ((lua-setmetatable -2) L)
     (⊨ (pointer? ((luaL-testudata -1 "Plain") L)))
     ((lua-pop 1) L)
     ;; nothing leaked: every handle is released once Lua drops it
     ((lua-settop 0) L)
     (run! L "H = nil COND = nil")
     (gc #t)
     ((lua-gc 'collect) L) ((lua-gc 'collect) L)
     (⊦= (+ base 1) (lua-live-handles L))                                ; raiser
     (lua-close L)))

  ;; Lua's own libraries trust registry entries they never check: a full userdata whose metatable
  ;; is registry["FILE*"] is read as liolib's LStream (io.close calls its closef, f:write reads its
  ;; FILE*), one with registry["_UBOX*"] as lauxlib's buffer box (its block is realloc'd), and a
  ;; userdata at registry._CLIBS[path] as a dlopen handle (package.loadlib and require call dlsym on
  ;; it).  Through the bound API a plain lua-newuserdatauv block (any size, uninitialized) took
  ;; FILE*'s metatable, or a userdata went into _CLIBS, or the handle metatable became
  ;; registry["FILE*"], and plain Lua code then crashed the process (SIGSEGV, turned by CHICKEN into
  ;; a condition that unwound through Lua's C frames).  Refused with (exn lua api), nothing consumed:
  ;; AUX_EMETATABLE for those two metatables given to a full or light userdata that does not have
  ;; it already, or any metatable given to _CLIBS; AUX_ELIBRARY for a change of registry["FILE*"],
  ;; ["_UBOX*"] or ["_CLIBS"] (removing one is allowed, and luaL-newmetatable's fresh table) and for a
  ;; userdata stored into _CLIBS under a string key, by lua-setfield, lua-setglobal, lua-settable or
  ;; lua-rawset.  The checks read those entries through a private table of key strings: replaced
  ;; (Lua code with debug.getregistry), it is made anew by the next binding that needs it.
  ((test/s3-library-entries _)
   (let ((L (fresh)))
     (define (code-of thunk)
       (condition-case (begin (thunk) 'no-raise) (e (exn lua api) (get-condition-property e 'lua 'code))))
     (define (refused code thunk)
       (let ((top ((lua-gettop) L))) (⊦= code (code-of thunk)) (⊦= top ((lua-gettop) L))))
     (define (wipe-keys!)                                               ; the private key table replaced
       (run! L "local r, n = debug.getregistry(), 0
                for k, v in next, r do
                  if type(v) == 'table' and rawget(v, 1) == 'FILE*' and rawget(v, 3) == '_CLIBS' then r[k] = 42; n = n + 1 end
                end
                assert(n == 1)"))
     ;; a buffer that outgrows luaL_Buffer's own space makes lauxlib's box metatable
     (run! L "local t = {} for i = 1, 200 do t[i] = ('x'):rep(100) end BIG = table.concat(t)")
     (⊦= LUA_TTABLE ((luaL-getmetatable "_UBOX*") L))                   ; 1
     (⊦= LUA_TTABLE ((luaL-getmetatable "FILE*") L))                    ; 2
     (⊦= LUA_TTABLE ((lua-getfield LUA_REGISTRYINDEX "_CLIBS") L))      ; 3
     ;; FILE*'s and _UBOX*'s metatables on a block the bindings made, whatever its size
     (for-each
       (λ (size)
         ((lua-newuserdatauv size 0) L)                                  ; 4
         (refused AUX_EMETATABLE (λ () ((luaL-setmetatable "FILE*") L)))
         (refused AUX_EMETATABLE (λ () ((luaL-setmetatable "_UBOX*") L)))
         ((lua-pushvalue 2) L) (refused AUX_EMETATABLE (λ () ((lua-setmetatable 4) L))) ((lua-pop 1) L)
         ((lua-pushvalue 1) L) (refused AUX_EMETATABLE (λ () ((lua-setmetatable -2) L))) ((lua-pop 1) L)
         ;; one with another metatable already: the same
         ((lua-newtable) L) ((lua-setmetatable 4) L)
         (refused AUX_EMETATABLE (λ () ((luaL-setmetatable "FILE*") L)))
         (⊨ ((lua-getmetatable 4) L)) (⊭ ((lua-rawequal -1 2) L)) ((lua-pop 1) L)
         ((lua-setglobal "U") L)
         (run! L "T = tostring(io.type(U)) OK, E = pcall(io.close, U)")
         (⊦= "nil" (global L "T"))
         (⊦= #f (global L "OK"))
         (⊨ (string-suffix? "(FILE* expected, got userdata)" (global L "E")))
         (⊦= 3 ((lua-gettop) L)))
       '(0 8 16 64))
     ;; a light userdata's metatable is its type's: then any light userdata would pass for a file
     ((lua-newuserdatauv 8 0) L)
     ((lua-pushlightuserdata ((lua-touserdata 4) L)) L)                 ; 5
     ((lua-pushvalue 2) L) (refused AUX_EMETATABLE (λ () ((lua-setmetatable 5) L))) ((lua-pop 1) L)
     (refused AUX_EMETATABLE (λ () ((luaL-setmetatable "FILE*") L)))
     (⊭ ((lua-getmetatable 5) L))
     ((lua-settop 3) L)
     ;; a real file keeps its own; a table may have FILE*'s (liolib reads userdata only)
     ((lua-getglobal "io") L) ((lua-getfield -1 "stdout") L) ((lua-remove -2) L)   ; 4: io.stdout
     ((luaL-setmetatable "FILE*") L)
     ((lua-pushvalue 2) L) ((lua-setmetatable 4) L)
     (run! L "T = io.type(io.stdout)")
     (⊦= "file" (global L "T"))
     ((lua-newtable) L) ((lua-pushvalue 2) L) ((lua-setmetatable -2) L)
     (⊨ ((lua-getmetatable -1) L)) (⊨ ((lua-rawequal -1 2) L))
     ((lua-pop 1) L) ((lua-pushnil) L) ((lua-setmetatable -2) L)       ; else its __gc (f_gc) warns
     ((lua-settop 3) L)
     ;; _CLIBS takes no userdata under a string key
     ((lua-newuserdatauv 64 0) L)                                        ; 4
     (refused AUX_ELIBRARY (λ () ((lua-setfield 3 "/fake/lib.so") L)))
     ((lua-pushstring "/fake/lib.so") L) ((lua-pushvalue 4) L)
     (refused AUX_ELIBRARY (λ () ((lua-rawset 3) L)))
     (refused AUX_ELIBRARY (λ () ((lua-settable 3) L)))
     ((lua-settop 4) L)
     ((lua-pushlightuserdata ((lua-touserdata 4) L)) L)
     (refused AUX_ELIBRARY (λ () ((lua-setfield 3 "/fake/lib.so") L)))
     ((lua-pop 1) L)
     ((lua-pushglobaltable) L)                                           ; 5: the globals
     ((lua-pushvalue 3) L) ((lua-rawseti LUA_REGISTRYINDEX LUA_RIDX_GLOBALS) L)   ; the globals being _CLIBS
     ((lua-pushvalue 4) L)
     (refused AUX_ELIBRARY (λ () ((lua-setglobal "/fake/lib.so") L)))
     ((lua-pop 1) L)
     ((lua-pushvalue 5) L) ((lua-rawseti LUA_REGISTRYINDEX LUA_RIDX_GLOBALS) L)   ; restored
     ((lua-settop 4) L)
     ;; nor a metatable (checkclib's lua_getfield would run its __index)
     ((lua-newtable) L) (refused AUX_EMETATABLE (λ () ((lua-setmetatable 3) L))) ((lua-pop 1) L)
     ((lua-pushvalue 3) L) (refused AUX_EMETATABLE (λ () ((luaL-setmetatable "_UBOX*") L))) ((lua-pop 1) L)
     ((lua-pushnil) L) ((lua-setmetatable 3) L)                          ; nil: allowed
     (⊭ ((lua-getmetatable 3) L))
     ;; other values, other keys, other tables are free; package.loadlib then fails cleanly
     ((lua-pushstring "not a handle") L) ((lua-setfield 3 "/fake/lib.so") L)
     ((lua-pushnil) L) ((lua-setfield 3 "/fake/lib.so") L)
     ((lua-pushvalue 4) L) ((lua-rawseti 3 1000) L)
     ((lua-pushnil) L) ((lua-rawseti 3 1000) L)
     ((lua-newtable) L) ((lua-pushvalue 4) L) ((lua-setfield -2 "/fake/lib.so") L) ((lua-pop 1) L)
     ((lua-pushvalue 4) L) ((lua-setglobal "UD") L)
     (run! L "local f, e, w = package.loadlib('/fake/lib.so', 'luaopen_x') W = w")
     (⊦= "open" (global L "W"))
     (⊦= 4 ((lua-gettop) L))
     ;; the three registry entries: removed or kept, never changed
     (for-each
       (λ (name)
         ((lua-newtable) L)
         (refused AUX_ELIBRARY (λ () ((lua-setfield LUA_REGISTRYINDEX name) L)))
         ((lua-pushstring name) L) ((lua-insert -2) L)                   ; k v
         (refused AUX_ELIBRARY (λ () ((lua-rawset LUA_REGISTRYINDEX) L)))
         (refused AUX_ELIBRARY (λ () ((lua-settable LUA_REGISTRYINDEX) L)))
         ((lua-pushvalue LUA_REGISTRYINDEX) L) ((lua-insert -3) L)       ; a copy of the registry
         (refused AUX_ELIBRARY (λ () ((lua-rawset -3) L)))
         ((lua-settop 4) L)
         ((lua-pushvalue 4) L)                                           ; a userdata: the same
         (refused AUX_ELIBRARY (λ () ((lua-setfield LUA_REGISTRYINDEX name) L)))
         ((lua-pop 1) L)
         ((lua-getfield LUA_REGISTRYINDEX name) L)                       ; the same value again: allowed
         ((lua-setfield LUA_REGISTRYINDEX name) L)
         (⊦= 4 ((lua-gettop) L)))
       '("FILE*" "_UBOX*" "_CLIBS"))
     ((lua-pushglobaltable) L)                                           ; 5
     ((lua-pushvalue LUA_REGISTRYINDEX) L) ((lua-rawseti LUA_REGISTRYINDEX LUA_RIDX_GLOBALS) L)
     ((lua-newtable) L)
     (refused AUX_ELIBRARY (λ () ((lua-setglobal "FILE*") L)))
     ((lua-pop 1) L)
     ((lua-pushvalue 5) L) ((lua-rawseti LUA_REGISTRYINDEX LUA_RIDX_GLOBALS) L)
     ((lua-settop 4) L)
     ((lua-newtable) L) ((lua-setfield LUA_REGISTRYINDEX "FILE") L)     ; other names are free
     ((lua-pushnil) L) ((lua-setfield LUA_REGISTRYINDEX "FILE") L)
     ((luaL-getmetatable "FILE*") L) (⊨ ((lua-rawequal 2 -1) L)) ((lua-pop 1) L)
     ;; inside a callback: the same refusals
     ((register! "forge" (λ (C)
                           ((lua-newuserdatauv 16 0) C)
                           (let ((c (condition-case (begin ((luaL-setmetatable "FILE*") C) 'no-raise)
                                      (e (exn lua api) (get-condition-property e 'lua 'code)))))
                             ((lua-pushboolean (equal? c AUX_EMETATABLE)) C) 1))) L)
     (run! L "R = forge()")
     (⊦= #t (global L "R"))
     ;; the private key table replaced: made anew, the checks unchanged
     (wipe-keys!)
     ((lua-newuserdatauv 8 0) L) ((lua-newtable) L) ((lua-setmetatable -2) L)   ; a plain metatable: set
     (⊨ ((lua-getmetatable -1) L))
     ((lua-settop 4) L)
     (wipe-keys!)
     ((lua-newuserdatauv 8 0) L) ((lua-pushvalue 2) L)
     (refused AUX_EMETATABLE (λ () ((lua-setmetatable -2) L)))
     ((lua-settop 4) L)
     (wipe-keys!)
     ((lua-newuserdatauv 8 0) L)
     (refused AUX_EMETATABLE (λ () ((luaL-setmetatable "FILE*") L)))
     ((lua-settop 4) L)
     (wipe-keys!)
     ((lua-pushvalue 4) L)
     (refused AUX_ELIBRARY (λ () ((lua-setfield 3 "/fake/lib.so") L)))
     ((lua-settop 4) L)
     (wipe-keys!)
     ((lua-pushstring "_UBOX*") L) ((lua-newtable) L)
     (refused AUX_ELIBRARY (λ () ((lua-settable LUA_REGISTRYINDEX) L)))
     ((lua-settop 4) L)
     (wipe-keys!)
     ((lua-pushvalue 4) L) ((lua-setglobal "UD2") L)                     ; a userdata elsewhere: free
     (run! L "OK = UD2 ~= nil")
     (⊦= #t (global L "OK"))
     (wipe-keys!)                                                        ; finds exactly one again
     (⊦= 4 ((lua-gettop) L))
     (lua-close L))
   ;; removed, then made fresh by luaL-newmetatable: no userdata has that table, and none gets it
   (let ((L (fresh)))
     (define (code-of thunk)
       (condition-case (begin (thunk) 'no-raise) (e (exn lua api) (get-condition-property e 'lua 'code))))
     ;; the standard files keep the old table, which liolib no longer recognizes: their __gc would
     ;; warn "FILE* expected, got FILE*"
     (run! L "warn('@off')")
     ((lua-pushnil) L) ((lua-setfield LUA_REGISTRYINDEX "FILE*") L)
     (run! L "T = tostring(io.type(io.stdout))")
     (⊦= "nil" (global L "T"))
     (⊦= #t ((luaL-newmetatable "FILE*") L))
     ((lua-newuserdatauv 16 0) L)
     (⊦= AUX_EMETATABLE (code-of (λ () ((luaL-setmetatable "FILE*") L))))
     ((lua-pushvalue 1) L)
     (⊦= AUX_EMETATABLE (code-of (λ () ((lua-setmetatable 2) L))))
     (⊦= 3 ((lua-gettop) L))
     (lua-close L)))

  ((test/s3-globals-register _)
   (let ((L (fresh)))
     ((lua-pushglobaltable) L)
     (⊦= LUA_TTABLE ((lua-type -1) L))
     ((lua-pushinteger 11) L)
     ((lua-setfield -2 "G") L)
     (⊦= 11 (global L "G"))
     (⊦= LUA_TFUNCTION ((lua-getfield 1 "print") L))
     ((lua-pop 1) L)
     ;; the global table is the registry's RIDX_GLOBALS slot, and _G
     ((lua-rawgeti LUA_REGISTRYINDEX LUA_RIDX_GLOBALS) L)
     ((lua-pushstring "same") L)
     ((lua-setfield -2 "SAME") L)
     (⊦= "same" (begin ((lua-getfield 1 "SAME") L) (begin1 ((lua-tostring -1) L) ((lua-pop 1) L))))
     (run! L "IS_G = (_G.SAME == 'same')")
     (⊦= #t (global L "IS_G"))
     ((lua-settop 0) L)
     ;; register: a Scheme procedure as a global function
     ((lua-register "twice" (λ (C) ((lua-pushinteger (* 2 ((lua-tointeger 1) C))) C) 1)) L)
     (⊦= 0 ((lua-gettop) L))
     (run! L "R = twice(21)")
     (⊦= 42 (global L "R"))
     ((lua-register "we\x0;ird" (λ (C) ((lua-pushstring "nul name") C) 1)) L)
     (run! L "R = _G['we\\0ird']()")
     (⊦= "nul name" (global L "R"))
     ;; re-registering replaces the global
     ((lua-register "twice" (λ (C) ((lua-pushinteger (* 20 ((lua-tointeger 1) C))) C) 1)) L)
     (run! L "R = twice(2)")
     (⊦= 40 (global L "R"))
     (⊦raises (exn lua api) ((lua-register 'name (λ (C) 0)) L))     ; nothing pushed
     (⊦raises (exn lua api) ((lua-register "x" 42) L))
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)))

  ;; registry[LUA_RIDX_GLOBALS] is read by lua_load and lua_setglobal through getGlobalTable, whose
  ;; api_check aborts an API-checked Lua on a non-table: no write binding stores one there (through
  ;; the pseudo-index or a copy of the registry, integer or integral float key); nothing is
  ;; consumed.  A table is accepted, and becomes the globals of the chunks loaded next.
  ((test/s3-registry-globals-writes _)
   (let ((L (fresh)))
     (define (code-of thunk)
       (condition-case (begin (thunk) 'no-raise)
         (e (exn lua api) (list (get-condition-property e 'lua 'code) ((condition-property-accessor 'exn 'location) e)))))
     (define (refused who thunk) (⊦= (list AUX_EGLOBALS who) (code-of thunk)))
     (define G LUA_RIDX_GLOBALS)
     (for-each
       (λ (push)
         (push)
         (refused 'lua-rawseti (λ () ((lua-rawseti LUA_REGISTRYINDEX G) L)))
         (refused 'lua-seti (λ () ((lua-seti LUA_REGISTRYINDEX G) L)))
         ((lua-pushinteger G) L) ((lua-rotate -2 1) L)                  ; k v
         (refused 'lua-rawset (λ () ((lua-rawset LUA_REGISTRYINDEX) L)))
         (refused 'lua-settable (λ () ((lua-settable LUA_REGISTRYINDEX) L)))
         ((lua-pushnumber (exact->inexact G)) L) ((lua-replace -3) L)   ; the key 2.0 is the key 2
         (refused 'lua-rawset (λ () ((lua-rawset LUA_REGISTRYINDEX) L)))
         (refused 'lua-settable (λ () ((lua-settable LUA_REGISTRYINDEX) L)))
         ((lua-pushvalue LUA_REGISTRYINDEX) L) ((lua-insert 1) L)       ; a copy of the registry at 1
         (refused 'lua-rawset (λ () ((lua-rawset 1) L)))
         (refused 'lua-settable (λ () ((lua-settable 1) L)))
         ((lua-remove 2) L)                                             ; reg v
         (refused 'lua-rawseti (λ () ((lua-rawseti 1 G) L)))
         (refused 'lua-seti (λ () ((lua-seti -2 G) L)))
         (⊦= 2 ((lua-gettop) L))                                        ; nothing consumed
         ((lua-settop 0) L))
       (list (λ () ((lua-pushinteger 5) L)) (λ () ((lua-pushnil) L)) (λ () ((lua-pushstring "t") L))
             (λ () ((lua-pushboolean #f) L)) (λ () ((lua-pushcfunction (λ (C) 0)) L))))
     ;; inside a callback: the same refusal
     ((register! "f" (λ (C) ((lua-pushinteger 5) C)
                        (let ((c (code-of (λ () ((lua-rawseti LUA_REGISTRYINDEX LUA_RIDX_GLOBALS) C)))))
                          ((lua-pushboolean (equal? c (list AUX_EGLOBALS 'lua-rawseti))) C) 1))) L)
     (run! L "R = f()")
     (⊦= #t (global L "R"))
     (run! L "OK = type(_G) == 'table'")                                ; the globals survived
     (⊦= #t (global L "OK"))
     ;; other keys of the registry, and other tables, are free
     ((lua-pushinteger 5) L) ((lua-setfield LUA_REGISTRYINDEX "2") L)
     ((lua-pushinteger 5) L) ((lua-rawseti LUA_REGISTRYINDEX 1000) L)
     ((lua-newtable) L) ((lua-pushinteger 5) L) ((lua-rawseti -2 G) L) ((lua-pushinteger 6) L) ((lua-seti -2 G) L)
     ((lua-pop 1) L)
     ;; LUA_RIDX_MAINTHREAD is not guarded: its one reader (coroutine.close) never trusts it
     ((lua-pushinteger 5) L) ((lua-rawseti LUA_REGISTRYINDEX LUA_RIDX_MAINTHREAD) L)
     (run! L "local ok, e = pcall(coroutine.close); E = e")
     (⊨ (string-suffix? "cannot close main thread" (global L "E")))
     ((lua-pushthread) L) ((lua-rawseti LUA_REGISTRYINDEX LUA_RIDX_MAINTHREAD) L)   ; restored
     ;; a table is accepted: the globals of the next loads
     ((lua-rawgeti LUA_REGISTRYINDEX G) L)                             ; 1: the original globals
     ((lua-newtable) L) ((lua-pushinteger 41) L) ((lua-setfield -2 "x") L)
     ((lua-rawseti LUA_REGISTRYINDEX G) L)
     (⊦= LUA_OK ((luaL-loadstring "return x + 1") L))
     ((lua-call 0 1) L)
     (⊦= 42 ((lua-tointeger -1) L))
     ((lua-pop 1) L)
     ((lua-pushvalue 1) L) ((lua-seti LUA_REGISTRYINDEX G) L)          ; restored
     ((lua-pop 1) L)
     (run! L "OK = print ~= nil")
     (⊦= #t (global L "OK"))
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)))

  ;; metamethods written in Scheme: the protected bindings then take the safe path
  ((test/s3-scheme-metamethods _)
   (let ((L (fresh)) (log '()))
     ;; proxy: __index answers "<key>!", __newindex records and stores raw
     ((lua-newtable) L)                                             ; 1: proxy
     ((lua-newtable) L)                                             ; 2: metatable
     ((lua-pushcfunction (λ (C) ((lua-pushstring (string-append ((lua-tostring 2) C) "!")) C) 1)) L)
     ((lua-setfield -2 "__index") L)
     ((lua-pushcfunction (λ (C)
                           (set! log (cons (value-at C 2) log))
                           ((lua-pushvalue 2) C)
                           ((lua-pushvalue 3) C)
                           ((lua-rawset 1) C)                       ; a P binding inside a callback
                           0))
      L)
     ((lua-setfield -2 "__newindex") L)
     ((lua-setmetatable 1) L)
     (⊦= 1 ((lua-gettop) L))
     ((lua-pushstring "k") L)
     (⊦= LUA_TSTRING ((lua-gettable 1) L))
     (⊦= "k!" ((lua-tostring -1) L))
     ((lua-pop 1) L)
     ((lua-pushinteger 7) L)
     ((lua-pushstring "v") L)
     ((lua-settable 1) L)
     ((lua-pushstring "w") L)
     ((lua-seti 1 8) L)
     (⊦= '(8 7) log)
     (⊦= LUA_TSTRING ((lua-geti 1 7) L))                            ; stored raw: no __index now
     (⊦= "v" ((lua-tostring -1) L))
     ((lua-pop 1) L)
     (⊦= 1 ((lua-gettop) L))
     ;; from Lua too
     ((lua-pushvalue 1) L)
     ((lua-setglobal "PX") L)
     (run! L "A = PX.zz; PX.q = 1; B = rawget(PX, 'q')")
     (⊦= "zz!" (global L "A"))
     (⊦= 1 (global L "B"))
     (⊦= "q" (car log))
     ;; a Scheme metamethod that raises: (exn lua) with the message, the state keeps working
     (run! L "BAD = setmetatable({}, {})")
     ((lua-getglobal "BAD") L)                                      ; 2
     ((lua-getmetatable 2) L)                                       ; 3
     ((lua-pushcfunction (λ (C) ((luaL-error "no field %s" ((lua-tostring 2) C)) C))) L)
     ((lua-setfield 3 "__index") L)
     ((lua-pop 1) L)
     ((lua-pushstring "nope") L)
     (let ((e (condition-case ((lua-gettable 2) L) (e (exn lua) e))))
       (⊦= "no field nope" (msg e)))
     (⊦= 2 ((lua-gettop) L))
     ;; a raised Scheme object comes back as itself
     ((lua-getmetatable 2) L)
     ((lua-pushcfunction (λ (C) (abort 'boom))) L)
     ((lua-setfield -2 "__index") L)
     ((lua-pop 1) L)
     (⊦= 'boom (condition-case ((lua-geti 2 1) L) (e () e)))
     (⊦= 2 ((lua-gettop) L))
     ;; a continuation escape out of __index
     ((lua-getmetatable 2) L)
     (let ((out #f))
       ((lua-pushcfunction (λ (C) (out 'escaped))) L)
       ((lua-setfield -2 "__index") L)
       ((lua-pop 1) L)
       (⊦= 'escaped (call/cc (λ (k) (set! out k) ((lua-pushstring "e") L) ((lua-gettable 2) L) 'returned))))
     ((lua-settop 2) L)
     ((lua-pushstring "fine") L)
     ((lua-rawseti 2 1) L)
     (⊦= "fine" (begin ((lua-rawgeti 2 1) L) (begin1 ((lua-tostring -1) L) ((lua-pop 1) L))))
     ;; GC stress: collections inside the metamethod while the bindings run on the safe path
     ((lua-getmetatable 2) L)
     (let ((n 0))
       ((lua-pushcfunction (λ (C) (set! n (+ n 1)) (gc (= 0 (modulo n 25))) ((lua-pushvalue 2) C) 1)) L))
     ((lua-setfield -2 "__index") L)
     ((lua-pop 1) L)
     (do ((i 1000 (+ i 1))) ((= i 1500))                            ; keys absent from BAD: __index answers them
       ((lua-pushinteger i) L)
       ((lua-gettable 2) L)
       (unless (= i ((lua-tointeger -1) L)) (⊦= i ((lua-tointeger -1) L)))
       ((lua-pop 1) L)
       ((lua-pushnil) L)
       (unless ((lua-next 2) L) (⊨ #f))
       ((lua-pop 2) L))
     (⊦= 2 ((lua-gettop) L))
     ((lua-settop 0) L)
     (lua-close L)))

  ;; upvalue pseudo-indices inside a Scheme closure; the registry anywhere
  ((test/s3-upvalues _)
   (let ((L (fresh)) (seen '()))
     (run! L "UPT = {7, 8, k = 'v'}")
     ((lua-getglobal "UPT") L)
     ((lua-pushcclosure
        (λ (C)
          (let ((u (lua-upvalueindex 1)))
            ((lua-rawgeti u 2) C)
            ((lua-geti u 1) C)
            ((lua-pushstring "k") C)
            ((lua-gettable u) C)
            ((lua-pushinteger 9) C)
            ((lua-seti u 3) C)
            ((lua-pushstring "new") C)
            ((lua-pushboolean #t) C)
            ((lua-settable u) C)
            (set! seen (list (value-at C 1) (value-at C 2) (value-at C 3)
                             ((lua-getmetatable u) C)
                             (condition-case ((lua-rawgeti (lua-upvalueindex 2) 1) C) (e (exn lua api) 'refused))
                             ((lua-rawgeti LUA_REGISTRYINDEX LUA_RIDX_GLOBALS) C)))
            0))
        1)
      L)
     ((lua-setglobal "F") L)
     (run! L "F() N = #UPT NEW = UPT.new")
     (⊦= (list 8 7 "v" #f 'refused LUA_TTABLE) seen)
     (⊦= 3 (global L "N"))
     (⊦= #t (global L "NEW"))
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)))

  ;; a hook view cannot lower the top below its floor (the hooked function's registers)
  ((test/s3-floor _)
   (let* ((L (fresh)) (H (make-luaR (luaR-state L) (luaR-u L) 'hook #f -1 2 #f)))
     ((lua-newtable) L)
     ((lua-pushstring "at floor") L)
     (⊦raises (exn lua api) ((lua-gettable 1) H))                  ; the key is a register below the floor
     (⊦raises (exn lua api) ((lua-rawget 1) H))
     (⊦raises (exn lua api) ((lua-rawseti 1 1) H))
     (⊦raises (exn lua api) ((lua-seti 1 1) H))
     (⊦raises (exn lua api) ((lua-setmetatable 1) H))
     (⊦raises (exn lua api) ((lua-next 1) H))
     (⊦= 2 ((lua-gettop) H))
     ((lua-pushstring "k") H)                                       ; values above the floor are fine
     (⊦= LUA_TNIL ((lua-rawget 1) H))
     ((lua-pushinteger 5) H)
     ((lua-seti 1 1) H)
     (⊦= 3 ((lua-gettop) H))
     ((lua-pushstring "k") H)
     ((lua-pushinteger 1) H)
     ((lua-settable 1) H)                                           ; slots 4 and 5: above the floor
     (⊦= 3 ((lua-gettop) H))
     (⊦raises (exn lua api) ((lua-settable 1) H))                  ; would consume slots 2 and 3
     ((lua-settop 2) L)
     ((lua-pushstring "k2") H)
     ((lua-pushinteger 1) H)
     ((lua-settable 1) H)                                           ; slots 3 and 4: fine
     (⊦= 2 ((lua-gettop) H))
     (⊦= 1 (begin ((lua-getfield 1 "k2") L) (begin1 ((lua-tointeger -1) L) ((lua-pop 1) L))))

     ((lua-settop 0) L)
     (lua-close L)))

  ;; I6: constructors (P*) work on a suspended coroutine through the helper thread, and so do the
  ;; D bindings; protected reads and writes are refused.  A thread dead in a Lua frame cannot have
  ;; its top lowered by the consuming D bindings either (the core's lua-settop rule).
  ((test/s3-non-ok-threads _)
   (let* ((L (fresh)) (ptr #f)
          (co (λ () (make-luaR ptr (luaR-u L) 'thread #f -1 0 #f))))
     ((lua-register "grab" (λ (C) (set! ptr (luaR-state C)) 0)) L)
     (run! L "CO = coroutine.create(function() grab() local t = coroutine.yield(1) return t.x end)
              assert(coroutine.resume(CO))")
     (let* ((T (co)) (top ((lua-gettop) T)))
       (⊦= LUA_YIELD (%status ptr))
       ((lua-createtable 0 1) T)
       (⊦= (+ top 1) ((lua-gettop) T))
       (⊦= LUA_TTABLE ((lua-type -1) T))
       (⊨ (pointer? ((lua-newuserdatauv 8 1) T)))
       (⊦= LUA_TUSERDATA ((lua-type -1) T))
       ((lua-pushstring "uv") T)
       (⊨ ((lua-setiuservalue -2 1) T))                             ; D: pops from a C frame, fine
       ((lua-newtable) T)
       ((lua-setmetatable -2) T)
       (⊨ ((lua-getmetatable -1) T))
       ((lua-pop 2) T)
       ((lua-pushglobaltable) T)
       (⊦= LUA_TTABLE ((lua-rawgeti LUA_REGISTRYINDEX LUA_RIDX_GLOBALS) T))
       ((lua-pop 2) T)
       ;; a Lua error of a P* op on the helper: raised, the thread's stack untouched
       (⊦= "table overflow" (condition-case ((lua-createtable 0 2147483647) T) (e (exn lua) (msg e))))
       (⊦= (+ top 1) ((lua-gettop) T))
       ;; protected ops refuse a suspended thread
       ((lua-pushstring "x") T)
       (⊦raises (exn lua api) ((lua-gettable -2) T))
       (⊦raises (exn lua api) ((lua-geti -2 1) T))
       ((lua-pushstring "v") T)
       (⊦raises (exn lua api) ((lua-settable -3) T))
       (⊦raises (exn lua api) ((lua-rawset -3) T))
       (⊦raises (exn lua api) ((lua-seti -3 1) T))
       ((lua-pop 1) T)
       (⊦raises (exn lua api) ((lua-next -2) T))
       ;; the D raw write path does not exist (rawset allocates): fill the table from L instead
       (⊦= (+ top 2) ((lua-gettop) T))
       ((lua-pop 2) T)
       (⊦= top ((lua-gettop) T)))
     (run! L "local ok, r = coroutine.resume(CO, {x = 'resumed'}) R = r")
     (⊦= "resumed" (global L "R"))
     ;; dead by an error raised in Lua code: pushes work, lowering the top is refused
     (run! L "CO2 = coroutine.create(function() grab() local n = nil; n = n + 1 end)
              coroutine.resume(CO2)")
     (let* ((T (co)) (top ((lua-gettop) T)))
       (⊨ (> (%status ptr) LUA_YIELD))
       ((lua-newtable) T)
       ((lua-newtable) T)
       (⊦raises (exn lua api) ((lua-setmetatable -2) T))
       (⊦= (+ top 2) ((lua-gettop) T))
       (⊦= LUA_TNIL ((lua-rawgeti -1 1) T))                         ; pushing reads are fine
       (⊦= (+ top 3) ((lua-gettop) T)))
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)))

  ;; a positive index above the top is the nil value, as in C: once the type nil has a metatable
  ;; (debug.setmetatable(nil, mt)), lua_getmetatable finds it there too and pushes it
  ((test/s3-getmetatable-nil-type _)
   (let ((L (fresh)))
     (⊭ ((lua-getmetatable 1) L))                                   ; no metatable for nil yet
     (⊦= 0 ((lua-gettop) L))
     (run! L "debug.setmetatable(nil, {kind = 'nil-mt'})")
     ((lua-settop 0) L)
     (for-each (λ (idx)
                 (⊨ ((lua-getmetatable idx) L))
                 (⊦= 1 ((lua-gettop) L))
                 ((lua-getfield -1 "kind") L)
                 (⊦= "nil-mt" ((lua-tostring -1) L))
                 ((lua-settop 0) L))
               (list 1 2 1000000))
     ((lua-pushinteger 7) L)                                        ; above a real slot
     (⊨ ((lua-getmetatable 2) L))
     (⊦= 2 ((lua-gettop) L))
     ((lua-settop 0) L)
     ((lua-pushnil) L)                                              ; the same nil, in a real slot
     (⊨ ((lua-getmetatable 1) L))
     (⊦= 2 ((lua-gettop) L))
     ((lua-settop 0) L)
     (⊦raises (exn lua api) ((lua-getmetatable 0) L))               ; invalid stays invalid
     (⊦raises (exn lua api) ((lua-getmetatable -1) L))
     (run! L "debug.setmetatable(nil, nil)")
     ((lua-settop 0) L)
     (⊭ ((lua-getmetatable 1) L))
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)))
  )

(unittest/✓ lua-table-suite)

;; ------------------------------------------------------------------------------------------
;; S4: states, threads, calls, load/dump, warnings, gc
;; ------------------------------------------------------------------------------------------

(define-suite lua-call-suite

  ;; every S4 binding is a procedure of the state only, refusing a closed state or a non-state
  ((test/s4-currying _)
   (let ((L (lua-make-empty)))
     (lua-close L)
     (for-each (λ (m)
                 (⊨ (procedure? m))
                 (⊦raises (exn lua api) (m L))
                 (⊦raises (exn lua api) (m 'not-a-state)))
               (list (lua-version) (lua-status) (lua-newthread) (lua-closethread #f) (lua-resetthread)
                     (lua-resume #f 0) (lua-load "return 1" "=c" #f) (lua-load (λ () #f) #f #f) (lua-dump #f)
                     (lua-warning "w" #f) (lua-gc 'count) (lua-gc 'collect) (lua-gc 'step 0)
                     (lua-gc 'param 'pause -1)))
     ;; the nullary bindings answer the same preallocated closure
     (⊨ (eq? (lua-version) (lua-version)))
     (⊨ (eq? (lua-newthread) (lua-newthread)))
     (⊨ (eq? (lua-resetthread) (lua-resetthread)))))

  ((test/s4-newstate _)
   (let ((L (lua-newstate 12345)))
     (⊨ (luaR? L))
     (⊦= 'main (luaR-kind L))
     (⊨ (pointer? (luaR-state L)))
     (⊦= 505.0 ((lua-version) L))
     (⊦= LUA_OK ((lua-status) L))
     (⊦= LUA_TNIL ((lua-getglobal "print") L))                       ; no library is opened
     ((lua-pop 1) L)
     ;; C's lua_newstate installs no warning function: a warning goes nowhere (luaL_newstate's
     ;; stderr writer is not there), until lua-setwarnf gives the universe a sink
     (⊦= "" (with-stderr-captured (λ () ((lua-warning "from newstate" #f) L) ((lua-warning "piece" #t) L) ((lua-warning "s" #f) L))))
     ((lua-setwarnf 'collect) L)
     (⊦= '() ((lua-warnings) L))
     ((lua-warning "now collected" #f) L)
     (⊦= '("now collected") ((lua-warnings) L))
     ((lua-setwarnf 'stderr) L)
     (⊦= "Lua warning: to stderr\n" (with-stderr-captured (λ () ((lua-warning "to stderr" #f) L))))
     ((luaL-openlibs) L)
     (run! L "local t = {} for i = 1, 100 do t['k' .. i] = i end S = 0 for k, v in pairs(t) do S = S + v end")
     (⊦= 5050 (global L "S"))
     ;; the counting allocator is there from the start: a limit needs no allocator switch
     ((lua-setmemorylimit (+ (* 1024 ((lua-gc 'count) L)) ((lua-gc 'countb) L) 100000)) L)
     (⊦= 'errmem (condition-case ((lua-pushlstring (make-string 500000 #\a)) L) (e (exn lua) (lua-condition-status e))))
     (⊦= 0 ((lua-gettop) L))
     ((lua-setmemorylimit 0) L)
     ((lua-pushlstring (make-string 500000 #\a)) L)
     ((lua-pop 1) L)
     (lua-close L)
     (⊦= #f (luaR-state L))
     (lua-close L))
   ;; the default seed, and the same seed twice
   (let ((A (lua-newstate)) (B (lua-newstate #f)) (C (lua-newstate 7)) (D (lua-newstate 7)) (E (lua-newstate 4294967295)))
     (for-each (λ (L) ((luaL-openlibs) L) (run! L "X = #string.rep('ab', 10)") (⊦= 20 (global L "X")) (lua-close L))
               (list A B C D E)))
   ;; the seed reaches lua_newstate: Lua 5.5 hashes strings with it, so the order of pairs over
   ;; string keys is the same for equal seeds and differs for these different ones
   (define (key-order seed)
     (let ((L (lua-newstate seed)))
       ((luaL-openlibs) L)
       (run! L "local t = {} for i = 1, 40 do t['key' .. i] = i end
                local o = {} for k in pairs(t) do o[#o + 1] = k end ORDER = table.concat(o, ',')")
       (begin1 (global L "ORDER") (lua-close L))))
   (⊦= (key-order 7) (key-order 7))
   (⊦= (key-order 4294967295) (key-order 4294967295))
   (⊭ (equal? (key-order 7) (key-order 8)))
   (⊭ (equal? (key-order 1) (key-order 2)))
   (⊦raises (exn lua api) (lua-newstate -1))
   (⊦raises (exn lua api) (lua-newstate 4294967296))
   (⊦raises (exn lua api) (lua-newstate 1.5))
   (⊦raises (exn lua api) (lua-newstate "seed"))
   ;; by contrast, lua-make-empty and luaL-newstate start as luaL_newstate: warnings on, to stderr
   (for-each (λ (L) (⊦= "Lua warning: default\n" (with-stderr-captured (λ () ((lua-warning "default" #f) L)))) (lua-close L))
             (list (lua-make-empty) (luaL-newstate)))
   ;; a dropped lua-newstate universe is closed by its finalizer
   (let ((u (luaR-u (lua-newstate 1))))
     (flush-and-collect (λ () (not (luaU-state u))))
     (⊦= #f (luaU-state u))))

  ((test/s4-pcall-call _)
   (let ((L (fresh)))
     (run! L "function ok(a, b) return a + b, a * b end
              function bad(m) error(m, 0) end
              function badmsgh(m) error('in handler') end
              function tb(m) return 'handled: ' .. m end")
     ;; statuses and results
     ((lua-getglobal "ok") L) ((lua-pushinteger 6) L) ((lua-pushinteger 7) L)
     (⊦= LUA_OK ((lua-pcall 2 2 0) L))
     (⊦= 2 ((lua-gettop) L))
     (⊦= 13 ((lua-tointeger 1) L))
     (⊦= 42 ((lua-tointeger 2) L))
     ((lua-settop 0) L)
     ((lua-getglobal "ok") L) ((lua-pushinteger 1) L) ((lua-pushinteger 2) L)
     (⊦= LUA_OK ((lua-pcall 2 LUA_MULTRET 0) L))
     (⊦= 2 ((lua-gettop) L))
     ((lua-settop 0) L)
     ((lua-getglobal "ok") L) ((lua-pushinteger 1) L) ((lua-pushinteger 2) L)
     (⊦= LUA_OK ((lua-pcall 2 5 0) L))                                 ; padded with nils
     (⊦= 5 ((lua-gettop) L))
     (⊦= LUA_TNIL ((lua-type 5) L))
     ((lua-settop 0) L)
     ;; an error: the value is left on the stack
     ((lua-getglobal "bad") L) ((lua-pushstring "boom") L)
     (⊦= LUA_ERRRUN ((lua-pcall 1 3 0) L))
     (⊦= 1 ((lua-gettop) L))
     (⊦= "boom" ((lua-tostring 1) L))
     ((lua-settop 0) L)
     ;; message handlers: one that works, one that fails (LUA_ERRERR)
     ((lua-getglobal "tb") L)
     ((lua-getglobal "bad") L) ((lua-pushstring "e1") L)
     (⊦= LUA_ERRRUN ((lua-pcall 1 0 1) L))
     (⊦= "handled: e1" ((lua-tostring -1) L))
     ((lua-settop 0) L)
     ((lua-getglobal "badmsgh") L)
     ((lua-getglobal "bad") L) ((lua-pushstring "e2") L)
     (⊦= LUA_ERRERR ((lua-pcall 1 0 1) L))
     (⊦= 2 ((lua-gettop) L))
     ((lua-settop 0) L)
     ;; lua-call raises the error value as (exn lua), with the C stack effect
     ((lua-getglobal "bad") L) ((lua-pushstring "raised") L)
     (let ((e (condition-case ((lua-call 1 0) L) (e (exn lua) e))))
       (⊦= "raised" (msg e))
       (⊦= 'errrun (lua-condition-status e))
       (⊦= "raised" (lua-condition-value e))
       (⊦= 'lua-call ((condition-property-accessor 'exn 'location) e)))
     (⊦= 0 ((lua-gettop) L))
     ((lua-getglobal "ok") L) ((lua-pushinteger 20) L) ((lua-pushinteger 22) L)
     ((lua-call 2 1) L)
     (⊦= 42 ((lua-tointeger -1) L))
     ((lua-pop 1) L)
     ;; a Scheme function called through lua-pcall and lua-call
     ((lua-pushcfunction (λ (C) ((lua-pushinteger (* 3 ((lua-tointeger 1) C))) C) 1)) L)
     ((lua-pushinteger 14) L)
     (⊦= LUA_OK ((lua-pcall 1 1 0) L))
     (⊦= 42 ((lua-tointeger -1) L))
     ((lua-pop 1) L)
     ;; api refusals: too few values, a bad nres
     (⊦raises (exn lua api) ((lua-call 0 0) L))
     ((lua-getglobal "ok") L)
     (⊦raises (exn lua api) ((lua-pcall 1 0 0) L))
     (⊦raises (exn lua api) ((lua-pcall 0 -2 0) L))
     ;; Lua 5.5 takes at most 250 results (MAXRESULTS): C's API check, which a release liblua
     ;; skips (the count wraps in the call's 8-bit field: 300 would give 44 results)
     (for-each (λ (n) (⊦= (list n AUX_ERANGE) (list n (condition-case ((lua-pcall 0 n 0) L) (e (exn lua api) (get-condition-property e 'lua 'code))))))
               '(251 255 256 300 100000))
     (⊦raises (exn lua api) ((lua-call 0 251) L))
     (⊦= 1 ((lua-gettop) L))
     ((lua-pop 1) L)
     ((lua-pushcfunction (λ (C) ((lua-pushinteger 7) C) 1)) L)
     (⊦= LUA_OK ((lua-pcall 0 250 0) L))                   ; one result, adjusted to 250
     (⊦= 250 ((lua-gettop) L))
     (⊦= 7 ((lua-tointeger 1) L))
     (⊦= LUA_TNIL ((lua-type 250) L))
     ((lua-settop 0) L)
     ;; the state stays usable after all of that
     (run! L "Z = 1")
     (⊦= 1 (global L "Z"))
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)))

  ((test/s4-load-string _)
   (let ((L (fresh)))
     (⊦= LUA_OK ((lua-load "return 40 + 2" "=answer" #f) L))
     (⊦= LUA_TFUNCTION ((lua-type -1) L))
     ((lua-call 0 1) L)
     (⊦= 42 ((lua-tointeger -1) L))
     ((lua-pop 1) L)
     ;; a bytevector with a NUL inside a string literal, mode "t"
     (⊦= LUA_OK ((lua-load (string->utf8 "return 'a\\0b', ...") "=bv" "t") L))
     ((lua-pushinteger 9) L)
     ((lua-call 1 2) L)
     (⊦= "a\x0;b" ((lua-tostring 1) L))
     (⊦= 9 ((lua-tointeger 2) L))
     ((lua-settop 0) L)
     ;; the chunk name appears in messages; #f is "?"
     (⊦= LUA_OK ((lua-load "error('x')" "=mychunk" #f) L))
     (⊦= "mychunk:1: x" (condition-case ((lua-call 0 0) L) (e (exn lua) (msg e))))
     (⊦= LUA_OK ((lua-load "error('y')" #f #f) L))
     (⊦= "[string \"?\"]:1: y" (condition-case ((lua-call 0 0) L) (e (exn lua) (msg e))))
     ;; syntax errors: the status, and the message pushed
     (⊦= LUA_ERRSYNTAX ((lua-load "return = 1" "=syn" #f) L))
     (⊦= 1 ((lua-gettop) L))
     (⊨ (string-prefix? "syn:1:" ((lua-tostring -1) L)))
     ((lua-pop 1) L)
     (⊦= 'errsyntax (condition-case ((lua-check (lua-load "x x" "=s" #f)) L) (e (exn lua) (lua-condition-status e))))
     (⊦= 0 ((lua-gettop) L))
     ;; mode "b" rejects text
     (⊦= LUA_ERRSYNTAX ((lua-load "return 1" "=m" "b") L))
     (⊨ (string-suffix? "attempt to load a text chunk (mode is 'b')" ((lua-tostring -1) L)))
     ((lua-pop 1) L)
     ;; the loaded chunk gets the globals as its first upvalue
     (⊦= LUA_OK ((lua-load "G = (G or 0) + 1" "=g" #f) L))
     ((lua-call 0 0) L)
     (⊦= 1 (global L "G"))
     ;; api refusals, before any Lua code runs
     (⊦raises (exn lua api) ((lua-load 42 "=n" #f) L))
     (⊦raises (exn lua api) ((lua-load "return 1" "na\x0;me" #f) L))
     (⊦raises (exn lua api) ((lua-load "return 1" 'name #f) L))
     (⊦raises (exn lua api) ((lua-load "return 1" "=n" "b\x0;t") L))
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)))

  ((test/s4-load-reader _)
   (let* ((L (fresh)) (u (luaR-u L)) (base (lua-live-handles)) (calls 0)
          (code "local s = 0 for i = 1, 10 do s = s + i end return s, 'r\\0z'")
          ;; one byte per call, each call forcing a collection (the pieces are copied at once)
          (u8-ref bytevector-u8-ref)
          (byte-reader (λ (s)
                         (let ((bv (string->utf8 s)) (i 0))
                           (λ ()
                             (set! calls (+ calls 1))
                             (gc (= 0 (modulo calls 20)))
                             (and (< i (bytevector-length bv))
                                  (begin (set! i (+ i 1)) (bytevector (u8-ref bv (- i 1))))))))))
     (⊦= LUA_OK ((lua-load (byte-reader code) "=bytes" "t") L))
     (⊦= (+ 1 (string-length code)) calls)                          ; one more call answers #f
     ((lua-call 0 2) L)
     (⊦= 55 ((lua-tointeger 1) L))
     (⊦= "r\x0;z" ((lua-tostring 2) L))
     ((lua-settop 0) L)
     (⊦= base (lua-live-handles))                                     ; the reader handle is released
     (⊦= 0 (luaU-callables u))                                        ; and the fast path chosen again
     ;; pieces as strings and bytevectors; "" ends the chunk, as a zero size does in C
     (let ((pieces (list "return " (string->utf8 "1 +") " 2" "" "error('never read')")))
       (⊦= LUA_OK ((lua-load (λ () (and (pair? pieces) (begin1 (car pieces) (set! pieces (cdr pieces))))) "=p" #f) L))
       (⊦= 1 (length pieces)))                                         ; not called after the end
     ((lua-call 0 1) L)
     (⊦= 3 ((lua-tointeger -1) L))
     ((lua-pop 1) L)
     ;; a syntax error read through a reader
     (let ((once #t))
       (⊦= LUA_ERRSYNTAX ((lua-load (λ () (and once (begin (set! once #f) "return +"))) "=r" #f) L)))
     (⊨ (string-prefix? "r:1:" ((lua-tostring -1) L)))
     ((lua-pop 1) L)
     ;; the reader raises: the load stops, the original condition is re-raised, the stack is as before
     (let* ((c (make-property-condition 'reader-failed))
            (n 0)
            (e (condition-case ((lua-load (λ () (set! n (+ n 1)) (if (= n 3) (abort c) "local x = 1 ")) "=r" #f) L)
                 (e () e))))
       (⊨ (eq? c e))
       (⊦= 3 n))
     (⊦= 0 ((lua-gettop) L))
     (⊦= "reader failed" (condition-case ((lua-load (λ () (error "reader failed")) "=r" #f) L) (e (exn) (msg e))))
     ;; a bad answer is an api error
     (⊦raises (exn lua api) ((lua-load (λ () 42) "=r" #f) L))
     (⊦= 0 ((lua-gettop) L))
     ;; what the reader leaves on the loading thread (in its own frame) is dropped
     (let ((once #t))
       (⊦= LUA_OK ((lua-load (λ () ((lua-pushinteger 1) L) (and once (begin (set! once #f) "return 1"))) "=r" #f) L)))
     (⊦= 1 ((lua-gettop) L))
     (⊦= LUA_TFUNCTION ((lua-type 1) L))
     ((lua-pop 1) L)
     (⊦= 0 ((lua-gettop) L))
     ;; a continuation escape out of the reader is resumed once lua_load returned
     (⊦= 'escaped (call/cc (λ (k) ((lua-load (λ () (k 'escaped)) "=r" #f) L) 'not-escaped)))
     (⊦= 0 ((lua-gettop) L))
     (⊦= #f %pending-escape)
     (⊦= 0 %depth)
     ;; an escape to a continuation inside the reader's own dynamic extent is plain Scheme
     (⊦= LUA_OK ((lua-load (λ () (call/cc (λ (k) (k #f)))) "=r" #f) L))
     ((lua-pop 1) L)
     ;; a reader inside a Scheme callback (the safe path), loading into the callback's own view
     ((register! "loadit" (λ (C)
                            (let ((pieces (list "return " "'from " "reader'")))
                              ((lua-load (λ () (and (pair? pieces) (begin1 (car pieces) (set! pieces (cdr pieces))))) "=cb" #f) C)
                              ((lua-call 0 1) C)
                              1)))
      L)
     (run! L "R = loadit()")
     (⊦= "from reader" (global L "R"))
     ;; the escape through a reader inside a callback: stopped at both boundaries, then resumed
     (⊦= 'deep (call/cc (λ (k)
                          ((register! "esc" (λ (C) ((lua-load (λ () (k 'deep)) "=e" #f) C) 0)) L)
                          (run! L "esc()")
                          'not-escaped)))
     (⊦= 0 ((lua-gettop) L))
     (⊦= 0 %depth)
     (run! L "Y = 5")
     (⊦= 5 (global L "Y"))
     (lua-close L)
     (⊦= base (lua-live-handles))))

  ((test/s4-dump _)
   (let ((L (fresh)) (u8-ref bytevector-u8-ref) (u8-set! bytevector-u8-set!))
     (run! L "local k = 10
              function F(x) if x == 0 then error('zero') end return x * k end
              function G() return 'no upvalues' end")
     ((lua-getglobal "F") L)
     ;; strip is C's int used as a boolean: #t / #f only (0 would strip with Scheme truthiness, the
     ;; opposite of C's lua_dump(L, w, d, 0)); refused before anything runs, the function stays
     (for-each (λ (x) (⊦= (list AUX_EARG 'lua-dump) (api-refusal (λ () ((lua-dump x) L))))) (list 0 1 'strip))
     (⊦= 1 ((lua-gettop) L))
     (let ((full ((lua-dump #f) L)) (stripped ((lua-dump #t) L)))
       (⊦= 1 ((lua-gettop) L))                                        ; the function stays
       (⊦= LUA_TFUNCTION ((lua-type -1) L))
       ((lua-pop 1) L)
       (⊨ (bytevector? full))
       (⊨ (bytevector? stripped))
       (⊨ (< (bytevector-length stripped) (bytevector-length full)))
       (⊦= 27 (u8-ref full 0))                              ; "\27Lua"
       ;; binary chunks load back (mode "b" and the default), not with mode "t"
       (⊦= LUA_OK ((lua-load full "=full" "b") L))
       ((lua-setglobal "F2") L)
       (⊦= LUA_OK ((lua-load stripped "=stripped" #f) L))
       ((lua-setglobal "F3") L)
       ;; a loaded chunk's first upvalue is the globals table; set the others by name
       ;; (stripped: no names, but the same order)
       (run! L "for i = 1, 2 do
                  local name = debug.getupvalue(F2, i)
                  local v = (name == 'k') and 10 or _ENV
                  debug.setupvalue(F2, i, v) debug.setupvalue(F3, i, v)
                end
                A = F2(4) B = F3(5)
                local ok, e = pcall(F2, 0) EA = e
                ok, e = pcall(F3, 0) EB = e")
       (⊦= 40 (global L "A"))
       (⊦= 50 (global L "B"))
       (⊨ (string-suffix? ":2: zero" (global L "EA")))               ; line information kept
       (⊦= "zero" (global L "EB"))                                    ; stripped: no position at all
       (⊦= LUA_ERRSYNTAX ((lua-load full "=t" "t") L))
       (⊨ (string-suffix? "attempt to load a binary chunk (mode is 't')" ((lua-tostring -1) L)))
       ((lua-pop 1) L)
       ;; a truncated binary chunk is a syntax error, never a crash
       (let ((half (make-bytevector (quotient (bytevector-length full) 2))))
         (do ((i 0 (+ i 1))) ((= i (bytevector-length half))) (u8-set! half i (u8-ref full i)))
         (⊦= LUA_ERRSYNTAX ((lua-load half "=half" "b") L))
         ((lua-pop 1) L)))
     ;; a function made by a Scheme reader, dumped and read back through a byte reader
     ((lua-getglobal "G") L)
     (let* ((bv ((lua-dump #f) L)) (i 0))
       ((lua-pop 1) L)
       (⊦= LUA_OK ((lua-load (λ () (and (< i (bytevector-length bv)) (begin (set! i (+ i 1)) (bytevector (u8-ref bv (- i 1))))))
                             "=g" "b")
                   L))
       ((lua-call 0 1) L)
       (⊦= "no upvalues" ((lua-tostring -1) L))
       ((lua-pop 1) L))
     ;; the safe path (a Scheme callable is alive in the universe)
     ((register! "cb" (λ (C) 0)) L)
     ((lua-getglobal "G") L)
     (⊨ (bytevector? ((lua-dump #t) L)))
     ((lua-pop 1) L)
     ;; refusals: not a Lua function, nothing on the stack
     (⊦raises (exn lua api) ((lua-dump #f) L))
     ((lua-getglobal "cb") L)
     (⊦raises (exn lua api) ((lua-dump #f) L))
     ((lua-getglobal "print") L)
     (⊦raises (exn lua api) ((lua-dump #f) L))
     ((lua-pushinteger 1) L)
     (⊦raises (exn lua api) ((lua-dump #f) L))
     (⊦= 3 ((lua-gettop) L))
     ((lua-settop 0) L)
     (lua-close L)))

  ((test/s4-threads _)
   (let* ((L (fresh)) (T ((lua-newthread) L)))
     (⊨ (luaR? T))
     (⊦= 'thread (luaR-kind T))
     (⊨ (pointer? (luaR-state T)))
     (⊦= 1 ((lua-gettop) L))                                          ; pushed, as C
     (⊦= LUA_TTHREAD ((lua-type -1) L))
     ((lua-setglobal "T") L)
     (⊦= LUA_OK ((lua-status) T))
     (⊦= 0 ((lua-gettop) T))
     (⊦= 505.0 ((lua-version) T))
     ;; a thread can run code of its universe directly
     ((lua-pushinteger 5) T)
     ((lua-setglobal "FROM_T") T)
     (⊦= 5 (global L "FROM_T"))
     (⊦raises (exn lua api) (lua-close T))                            ; only a main state closes
     ;; the thread is anchored: Lua dropping it does not invalidate T
     (run! L "T = nil collectgarbage() collectgarbage()")
     ((lua-pushstring "still here") T)
     (⊦= "still here" ((lua-tostring -1) T))
     ((lua-pop 1) T)
     ;; threads of threads
     (let ((T2 ((lua-newthread) T)))
       (⊦= 1 ((lua-gettop) T))
       ((lua-pop 1) T)
       ((lua-pushinteger 2) T2)
       (⊦= 2 ((lua-tointeger -1) T2))
       ((lua-pop 1) T2)
       ;; a new thread pushed onto a suspended coroutine (the P* helper path), then run
       (run! L "function y() coroutine.yield() end function add(a, b) return a + b end")
       ((lua-getglobal "y") T2)
       (let-values (((st n) ((lua-resume T 0) T2))) (⊦= LUA_YIELD st))
       (⊦= LUA_YIELD ((lua-status) T2))
       (let* ((top ((lua-gettop) T2)) (T3 ((lua-newthread) T2)))
         (⊦= (+ top 1) ((lua-gettop) T2))
         (⊦= LUA_TTHREAD ((lua-type -1) T2))
         ((lua-pop 1) T2)
         ((lua-getglobal "add") T3) ((lua-pushinteger 40) T3) ((lua-pushinteger 2) T3)
         (let-values (((st n) ((lua-resume T2 2) T3)))
           (⊦= LUA_OK st)
           (⊦= 1 n)
           (⊦= 42 ((lua-tointeger -1) T3)))))
     ;; closing the universe invalidates its threads
     (lua-close L)
     (⊦raises (exn lua api) ((lua-gettop) T))
     (⊦raises (exn lua api) ((lua-newthread) T))))

  ;; dropped thread luaRs release their anchors (a private table of the universe, not registry
  ;; refs), and the anchor ids are reused
  ((test/s4-thread-anchors _)
   (let ((L (fresh)))
     (define (registry-threads)
       (run! L "local n = 0 for k, v in pairs(debug.getregistry()) do if type(v) == 'thread' then n = n + 1 end end N = n")
       (global L "N"))
     (define (anchored-threads)
       (run! L "local n = 0
                for k, v in pairs(debug.getregistry()) do
                  if type(k) == 'userdata' and type(v) == 'table' then
                    for _, x in pairs(v) do if type(x) == 'thread' then n = n + 1 end end
                  end
                end
                N = n")
       (global L "N"))
     (let ((n0 (registry-threads)) (a0 (anchored-threads)))
       (let loop ((i 0)) (when (< i 50) ((lua-newthread) L) ((lua-pop 1) L) (loop (+ i 1))))
       (⊦= n0 (registry-threads))                                     ; no registry ref
       (⊦= (+ a0 50) (anchored-threads))
       (flush-and-collect (λ () (>= (length (luaU-unanchors (luaR-u L))) 50)))
       ((lua-pushnil) L) ((lua-pop 1) L)                              ; a binding drained the anchors
       (run! L "collectgarbage()")
       (⊦= a0 (anchored-threads))
       (let ((high (luaU-anext (luaR-u L))))
         (let loop ((i 0)) (when (< i 20) ((lua-newthread) L) ((lua-pop 1) L) (loop (+ i 1))))
         (⊦= high (luaU-anext (luaR-u L)))))                          ; freed ids come back
     (lua-close L)))

  ((test/s4-resume _)
   (let* ((L (fresh)) (T ((lua-newthread) L)))
     ((lua-pop 1) L)
     (run! L "function gen(a)
                local b = coroutine.yield(a + 1, a + 2)
                local c = coroutine.yield(b * 10)
                return 'done', c
              end
              function fails() local x = nil; return x.field end")
     ((lua-getglobal "gen") T)
     ((lua-pushinteger 1) T)
     (let-values (((st n) ((lua-resume L 1) T)))
       (⊦= LUA_YIELD st)
       (⊦= 2 n))
     (⊦= LUA_YIELD ((lua-status) T))
     (⊦= 2 ((lua-gettop) T))
     (⊦= 2 ((lua-tointeger 1) T))
     (⊦= 3 ((lua-tointeger 2) T))
     ((lua-pop 2) T)
     ((lua-pushinteger 5) T)
     (let-values (((st n) ((lua-resume #f 1) T)))                     ; from #f
       (⊦= LUA_YIELD st)
       (⊦= 1 n)
       (⊦= 50 ((lua-tointeger -1) T)))
     ((lua-pop 1) T)
     ((lua-pushinteger 7) T)
     (let-values (((st n) ((lua-resume L 1) T)))
       (⊦= LUA_OK st)
       (⊦= 2 n)
       (⊦= "done" ((lua-tostring 1) T))
       (⊦= 7 ((lua-tointeger 2) T)))
     ((lua-settop 0) T)
     (⊦= LUA_OK ((lua-status) T))
     ;; resuming a finished coroutine: a Lua error, with its message on the thread (C semantics)
     (let-values (((st n) ((lua-resume L 0) T)))
       (⊦= LUA_ERRRUN st)
       (⊦= "cannot resume dead coroutine" ((lua-tostring -1) T)))
     ((lua-settop 0) T)
     ;; an error inside the coroutine: status and message; the thread is dead
     ((lua-getglobal "fails") T)
     (let-values (((st n) ((lua-resume L 0) T)))                     ; nres is meaningless on errors (C)
       (⊦= LUA_ERRRUN st)
       (⊨ (string-suffix? "attempt to index a nil value (local 'x')" ((lua-tostring -1) T))))
     (⊦= LUA_ERRRUN ((lua-status) T))
     (let-values (((st n) ((lua-resume L 0) T)))                     ; dead by error
       (⊦= LUA_ERRRUN st)
       (⊦= "cannot resume dead coroutine" ((lua-tostring -1) T)))
     (⊦= LUA_ERRRUN ((lua-closethread L) T))                          ; resets it, answering the original error
     ((lua-settop 0) T)
     (⊦= 0 ((lua-gettop) T))
     ;; lua-check raises the error left on the coroutine
     ((lua-getglobal "fails") T)
     (let ((e (condition-case ((lua-check (lua-resume L 0)) T) (e (exn lua) e))))
       (⊦= 'errrun (lua-condition-status e))
       (⊨ (string-suffix? "attempt to index a nil value (local 'x')" (msg e))))
     (⊦= LUA_ERRRUN ((lua-resetthread) T))
     ((lua-settop 0) T)
     ;; api refusals
     (⊦raises (exn lua api) ((lua-resume L 1) T))                    ; no arguments on the stack
     (⊦raises (exn lua api) ((lua-resume L -1) T))
     ;; the main thread cannot be resumed (from Scheme, or from inside a callback on it)
     ((lua-getglobal "gen") L) ((lua-pushinteger 1) L)
     (⊦raises (exn lua api) ((lua-resume #f 1) L))
     (⊦= 2 ((lua-gettop) L))
     ((lua-settop 0) L)
     ((register! "resmain" (λ (C) (condition-case (begin ((lua-resume #f 0) C) 'resumed) (e (exn lua api) ((lua-pushstring "refused") C) 1)))) L)
     (run! L "R = resmain()")
     (⊦= "refused" (global L "R"))
     ;; the P and S bindings refuse a suspended coroutine (lua-dump, lua-load, lua-pcall)
     ((lua-getglobal "gen") T) ((lua-pushinteger 1) T)
     (let-values (((st n) ((lua-resume L 1) T))) (⊦= LUA_YIELD st))
     ((lua-getglobal "gen") T)                                        ; P*: pushed from the helper
     (⊦raises (exn lua api) ((lua-dump #f) T))
     (⊦raises (exn lua api) ((lua-load "return 1" "=x" #f) T))
     (⊦raises (exn lua api) ((lua-load (λ () #f) "=x" #f) T))
     (⊦raises (exn lua api) ((lua-pcall 0 0 0) T))
     (⊦= 3 ((lua-gettop) T))
     ((lua-pop 1) T)
     (⊦= LUA_OK ((lua-closethread L) T))
     (⊦= 0 ((lua-gettop) T))
     (let ((other (fresh)))
       ((lua-getglobal "gen") T) ((lua-pushinteger 1) T)
       (⊦raises (exn lua api) ((lua-resume other 1) T))              ; another universe
       (lua-close other)
       (⊦raises (exn lua api) ((lua-resume other 1) T))              ; a closed one
       ((lua-settop 0) T))
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)))

  ;; lua-resume on a coroutine suspended by a hook's yield, or dead in a Lua frame: Lua lowers its
  ;; top by nargs (resume() discards the arguments, resume_error writes its message where the top
  ;; lands), so nargs may only cover what lies above the height it had then: the values pushed
  ;; since, and the error value lua-resume left on a dead one.  More is refused (AUX_ETHREAD):
  ;; a pending CALL would count a negative number of arguments (a write past the stack), a
  ;; message would overwrite a to-be-closed variable (lua-closethread would never close it).
  ((test/s4-resume-frame-args _)
   (let ((L (fresh)) (yielded 0) (chunk "local function f(...) return select('#', ...) end
                                         local function g() return 1, 2, 3, 4, 5, 6, 7, 8 end
                                         return f(g())"))
     (define (code-of thunk)
       (condition-case (begin (thunk) 'no-raise) (e (exn lua api) (get-condition-property e 'lua 'code))))
     (define (res T n) (call-with-values (λ () ((lua-resume L n) T)) list))
     ;; a count hook yields at every point of f(g()), the CALL whose argument count is the live top
     (do ((count 1 (+ count 1))) ((= count 30))
       (let ((T ((lua-newthread) L)) (r #f))
         ((lua-pop 1) L)
         ((luaL-loadstring chunk) T)
         ((lua-sethook (λ (H e ar) ((lua-yield 0) H)) LUA_MASKCOUNT count) T)
         (set! r (res T 0))
         ((lua-sethook #f 0 0) T)
         (when (= (car r) LUA_YIELD)
           (set! yielded (+ yielded 1))
           (let ((top ((lua-gettop) T)))
             (⊦= AUX_ETHREAD (code-of (λ () (res T top))))           ; nothing was pushed since
             (⊦= AUX_ETHREAD (code-of (λ () (res T 1))))
             (⊦= top ((lua-gettop) T))
             ((lua-pushinteger 100) T)
             ((lua-pushstring "x") T)
             (⊦= AUX_ETHREAD (code-of (λ () ((lua-xmove L 3) T))))    ; lua-xmove: the same height
             ((lua-xmove L 2) T)
             ((lua-xmove T 2) L)
             (⊦= AUX_ETHREAD (code-of (λ () (res T 3))))
             (set! r (res T 2))))                                    ; discarded, as C does
         (⊦= (list LUA_OK 1 8) (list (car r) (cadr r) ((lua-tointeger -1) T)))))
     (⊨ (> yielded 15))                                              ; every instruction of the chunk
     ;; the same when Lua's coroutine.resume ran it: the height is read by the first binding
     ((register! "hookyield" (λ (C)
                               ((lua-sethook (λ (H e ar) ((lua-yield 0) H)) LUA_MASKCOUNT ((lua-tointeger 2) C))
                                ((lua-tothread 1) C))
                               0))
      L)
     (set! yielded 0)
     (do ((count 1 (+ count 1))) ((= count 30))
       (run! L (string-append "CO = coroutine.create(function() " chunk " end)
                               hookyield(CO, " (number->string count) ") coroutine.resume(CO)"))
       ((lua-getglobal "CO") L)
       (let ((T ((lua-tothread -1) L)))
         ((lua-pop 1) L)
         ((lua-sethook #f 0 0) T)
         (when (= LUA_YIELD ((lua-status) T))
           (set! yielded (+ yielded 1))
           (⊦= AUX_ETHREAD (code-of (λ () (res T 1))))
           ((lua-pushinteger 100) T)
           (⊦= (list LUA_OK 1) (res T 1))
           (⊦= 8 ((lua-tointeger -1) T)))))
     (⊨ (> yielded 15))                                              ; every instruction of the chunk
     ;; resumed by Lua meanwhile, it yields from its hook again, in another frame or at another
     ;; height: the height is the one of the last yield, never the one a binding saw before
     (do ((k 0 (+ k 1))) ((= k 12))
       (run! L (string-append "CO = coroutine.create(function() " chunk " end)"))
       ((lua-getglobal "CO") L)
       (let ((T ((lua-tothread -1) L)))
         ((lua-pop 1) L)
         ((lua-sethook (λ (H e ar) ((lua-yield 0) H)) LUA_MASKCOUNT 1) T)
         (⊦= LUA_YIELD (car (res T 0)))
         (⊦= AUX_ETHREAD (code-of (λ () (res T 1))))                  ; seen by a binding: recorded
         (run! L (string-append "for i = 1, " (number->string k) " do coroutine.resume(CO) end"))
         (when (= LUA_YIELD ((lua-status) T))
           (⊦= AUX_ETHREAD (code-of (λ () (res T 1))))
           (⊦= AUX_ETHREAD (code-of (λ () ((lua-xmove L 1) T)))))
         ((lua-sethook #f 0 0) T)
         (let loop () (when (= LUA_YIELD ((lua-status) T)) ((lua-pushinteger 0) T) (res T 1) (loop)))
         (⊦= 8 ((lua-tointeger -1) T))))
     ;; dead in a Lua frame: repeated resumes never drain its registers
     (run! L "closed = 0")
     (let ((T ((lua-newthread) L)))
       ((lua-pop 1) L)
       ((luaL-loadstring "local x <close> = setmetatable({}, {__close = function() closed = closed + 1 end})
                           local y = nil; local z = y + 1") T)
       (⊦= LUA_ERRRUN (car (res T 0)))
       (let ((top ((lua-gettop) T)))
         (⊦= AUX_ETHREAD (code-of (λ () (res T top))))
         (⊦= AUX_ETHREAD (code-of (λ () (res T 2))))
         (⊦= top ((lua-gettop) T))
         (do ((k 0 (+ k 1))) ((= k 5))
           (⊦= (list LUA_ERRRUN 0) (res T 1)))                        ; the message replaces the error value
         (⊦= top ((lua-gettop) T))
         (⊦= "cannot resume dead coroutine" ((lua-tostring -1) T))
         ((lua-pushinteger 1) T)
         ((lua-pushinteger 2) T)
         (⊦= AUX_ETHREAD (code-of (λ () (res T 4))))
         (⊦= (list LUA_ERRRUN 0) (res T 3))
         (⊦= top ((lua-gettop) T))
         (⊦= (list LUA_ERRRUN 0) (res T 0))                          ; pushed on top
         (⊦= (+ top 1) ((lua-gettop) T))
         (⊦= (list LUA_ERRRUN 0) (res T 2))                          ; both messages: the same height
         (⊦= top ((lua-gettop) T))
         (⊦= 0 (global L "closed"))
         (⊦= LUA_ERRRUN ((lua-closethread L) T))
         (⊦= 1 (global L "closed"))))
     ;; the error value stays consumable through another luaR of the thread once the first is gone
     (let ((T ((lua-newthread) L)))
       ((lua-setglobal "DT") L)
       ((luaL-loadstring "local x <close> = setmetatable({}, {__close = function() end}) local y = nil; local z = y + 1") T)
       (⊦= LUA_ERRRUN (car (res T 0)))
       ((lua-getglobal "DT") L)
       (let ((T2 ((lua-tothread -1) L)) (top ((lua-gettop) T)))
         ((lua-pop 1) L)
         (set! T #f) (gc #t) (gc #t)
         ((lua-gc 'collect) L)                                      ; a drain point: T's anchor goes
         (⊦= (list LUA_ERRRUN 0) (res T2 1))
         (⊦= AUX_ETHREAD (code-of (λ () (res T2 2))))
         (⊦= top ((lua-gettop) T2))
         (⊦= LUA_ERRRUN ((lua-closethread L) T2))))
     ;; dead by Lua's coroutine.resume, which took its error value: only values pushed since
     (run! L "CO = coroutine.create(function()
                local x <close> = setmetatable({}, {__close = function() closed = closed + 10 end})
                local y = nil; local z = y + 1
              end)
              coroutine.resume(CO)")
     ((lua-getglobal "CO") L)
     (let ((T ((lua-tothread -1) L)))
       ((lua-pop 1) L)
       (⊦= AUX_ETHREAD (code-of (λ () (res T 1))))
       ((lua-pushinteger 1) T)
       (⊦= (list LUA_ERRRUN 0) (res T 1))
       (⊦= "cannot resume dead coroutine" ((lua-tostring -1) T))
       (⊦= LUA_ERRRUN ((lua-closethread L) T))
       (⊦= 11 (global L "closed")))
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)))

  ((test/s4-resume-scheme _)
   (let* ((L (fresh)) (T ((lua-newthread) L)) (kstatus #f) (yieldable '()) (seen #f))
     ((lua-pop 1) L)
     ;; a Scheme function yields (tail request); the next resume's values become its results
     ((register! "sy" (λ (C)
                        (set! yieldable (cons ((lua-isyieldable) C) yieldable))
                        ((lua-pushinteger (* 2 ((lua-tointeger 1) C))) C)
                        ((lua-pushstring "two") C)
                        ((lua-yield 2) C)))
      L)
     (run! L "function co1(a) local r1, r2 = sy(a) return r1 + r2 end")
     ((lua-getglobal "co1") T)
     ((lua-pushinteger 21) T)
     (let-values (((st n) ((lua-resume L 1) T)))
       (⊦= LUA_YIELD st)
       (⊦= 2 n)
       (⊦= 42 ((lua-tointeger -2) T))
       (⊦= "two" ((lua-tostring -1) T)))
     ((lua-pop 2) T)                                                  ; only the yielded values (C)
     ((lua-pushinteger 30) T) ((lua-pushinteger 12) T)
     (let-values (((st n) ((lua-resume L 2) T)))
       (⊦= LUA_OK st)
       (⊦= 1 n)
       (⊦= 42 ((lua-tointeger -1) T)))
     ((lua-settop 0) T)
     (⊦= '(#t) yieldable)
     ;; yieldk: the continuation runs on resume, with the resume values on top
     ((register! "syk" (λ (C)
                         ((lua-pushinteger 1) C)
                         ((lua-yieldk 1 (λ (status C)
                                          (set! kstatus status)
                                          ((lua-pushinteger (+ 100 ((lua-tointeger -1) C))) C)
                                          1))
                          C)))
      L)
     (run! L "function co2() return syk() * 2 end")
     ((lua-getglobal "co2") T)
     (let-values (((st n) ((lua-resume L 0) T))) (⊦= LUA_YIELD st) (⊦= 1 n))
     (⊦= 1 ((lua-gettop) T))                                          ; the yielded value, nothing else (C)
     ((lua-pop 1) T)
     ((lua-pushinteger 5) T)
     (let-values (((st n) ((lua-resume L 1) T))) (⊦= LUA_OK st) (⊦= 1 n))
     (⊦= 210 ((lua-tointeger -1) T))
     (⊦= LUA_YIELD kstatus)
     ((lua-settop 0) T)
     ;; yield across lua-call (no continuation): Lua's own refusal, raised in Scheme and caught
     ((register! "across" (λ (C)
                            ((lua-getglobal "coroutine") C)
                            ((lua-getfield -1 "yield") C)
                            (set! seen (condition-case (begin ((lua-call 0 0) C) 'no-error) (e (exn lua) (msg e))))
                            0))
      L)
     (run! L "function co3() across() return 'finished' end")
     ((lua-getglobal "co3") T)
     (let-values (((st n) ((lua-resume L 0) T)))
       (⊦= LUA_OK st)
       (⊦= "finished" ((lua-tostring -1) T)))
     (⊦= "attempt to yield across a C-call boundary" seen)
     ((lua-settop 0) T)
     ;; resuming a running coroutine from inside it (the callback's own view, or the thread luaR T,
     ;; the same thread): as C's lua_resume, the arguments are popped, "cannot resume non-suspended
     ;; coroutine" is pushed and the status is LUA_ERRRUN (C returns it: nothing is raised)
     (let ((self '()))
       ((register! "selfresume" (λ (C)
                                  ((lua-pushinteger 1) C) ((lua-pushinteger 2) C)
                                  (set! self (list (resume-answer #f 2 C)
                                                   (resume-answer L 0 T)
                                                   ((lua-gettop) C)))
                                  0))
        L)
       (run! L "function co4() selfresume() return 1 end")
       ((lua-getglobal "co4") T)
       (let-values (((st n) ((lua-resume L 0) T))) (⊦= LUA_OK st) (⊦= 1 n))
       (⊦= (list (list LUA_ERRRUN 0 1 "cannot resume non-suspended coroutine")
                 (list LUA_ERRRUN 0 1 "cannot resume non-suspended coroutine") 0)
           self)
       ((lua-settop 0) T))
     ;; a Scheme condition raised in the coroutine: the status, then the original object
     (let ((c (make-property-condition 'in-coroutine)))
       ((register! "thrower" (λ (C) (abort c))) L)
       ((lua-getglobal "thrower") T)
       (let-values (((st n) ((lua-resume L 0) T)))
         (⊦= LUA_ERRRUN st)
         (⊨ (eq? c ((lua-toobject -1) T))))
       ((lua-resetthread) T)
       ((lua-getglobal "thrower") T)
       (⊨ (eq? c (condition-case ((lua-check (lua-resume L 0)) T) (e () e))))
       ((lua-resetthread) T))
     ;; a continuation escape out of the coroutine, through lua-resume
     (let ((k #f))
       ((register! "esc" (λ (C) (k 'escaped))) L)
       (run! L "function co5() local x <close> = setmetatable({}, {__close = function() CLOSED = true end}) esc() end")
       ((lua-getglobal "co5") T)
       (⊦= 'escaped (call/cc (λ (kk) (set! k kk) ((lua-resume L 0) T) 'not-escaped)))
       (⊦= #f %pending-escape)
       (⊦= 0 %depth)
       (⊭ ((lua-isyieldable) L))
       (⊦= LUA_ERRRUN ((lua-status) T))                               ; the coroutine died by the escape
       (⊦= 0 ((lua-gettop) T))                                       ; its sentinels were dropped
       (⊦= LUA_ERRRUN ((lua-resetthread) T))                          ; the original error status (C)
       ((lua-settop 0) T)
       (⊦= #t (global L "CLOSED")))
     ;; a coroutine that yields many times, driven from Scheme
     (run! L "function counter(n) for i = 1, n do coroutine.yield(i) end return 'end' end")
     ((lua-getglobal "counter") T)
     ((lua-pushinteger 1000) T)
     (let loop ((nargs 1) (sum 0))
       (let-values (((st n) ((lua-resume L nargs) T)))
         (if (= st LUA_YIELD)
           (let ((i ((lua-tointeger -1) T))) ((lua-pop 1) T) (loop 0 (+ sum i)))
           (begin (⊦= LUA_OK st) (⊦= "end" ((lua-tostring -1) T)) (⊦= 500500 sum)))))
     ((lua-settop 0) T)
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)))

  ((test/s4-closethread _)
   (let* ((L (fresh)) (T ((lua-newthread) L)) (refusals '()))
     ((lua-pop 1) L)
     ((register! "note" (λ (C) (run! C "NOTED = (NOTED or 0) + 1") 0)) L)
     (run! L "CLOSED = 0
              function susp()
                local x <close> = setmetatable({}, {__close = function() CLOSED = CLOSED + 1 note() end})
                coroutine.yield(1)
              end
              function dies()
                local x <close> = setmetatable({}, {__close = function() CLOSED = CLOSED + 10 end})
                local n = nil; return n + 1
              end
              function badclose()
                local x <close> = setmetatable({}, {__close = function() error('close failed', 0) end})
                coroutine.yield()
              end")
     ;; a suspended coroutine: its pending __close runs (and calls Scheme), the thread is reusable
     ((lua-getglobal "susp") T)
     (let-values (((st n) ((lua-resume L 0) T))) (⊦= LUA_YIELD st))
     (⊦= LUA_OK ((lua-closethread L) T))
     (⊦= 1 (global L "CLOSED"))
     (⊦= 1 (global L "NOTED"))
     (⊦= LUA_OK ((lua-status) T))
     (⊦= 0 ((lua-gettop) T))
     ((lua-getglobal "susp") T)
     (let-values (((st n) ((lua-resume L 0) T))) (⊦= LUA_YIELD st))
     (⊦= LUA_OK ((lua-resetthread) T))                               ; the same, with from = NULL
     (⊦= 2 (global L "CLOSED"))
     ;; dead by an error raised in a Lua frame: its __close runs now, the original error comes back
     ((lua-getglobal "dies") T)
     (let-values (((st n) ((lua-resume L 0) T))) (⊦= LUA_ERRRUN st))
     (⊦= 2 (global L "CLOSED"))                                       ; still pending
     (⊦raises (exn lua api) ((lua-settop 0) T))                      ; lowering is refused (core, J5)
     (⊦= LUA_ERRRUN ((lua-closethread L) T))
     (⊦= 12 (global L "CLOSED"))
     (⊦= 1 ((lua-gettop) T))
     (⊨ (string-suffix? "attempt to perform arithmetic on a nil value (local 'n')" ((lua-tostring -1) T)))
     (⊦= LUA_OK ((lua-status) T))
     ((lua-settop 0) T)
     ;; a raising __close: its error is the status and the value
     ((lua-getglobal "badclose") T)
     (let-values (((st n) ((lua-resume L 0) T))) (⊦= LUA_YIELD st))
     (⊦= LUA_ERRRUN ((lua-closethread #f) T))
     (⊦= "close failed" ((lua-tostring -1) T))
     ((lua-settop 0) T)
     ;; a thread not started, and one already reset: nothing to close
     ((lua-getglobal "susp") T)
     (⊦= LUA_OK ((lua-closethread L) T))
     (⊦= 0 ((lua-gettop) T))
     (⊦= LUA_OK ((lua-closethread L) T))
     ;; refusals: the main thread, the calling thread, a running thread (from inside it), a normal one
     (⊦raises (exn lua api) ((lua-closethread #f) L))
     (⊦raises (exn lua api) ((lua-resetthread) L))
     (⊦raises (exn lua api) ((lua-closethread T) T))
     ((register! "closeme" (λ (C)
                             (set! refusals (cons (condition-case (begin ((lua-closethread #f) C) 'closed) (e (exn lua api) 'refused)) refusals))
                             (set! refusals (cons (condition-case (begin ((lua-closethread #f) T) 'closed) (e (exn lua api) 'refused)) refusals))
                             0))
      L)
     (run! L "function running() closeme() return 'ok' end")
     ((lua-getglobal "running") T)
     (let-values (((st n) ((lua-resume L 0) T))) (⊦= LUA_OK st))
     (⊦= '(refused refused) refusals)
     ((lua-settop 0) T)
     (set! refusals '())
     ;; T resumes an inner coroutine whose callback tries to close T (normal, not suspended)
     (run! L "function outer() local co = coroutine.create(function() closeme() end) coroutine.resume(co) return 'outer done' end")
     ((lua-getglobal "outer") T)
     (let-values (((st n) ((lua-resume L 0) T))) (⊦= LUA_OK st))
     (⊦= '(refused refused) refusals)                                ; its own view (running), and T (normal)
     ((lua-settop 0) T)
     (⊦= 0 ((lua-gettop) L))
     ;; an escape out of a __close run by lua-closethread
     (let ((k #f))
       ((register! "esc" (λ (C) (k 'escaped))) L)
       (run! L "function escclose() local x <close> = setmetatable({}, {__close = function() esc() end}) coroutine.yield() end")
       ((lua-getglobal "escclose") T)
       (let-values (((st n) ((lua-resume L 0) T))) (⊦= LUA_YIELD st))
       (⊦= 'escaped (call/cc (λ (kk) (set! k kk) ((lua-closethread L) T) 'not-escaped)))
       (⊦= #f %pending-escape)
       (⊦= 0 ((lua-gettop) T))
       (⊦= LUA_OK ((lua-status) T)))
     (lua-close L)))

  ((test/s4-warnings _)
   (let ((L (fresh)))
     ((lua-setwarnf 'collect) L)
     ((lua-warning "on by default" #f) L)                             ; 5.5: warnings start on
     ((lua-warning "a" #t) L)
     ((lua-warning "b" #t) L)
     ((lua-warning "c" #f) L)
     ((lua-warning "@off" #f) L)
     ((lua-warning "hidden" #f) L)
     ((lua-warning "hid" #t) L) ((lua-warning "den" #f) L)
     ((lua-warning "@on" #f) L)
     ((lua-warning "@unknown" #f) L)                                  ; an unknown control message: ignored
     ((lua-warning "x" #f) L)
     (⊦= '("on by default" "abc" "x") ((lua-warnings) L))
     ;; mixed with Lua's warn, a continued message finished by Lua
     ((lua-warning "from scheme, " #t) L)
     (run! L "warn('from lua')")
     (⊦= '("from scheme, from lua") ((lua-warnings) L))
     ;; UTF-8 text survives
     ((lua-warning "caffè ☕" #f) L)
     (⊦= '("caffè ☕") ((lua-warnings) L))
     ;; discarded while the mode is off
     ((lua-setwarnf 'off) L)
     ((lua-warning "dropped" #f) L)
     ((lua-setwarnf 'collect) L)
     (⊦= '() ((lua-warnings) L))
     ;; a message is a string without NUL; tocont is #t or #f only (0 would continue with Scheme
     ;; truthiness, the opposite of C): refused before anything is emitted
     (⊦raises (exn lua api) ((lua-warning "a\x0;b" #f) L))
     (⊦raises (exn lua api) ((lua-warning (string->utf8 "bytes") #f) L))
     (⊦raises (exn lua api) ((lua-warning 42 #f) L))
     (for-each (λ (c) (⊦= (list AUX_EARG 'lua-warning) (api-refusal (λ () ((lua-warning "never" c) L)))))
               (list 0 1 'anything))
     ((lua-warning "t" #t) L) ((lua-warning "ail" #f) L)
     (⊦= '("tail") ((lua-warnings) L))
     ;; from a thread and from a callback view: the same universe-wide queue
     (let ((T ((lua-newthread) L)))
       ((lua-pop 1) L)
       ((lua-warning "thread" #f) T))
     ((register! "w" (λ (C) ((lua-warning "view" #f) C) 0)) L)
     (run! L "w()")
     (⊦= '("thread" "view") ((lua-warnings) L))
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)))

  ;; lua-setwarnf installs a warning function, as lua_setwarnf does: 'off is C's NULL (nothing
  ;; reaches it, "@off" and pieces included, as with lua_newstate's), 'stderr and 'collect start on
  ;; and at a new message, whatever the function they replace was told
  ((test/s4-warnings-setwarnf-fresh _)
   (let ((L (lua-newstate 42)))
     ((lua-warning "@off" #f) L)                                  ; no warning function yet: dropped
     ((lua-setwarnf 'collect) L)
     ((lua-warning "after setwarnf collect" #f) L)
     (⊦= '("after setwarnf collect") ((lua-warnings) L))
     (lua-close L))
   (let ((L (lua-newstate 42)))
     ((lua-warning "discarded piece " #t) L)                     ; dropped too: no message to continue
     ((lua-setwarnf 'stderr) L)
     (⊦= "Lua warning: x\n" (with-stderr-captured (λ () ((lua-warning "x" #f) L))))
     ((lua-warning "@off" #f) L)
     ((lua-setwarnf 'stderr) L)                                   ; lauxlib's function, installed again
     (⊦= "Lua warning: y\n" (with-stderr-captured (λ () ((lua-warning "y" #f) L))))
     (lua-close L))
   (let ((M (fresh)))
     ((lua-setwarnf 'off) M)
     ((lua-warning "@off" #f) M)
     ((lua-warning "piece" #t) M)
     (run! M "warn('@off') warn('from lua, ', 'continued')")
     ((lua-setwarnf 'collect) M)
     ((lua-warning "on again" #f) M)
     (⊦= '("on again") ((lua-warnings) M))
     ((lua-warning "@off" #f) M)
     ((lua-setwarnf 'collect) M)                                  ; installed again: on
     ((lua-warning "on" #f) M)
     ;; the pieces 'collect had of an unfinished message are kept as a warning when it is replaced
     ((lua-warning "half " #t) M)
     ((lua-warning "done" #t) M)
     ((lua-setwarnf 'stderr) M)
     (⊦= "Lua warning: z\n" (with-stderr-captured (λ () ((lua-warning "z" #f) M))))
     (⊦= '("on" "half done") ((lua-warnings) M))
     (lua-close M)))

  ((test/s4-gc _)
   (let* ((L (fresh)) (fins 0) (in-fin '()))
     ((register! "fin" (λ (C) (set! fins (+ fins 1)) 0)) L)
     (run! L "function garbage(n) for i = 1, n do setmetatable({}, {__gc = function() fin() end}) end end")
     ;; counters and the running flag; codes and names are the same options
     (let ((kb ((lua-gc 'count) L)) (b ((lua-gc 'countb) L)))
       (⊨ (> kb 0))
       (⊨ (and (>= b 0) (< b 1024)))
       (⊨ (fixnum? ((lua-gc 3) L))))                                  ; LUA_GCCOUNT
     (⊦= 1 ((lua-gc 'isrunning) L))
     (⊦= 0 ((lua-gc 'stop) L))
     (⊦= 0 ((lua-gc 'isrunning) L))
     (⊦= 0 ((lua-gc 'restart) L))
     (⊦= 1 ((lua-gc 6) L))                                            ; LUA_GCISRUNNING
     ;; a full collection runs the (Scheme) finalizers
     (run! L "garbage(20)")
     (set! fins 0)
     (⊦= 0 ((lua-gc 'collect) L))
     (⊦= 20 fins)
     ;; steps: 0 is one basic step; the cycle ends eventually, running the finalizers
     (run! L "garbage(10)")
     (set! fins 0)
     (let loop ((i 0)) (when (and (< i 10000) (< fins 10)) ((lua-gc 'step 0) L) (loop (+ i 1))))
     (⊦= 10 fins)
     (⊨ (and (memv ((lua-gc 'step 1000000) L) '(0 1)) #t))
     ;; generational mode (J3: entering it runs the pending finalizers); the previous mode is answered
     (run! L "garbage(50)")
     (set! fins 0)
     (⊦= 8 ((lua-gc 'gen) L))                                         ; LUA_GCINC: 5.5 starts incremental
     (⊦= 50 fins)
     (⊦= 7 ((lua-gc 'gen) L))                                         ; LUA_GCGEN
     (⊦= 7 ((lua-gc 'inc) L))
     (⊦= 8 ((lua-gc 'inc) L))
     ;; parameters: a negative value only reads, a set answers the previous value
     (let ((pause ((lua-gc 'param 'pause -1) L)))
       (⊨ (> pause 0))
       (⊦= pause ((lua-gc 'param 'pause -1) L))
       (⊦= pause ((lua-gc 'param 3 300) L))                            ; LUA_GCPPAUSE
       (let ((new ((lua-gc 'param 'pause -1) L)))
         (⊨ (< (abs (- new 300)) 20))                                 ; stored in Lua's compact encoding
         (⊦= new ((lua-gc 'param 'pause pause) L))))
     (for-each (λ (p) (⊨ (>= ((lua-gc 'param p -1) L) 0))) '(minormul majorminor minormajor pause stepmul stepsize))
     ;; -1 inside a finalizer, as C
     ((register! "gcinfin" (λ (C) (set! in-fin (cons ((lua-gc 'count) C) in-fin)) 0)) L)
     (run! L "setmetatable({}, {__gc = function() gcinfin() end})")
     ((lua-gc 'collect) L)
     (⊦= '(-1) in-fin)
     ;; on a suspended thread the collector runs all the same
     (let ((T ((lua-newthread) L)))
       ((lua-pop 1) L)
       (run! L "function y() coroutine.yield() end")
       ((lua-getglobal "y") T)
       (let-values (((st n) ((lua-resume L 0) T))) (⊦= LUA_YIELD st))
       (run! L "garbage(5)")
       (set! fins 0)
       (⊦= 0 ((lua-gc 'collect) T))
       (⊦= 5 fins)
       (⊨ (> ((lua-gc 'count) T) 0)))
     ;; an escape out of a finalizer run by lua-gc completes; Lua turned the sentinel into a warning
     (let ((k #f))
       ((lua-setwarnf 'collect) L)
       ((register! "escfin" (λ (C) (k 'escaped))) L)
       (run! L "setmetatable({}, {__gc = function() escfin() end})")
       (⊦= 'escaped (call/cc (λ (kk) (set! k kk) ((lua-gc 'collect) L) 'not-escaped)))
       (⊦= #f %pending-escape)
       ((lua-warnings) L))
     ;; api refusals
     (⊦raises (exn lua api) ((lua-gc 'bogus) L))
     (⊦raises (exn lua api) ((lua-gc 42) L))
     (⊦raises (exn lua api) ((lua-gc "count") L))
     (⊦raises (exn lua api) ((lua-gc 'count 1) L))
     (⊦raises (exn lua api) ((lua-gc 'step) L))
     (⊦raises (exn lua api) ((lua-gc 'step -1) L))
     (⊦raises (exn lua api) ((lua-gc 'step 1.5) L))
     (⊦raises (exn lua api) ((lua-gc 'param 'pause) L))
     (⊦raises (exn lua api) ((lua-gc 'param 'bogus 1) L))
     (⊦raises (exn lua api) ((lua-gc 'param 6 1) L))                 ; LUA_GCPN
     (⊦raises (exn lua api) ((lua-gc 'param 'pause 4294967296) L))
     (⊦raises (exn lua api) ((lua-gc 'collect 1) L))
     (⊦= 1 ((lua-gc 'isrunning) L))
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)))

  ;; memory errors from pushes, from Lua code, from loads and inside coroutines; then recovery
  ((test/s4-memory _)
   (let* ((L (fresh)) (T ((lua-newthread) L)))
     ((lua-pop 1) L)
     (run! L "function hog() local t = {} for i = 1, 1e7 do t[i] = string.rep('x', 100) .. i end end")
     ((lua-gc 'collect) L)
     ((lua-setmemorylimit (+ (* 1024 ((lua-gc 'count) L)) 300000)) L)
     (let ((e (condition-case ((lua-pushlstring (make-string 1000000 #\a)) L) (e (exn lua) e))))
       (⊦= 'errmem (lua-condition-status e))
       (⊦= "not enough memory" (msg e)))
     (⊦= 0 ((lua-gettop) L))
     (⊦= LUA_ERRMEM ((luaL-dostring "S = string.rep('x', 1000000)") L))
     (⊦= "not enough memory" ((lua-tostring -1) L))
     ((lua-pop 1) L)
     (let ((big (string-append "return '" (make-string 1000000 #\z) "'")))
       (⊦= LUA_ERRMEM ((lua-load big "=big" #f) L))
       ((lua-pop 1) L))
     ((lua-getglobal "hog") L)
     (⊦= LUA_ERRMEM ((lua-pcall 0 0 0) L))
     ((lua-pop 1) L)
     ;; raising bindings: (exn lua) errmem, with Lua's message
     ((lua-getglobal "hog") L)
     ((lua-gc 'collect) L)
     ((lua-setmemorylimit (+ (* 1024 ((lua-gc 'count) L)) ((lua-gc 'countb) L) 1)) L)
     (let ((e (condition-case ((lua-dump #f) L) (e (exn lua) e))))
       (⊦= 'errmem (lua-condition-status e))
       (⊦= "not enough memory" (msg e)))
     (⊦= 1 ((lua-gettop) L))                                          ; the function stays
     (⊦= "not enough memory" (condition-case ((lua-newthread) L) (e (exn lua) (msg e))))
     (⊦= 1 ((lua-gettop) L))
     ((lua-setmemorylimit (+ (* 1024 ((lua-gc 'count) L)) 300000)) L)
     (⊨ (bytevector? ((lua-dump #f) L)))
     ((lua-pop 1) L)
     ((lua-getglobal "hog") T)
     (let-values (((st n) ((lua-resume L 0) T)))
       (⊦= LUA_ERRMEM st)
       (⊦= "not enough memory" ((lua-tostring -1) T)))
     (⊦= LUA_ERRMEM ((lua-resetthread) T))                            ; the original error
     ((lua-settop 0) T)
     ((lua-gc 'collect) L)
     ;; raising the limit: everything works again
     ((lua-setmemorylimit 0) L)
     ((lua-pushlstring (make-string 1000000 #\a)) L)
     (⊦= 1000000 (bytevector-length ((lua-tobytevector -1) L)))
     ((lua-pop 1) L)
     (run! L "S = #string.rep('x', 1000000)")
     (⊦= 1000000 (global L "S"))
     (run! L "function small() coroutine.yield(#string.rep('y', 100000)) end")
     ((lua-getglobal "small") T)
     (let-values (((st n) ((lua-resume L 0) T))) (⊦= LUA_YIELD st) (⊦= 100000 ((lua-tointeger -1) T)))
     (lua-close L)))

  ;; resuming a running or normal coroutine answers as C's lua_resume (its resume_error): the
  ;; arguments are popped, "cannot resume non-suspended coroutine" is pushed, LUA_ERRRUN and 0
  ;; results; nothing raises.  lua_resume is never called for it (an out-of-memory in C's
  ;; resume_error would be thrown through the coroutine's own active resume, below the callback's
  ;; C frames): the message is pushed by a protected op of the binding, so out of memory it is an
  ;; (exn lua) 'errmem, raised in Scheme.  Closing such a thread is still refused, and so is
  ;; popping the arguments below a hook's floor.  The main thread cannot be resumed at all.
  ((test/s4-resume-active _)
   (let ((L (fresh)) (A #f) (seen '()) (hooked '()))
     ((register! "grab" (λ (C) (set! A ((lua-tothread 1) C)) 0)) L)
     ((register! "tryA" (λ (C)
                          (let ((topA ((lua-gettop) A)) (topC ((lua-gettop) C)))
                            ((lua-pushinteger 1) A) ((lua-pushinteger 2) A)
                            ((lua-pushstring "arg") C)
                            (set! seen (list (resume-answer #f 2 A)
                                             (- ((lua-gettop) A) topA)
                                             (resume-answer C 0 A)
                                             (api-outcome (λ () ((lua-closethread #f) A)))
                                             (resume-answer #f 1 C)
                                             (- ((lua-gettop) C) topC)
                                             (api-outcome (λ () ((lua-closethread #f) C)))
                                             (api-outcome (λ () ((lua-resume #f 0) L)))
                                             (api-outcome (λ () ((lua-resume C (+ ((lua-gettop) A) 1)) A)))))) ; more args than values
                          0))
      L)
     ((register! "oom" (λ (C)
                         ((lua-setmemorylimit 1) C)
                         (let ((r (condition-case (begin ((lua-resume #f 0) C) 'no-raise)
                                    (e (exn lua) (get-condition-property e 'lua 'status)))))
                           ((lua-setmemorylimit 0) C)
                           (set! seen (cons r seen)))
                         0))
      L)
     ;; from a hook: the hooked function's registers are below the floor
     ((register! "sethk" (λ (C)
                           ((lua-sethook (λ (H ev ar)
                                           ((lua-sethook #f 0 0) H)
                                           (set! hooked (list (api-outcome (λ () ((lua-resume #f 1) H)))
                                                              (begin ((lua-pushnil) H) (resume-answer #f 1 H)))))
                                         '(line) 0)
                            C)
                           0))
      L)
     (run! L "A = coroutine.create(function()
                grab(coroutine.running())
                local B = coroutine.create(function() tryA() end)    -- A is normal meanwhile
                assert(coroutine.resume(B))
                return 'A done'
              end)
              OK1, V1 = coroutine.resume(A)
              OK2, V2 = coroutine.resume(coroutine.create(function() oom() return 'finished' end))
              OK3, V3 = coroutine.resume(coroutine.create(function() local a = 1 sethk()
                local b = 2
                return a + b end))")
     (let ((answer (list LUA_ERRRUN 0 1 "cannot resume non-suspended coroutine")))
       (⊦= (list 'errmem answer 0 answer 'refused answer 0 'refused 'refused 'refused) seen))
     (⊦= (list 'refused (list LUA_ERRRUN 0 1 "cannot resume non-suspended coroutine")) hooked)
     (⊦= "A done" (global L "V1"))
     (⊦= "finished" (global L "V2"))
     (⊦= 3 (global L "V3"))
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)))

  ;; while a lua-load reader runs, the loading thread is inside the load, as in a C function
  ;; calling lua_load: the reader sees it in an empty frame of its own (any luaR of the thread
  ;; does), may push, pop and leave values there (dropped), and the thread is not idle, so neither
  ;; the bindings nor Lua's coroutine library can resume or reset it under the parser
  ((test/s4-load-reader-frame _)
   (let* ((L (fresh)) (T ((lua-newthread) L)) (n 0) (seen '()) (M #f))
     ((lua-setglobal "T") L)
     ((lua-pushstring "below") T)                                        ; a value of T's own
     ((lua-rawgeti LUA_REGISTRYINDEX LUA_RIDX_MAINTHREAD) L)
     (set! M ((lua-tothread -1) L))                                      ; another luaR of the main thread
     ((lua-pop 1) L)
     (define (reader)
       (set! n (+ n 1))
       (case n
         ((1) "local a = 'first piece' ")
         ((2) (set! seen
                (list ((lua-gettop) T)                                   ; empty: "below" is out of sight
                      (api-outcome (λ () ((lua-pushinteger 9) T) ((lua-pushinteger 8) T) ((lua-pop 2) T)))
                      (api-outcome (λ () ((lua-settop 0) T)))
                      (resume-answer #f 0 T)
                      (resume-answer L 0 T)
                      (api-outcome (λ () ((lua-closethread #f) T)))
                      (api-outcome (λ () ((lua-resetthread) T)))
                      (begin (run! L "local ok, e = pcall(coroutine.close, T) CLOSE = e
                                      local ok2, e2 = coroutine.resume(T) RESUME = e2
                                      STATUS = coroutine.status(T)")
                             (list (global L "STATUS") (global L "CLOSE") (global L "RESUME")))))
              (run! L "collectgarbage() collectgarbage()")
              ((lua-pushstring "left behind") T)                        ; dropped with the reader's frame
              "local b = 'second piece' .. string.rep('x', 100) return a .. b")
         (else #f)))
     (⊦= LUA_OK ((lua-load reader "=chunk" #f) T))
     (⊦= (list 0 'ok 'ok
               (list LUA_ERRRUN 0 1 "cannot resume non-suspended coroutine")   ; as C: T is not suspended
               (list LUA_ERRRUN 0 1 "cannot resume non-suspended coroutine")
               'refused 'refused
               (list "normal" "cannot close a normal coroutine" "cannot resume non-suspended coroutine"))
         seen)
     (⊦= 2 ((lua-gettop) T))                                             ; "below" and the chunk
     (⊦= "below" ((lua-tostring 1) T))
     ((lua-call 0 1) T)
     (⊦= (string-append "first piece" "second piece" (make-string 100 #\x)) ((lua-tostring -1) T))
     ((lua-settop 0) T)
     ;; on the main thread too, through any of its luaRs
     (set! n 0)
     (set! seen '())
     (let ((reader (λ ()
                     (set! n (+ n 1))
                     (case n
                       ((1) "local x = 1 ")
                       ((2) (set! seen (list ((lua-gettop) L) ((lua-gettop) M))) ((lua-pushinteger 1) M) "return x")
                       (else #f)))))
       ((lua-pushinteger 5) L)
       (⊦= LUA_OK ((lua-load reader "=main" #f) L))
       (⊦= '(0 0) seen)
       (⊦= 2 ((lua-gettop) L))
       ((lua-settop 0) L))
     ;; the parser works on a thread of its own: a Lua finalizer running there (a collection step
     ;; of the parse) may keep it, but no binding can use it, and Lua cannot close or resume it
     (run! L "collectgarbage('stop') collectgarbage('param', 'pause', 0)
              for i = 1, 50 do setmetatable({}, {__gc = function() P = P or coroutine.running() end}) end
              collectgarbage('restart')")
     (set! n 0)
     (set! seen #f)
     (let ((reader (λ ()
                     (set! n (+ n 1))
                     (cond ((< n 5) (apply string-append (map (λ (i) (string-append "X = 'k" (number->string n) "_" (number->string i) "' ")) (iota 50))))
                           ((= n 5)
                            ((lua-getglobal "P") L)
                            (let ((P ((lua-tothread -1) L)))
                              ((lua-pop 1) L)
                              (set! seen
                                (list (and P (condition-case ((lua-gettop) P) (e (exn lua api) (get-condition-property e 'lua 'code))))
                                      (and P (api-outcome (λ () ((lua-pushinteger 1) P))))
                                      (begin (run! L "local ok, e = pcall(coroutine.close, P) CLOSEP = e") (global L "CLOSEP")))))
                            "return 'parsed'")
                           (else #f)))))
       (⊦= LUA_OK ((lua-load reader "=c" #f) L))
       (⊦= (list AUX_ELOADING 'refused "cannot close a normal coroutine") seen)
       ((lua-call 0 1) L)
       (⊦= "parsed" ((lua-tostring -1) L))
       ((lua-pop 1) L)
       (run! L "PS = coroutine.status(P)")
       (⊦= "dead" (global L "PS")))                                      ; an ordinary thread again
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)))

  ;; the parser thread is pinned out of Lua's reach: Lua code the reader runs on the loading thread
  ;; finds it in no C frame slot (debug.getlocal) and in no registry table (debug.getregistry), so
  ;; it cannot drop it (debug.setlocal, or clearing the registry) and have a collection free it under
  ;; the parser, in incremental or generational mode; once the load is over, the pin lets it go
  ((test/s4-load-parser-pinned _)
   (define scan "
     local found, cleared = {}, 0
     local lvl = 1
     while debug.getinfo(lvl, 'f') do                        -- every frame of the loading thread
       local i = 1
       while true do
         local name, v = debug.getlocal(lvl, i)
         if not name then break end
         if type(v) == 'thread' then found[#found + 1] = lvl .. ':' .. i; debug.setlocal(lvl, i, nil); cleared = cleared + 1 end
         i = i + 1
       end
       lvl = lvl + 1
     end
     local R, main = debug.getregistry(), coroutine.running()
     for k, v in pairs(R) do                                 -- the registry, one table deep
       if type(v) == 'thread' and v ~= main then R[k] = nil; cleared = cleared + 1 end
       if type(v) == 'table' then
         for k2, v2 in pairs(v) do if type(v2) == 'thread' and v2 ~= main then v[k2] = nil; cleared = cleared + 1 end end
       end
     end
     SCAN = #found; CLEARED = cleared
     collectgarbage() collectgarbage()
     local t = {} for i = 1, 3000 do t[i] = {i, tostring(i)} end  -- memory a freed thread would be reused for
     t = nil collectgarbage() collectgarbage()")
   (for-each
     (λ (mode)
       (let ((L (fresh)) (n 0) (scans '()))
         (run! L (string-append "collectgarbage('" mode "')"))
         (let ((reader (λ ()
                         (set! n (+ n 1))
                         (cond
                           ((= n 1) "local a = 1 local b = {")
                           ((<= n 6) (run! L scan) (set! scans (cons (list (global L "SCAN") (global L "CLEARED")) scans))
                                     (string-append (make-string 2000 #\space) (number->string n) ", "))
                           ((= n 7) "'x' .. 'y' } local function f() return a end return #b, f(), b[5]")
                           (else #f)))))
           (⊦= LUA_OK ((lua-load reader "=pinned" "t") L))
           (⊦= '((0 0) (0 0) (0 0) (0 0) (0 0)) scans)                               ; nothing of the load is reachable
           ((lua-call 0 3) L)
           (⊦= '(6 1 6) (list ((lua-tointeger 1) L) ((lua-tointeger 2) L) ((lua-tointeger 3) L)))
           ((lua-settop 0) L))
         ;; a nested load, from inside a reader, is pinned on its own
         (set! n 0)
         (let* ((inner #f)
                (reader (λ ()
                          (set! n (+ n 1))
                          (case n
                            ((1) (let ((m 0))
                                   (set! inner ((lua-load (λ () (set! m (+ m 1))
                                                                (case m ((1) "return ") ((2) (run! L scan) "'inner'") (else #f)))
                                                          "=inner" "t") L))
                                   ((lua-pop 1) L))
                                 (run! L scan) "return 'outer'")
                            (else #f)))))
           (⊦= LUA_OK ((lua-load reader "=outer" "t") L))
           (⊦= LUA_OK inner)
           (⊦= '(0 0) (list (global L "SCAN") (global L "CLEARED")))
           ((lua-call 0 1) L)
           (⊦= "outer" ((lua-tostring -1) L))
           ((lua-settop 0) L))
         ;; the pins end with their loads: many loads leave no parser thread behind
         (let ((kb (λ () (run! L "collectgarbage() collectgarbage() collectgarbage() KB = collectgarbage('count')") (global L "KB")))
               (load-once (λ () (let ((once #t)) ((lua-load (λ () (and once (begin (set! once #f) "return 1"))) "=l" "t") L)) ((lua-pop 1) L))))
           (load-once)
           (let ((before (kb)))
             (do ((i 0 (+ i 1))) ((= i 500)) (load-once))
             (⊨ (< (- (kb) before) 64.0))))                                ; 500 kept threads: well above 300 KB
         (⊦= 0 ((lua-gettop) L))
         (lua-close L)))
     '("incremental" "generational")))

  ;; Lua -> Scheme -> lua-resume (from #f) recursion ends with "C stack overflow" within Lua's
  ;; C-call limit (200): the count carries over from level to level
  ((test/s4-resume-recursion-bounded _)
   (let ((L (fresh)))
     (set! resume-depth 0)
     (set! resume-deepest #f)
     ((register! "nextlevel" resume-next-level) L)
     (run! L "function body()
                local function nest(n)
                  if n == 0 then return nextlevel() end
                  local ok, v = pcall(nest, n - 1)
                  if not ok then error(v, 0) end
                  return v
                end
                return nest(3)
              end")
     ((lua-getglobal "body") L)
     ((lua-call 0 1) L)
     (⊨ (pair? resume-deepest))
     (⊨ (string-suffix? "C stack overflow" (cadr resume-deepest)))
     (⊨ (< 10 (car resume-deepest) 60))
     ((lua-settop 0) L)
     (lua-close L)))

  ;; Lua 5.5's mode "B" (fixed buffer) is refused by every loading binding: the loaded function
  ;; would keep pointers into a buffer that does not outlive the call (a moving Scheme bytevector,
  ;; a copy freed at once, luaL_loadfilex's own C frame); "b", "t" and "bt" load as usual
  ((test/s4-load-mode-B _)
   (let* ((L (fresh)) (path (create-temporary-file)))
     (run! L "function src(a) local s = 'a long string constant, longer than the short-string limit' return s .. a end")
     ((lua-getglobal "src") L)
     (let ((bin ((lua-dump #f) L)))
       ((lua-pop 1) L)
       ((lua-pushstring path) L) ((lua-setglobal "PATH") L)
       (run! L "local f = assert(io.open(PATH, 'wb')) f:write(string.dump(src)) f:close()")
       (for-each
         (λ (mode)
           (⊦= (list 'api AUX_EARG) (condition-case ((luaL-loadbufferx bin "=b" mode) L) (e (exn lua api) (list 'api (get-condition-property e 'lua 'code)))))
           (⊦raises (exn lua api) ((lua-load bin "=b" mode) L))
           (⊦raises (exn lua api) ((lua-load (let ((d #f)) (λ () (and (not d) (begin (set! d #t) bin)))) "=b" mode) L))
           (⊦raises (exn lua api) ((luaL-loadfilex path mode) L)))
         '("B" "bB" "Bt" "btB"))
       (⊦= 0 ((lua-gettop) L))
       (for-each
         (λ (load)
           (⊦= LUA_OK (load))
           ((lua-pushstring "!") L)
           ((lua-call 1 1) L)
           (⊦= "a long string constant, longer than the short-string limit!" ((lua-tostring -1) L))
           ((lua-pop 1) L))
         (list (λ () ((luaL-loadbufferx bin "=b" "b") L))
               (λ () ((lua-load bin "=b" "bt") L))
               (λ () ((lua-load (let ((d #f)) (λ () (and (not d) (begin (set! d #t) bin)))) "=b" "b") L))
               (λ () ((luaL-loadfilex path "b") L))
               (λ () ((luaL-loadfile path) L)))))
     (delete-file path)
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)))

  ;; Lua 5.5's coroutine.close() of the RUNNING coroutine longjmps to that thread's OUTERMOST
  ;; protected call.  When a binding runs Lua on the thread (lua-pcall, a P op, lua-gc's finalizers)
  ;; that jump would cross the binding's C stub and the Scheme callback frames below it: the
  ;; module's coroutine.close refuses it with a Lua error instead.
  ((test/s4-selfclose-refused-in-callback _)
   (let ((L (fresh)) (seen #f))
     ;; a Scheme callback on co runs Lua on co itself (luaL-dostring = load + lua-pcall)
     ((register! "cb" (λ (C) (let ((st ((luaL-dostring "coroutine.close()") C)))
                               (set! seen (list st ((lua-tostring -1) C)))
                               ((lua-pop 1) C)
                               0)))
      L)
     (run! L "co = coroutine.create(function() cb() return 'finished' end)
              OK, RES = coroutine.resume(co)")
     (⊦= LUA_ERRRUN (car seen))
     (⊨ (string-suffix? "cannot close a running coroutine while a Scheme binding runs Lua on it" (cadr seen)))
     (⊦= '(#t "finished") (list (global L "OK") (global L "RES")))  ; the coroutine went on
     ;; a P op whose metamethod closes the running coroutine, co resumed from Scheme
     (set! seen #f)
     (run! L "t = setmetatable({}, {__index = function() coroutine.close() end})")
     ((register! "cb2" (λ (C) ((lua-getglobal "t") C)
                          (set! seen (condition-case (begin ((lua-getfield -1 "x") C) 'no-raise)
                                       (e (exn lua) (msg e))))
                          ((lua-pushstring "after") C)
                          1))
      L)
     (let ((T ((lua-newthread) L)))
       ((lua-getglobal "cb2") T)
       (⊦= (list LUA_OK 1) (call-with-values (λ () ((lua-resume #f 0) T)) list))
       (⊦= "after" ((lua-tostring -1) T)))
     ((lua-pop 1) L)
     (⊨ (string-suffix? "cannot close a running coroutine while a Scheme binding runs Lua on it" seen))
     ;; lua-gc from a callback on co: a finalizer running on co closes co
     (run! L "GCMSG = nil
              function mkgarbage() setmetatable({}, {__gc = function() local ok, e = pcall(coroutine.close) GCMSG = e end}) end
              co = coroutine.create(function() cbgc() return 'finished' end)")
     ;; the garbage is made under the callback (all Lua on co then runs under a binding), then collected
     ((register! "cbgc" (λ (C) ((lua-check (luaL-dostring "mkgarbage()")) C) ((lua-gc 'collect) C) 0)) L)
     (run! L "OK, RES = coroutine.resume(co)")
     (⊦= '(#t "finished") (list (global L "OK") (global L "RES")))
     (⊨ (string-suffix? "cannot close a running coroutine while a Scheme binding runs Lua on it" (global L "GCMSG")))
     (⊦= 0 ((lua-gettop) L))
     (gc #t)
     (run! L "collectgarbage()")
     (lua-close L)))

  ;; the same without any callback: lua-pcall on a thread used as a plain stack.  C would reset the
  ;; thread (shrinking its stack) under the binding's lua_pcall; the module refuses, the stack is kept
  ((test/s4-selfclose-refused-on-plain-thread _)
   (let* ((L (fresh)) (T ((lua-newthread) L)))
     (do ((i 0 (+ i 1))) ((= i 150)) ((lua-pushinteger i) T))
     ((luaL-loadstring "coroutine.close()") T)
     (⊦= LUA_ERRRUN ((lua-pcall 0 0 0) T))
     (⊦= 151 ((lua-gettop) T))
     (⊨ (string-suffix? "cannot close a running coroutine while a Scheme binding runs Lua on it" ((lua-tostring -1) T)))
     (⊦= 149 ((lua-tointeger 150) T))
     ((lua-pop 1) T)
     ;; a pending __close that raises: its error wins, as with any error, and the stack is intact
     ((luaL-loadstring "local x <close> = setmetatable({}, {__close = function() error('from __close', 0) end})
                        coroutine.close()") T)
     (⊦= LUA_ERRRUN ((lua-pcall 0 0 0) T))
     (⊦= '(151 "from __close") (list ((lua-gettop) T) ((lua-tostring -1) T)))
     ((lua-settop 150) T)
     ;; a P op: its metamethod closes the thread the op runs on
     (run! L "tc = setmetatable({}, {__index = function() coroutine.close() end})")
     ((lua-getglobal "tc") T)
     (⊨ (string-suffix? "cannot close a running coroutine while a Scheme binding runs Lua on it"
                        (condition-case (begin ((lua-getfield -1 "x") T) "no raise") (e (exn lua) (msg e)))))
     (⊦= 151 ((lua-gettop) T))
     ((lua-settop 0) T)
     ((lua-pop 1) L)
     ;; closing the main thread keeps lcorolib's own message
     (⊨ (string-suffix? "cannot close main thread" (condition-case (run! L "coroutine.close()") (e (exn lua) (msg e)))))
     (⊦= 0 ((lua-gettop) L))
     (gc #t)
     (⊦= 1 (begin (run! L "collectgarbage() X = 1") (global L "X")))
     (lua-close L)))

  ;; the running MAIN thread is recognized by its address, never by registry[LUA_RIDX_MAINTHREAD]
  ;; (lcorolib's test), which Lua code or the bound API can overwrite: with the slot forged,
  ;; lcorolib would close the main thread under the protected call running it (a longjmp to its
  ;; outermost protected call: across a callback's C frames, or into a pcall whose stack the reset
  ;; shrank).  The close is refused with lcorolib's own message, and every stack stays intact.
  ((test/s4-mainclose-forged-registry _)
   (let ((L (fresh)) (seen #f))
     ;; from a callback: Lua runs on the main thread again, under luaL-dostring
     ((register! "cb" (λ (C) (let ((st ((luaL-dostring "debug.getregistry()[3] = false coroutine.close()") C)))
                               (set! seen (list st ((lua-tostring -1) C) ((lua-gettop) C)))
                               ((lua-pop 1) C)
                               0)))
      L)
     (run! L "cb() AFTER = 'after'")
     (⊦= (list LUA_ERRRUN #t 1) (list (car seen) (string-suffix? ":1: cannot close main thread" (cadr seen)) (caddr seen)))
     (⊦= "after" (global L "AFTER"))
     ;; with no callback: a deep stack under lua-pcall, a raising __close pending
     (do ((i 0 (+ i 1))) ((= i 150)) ((lua-pushinteger i) L))
     ((luaL-loadstring "debug.getregistry()[3] = false
                        local x <close> = setmetatable({}, {__close = function() error('from __close', 0) end})
                        coroutine.close()") L)
     (⊦= LUA_ERRRUN ((lua-pcall 0 0 0) L))
     (⊦= '(151 "from __close") (list ((lua-gettop) L) ((lua-tostring -1) L)))   ; the pending __close ran, as with any error
     (⊦= 149 ((lua-tointeger 150) L))
     ((lua-settop 0) L)
     ;; the slot forged through the bound API, coroutine.close given the main thread explicitly
     ((lua-pushboolean #f) L)
     ((lua-rawseti LUA_REGISTRYINDEX LUA_RIDX_MAINTHREAD) L)
     ((luaL-loadstring "coroutine.close(coroutine.running())") L)
     (⊦= LUA_ERRRUN ((lua-pcall 0 0 0) L))
     (⊨ (string-suffix? ":1: cannot close main thread" ((lua-tostring -1) L)))
     ((lua-pop 1) L)
     ;; the module's own coroutines are unaffected
     (run! L "local co = coroutine.create(function() coroutine.yield(1) end) coroutine.resume(co)
              assert(coroutine.close(co)) R = coroutine.status(co)")
     (⊦= "dead" (global L "R"))
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)))

  ;; where no binding runs Lua on it, a coroutine closes itself as in C (see sc.c-like reference:
  ;; lua_resume answers LUA_OK and no result; a raising __close makes it LUA_ERRRUN)
  ((test/s4-selfclose-allowed _)
   (let ((L (fresh)))
     (define (resume-chunk code)
       (let ((T ((lua-newthread) L)))
         ((luaL-loadstring code) T)
         (let-values (((st n) ((lua-resume #f 0) T)))
           (begin1 (list st n ((lua-gettop) T) (and (> ((lua-gettop) T) 0) ((lua-tostring -1) T)))
                   ((lua-pop 1) L)))))
     (⊦= (list LUA_OK 0 0 #f) (resume-chunk "local a = 1 coroutine.close() return 5"))
     (⊦= LUA_ERRRUN (car (resume-chunk "local x <close> = setmetatable({}, {__close = function() error('from __close', 0) end})
                                        coroutine.close() return 5")))
     (⊦= "from __close" (cadddr (resume-chunk "local x <close> = setmetatable({}, {__close = function() error('from __close', 0) end})
                                               coroutine.close() return 5")))
     ;; resumed from Lua, under lua-pcall on the main thread: the coroutine is not the pcall's thread
     (run! L "co = coroutine.create(function() coroutine.close() return 5 end)
              R = table.pack(coroutine.resume(co)) N, OK, ST = R.n, R[1], coroutine.status(co)")
     (⊦= '(1 #t "dead") (list (global L "N") (global L "OK") (global L "ST")))
     ;; a callback that returned before the close leaves no binding on the thread
     ((register! "cb" (λ (C) 0)) L)
     (run! L "co = coroutine.create(function() cb() coroutine.close() end)
              OK = coroutine.resume(co) ST = coroutine.status(co)")
     (⊦= '(#t "dead") (list (global L "OK") (global L "ST")))
     ;; closing another (suspended) coroutine is untouched
     (run! L "CL = 0 co = coroutine.create(function() local x <close> = setmetatable({}, {__close = function() CL = 1 end}) coroutine.yield() end)
              coroutine.resume(co) OK = coroutine.close(co)")
     (⊦= '(#t 1) (list (global L "OK") (global L "CL")))
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)))

  ;; the coroutine library of every opener is the module's: luaopen-coroutine (luaL-requiref),
  ;; luaL-openselectedlibs (loaded or preloaded then required) and luaL-openlibs
  ((test/s4-selfclose-every-opener _)
   (let ()
   (define (refused-in S)
     (let ((T ((lua-newthread) S)))
       ((luaL-loadstring "local c = ... return c.close()") T)
       (⊦= LUA_TTABLE ((lua-getglobal "C") T))
       (begin1 (and (= LUA_ERRRUN ((lua-pcall 1 0 0) T))
                    (string-suffix? "cannot close a running coroutine while a Scheme binding runs Lua on it" ((lua-tostring -1) T)))
               ((lua-settop 0) T)
               ((lua-pop 1) S))))
   (let ((S (lua-make-empty)))
     ((luaL-requiref "coroutine" luaopen-coroutine #t) S)
     ((lua-setglobal "C") S)
     (⊨ (refused-in S))
     (lua-close S))
   (let ((S (lua-make-empty)))
     ((luaL-openselectedlibs LUA_COLIBK 0) S)
     ((lua-getglobal "coroutine") S) ((lua-setglobal "C") S)
     (⊨ (refused-in S))
     (lua-close S))
   (let ((S (lua-make-empty)))
     ((luaL-openselectedlibs (+ LUA_GLIBK LUA_LOADLIBK) LUA_COLIBK) S)
     (⊦= LUA_TNIL ((lua-getglobal "coroutine") S))                   ; preloaded only
     ((lua-pop 1) S)
     (run! S "C = require 'coroutine'")
     (⊨ (refused-in S))
     (lua-close S))
   (let ((S (fresh)))
     (run! S "C = coroutine")
     (⊨ (refused-in S))
     (⊨ (string-suffix? "(thread expected, got string)"
                        (condition-case (run! S "coroutine.close('x')") (e (exn lua) (msg e)))))  ; lcorolib's own checks
     (lua-close S))))
  )

(unittest/✓ lua-call-suite)

;; ------------------------------------------------------------------------------------------
;; S5: the debug interface
;; ------------------------------------------------------------------------------------------

(define-suite lua-debug-suite

  ;; (hook masks as literals: call 1, return 2, line 4, count 8, i.e. LUA_MASKCALL ... LUA_MASKCOUNT)

  ;; lua-getstack / lua-getinfo on Lua functions, a Scheme closure and the main chunk
  ((test/s5-getstack-getinfo _)
   (let ((L (fresh)) (seen #f) (probe #f))
     (set! probe
       (λ (C)
         (let ((a0 ((lua-getstack 0) C)) (a1 ((lua-getstack 1) C)) (a2 ((lua-getstack 2) C)) (a3 ((lua-getstack 3) C)))
           (set! seen
             (list (map (λ (a) (and a (luaDebug? a))) (list a0 a1 a2 a3))
                   ((lua-getstack 4) C) ((lua-getstack 99) C) ((lua-getstack -1) C)
                   ;; before any getinfo every field is #f
                   (list (lua-debug-what a1) (lua-debug-currentline a1) (lua-debug-event a1))
                   (begin ((lua-getinfo "Slnutr" a0) C)
                          (list (lua-debug-what a0) (lua-debug-name a0) (lua-debug-namewhat a0) (lua-debug-source a0)
                                (lua-debug-short-src a0) (lua-debug-currentline a0) (lua-debug-linedefined a0)
                                (lua-debug-lastlinedefined a0) (lua-debug-nups a0) (lua-debug-nparams a0)
                                (lua-debug-isvararg a0) (lua-debug-istailcall a0) (lua-debug-extraargs a0)
                                (lua-debug-ftransfer a0) (lua-debug-ntransfer a0)))
                   (begin ((lua-getinfo "Slnut" a1) C)
                          (list (lua-debug-what a1) (lua-debug-name a1) (lua-debug-namewhat a1) (lua-debug-source a1)
                                (lua-debug-short-src a1) (lua-debug-currentline a1) (lua-debug-linedefined a1)
                                (lua-debug-lastlinedefined a1) (lua-debug-nups a1) (lua-debug-nparams a1)
                                (lua-debug-isvararg a1) (lua-debug-istailcall a1)))
                   (begin ((lua-getinfo "nSl" a2) C) (list (lua-debug-what a2) (lua-debug-name a2) (lua-debug-currentline a2)))
                   (begin ((lua-getinfo "S" a3) C) (list (lua-debug-what a3) (lua-debug-linedefined a3) (lua-debug-name a3)))
                   ((lua-gettop) C)))
           ;; 'f' pushes the running function, 'L' its valid lines, in that order
           (let ((r ((lua-getinfo "fL" a1) C)))
             (⊨ (eq? r a1))
             (⊦= 3 ((lua-gettop) C))
             (⊦= LUA_TTABLE ((lua-type -1) C))
             (⊦= LUA_TFUNCTION ((lua-type -2) C))
             ((lua-setglobal "LINES") C)
             ((lua-setglobal "FN") C))
           ((lua-getinfo "f" a0) C)
           (⊨ (eq? probe ((lua-toprocedure -1) C)))                  ; the Scheme closure itself
           ((lua-getinfo "L" a0) C)                                  ; a C function: nil
           (⊦= LUA_TNIL ((lua-type -1) C))
           ((lua-pop 2) C)
           (⊦= 1 ((lua-gettop) C))
           0)))
     ((register! "probe" probe) L)
     (⊦= LUA_OK ((luaL-loadbufferx "function inner(a, b)\n  local c = a + b\n  probe(c)\n  return c\nend\nfunction outer(...) local r = inner(1, 2) return r end\nR = outer()\nSAME = (FN == inner)\nlocal l = {} for k in pairs(LINES) do l[#l + 1] = k end table.sort(l) LL = table.concat(l, ',')" "=s5chunk" #f) L))
     ((lua-call 0 0) L)
     (let ((s seen))
       (⊦= '(#t #t #t #t) (list-ref s 0))                          ; probe, inner, outer, the main chunk
       (⊦= '(#f #f #f) (list (list-ref s 1) (list-ref s 2) (list-ref s 3)))
       (⊦= '(#f #f #f) (list-ref s 4))
       (⊦= '("C" "probe" "global" "=[C]" "[C]" -1 -1 -1 1 0 #t #f 0 0 0) (list-ref s 5))   ; nups 1: the handle
       (⊦= '("Lua" "inner" "global" "=s5chunk" "s5chunk" 3 1 5 1 2 #f #f) (list-ref s 6))  ; nups 1: _ENV
       (⊦= '("Lua" "outer" 6) (list-ref s 7))
       (⊦= '("main" 0 #f) (list-ref s 8))
       (⊦= 1 (list-ref s 9)))                                      ; queries push nothing
     (⊦= 3 (global L "R"))
     (⊦= #t (global L "SAME"))
     (⊦= "2,3,4,5" (global L "LL"))
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)))

;; extraargs ('t'): each __call metavalue adds one argument (manual 4.7), so a function reached
  ;; through two nested __call metavalues has two
  ((test/s5-getinfo-extraargs _)
   (let ((L (fresh)) (seen '()))
     ((register! "probe" (λ (C)
                           (let ((a ((lua-getstack 0) C)))
                             ((lua-getinfo "t" a) C)
                             (set! seen (cons (list (lua-debug-extraargs a) (lua-debug-istailcall a) ((lua-gettop) C)) seen)))
                           0))
      L)
     (run! L "probe(1)
              local one = setmetatable({}, {__call = probe}) one(1)
              local two = setmetatable({}, {__call = one}) two(1)
              local function tail() return one(1) end tail()")
     ;; (a C function in tail position runs at once: its frame is not marked as a tail call)
     (⊦= '((0 #f 1) (1 #f 2) (2 #f 3) (1 #f 2)) (reverse seen))
     (lua-close L)))

  ;; istailcall ('t'): a Lua function entered by a tail call is marked (and has no name, its caller's
  ;; frame being gone); the same function entered by an ordinary call is not
  ((test/s5-getinfo-istailcall _)
   (let ((L (fresh)) (seen '()))
     ((register! "probe" (λ (C)
                           (let ((a ((lua-getstack 1) C)))                 ; the Lua function that called probe
                             ((lua-getinfo "nt" a) C)
                             (set! seen (cons (list (lua-debug-name a) (lua-debug-istailcall a)) seen)))
                           0))
      L)
     (run! L "local function leaf() probe() return 1 end
              local function viatail() return leaf() end
              local function viacall() local r = leaf() return r end
              viatail() viacall()")
     (⊦= '((#f #t) ("leaf" #f)) (reverse seen))
     (lua-close L)))

  ;; '>' describes the function on top (popped), with a fresh or a given luaDebug
  ;; what, as C: any length, letters repeated or none at all; an unknown letter (C: returns 0)
  ;; is refused with (exn lua api) before anything is filled or pushed
  ((test/s5-getinfo-what-letters _)
   (let ((L (fresh)) (seen #f))
     (run! L "function V(a, b) return a end")
     ((lua-getglobal "V") L)
     (let ((d ((lua-getinfo ">" #f) L)))                          ; pops the function, fills nothing
       (⊦= 0 ((lua-gettop) L))
       (⊦= '(#f #f #f) (list (lua-debug-what d) (lua-debug-nparams d) (lua-debug-currentline d))))
     ((lua-getglobal "V") L)
     (let ((d ((lua-getinfo ">SSSSSSSSuuuuuuuuuuuuuuuuuuuuuuuuSSSSu" #f) L)))   ; well past 16 letters
       (⊦= 0 ((lua-gettop) L))
       (⊦= '("Lua" 1 2 #f) (list (lua-debug-what d) (lua-debug-linedefined d) (lua-debug-nparams d) (lua-debug-isvararg d))))
     ((lua-getglobal "V") L)
     (let ((d ((lua-getinfo (string-append ">" (make-string 40 #\f) (make-string 40 #\L) "fLfLSu") #f) L)))
       (⊦= 2 ((lua-gettop) L))                                     ; 'f' and 'L' push once each
       (⊦= (list LUA_TFUNCTION LUA_TTABLE) (list ((lua-type 1) L) ((lua-type 2) L)))
       (⊦= '("Lua" 2) (list (lua-debug-what d) (lua-debug-nparams d))))
     ((lua-settop 0) L)
     ((lua-getglobal "V") L)
     (⊦= (list AUX_EARG 'lua-getinfo) (api-refusal (λ () ((lua-getinfo ">Sx" #f) L))))
     (⊦= 1 ((lua-gettop) L))                                       ; not consumed
     ((lua-pop 1) L)
     ((register! "probe"
        (λ (C)
          (let ((a1 ((lua-getstack 1) C)) (t0 ((lua-gettop) C)))
            (set! seen
              (list (eq? a1 ((lua-getinfo "" a1) C))                ; C: returns 1, fills nothing
                    (lua-debug-what a1) (lua-debug-currentline a1)
                    (- ((lua-gettop) C) t0)
                    (api-refusal (λ () ((lua-getinfo "Sx" a1) C)))
                    (api-refusal (λ () ((lua-getinfo "lS>" a1) C)))
                    (lua-debug-what a1)                              ; the refusals filled nothing
                    (begin ((lua-getinfo (apply string-append (map (λ (i) "Sl") (iota 9))) a1) C)
                           (list (lua-debug-what a1) (lua-debug-currentline a1) (lua-debug-linedefined a1)))
                    (- ((lua-gettop) C) t0)))
            0)))
      L)
     (run! L "function W(x)\n  probe()\n  return x\nend\nW(1)")
     (⊦= (list #t #f #f 0 (list AUX_EARG 'lua-getinfo) (list AUX_EARG 'lua-getinfo) #f '("Lua" 2 1) 0) seen)
     (lua-close L)))

  ((test/s5-getinfo-function _)
   (let ((L (fresh)))
     (run! L "function V(a, ...) return a end\nlocal up = 1\nfunction U() return up end")
     ((lua-getglobal "V") L)
     (let ((d ((lua-getinfo ">Su" #f) L)))
       (⊨ (luaDebug? d))
       (⊦= 0 ((lua-gettop) L))                                     ; consumed
       (⊦= '("Lua" 1 1 #t 0) (list (lua-debug-what d) (lua-debug-linedefined d) (lua-debug-nparams d) (lua-debug-isvararg d) (lua-debug-nups d)))
       (⊦= #f (lua-debug-currentline d))
       ;; the same record, reused for another function
       ((lua-getglobal "U") L)
       (⊨ (eq? d ((lua-getinfo ">Su" d) L)))
       (⊦= '(3 0 #f 1) (list (lua-debug-linedefined d) (lua-debug-nparams d) (lua-debug-isvararg d) (lua-debug-nups d)))
       ;; a frame-less record cannot describe an active function
       (⊦raises (exn lua api) ((lua-getinfo "S" d) L)))
     ;; srclen: the length of source in bytes, as C counts it ("=café" is 6 bytes, 5 characters);
     ;; #f until an 'S' fills it, like every field
     ((luaL-loadbuffer "return 1" "=café") L)
     (let ((d ((lua-getinfo ">S" #f) L)))
       (⊦= '("=café" 6 5) (list (lua-debug-source d) (lua-debug-srclen d) (string-length (lua-debug-source d)))))
     ((luaL-loadstring "return 2 -- a chunk named by its text") L)
     (let ((d ((lua-getinfo ">l" #f) L)))
       (⊦= #f (lua-debug-srclen d))
       ((lua-getglobal "V") L)
       ((lua-getinfo ">S" d) L)
       (⊦= (string-length (lua-debug-source d)) (lua-debug-srclen d)))
     ;; C functions; 'f' and 'L' after '>'
     ((lua-getglobal "print") L)
     (let ((d ((lua-getinfo ">SfL" #f) L)))
       (⊦= '("C" "=[C]" "[C]" -1 -1) (list (lua-debug-what d) (lua-debug-source d) (lua-debug-short-src d) (lua-debug-linedefined d) (lua-debug-lastlinedefined d)))
       (⊦= 4 (lua-debug-srclen d))
       (⊦= 2 ((lua-gettop) L))
       (⊦= LUA_TFUNCTION ((lua-type 1) L))
       (⊦= LUA_TNIL ((lua-type 2) L)))
     ((lua-settop 0) L)
     ((lua-getglobal "V") L)
     ((lua-getinfo ">L" #f) L)                                     ; the function is replaced by its lines
     (⊦= 1 ((lua-gettop) L))
     (⊦= LUA_TTABLE ((lua-type 1) L))
     ((lua-pop 1) L)
     ;; 'L' failing for memory: the Lua error is raised, the function is consumed, nothing is left
     ((lua-getglobal "V") L)
     ((lua-setmemorylimit 1) L)                                    ; every allocation fails
     (let ((e (condition-case ((lua-getinfo ">fL" #f) L) (e (exn lua) e))))
       (⊦= 'errmem (lua-condition-status e)))
     (⊦= 0 ((lua-gettop) L))
     ((lua-setmemorylimit 0) L)
     ((lua-getglobal "V") L)
     ((lua-getinfo ">fL" #f) L)                                    ; and works again
     (⊦= '(6 5) (list ((lua-type 1) L) ((lua-type 2) L)))
     ((lua-settop 0) L)
     ;; refusals: before anything runs, the stack unchanged
     ((lua-pushinteger 1) L)
     (⊦raises (exn lua api) ((lua-getinfo ">S" #f) L))              ; not a function on top
     (⊦= 1 ((lua-gettop) L))
     ((lua-pop 1) L)
     (⊦raises (exn lua api) ((lua-getinfo ">S" #f) L))              ; nothing on top
     ((lua-getglobal "V") L)
     (⊦raises (exn lua api) ((lua-getinfo ">X" #f) L))              ; unknown option
     (⊦raises (exn lua api) ((lua-getinfo "S>" #f) L))              ; '>' not first
     (⊦raises (exn lua api) ((lua-getinfo ">S\x0;" #f) L))          ; NUL
     (⊦raises (exn lua api) ((lua-getinfo "" #f) L))
     (⊦raises (exn lua api) ((lua-getinfo 'S #f) L))
     (⊦raises (exn lua api) ((lua-getinfo "S" #f) L))               ; no record without '>'
     (⊦raises (exn lua api) ((lua-getinfo "S" 'not-a-record) L))
     (⊦= 1 ((lua-gettop) L))
     ((lua-pop 1) L)
     (⊦= #f ((lua-getstack 0) L))                                   ; nothing runs at top level
     (lua-close L)))

  ;; locals: named, temporaries, varargs, parameters of a function value; setlocal pops only on success
  ((test/s5-locals _)
   (let ((L (fresh)) (log '()))
     ((register! "probe" (λ (C)
                           (let ((a1 ((lua-getstack 1) C)) (a0 ((lua-getstack 0) C)))
                             (define (local-of a n)
                               (let ((name ((lua-getlocal a n) C)))
                                 (if name (begin1 (cons name (value-at C -1)) ((lua-pop 1) C)) name)))
                             (set! log (list (map (λ (n) (local-of a1 n)) '(1 2 4 5))
                                             ((lua-getlocal a1 3) C)             ; 5.5: a vararg function's table
                                             (begin ((lua-pop 1) C) ((lua-gettop) C))
                                             (local-of a1 -1) (local-of a1 -2) (local-of a1 -3)
                                             (local-of a0 1) (local-of a0 2)
                                             ((lua-gettop) C)))
                             ;; setlocal: pops on success, leaves the value when there is no such local
                             ((lua-pushinteger 100) C)
                             (⊦= "c" ((lua-setlocal a1 4) C))
                             (⊦= 1 ((lua-gettop) C))
                             ((lua-pushinteger 5) C)
                             (⊦= #f ((lua-setlocal a1 50) C))
                             (⊦= 2 ((lua-gettop) C))
                             ((lua-pop 1) C)
                             (⊦raises (exn lua api) ((lua-setlocal #f 1) C))    ; setlocal needs a record
                             ((lua-pop 1) C)
                             (⊦raises (exn lua api) ((lua-setlocal a1 1) C))    ; no value to assign
                             (⊦= 0 ((lua-gettop) C)))
                           0))
      L)
     (run! L "function f(a, b, ...)\n  local c = a + b\n  probe(7)\n  return c\nend\nR = f(1, 2, 'x', 'y')")
     (let ((s log))
       (⊦= '(("a" . 1) ("b" . 2) ("c" . 3) #f) (list-ref s 0))
       (⊦= "(vararg table)" (list-ref s 1))
       (⊦= 1 (list-ref s 2))
       (⊦= '("(vararg)" . "x") (list-ref s 3))
       (⊦= '("(vararg)" . "y") (list-ref s 4))
       (⊦= #f (list-ref s 5))
       (⊦= '("(C temporary)" . 7) (list-ref s 6))                  ; the callback's own argument
       (⊦= #f (list-ref s 7))
       (⊦= 1 (list-ref s 8)))
     (⊦= 100 (global L "R"))
     ;; parameter names of a function value (ar #f): nothing pushed
     ((lua-getglobal "f") L)
     (⊦= '("a" "b" #f) (map (λ (n) ((lua-getlocal #f n) L)) '(1 2 3)))   ; live at the start: parameters only
     (⊦= 1 ((lua-gettop) L))
     ((lua-pop 1) L)
     ((lua-getglobal "print") L)
     (⊦= #f ((lua-getlocal #f 1) L))                                ; a C function has no parameter names
     ((lua-pop 1) L)
     (⊦raises (exn lua api) ((lua-getlocal #f 1) L))                ; nothing on top
     (⊦raises (exn lua api) ((lua-getlocal 'nope 1) L))
     (⊦raises (exn lua api) ((lua-getlocal #f 4294967296) L))
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)))

  ;; upvalues: Lua and Scheme closures (raw numbering), ids, joins, and every refusal
  ((test/s5-upvalues _)
   (let ((L (fresh)))
     (run! L "local x, y = 1, 2\nfunction F() return x + y end\nfunction G() return x end\nfunction H() return 0 end")
     ((lua-getglobal "F") L)                                       ; 1
     ((lua-getglobal "G") L)                                       ; 2
     (⊦= "x" ((lua-getupvalue 1 1) L))
     (⊦= 1 ((lua-tointeger -1) L))
     ((lua-pop 1) L)
     (⊦= "y" ((lua-getupvalue 1 2) L))
     (⊦= 2 ((lua-tointeger -1) L))
     ((lua-pop 1) L)
     (⊦= #f ((lua-getupvalue 1 3) L))
     (⊦= #f ((lua-getupvalue 1 0) L))
     (⊦= 2 ((lua-gettop) L))
     ((lua-pushinteger 40) L)
     (⊦= "y" ((lua-setupvalue 1 2) L))                               ; pops
     (⊦= 2 ((lua-gettop) L))
     ((lua-pushinteger 5) L)
     (⊦= #f ((lua-setupvalue 1 9) L))                                ; nothing popped (C)
     (⊦= 3 ((lua-gettop) L))
     ((lua-pop 1) L)
     (run! L "R = F()")
     (⊦= 41 (global L "R"))
     ;; ids: x is shared by F and G
     (let ((fx ((lua-upvalueid 1 1) L)) (fy ((lua-upvalueid 1 2) L)) (gx ((lua-upvalueid 2 1) L)))
       (⊨ (pointer? fx))
       (⊨ (equal? fx gx))
       (⊭ (equal? fx fy))
       (⊦= #f ((lua-upvalueid 1 3) L))
       (⊦= #f ((lua-upvalueid 1 0) L)))
     ;; join: F's y becomes G's x (1), so F() = 1 + 1
     ((lua-upvaluejoin 1 2 2 1) L)
     (⊨ (equal? ((lua-upvalueid 1 2) L) ((lua-upvalueid 2 1) L)))
     (run! L "R = F()")
     (⊦= 2 (global L "R"))
     (⊦raises (exn lua api) ((lua-upvaluejoin 1 3 2 1) L))         ; out of range
     (⊦raises (exn lua api) ((lua-upvaluejoin 1 1 2 2) L))
     ((lua-getglobal "print") L)                                   ; 3: a light C function
     (⊦= #f ((lua-getupvalue 3 1) L))
     (⊦= #f ((lua-upvalueid 3 1) L))
     (⊦raises (exn lua api) ((lua-upvaluejoin 1 1 3 1) L))         ; not a Lua closure
     ;; a Scheme closure: upvalue 1 is its handle ("" as for every C closure), then the user's
     ((lua-pushstring "u1") L)
     ((lua-pushcclosure (λ (C) 0) 1) L)                            ; 4
     (⊦= "" ((lua-getupvalue 4 1) L))
     (⊦= LUA_TUSERDATA ((lua-type -1) L))
     ((lua-pop 1) L)
     (⊦= "" ((lua-getupvalue 4 2) L))
     (⊦= "u1" ((lua-tostring -1) L))
     ((lua-pop 1) L)
     (⊦= #f ((lua-getupvalue 4 3) L))
     (⊨ (pointer? ((lua-upvalueid 4 2) L)))
     (⊦= #f ((lua-upvalueid 4 3) L))
     (⊦raises (exn lua api) ((lua-upvaluejoin 4 2 1 1) L))         ; a C closure
     ;; refusals
     ((lua-pushinteger 3) L)                                       ; 5
     (⊦raises (exn lua api) ((lua-upvalueid 5 1) L))               ; not a function
     (⊦raises (exn lua api) ((lua-upvalueid 9 1) L))               ; above the top
     (⊦raises (exn lua api) ((lua-getupvalue 0 1) L))
     (⊦raises (exn lua api) ((lua-getupvalue -9 1) L))
     (⊦raises (exn lua api) ((lua-getupvalue (lua-upvalueindex 1) 1) L))   ; no Scheme closure runs
     (⊦raises (exn lua api) ((lua-upvaluejoin 9 1 1 1) L))
     (⊦= #f ((lua-getupvalue LUA_REGISTRYINDEX 1) L))              ; not a closure
     ;; lua-setupvalue of an upvalue that does not exist answers #f and pops nothing, as C's NULL,
     ;; whatever the function: print (a light C function) has none, a Scheme closure none past its
     ;; own, a value that is no function none at all.  Only an existing C upvalue is refused.
     ((lua-pushstring "v") L)                                      ; 6
     (for-each (λ (fi n)
                 (⊦= (list fi n #f) (list fi n ((lua-setupvalue fi n) L)))
                 (⊦= 6 ((lua-gettop) L)))
               '(3 3 3 4 4 4 5 5) '(1 0 -1 3 0 -1 1 0))
     (⊦raises (exn lua api) ((lua-setupvalue 4 1) L))              ; the handle: C state
     (⊦= 6 ((lua-gettop) L))
     (⊦= "" ((lua-setupvalue 4 2) L))                              ; the user upvalue: popped
     ((lua-getupvalue 4 2) L)
     (⊦= "v" ((lua-tostring -1) L))
     ((lua-pop 1) L)
     ;; the loop C code writes (assign upvalues until NULL) stops on a C function, as in C
     (⊦= '(0 1)
         (map (λ (fi) (let loop ((n 1))
                        ((lua-pushnil) L)
                        (if ((lua-setupvalue fi n) L) (loop (+ n 1)) (begin ((lua-pop 1) L) (- n 1)))))
              '(3 2)))
     ((lua-getglobal "G") L) ((lua-pushinteger 1) L) ((lua-setupvalue -2 1) L) ((lua-pop 1) L)  ; x again
     (⊦= 5 ((lua-gettop) L))
     ((lua-settop 0) L)
     (⊦raises (exn lua api) ((lua-setupvalue 1 1) L))              ; nothing on the stack
     (lua-close L)))

  ;; inside a Scheme closure, its own upvalues are reachable through the pseudo-indices
  ((test/s5-upvalues-in-callback _)
   (let ((L (fresh)) (log #f))
     (run! L "local z = 9\nfunction Z() return z end")
     ((lua-getglobal "Z") L)
     ((lua-pushcclosure (λ (C)
                          (set! log (list ((lua-getupvalue (lua-upvalueindex 1) 1) C)   ; Z's z
                                          ((lua-tointeger -1) C)))
                          ((lua-pop 1) C)
                          ((lua-pushinteger 10) C)
                          (⊦= "z" ((lua-setupvalue (lua-upvalueindex 1) 1) C))
                          0)
                        1)
      L)
     ((lua-setglobal "cb") L)
     (run! L "cb() R = Z()")
     (⊦= '("z" 9) log)
     (⊦= 10 (global L "R"))
     (lua-close L)))

  ;; a luaDebug is refused once its view died, its frame is gone, a resume happened, or it
  ;; belongs to another universe
  ((test/s5-stale _)
   (let ((L (fresh)) (saved #f) (saved-main #f) (view #f))
     ((register! "keep" (λ (C)
                          (set! view C)
                          (set! saved ((lua-getstack 1) C))
                          (set! saved-main ((lua-getstack 1) L))  ; the main luaR, while its code runs
                          ((lua-getinfo "l" saved) C)
                          ((lua-getinfo "l" saved-main) L)
                          0))
      L)
     (run! L "function f()\n  keep()\nend\nf()")
     (⊦= 2 (lua-debug-currentline saved))                          ; the copies stay readable
     (⊦raises (exn lua api) ((lua-getinfo "l" saved) L))           ; the view died
     (⊦raises (exn lua api) ((lua-getlocal saved 1) L))
     (⊦raises (exn lua api) ((lua-getinfo "l" saved-main) L))      ; its frame is gone
     (⊦raises (exn lua api) ((lua-getlocal saved-main 1) L))
     ((lua-pushnil) L)
     (⊦raises (exn lua api) ((lua-setlocal saved-main 1) L))
     (⊦= 1 ((lua-gettop) L))
     ((lua-pop 1) L)
     (let ((c (condition-case ((lua-getinfo "l" saved-main) L) (e (exn lua api) e))))
       (⊦= "stale lua_Debug" (msg c)))
     ;; another universe
     (let ((other (fresh)) (d #f))
       ((register! "keep2" (λ (C) (⊦raises (exn lua api) ((lua-getinfo "S" d) C)) 0)) other)
       ((lua-getglobal "print") L)
       (set! d ((lua-getinfo ">S" #f) L))
       (run! other "keep2()")
       (lua-close other))
     ;; the epoch: what lua-resume and lua-closethread do
     ((register! "epoch" (λ (C)
                           (let ((d ((lua-getstack 1) C)))
                             ((lua-getinfo "l" d) C)
                             (%dbg-epoch-bump! (luaR-state C))
                             (⊦raises (exn lua api) ((lua-getinfo "l" d) C))
                             (⊦raises (exn lua api) ((lua-getlocal d 1) C))
                             ((lua-pushstring "fresh ok") C)
                             1)))
      L)
     (run! L "R = epoch()")
     (⊦= "fresh ok" (global L "R"))
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)
     (⊦raises (exn lua api) ((lua-getinfo "l" saved) L))))

  ;; a suspended coroutine: its frames are described until it moves; a resume that changes
  ;; its stack makes an old record stale (the frame check, even without the epoch)
  ((test/s5-coroutine-frames _)
   (let* ((L (fresh)) (ptr #f) (T #f))
     ((register! "grab" (λ (C) (set! ptr (luaR-state C)) 0)) L)
     (run! L "function g(a)\n  local b = a * 2\n  coroutine.yield(b)\n  return b\nend\nCO = coroutine.create(function(n)\n  grab()\n  local r = g(n)\n  coroutine.yield(r)\nend)\nassert(coroutine.resume(CO, 21))")
     (set! T (make-luaR ptr (luaR-u L) 'thread #f -1 0 #f))           ; S4's lua-tothread builds these
     (let ((top ((lua-gettop) T))
           (d0 ((lua-getstack 0) T)) (d1 ((lua-getstack 1) T)) (d2 ((lua-getstack 2) T)))
       ((lua-getinfo "Sln" d0) T)
       ((lua-getinfo "Sln" d1) T)
       ((lua-getinfo "Sl" d2) T)
       (⊦= '("C" "yield") (list (lua-debug-what d0) (lua-debug-name d0)))
       (⊦= '("Lua" 3 "g") (list (lua-debug-what d1) (lua-debug-currentline d1) (lua-debug-name d1)))
       (⊦= '("Lua" 8) (list (lua-debug-what d2) (lua-debug-currentline d2)))
       (⊦= #f ((lua-getstack 3) T))
       (⊦= "b" ((lua-getlocal d1 2) T))                              ; read a suspended frame
       (⊦= 42 ((lua-tointeger -1) T))
       ((lua-pop 1) T)
       ((lua-pushinteger 50) T)
       (⊦= "b" ((lua-setlocal d1 2) T))                              ; and write it
       (⊦= top ((lua-gettop) T))
       ((lua-getinfo "fL" d1) T)                                      ; 'L' allocates: on the helper
       (⊦= (+ top 2) ((lua-gettop) T))
       (⊦= LUA_TTABLE ((lua-type -1) T))
       ((lua-pop 2) T)
       ;; 'L' failing for memory on a suspended thread (helper path): only the error, T unchanged
       ((lua-setmemorylimit 1) L)                                     ; every allocation fails
       (let ((e (condition-case ((lua-getinfo "fL" d1) T) (e (exn lua) e))))
         (⊦= 'errmem (lua-condition-status e))
         (⊦= "not enough memory" (msg e)))
       ((lua-setmemorylimit 0) L)
       (⊦= top ((lua-gettop) T))
       ;; a hook set on the suspended thread (helper path) fires when Lua resumes it
       (let* ((events '()) (hook (λ (H ev ar) ((lua-getinfo "S" ar) H) (set! events (cons (lua-debug-what ar) events)))))
         ((lua-sethook hook 4 0) T)
         (⊨ (eq? hook ((lua-gethook) T)))
         (⊦= #f ((lua-gethook) L))                                     ; per thread
         (⊦= 4 ((lua-gethookmask) T))
         (⊦= top ((lua-gettop) T))
         (run! L "local ok, v = coroutine.resume(CO) V = v")          ; g returns 50: the stack moves
         (⊦= 50 (global L "V"))
         (⊨ (and (member "Lua" events) #t))                            ; lines of g and of the coroutine body
         ((lua-sethook #f 0 0) T)
         (⊦= #f ((lua-gethook) T))
         (⊦= top ((lua-gettop) T)))
       (⊦raises (exn lua api) ((lua-getinfo "l" d1) T))
       (⊦raises (exn lua api) ((lua-getlocal d1 1) T)))
     (run! L "CO = nil collectgarbage() collectgarbage()")
     (⊦= 1 (lua-live-handles L))                                      ; grab only
     (lua-close L)))

  ;; line hooks, gethook / gethookmask / gethookcount, removal, handle accounting
  ((test/s5-hook-lines _)
   (let* ((L (fresh)) (base (lua-live-handles L)) (lines '()) (tops '())
          (hook (λ (H ev ar)
                  ((lua-getinfo "S" ar) H)
                  (set! lines (cons (list ev (lua-debug-currentline ar) (lua-debug-short-src ar) (lua-debug-what ar)) lines))
                  ((lua-pushinteger 1) H)                              ; the hook's pushes are discarded
                  #t)))
     (⊦= #f ((lua-gethook) L))
     (⊦= 0 ((lua-gethookmask) L))
     ((lua-sethook hook 4 0) L)
     (⊨ (eq? hook ((lua-gethook) L)))
     (⊦= 4 ((lua-gethookmask) L))
     (⊦= 0 ((lua-gethookcount) L))
     (⊦= (+ base 1) (lua-live-handles L))
     (⊦= LUA_OK ((luaL-loadbufferx "local x = 1\nlocal y = 2\nZ = x + y" "=lines" #f) L))
     ((lua-call 0 0) L)
     (⊦= '((line 1 "lines" "main") (line 2 "lines" "main") (line 3 "lines" "main")) (reverse lines))
     (⊦= 3 (global L "Z"))
     (⊦= 0 ((lua-gettop) L))
     ;; the list form of the mask, and a count
     ((lua-sethook hook '(line count) 7) L)
     (⊦= 12 ((lua-gethookmask) L))
     (⊦= 7 ((lua-gethookcount) L))
     ;; removal: the procedure is released once Lua collects its handle
     ((lua-sethook #f 0 0) L)
     (⊦= #f ((lua-gethook) L))
     (⊦= 0 ((lua-gethookmask) L))
     (set! lines '())
     (run! L "local a = 1 collectgarbage() collectgarbage()")
     (⊦= '() lines)
     (⊦= base (lua-live-handles L))
     (⊦= 0 (luaU-callables (luaR-u L)))
     ((lua-sethook hook 0 0) L)                                    ; mask 0 is a removal too
     (⊦= #f ((lua-gethook) L))
     ;; a removal still stores the count, as C's lua_sethook (lua_gethookcount answers it)
     ((lua-sethook hook 0 5) L)
     (⊦= (list #f 0 5) (list ((lua-gethook) L) ((lua-gethookmask) L) ((lua-gethookcount) L)))
     ((lua-sethook #f LUA_MASKLINE 7) L)
     (⊦= (list #f 0 7) (list ((lua-gethook) L) ((lua-gethookmask) L) ((lua-gethookcount) L)))
     (let ((T ((lua-newthread) L)))                                ; a suspended thread: the op runs on the main one
       ((lua-pop 1) L)
       ((luaL-loadstring "coroutine.yield()") T)
       (let-values (((st n) ((lua-resume L 0) T))) (⊦= LUA_YIELD st))
       ((lua-sethook hook '(line) 3) T)
       ((lua-sethook #f 0 9) T)
       (⊦= (list #f 0 9) (list ((lua-gethook) T) ((lua-gethookmask) T) ((lua-gethookcount) T))))
     ((lua-sethook #f 0 0) L)
     (⊦= 0 ((lua-gethookcount) L))
     (run! L "collectgarbage() collectgarbage()")
     (⊦= base (lua-live-handles L))
     ;; a C hook (the debug library's) is 'foreign
     (run! L "debug.sethook(function() end, 'l')")
     (⊦= 'foreign ((lua-gethook) L))
     (run! L "debug.sethook()")
     (⊦= #f ((lua-gethook) L))
     ;; refusals
     (⊦raises (exn lua api) ((lua-sethook hook 16 0) L))
     (⊦raises (exn lua api) ((lua-sethook hook -1 0) L))
     (⊦raises (exn lua api) ((lua-sethook hook '(line bogus) 0) L))
     (⊦raises (exn lua api) ((lua-sethook hook 4 -1) L))
     (⊦raises (exn lua api) ((lua-sethook 'no 4 0) L))
     (⊦= #f ((lua-gethook) L))
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)
     (⊦raises (exn lua api) ((lua-gethook) L))
     (⊦raises (exn lua api) ((lua-sethook hook 4 0) L))))

  ;; call / return / tailcall events, with names and transfer information
  ((test/s5-hook-events _)
   (let ((L (fresh)) (log '()))
     ((lua-sethook (λ (H ev ar)
                     ((lua-getinfo "nSr" ar) H)
                     (when (equal? "Lua" (lua-debug-what ar))
                       (set! log (cons (list ev (lua-debug-name ar) (lua-debug-ftransfer ar) (lua-debug-ntransfer ar)) log))))
                   '(call return) 0)
      L)
     (run! L "local function leaf(a, b) return a, b, 3 end\nlocal function mid(x) return leaf(x, x) end\nfunction top() local r = mid(1) return r end\ntop()")
     ((lua-sethook #f 0 0) L)
     (let ((l (reverse log)))
       (⊨ (and (member '(call "top" 1 0) l) #t))
       (⊨ (and (member '(call "mid" 1 1) l) #t))                              ; one parameter transferred
       (⊨ (and (member '(tailcall #f 1 2) l) #t))                            ; leaf, entered by a tail call
       (⊨ (pair? (filter (λ (e) (and (eq? 'return (car e)) (= 3 (cadddr e)))) l))))   ; leaf returns 3 values
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)))

  ;; a count hook that yields the hooked coroutine; yields that hooks cannot make
  ((test/s5-hook-yield _)
   (let* ((L (fresh)) (base (lua-live-handles L)))
     ((register! "sethk" (λ (C) ((lua-sethook (λ (H ev ar) ((lua-yield 0) H)) 8 10) C) 0)) L)
     (run! L "co = coroutine.create(function() sethk() local s = 0 for i = 1, 100 do s = s + i end S = s end)
              local n = 0
              while coroutine.status(co) ~= 'dead' do assert(coroutine.resume(co)) n = n + 1 end
              N = n")
     (⊦= 5050 (global L "S"))
     (⊨ (> (global L "N") 10))
     (⊦= (+ base 2) (lua-live-handles L))                          ; sethk and the coroutine's hook
     (run! L "co = nil collectgarbage() collectgarbage()")         ; the hook table is weak-keyed
     (⊦= (+ base 1) (lua-live-handles L))
     ;; yielding from a call event, or with values: a Lua error in the hooked function
     ((register! "sethk2" (λ (C) ((lua-sethook (λ (H ev ar) ((lua-yield 0) H)) 1 0) C) 0)) L)
     ((register! "sethk3" (λ (C) ((lua-sethook (λ (H ev ar) ((lua-pushinteger 1) H) ((lua-yield 1) H)) 4 0) C) 0)) L)
     (run! L "local function f() return 1 end
              local co2 = coroutine.create(function() sethk2() f() end)
              OK2, E2 = coroutine.resume(co2)
              local co3 = coroutine.create(function() sethk3()\n local x = 1 end)
              OK3, E3 = coroutine.resume(co3)")
     (⊦= #f (global L "OK2"))
     (⊨ (string-suffix? "a hook can only yield with no values, from a line or count event" (global L "E2")))
     (⊦= #f (global L "OK3"))
     (⊨ (string-suffix? "a hook can only yield with no values, from a line or count event" (global L "E3")))
     ;; the main thread cannot yield: lua-yield raises in the hook, a Lua error with the hooked
     ;; function's position (C's luaG_runerror, checked against liblua)
     ((lua-sethook (λ (H ev ar) ((lua-yield 0) H)) 4 0) L)
     (let ((e (condition-case (run! L "local x = 1") (e (exn lua) e))))
       (⊦= "[string \"local x = 1\"]:1: attempt to yield from outside a coroutine" (msg e)))
     ((lua-sethook #f 0 0) L)
     ;; neither can a coroutine whose hooked code runs under lua-call (no continuation): C's
     ;; "across a C-call boundary", raised in the hooked function and caught by the pcall
     ((register! "callhooked" (λ (C)
                                ((lua-sethook (λ (H ev ar) ((lua-yield 0) H)) '(count) 1) C)
                                ((luaL-loadstring "local x = 1 return x") C)
                                (let ((r (condition-case (begin ((lua-call 0 1) C) 'returned) (e (exn lua) (msg e)))))
                                  ((lua-sethook #f 0 0) C)
                                  ((lua-pushstring r) C)
                                  1)))
      L)
     (run! L "R = coroutine.wrap(function() return callhooked() end)()")
     (⊦= "[string \"local x = 1 return x\"]:1: attempt to yield across a C-call boundary" (global L "R"))
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)))

  ;; hooks that raise (Lua errors with C's position, Scheme conditions with identity) or escape
  ((test/s5-hook-raise-escape _)
   (let ((L (fresh)) (c (make-property-condition 'mine)) (k #f) (fired 0))
     (run! L "function f(a)\n  local x = a\n  return x\nend")
     ((lua-sethook (λ (H ev ar) ((lua-getinfo "S" ar) H) (when (> (lua-debug-linedefined ar) 0) ((luaL-error "boom %d" 7) H))) 4 0) L)
     (run! L "local ok, e = pcall(f, 1) E = e")                   ; luaL_where(1): f's caller, the pcall
     (⊦= "boom 7" (global L "E"))
     (run! L "E = (function() return f(1) end)")
     (let ((e (condition-case (run! L "E()") (e (exn lua) e))))
       (⊨ (string-suffix? "boom 7" (msg e)))
       (⊨ (string-prefix? "[string" (msg e))))                     ; the Lua caller's position
     ((lua-sethook (λ (H ev ar) (abort c)) 4 0) L)
     (⊨ (eq? c (condition-case (run! L "local y = 2") (e () e))))  ; identity through Lua
     (⊦= 0 ((lua-gettop) L))
     ((lua-sethook (λ (H ev ar) (set! fired (+ fired 1)) (k 'escaped)) 4 0) L)
     (⊦= 'escaped (call/cc (λ (kk) (set! k kk) (run! L "local z = 3\nlocal w = 4") 'no)))
     (⊦= 1 fired)                                                  ; nothing ran after the escape
     (⊦= #f %pending-escape)
     (⊦= 0 %depth)
     ((lua-sethook #f 0 0) L)
     (⊦= 0 ((lua-gettop) L))
     (run! L "Q = 1")                                              ; still usable
     (⊦= 1 (global L "Q"))
     (lua-close L)))

  ;; the hook view cannot go below its height at entry; its luaDebug dies with it
  ((test/s5-hook-view _)
   (let ((L (fresh)) (log #f) (kept #f) (kept-view #f))
     (run! L "function f(a, b, c)\n  local x = a\n  return x\nend")
     ((lua-sethook (λ (H ev ar)
                     ((lua-getinfo "S" ar) H)
                     (let ((top ((lua-gettop) H)))
                       (when (and (not log) (= 1 (lua-debug-linedefined ar)))
                         (set! kept ar) (set! kept-view H)
                         (set! log
                           (list (luaR-kind H)
                                 (condition-case ((lua-settop 0) H) (e (exn lua api) 'refused))
                                 (condition-case ((lua-pop 1) H) (e (exn lua api) 'refused))
                                 (begin ((lua-pushinteger 5) H) ((lua-pop 1) H) 'push-pop-ok)
                                 (begin ((lua-pushinteger 5) H) ((lua-settop top) H) 'settop-ok)
                                 ;; a value above the floor may be consumed; at the floor, nothing is
                                 (condition-case ((lua-setlocal ((lua-getstack 0) H) 1) H) (e (exn lua api) 'refused))
                                 ;; slot -1 holds no function: no upvalue 1, #f and nothing popped, as C
                                 (condition-case ((lua-setupvalue -1 1) H) (e (exn lua api) 'refused))
                                 (condition-case ((lua-getinfo ">S" #f) H) (e (exn lua api) 'refused))
                                 (begin ((lua-pushinteger 11) H) ((lua-setlocal ar 1) H))
                                 (begin ((lua-getinfo "Sl" ar) H) (lua-debug-currentline ar))
                                 (= top ((lua-gettop) H)))))))
                   4 0)
      L)
     (run! L "R = f(1, 2, 3)")
     ((lua-sethook #f 0 0) L)
     (⊦= '(hook refused refused push-pop-ok settop-ok refused #f refused "a" 2 #t) log)
     (⊦= 11 (global L "R"))                                        ; a = 11 before `local x = a'
     (⊦= #f (luaR-state kept-view))
     (⊦raises (exn lua api) ((lua-getinfo "l" kept) L))            ; the hook's record died with it
     (⊦= 2 (lua-debug-currentline kept))
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)))

  ;; many hook events and records under forced collections: nothing moves under Lua, records are freed
  ((test/s5-gc-stress _)
   (let ((L (fresh)) (calls 0) (sum 0))
     ((lua-sethook (λ (H ev ar)
                     (set! calls (+ calls 1))
                     (if (= 0 (modulo calls 500)) (gc #t) (gc #f))
                     ((lua-getinfo "Sl" ar) H)
                     (let ((d ((lua-getstack 0) H)))                   ; a malloc'd record per event
                       ((lua-getinfo "l" d) H)
                       (set! sum (+ sum (lua-debug-currentline d) (- (lua-debug-currentline ar))))))
                   '(line) 0)
      L)
     (run! L "local s = 0\nfor i = 1, 3000 do\n  s = s + i\nend\nS = s")
     ((lua-sethook #f 0 0) L)
     (⊨ (> calls 6000))
     (⊦= 0 sum)                                                    ; both records name the same line
     (⊦= 4501500 (global L "S"))
     (gc #t)
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)))

  ;; every S5 binding is curried, refuses a closed state or a non-state with (exn lua api)
  ((test/s5-currying _)
   (let ((L (lua-make-empty)))
     (lua-close L)
     (for-each (λ (m)
                 (⊨ (procedure? m))
                 (⊦raises (exn lua api) (m L))
                 (⊦raises (exn lua api) (m 'not-a-state)))
               (list (lua-getstack 0) (lua-getinfo ">S" #f) (lua-getlocal #f 1) (lua-getupvalue 1 1)
                     (lua-setupvalue 1 1) (lua-upvalueid 1 1) (lua-upvaluejoin 1 1 2 1)
                     (lua-sethook #f 0 0) (lua-sethook (λ (H e a) #f) 4 0) (lua-gethook) (lua-gethookmask) (lua-gethookcount)))))

  ;; inside a hook the hooked function's registers are protected from every luaR of the thread,
  ;; not only from the hook view
  ((test/s5-hook-floor-all-luaRs _)
   (let* ((L (fresh)) (fired #f) (res #f) (M #f) (T ((lua-newthread) L)))
     ((lua-pop 1) L)
     ((lua-rawgeti LUA_REGISTRYINDEX LUA_RIDX_MAINTHREAD) L)
     (set! M ((lua-tothread -1) L))
     ((lua-pop 1) L)
     (define (hook H ev ar)
       (when (and (not fired) (eq? ev 'line) (= 4 (lua-debug-currentline ((lua-getinfo "l" ar) H))))
         (set! fired #t)
         (set! res
           (list (api-outcome (λ () ((lua-settop 0) H)))
                 (api-outcome (λ () ((lua-settop 0) L)))
                 (api-outcome (λ () ((lua-settop 0) M)))
                 (api-outcome (λ () ((lua-pop 1) L)))
                 (api-outcome (λ () ((lua-remove 1) L)))
                 (api-outcome (λ () ((lua-insert 1) L)))
                 (api-outcome (λ () ((lua-copy 2 1) L)))
                 (api-outcome (λ () ((lua-replace 1) M)))
                 (api-outcome (λ () ((lua-setfield 1 "k") L)))
                 (api-outcome (λ () ((lua-setglobal "k") H)))
                 (api-outcome (λ () ((lua-arith LUA_OPUNM) L)))
                 (api-outcome (λ () ((lua-concat 2) L)))
                 (api-outcome (λ () ((lua-pcall 0 0 0) L)))
                 (api-outcome (λ () ((lua-error) H)))
                 (api-outcome (λ () ((lua-check (λ (L) LUA_ERRRUN)) L)))
                 (api-outcome (λ () ((lua-xmove T 1) M)))
                 ;; above the floor, as usual
                 (api-outcome (λ () ((lua-pushinteger 1) L) ((lua-pushinteger 2) M)
                                ((lua-arith LUA_OPADD) L) ((lua-setglobal "three") M))))))
       0)
     (run! L "CLOSED = 0
              function f()
                local x <close> = setmetatable({}, {__close = function() CLOSED = CLOSED + 1 end})
                local y = 1
                return y
              end")
     ((lua-sethook hook '(line) 0) L)
     ((lua-getglobal "f") L)
     (⊦= LUA_OK ((lua-pcall 0 1 0) L))
     ((lua-sethook #f 0 0) L)
     (⊦= '(refused refused refused refused refused refused refused refused refused refused refused refused refused refused refused refused ok) res)
     (⊦= 1 ((lua-tointeger -1) L))
     (⊦= 1 (global L "CLOSED"))                                          ; closed once, by Lua, on return
     (⊦= 3 (global L "three"))
     ((lua-pop 1) L)
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)))

  ;; the registers of a hooked function the VM relies on: lua-copy into them, and lua-setlocal of
  ;; an internal local ("(for state)"), are refused; named locals can still be assigned
  ((test/s5-hook-registers _)
   (let ((L (fresh)) (done #f) (res '()))
     (define (hook H ev ar)
       (when (and (not done) (eq? ev 'line) (= 4 (lua-debug-currentline ((lua-getinfo "l" ar) H))))
         (set! done #t)
         (let loop ((i 1))
           (let ((name ((lua-getlocal ar i) H)))
             (when name ((lua-pop 1) H) (set! res (cons name res)) (loop (+ i 1)))))
         ((lua-newtable) H)
         (let ((top ((lua-gettop) H)))
           (set! res (list (reverse res)
                           (api-outcome (λ () ((lua-copy -1 2) H)))
                           (api-outcome (λ () ((lua-copy -1 (- top 1)) H)))
                           (api-outcome (λ () ((lua-setlocal ar 2) H)))
                           ((lua-gettop) H)                                 ; nothing was popped
                           (begin ((lua-pop 1) H) ((lua-pushinteger 100) H) ((lua-setlocal ar 1) H))))))
       0)
     (run! L "function f()
                local s = 0
                for i = 1, 10 do
                  s = s + i
                end
                return s
              end")
     ((lua-sethook hook '(line) 0) L)
     ((lua-getglobal "f") L)
     ((lua-call 0 1) L)
     ((lua-sethook #f 0 0) L)
     (⊦= "s" (car (car res)))
     (⊦= "(for state)" (cadr (car res)))
     (⊦= '(refused refused refused) (list (list-ref res 1) (list-ref res 2) (list-ref res 3)))
     (⊦= "s" (list-ref res 5))
     (⊦= 155 ((lua-tointeger -1) L))                                     ; s = 100 at i = 1, then + 2..10
     ((lua-pop 1) L)
     ;; the upvalues of a C closure are its C state: only a Scheme closure's user upvalues
     (run! L "G = string.gmatch('a b', '%a')")
     ((lua-getglobal "G") L)
     ((lua-pushinteger 1) L)
     (⊦raises (exn lua api) ((lua-setupvalue 1 1) L))
     (⊦raises (exn lua api) ((lua-setupvalue 1 3) L))
     (⊦= #f ((lua-setupvalue 1 4) L))                                   ; none: NULL, nothing popped
     (⊦= #f ((lua-setupvalue 1 0) L))
     (⊦= 2 ((lua-gettop) L))
     ((lua-settop 0) L)
     ((lua-pushinteger 7) L)
     ((lua-pushcclosure (λ (C) ((lua-pushvalue (lua-upvalueindex 1)) C) 1) 1) L)
     ((lua-pushinteger 8) L)
     (⊦raises (exn lua api) ((lua-setupvalue 1 1) L))                   ; its handle
     (⊦= "" ((lua-setupvalue 1 2) L))                                     ; its user upvalue 1
     ((lua-call 0 1) L)
     (⊦= 8 ((lua-tointeger -1) L))
     ((lua-settop 0) L)
     ;; an extra argument ("(vararg)") is a value of the program: it can be assigned
     ((register! "setva" (λ (C)
                           (let ((ar ((lua-getstack 1) C)))
                             ((lua-pushinteger 99) C)
                             ((lua-pushstring (or ((lua-setlocal ar -1) C) "none")) C)
                             1)))
      L)
     (run! L "function va(...) NAME = setva() return ... end R1, R2 = va(1, 2)")
     (⊦= "(vararg)" (global L "NAME"))
     (⊦= 99 (global L "R1"))
     (⊦= 2 (global L "R2"))
     (lua-close L)))

  ;; the event a hook's luaDebug records
  ((test/s5-debug-event _)
   (let ((L (fresh)) (log '()))
     ((lua-sethook (λ (H ev ar) (set! log (cons (cons ev (lua-debug-event ar)) log)) 0) '(call return line count) 1) L)
     (run! L "local function g(x) return x end\nlocal function h(x) return g(x) end\nh(1)")
     ((lua-sethook #f 0 0) L)
     (⊨ (pair? log))
     (⊦= '() (filter (λ (e) (not (eq? (car e) (cdr e)))) log))
     (⊦= '(call count line return tailcall)
         (filter (λ (s) (assq s log)) '(call count line return tailcall)))
     (lua-close L)))

  ;; the locals the VM trusts are refused with AUX_ERDONLY, the value left on top as C leaves it
  ;; when nothing is assigned: a for loop's control variable (read-only in 5.5; OP_FORLOOP reads a
  ;; numeric loop's as an integer or a float, unchecked) and a named vararg parameter (the vararg
  ;; table, read as a table, unchecked).  The loop's state is internal (AUX_EARG); every other
  ;; local of the same frames is still assigned, and the loops go on as Lua defines them.
  ((test/s5-setlocal-readonly _)
   (let ((L (fresh)) (log '()))
     (define (assign C ar i)
       ((lua-pushinteger 1000) C)
       (let ((top ((lua-gettop) C)))
         (condition-case (begin ((lua-setlocal ar i) C) 'ok)
           (e (exn lua api)
              (unless (= top ((lua-gettop) C)) (error "setlocal popped a refused value"))
              ((lua-pop 1) C)
              (let ((code (get-condition-property e 'lua 'code)))
                (cond ((= code AUX_ERDONLY) 'read-only) ((= code AUX_EARG) 'internal) (else code)))))))
     ((register! "probe" (λ (C)
                           (let ((ar ((lua-getstack 1) C)))
                             (let loop ((i 1) (acc '()))
                               (let ((name ((lua-getlocal ar i) C)))
                                 (if name
                                   (begin ((lua-pop 1) C) (loop (+ i 1) (cons (list name (assign C ar i)) acc)))
                                   (set! log (cons (reverse acc) log))))))
                           0))
      L)
     (run! L "function numfor() local s = 0 for i = 1, 3 do if i == 1 then probe() end s = s + i end return s end
              function fltfor() local c = 0 for x = 0.5, 2.5 do if c == 0 then probe() end c = c + 1 end return c end
              function genfor(t) local n = 0 for k, v in pairs(t) do probe() n = n + v end return n end
              function va(...t) probe() return select('#', ...) end
              function vacap(...t) local get = function() return t end probe() return t.n end
              function va2(a, ...) probe() return ... end
              R = {numfor(), fltfor(), genfor({a = 5}), va(1, 2, 3), vacap(4, 5), va2(7, 8)}")
     (⊦= '((("s" ok) ("(for state)" internal) ("(for state)" internal) ("i" read-only))
           (("c" ok) ("(for state)" internal) ("(for state)" internal) ("x" read-only))
           (("t" ok) ("n" ok) ("(for state)" internal) ("(for state)" internal) ("(for state)" internal) ("k" read-only) ("v" ok))
           (("t" read-only))
           (("t" read-only) ("get" ok))
           (("a" ok) ("(vararg table)" internal)))
         (reverse log))
     (run! L "R1, R2, R3, R4, R5, R6 = table.unpack(R)")
     (⊦= '(1006 1003 2000 3 2 8) (map (λ (g) (global L g)) '("R1" "R2" "R3" "R4" "R5" "R6")))
     ;; a main chunk is vararg (5.5) but has no vararg parameter: its local 1 (nparams + 1) is its
     ;; first ordinary local, assigned as C does -- a plain one, or a local function (registered at
     ;; pc 1, as a named vararg parameter is), through every loader
     (set! log '())
     (run! L "local x = 1 local function h() return x end probe() M1, M2 = x, h")
     (⊦= 1000 (global L "M1"))
     (⊦= 1000 (global L "M2"))
     (run! L "local function h() return 1 end local x = 2 probe() M1, M2 = h, x")
     (⊦= 1000 (global L "M1"))
     (⊦= 1000 (global L "M2"))
     (⊦= LUA_OK ((luaL-loadbufferx "local y = 3 probe() return y" "=main" "t") L))
     ((lua-call 0 1) L)
     (⊦= 1000 ((lua-tointeger -1) L))
     ((lua-pop 1) L)
     (⊦= LUA_OK ((lua-load "local z = 4 probe() return z, ..." "=main" #f) L))
     ((lua-pushstring "extra") L)
     ((lua-call 1 2) L)
     (⊦= '(1000 "extra") (list ((lua-tointeger -2) L) ((lua-tostring -1) L)))
     ((lua-pop 2) L)
     (⊦= '((("x" ok) ("h" ok)) (("h" ok) ("x" ok)) (("y" ok)) (("z" ok))) (reverse log))
     ;; the same chunk from a hook (a line event of the main chunk, after `local x')
     (set! log '())
     ((lua-sethook (λ (H ev ar)
                     (when (and (eq? ev 'line) (= 2 (lua-debug-currentline ar)))
                       ((lua-pushinteger 77) H)
                       (set! log (cons (condition-case ((lua-setlocal ar 1) H) (e (exn lua api) (list 'refused (get-condition-property e 'lua 'code)))) log)))
                     0)
                   '(line) 0)
      L)
     (run! L "local x = 1\nM1 = x\n")
     ((lua-sethook #f 0 0) L)
     (⊦= '("x") log)
     (⊦= 77 (global L "M1"))
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)))

  ;; lua-setupvalue refuses (AUX_ERDONLY, the value left on top) an upvalue of a Lua closure bound
  ;; to a read-only variable: <const>, <close>, a for loop's control variable (while the loop runs
  ;; it is the loop's own register), a named vararg parameter (the vararg table); the kind is read
  ;; from lua_dump's output, so it works for stripped functions and on suspended threads too
  ((test/s5-setupvalue-readonly _)
   (let ((L (fresh)) (seen '()))
     (define (try-set name n)
       ((lua-getglobal name) L) ((lua-pushstring "new") L)
       (begin1 (condition-case ((lua-setupvalue -2 n) L)
                 (e (exn lua api) (list 'refused (get-condition-property e 'lua 'code) ((lua-gettop) L))))
               ((lua-settop 0) L)))
     (run! L "local up = 1
              local k <const> = {}
              function reg() return up end
              function konst() return k end
              function closing() local c <close> = setmetatable({}, {__close = function() end}) return function() return c end end
              C = closing()
              function va(...t) return function() return t end end
              V = va(1, 2)
              function forcap() local r for i = 1, 2 do r = function() return i end end return r end
              F = forcap()
              function gfor() local r for key, v in pairs({a = 1}) do r = function() return key, v end end return r end
              G = gfor()
              S = load(string.dump(function() return up end, true))
              S2 = load(string.dump(function() local q <const> = {} return function() return q, 'a long string constant, longer than forty bytes', 2^53, 7 end end, true))()")
     (⊦= "up" (try-set "reg" 1))
     (run! L "R = reg()")
     (⊦= "new" (global L "R"))
     (for-each (λ (name n) (⊦= (list 'refused AUX_ERDONLY 2) (try-set name n)))
               '("konst" "C" "V" "F" "G" "S2")
               '(1 1 1 1 1 1))
     (⊦= "v" (try-set "G" 2))                                               ; the loop's other variable
     (⊦= "(no name)" (try-set "S" 1))                                       ; stripped: no name, but assigned
     ((lua-getglobal "reg") L) ((lua-pushstring "kept") L)
     (⊦= #f ((lua-setupvalue -2 5) L))                                       ; no such upvalue: nothing popped
     (⊦= 2 ((lua-gettop) L))
     ((lua-settop 0) L)
     ;; the open upvalue of a running numeric for loop: the loop's own register
     ((register! "f" (λ (C)
                       ((lua-newtable) C)
                       (set! seen (cons (condition-case ((lua-setupvalue 1 1) C) (e (exn lua api) 'refused)) seen))
                       ((lua-settop 0) C)
                       0))
      L)
     (run! L "function g() local s = {} for i = 1, 4 do f(function() return i end); s[#s+1] = i end return table.concat(s, ',') end
              R = g()")
     (⊦= '(refused refused refused refused) seen)
     (⊦= "1,2,3,4" (global L "R"))
     ;; a function on the stack of a suspended coroutine: the op runs on the main thread
     (run! L "co = coroutine.create(function()
                local q = 1
                local k <const> = {}
                local f = function() return q, k end
                coroutine.yield(f)
                return (f())
              end)")
     ((lua-getglobal "co") L)
     (let ((T ((lua-tothread -1) L)))
       ((lua-pop 1) L)
       (let-values (((st n) ((lua-resume #f 0) T))) (⊦= (list LUA_YIELD 1) (list st n)))
       ((lua-pushstring "changed") T)
       (⊦= "q" ((lua-setupvalue 1 1) T))
       ((lua-pushstring "no") T)
       (⊦raises (exn lua api) ((lua-setupvalue 1 2) T))
       (⊦= 2 ((lua-gettop) T))
       ((lua-pop 1) T)
       (let-values (((st n) ((lua-resume #f 0) T))) (⊦= (list LUA_OK 1) (list st n)))
       (⊦= "changed" ((lua-tostring -1) T)))
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)))

  ;; lua-upvaluejoin refuses (AUX_ERDONLY) to share an upvalue bound to a read-only variable:
  ;; through the joined closure (whose own prototype says "regular"), lua-setupvalue or the
  ;; closure's own code could assign it -- a running numeric for loop's register, which OP_FORLOOP
  ;; then updates as an integer under another type.  The other direction (a read-only upvalue of
  ;; f1 replaced by a regular one) is harmless and allowed, as are regular ones, stripped included.
  ((test/s5-upvaluejoin-readonly _)
   (let ((L (fresh)) (seen '()))
     (define (join f1 n1 f2 n2)
       ((lua-getglobal f1) L) ((lua-getglobal f2) L)
       (begin1 (condition-case (begin ((lua-upvaluejoin 1 n1 2 n2) L) 'joined)
                 (e (exn lua api) (list 'refused (get-condition-property e 'lua 'code) ((lua-gettop) L))))
               ((lua-settop 0) L)))
     (run! L "local up, up2 = 1, 2
              local k <const> = {}
              function reg() return up end
              function reg2() return up2 end
              function konst() return k end
              function closing() local c <close> = setmetatable({}, {__close = function() end}) return function() return c end end
              C = closing()
              function va(...t) return function() return t end end
              V = va(1, 2)
              function forcap() local r for i = 1, 2 do r = function() return i end end return r end
              F = forcap()
              S = load(string.dump(function() return up end, true))")
     (for-each (λ (f2) (⊦= (list 'refused AUX_ERDONLY 2) (join "reg" 1 f2 1))) '("konst" "C" "V" "F"))
     (⊦= 1 (begin (run! L "R = reg()") (global L "R")))                    ; unchanged
     (⊦= 'joined (join "reg" 1 "reg2" 1))                                  ; regular onto regular
     (⊦= 2 (begin (run! L "R = reg()") (global L "R")))
     (⊦= 'joined (join "konst" 1 "reg2" 1))                                ; a read-only one replaced
     (⊦= 2 (begin (run! L "R = konst()") (global L "R")))
     (⊦= 'joined (join "reg" 1 "S" 1))                                     ; stripped, regular
     ;; the open upvalue of a running numeric for loop, joined into a closure with a regular upvalue
     ((register! "cb" (λ (C)
                        (set! seen (cons (condition-case (begin ((lua-upvaluejoin 1 1 2 1) C) 'joined)
                                           (e (exn lua api) (get-condition-property e 'lua 'code)))
                                         seen))
                        ((lua-newtable) C)
                        (set! seen (cons ((lua-setupvalue 1 1) C) seen))   ; f1's own y: no harm
                        ((lua-settop 0) C)
                        0))
      L)
     (run! L "local y = 0
              local f1 = function() return y end
              local s = {}
              for i = 1, 1000000, 65536 do
                local f2 = function() return i end
                cb(f1, f2)
                s[#s + 1] = math.type(i)
              end
              N, T, Y = #s, s[1], type(f1())")
     (⊦= (list 16 "integer" "table") (list (global L "N") (global L "T") (global L "Y")))
     (⊦= (apply append (map (λ (i) (list "y" AUX_ERDONLY)) (iota 16))) seen)
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)))

  ;; the slots of a protected binding's own C frame are C state (operand copies; the parser's
  ;; half-built chunk and anchor table during a load): lua-getlocal refuses them, whether the frame
  ;; is seen from a call hook or from a Scheme finalizer that runs during the parse
  ((test/s5-getlocal-op-frame _)
   (let ((L (fresh)) (seen '()) (calls '()))
     (run! L "T = setmetatable({}, {__newindex = function(t, k, v) rawset(t, k, v) end})")
     ((lua-sethook (λ (H ev ar)
                     (when (eq? ev 'call)
                       (set! calls (cons (lua-debug-what ((lua-getinfo "S" ar) H)) calls))
                       (let ((up ((lua-getstack 1) H)))                   ; the caller of the hooked function
                         (when (and up (equal? "C" (lua-debug-what ((lua-getinfo "S" up) H))))
                           (set! seen (cons (condition-case (begin ((lua-getlocal up 1) H) ((lua-pop 1) H) 'read)
                                              (e (exn lua api) 'refused))
                                            seen)))))
                     0)
                   '(call) 0)
      L)
     ((lua-getglobal "T") L) ((lua-pushstring "k") L) ((lua-pushstring "v") L)
     (set! calls '())
     ((lua-settable 1) L)                                 ; a P binding: its trampoline calls __newindex
     ((lua-sethook #f 0 0) L)
     (⊦= '("Lua" "C") (reverse calls))                    ; __newindex, rawset: never the trampoline itself
     (⊦= '(refused) seen)                                 ; __newindex's caller is the op's C frame
     ((lua-settop 0) L)
     ;; finalizers during a parse: every one of them looks at the frame below
     (set! seen '())
     (define parsing #f)
     ((register! "sgc" (λ (C)
                         (let ((ar ((lua-getstack 1) C)))
                           (when (and parsing ar (equal? "C" (lua-debug-what ((lua-getinfo "S" ar) C))))
                             (set! seen (cons (condition-case (begin ((lua-getlocal ar 1) C) ((lua-pop 1) C) 'read)
                                                (e (exn lua api) 'refused))
                                              seen))))
                         0))
      L)
     (define chunk (apply string-append "local t = {} " (map (λ (i) (string-append "t[" (number->string i) "] = 'constant number " (number->string i) "' ")) (iota 400 1))))
     (run! L "collectgarbage('stop') collectgarbage('param', 'pause', 0)
              for i = 1, 300 do setmetatable({}, {__gc = sgc}) end
              collectgarbage('restart')")
     (set! parsing #t)
     (⊦= LUA_OK ((luaL-loadbufferx (string-append chunk "return #t") "=big" #f) L))
     (set! parsing #f)
     (⊨ (pair? seen))
     (⊦= '() (filter (λ (x) (not (eq? x 'refused))) seen))
     ((lua-call 0 1) L)
     (⊦eqv 400 ((lua-tointeger -1) L))
     ((lua-settop 0) L)
     (lua-close L)))

  ;; with '>' lua-getinfo only writes its luaDebug, as C: a record that describes a frame no longer
  ;; valid (its view died, a lua-resume or lua-closethread happened) can still be the output of a
  ;; '>' query; a hook's record cannot once the hook returned (it is Lua's own memory)
  ((test/s5-getinfo-output-reuse _)
   (let ((L (fresh)) (d #f) (har #f) (inside #f))
     ((register! "keep" (λ (C) (set! d ((lua-getstack 0) C)) 0)) L)
     (run! L "keep()")
     ((lua-getglobal "print") L)
     (⊦= "C" (lua-debug-what ((lua-getinfo ">S" d) L)))                    ; its view died
     (⊦raises (exn lua api) ((lua-getinfo "S" d) L))                         ; no frame to describe
     (run! L "co = coroutine.wrap(function() coroutine.yield() end) co() co()")   ; resumes
     (let ((T ((lua-newthread) L)))
       ((lua-pop 1) L)
       ((lua-closethread #f) T))
     (run! L "function f(a, b) return a end")
     ((lua-getglobal "f") L)
     (⊦= '("Lua" 2) (let ((d ((lua-getinfo ">Su" d) L))) (list (lua-debug-what d) (lua-debug-nparams d))))
     ;; a hook's record: usable with '>' inside the hook, where it still describes the hooked frame
     ((lua-sethook (λ (H ev ar)
                     (when (and (not har) (= 2 (lua-debug-nparams ((lua-getinfo "u" ar) H))))   ; the call of f
                       (set! har ar)
                       ((lua-getglobal "print") H)
                       (let ((w (lua-debug-what ((lua-getinfo ">S" ar) H))))
                         (set! inside (list w (and ((lua-getlocal ar 1) H) (begin ((lua-pop 1) H) #t))))))
                     0)
                   '(call) 0)
      L)
     (run! L "f(1, 2)")
     ((lua-sethook #f 0 0) L)
     (⊦= '("C" #t) inside)
     ((lua-getglobal "print") L)
     (⊦raises (exn lua api) ((lua-getinfo ">S" har) L))
     (⊦= 1 ((lua-gettop) L))                                                 ; nothing popped
     (⊦raises (exn lua api) ((lua-getinfo ">S" 'not-a-record) L))
     ((lua-settop 0) L)
     (lua-close L)))
  )

(unittest/✓ lua-debug-suite)

;; ------------------------------------------------------------------------------------------
;; S6: lauxlib and lualib
;; ------------------------------------------------------------------------------------------

(define-suite lua-auxlib-suite

  ;; every S6 binding is curried with the state last, and refuses a closed state or a non-state
  ((test/s6-currying _)
   (let ((L (lua-make-empty)) (T (lua-make-empty)))
     (lua-close L)
     (for-each (λ (m)
                 (⊨ (procedure? m))
                 (⊦raises (exn lua api) (m L))
                 (⊦raises (exn lua api) (m 'not-a-state)))
               (list (luaL-checkversion_ 505 LUAL_NUMSIZES) (luaL-checkversion) (luaL-getmetafield 1 "e")
                     (luaL-callmeta 1 "e") (luaL-tolstring 1) (luaL-checklstring 1) (luaL-checkstring 1)
                     (luaL-optlstring 1 "d") (luaL-optstring 1 "d") (luaL-checknumber 1) (luaL-optnumber 1 0)
                     (luaL-checkinteger 1) (luaL-optinteger 1 0) (luaL-checkstack 1 "m") (luaL-checktype 1 LUA_TNIL)
                     (luaL-checkany 1) (luaL-newmetatable "t") (luaL-setmetatable "t") (luaL-testudata 1 "t")
                     (luaL-checkudata 1 "t") (luaL-where 1) (luaL-checkoption 1 #f '("a")) (luaL-ref 1)
                     (luaL-unref 1 1) (luaL-loadfilex "f" #f) (luaL-loadfile "f") (luaL-len 1) (luaL-gsub "a" "b" "c")
                     (luaL-setfuncs '() 0) (luaL-getsubtable 1 "f") (luaL-traceback T #f 0)
                     (luaL-requiref "m" luaopen-math #f) (luaL-requiref "m" (λ (C) 0) #f) (luaL-newlibtable '())
                     (luaL-newlib '()) (luaL-argcheck #t 1 "m") (luaL-argexpected #t 1 "t") (luaL-typename 1)
                     (luaL-dofile "f") (luaL-getmetatable "t") (luaL-opt luaL-checkinteger 1 0) (luaL-pushfail)
                     (luaL-openselectedlibs 0 0) (luaL-makeseed)))
     ;; the traceback's L1 is checked too
     (⊦raises (exn lua api) ((luaL-traceback L #f 0) T))
     (let ((O (lua-make-empty)))
       (⊦raises (exn lua api) ((luaL-traceback O #f 0) T))          ; another universe
       (lua-close O))
     (lua-close T)))

  ;; the checks raise, inside a callback, byte for byte the message C raises from the same position
  ;; (both calls sit on one line, so their chunk:line: prefixes agree)
  ((test/s6-check-messages _)
   (let ((L (fresh)) (opts '("stop" "restart" "collect")))
     (define (same! c s)
       (run! L (string-append "local ok1, e1 = pcall(function() local r = " c "; return r end) "
                              "local ok2, e2 = pcall(function() local r = " s "; return r end) "
                              "CE, SE, OK1, OK2 = e1, e2, ok1, ok2"))
       (let ((ce (global L "CE")) (se (global L "SE")))
         (⊭ (global L "OK1"))
         (⊭ (global L "OK2"))
         (⊨ (string? ce))
         (⊦= ce se)
         ce))
     (define (fn name f) ((lua-getglobal "S") L) ((lua-pushcfunction f) L) ((lua-setfield -2 name) L) ((lua-pop 1) L))
     (run! L "S = {}")
     (fn "rep" (λ (C) ((luaL-checklstring 1) C) ((luaL-checkinteger 2) C) ((luaL-optlstring 3 "") C) 0))
     (fn "sub" (λ (C) ((luaL-checkstring 1) C) ((luaL-checkinteger 2) C) ((luaL-optinteger 3 -1) C) 0))
     (fn "sqrt" (λ (C) ((luaL-checknumber 1) C) 0))
     (fn "log" (λ (C) ((luaL-checknumber 1) C) ((luaL-optnumber 2 10) C) 0))
     (fn "type" (λ (C) ((luaL-checkany 1) C) 0))
     (fn "setmetatable" (λ (C)
                          ((luaL-checktype 1 LUA_TTABLE) C)
                          (let ((t ((lua-type 2) C))) ((luaL-argexpected (or (= t LUA_TNIL) (= t LUA_TTABLE)) 2 "nil or table") C))
                          0))
     (fn "collectgarbage" (λ (C) ((luaL-checkoption 1 "collect" opts) C) 0))
     (fn "seek" (λ (C) ((luaL-checkudata 1 "FILE*") C) 0))
     (fn "char" (λ (C) ((luaL-argcheck (<= 0 ((luaL-checkinteger 1) C) 255) 1 "value out of range") C) 0))
     (fn "byte" (λ (C) ((luaL-checkstack 2000000 "string slice too long") C) 0))
     (⊨ (string-suffix? "bad argument #1 to 'rep' (string expected, got nil)" (same! "string.rep(nil)" "S.rep(nil)")))
     (⊨ (string-suffix? "bad argument #2 to 'rep' (number has no integer representation)" (same! "string.rep('x', 1.5)" "S.rep('x', 1.5)")))
     (⊨ (string-suffix? "bad argument #2 to 'rep' (number expected, got table)" (same! "string.rep('x', {})" "S.rep('x', {})")))
     (⊨ (string-suffix? "bad argument #2 to 'rep' (number has no integer representation)" (same! "string.rep('x', '2.5')" "S.rep('x', '2.5')")))
     (⊨ (string-suffix? "bad argument #3 to 'rep' (string expected, got table)" (same! "string.rep('x', 2, {})" "S.rep('x', 2, {})")))
     (⊨ (string-suffix? "bad argument #2 to 'rep' (number expected, got no value)" (same! "string.rep('x')" "S.rep('x')")))
     (⊨ (string-suffix? "bad argument #3 to 'sub' (number expected, got table)" (same! "string.sub('x', 1, {})" "S.sub('x', 1, {})")))
     (⊨ (string-suffix? "bad argument #1 to 'sub' (string expected, got boolean)" (same! "string.sub(true, 1)" "S.sub(true, 1)")))
     (⊨ (string-suffix? "bad argument #1 to 'sqrt' (number expected, got string)" (same! "math.sqrt('a')" "S.sqrt('a')")))
     (⊨ (string-suffix? "bad argument #1 to 'sqrt' (number expected, got no value)" (same! "math.sqrt()" "S.sqrt()")))
     (⊨ (string-suffix? "bad argument #2 to 'log' (number expected, got table)" (same! "math.log(1, {})" "S.log(1, {})")))
     (⊨ (string-suffix? "bad argument #1 to 'type' (value expected)" (same! "type()" "S.type()")))
     (⊨ (string-suffix? "bad argument #1 to 'setmetatable' (table expected, got number)" (same! "setmetatable(1, {})" "S.setmetatable(1, {})")))
     (⊨ (string-suffix? "bad argument #2 to 'setmetatable' (nil or table expected, got number)" (same! "setmetatable({}, 1)" "S.setmetatable({}, 1)")))
     (⊨ (string-suffix? "bad argument #2 to 'setmetatable' (nil or table expected, got no value)" (same! "setmetatable({})" "S.setmetatable({})")))
     (⊨ (string-suffix? "bad argument #1 to 'collectgarbage' (invalid option 'bogus')" (same! "collectgarbage('bogus')" "S.collectgarbage('bogus')")))
     (⊨ (string-suffix? "bad argument #1 to 'seek' (FILE* expected, got number)" (same! "io.stdout.seek(42)" "S.seek(42)")))
     (⊨ (string-suffix? "bad argument #1 to 'seek' (FILE* expected, got table)" (same! "io.stdout.seek(setmetatable({}, {}))" "S.seek(setmetatable({}, {}))")))
     (⊨ (string-suffix? "bad argument #1 to 'char' (value out of range)" (same! "string.char(256)" "S.char(256)")))
     (run! L "BIG = string.rep('x', 2000000)")
     (⊨ (string-suffix? "stack overflow (string slice too long)" (same! "BIG:byte(1, -1)" "S.byte(BIG)")))
     (run! L "BIG = nil collectgarbage()")
     ;; the successes go on normally, in the same callbacks
     (run! L "S.rep('x', 2) S.rep(3, '4', nil) S.sub('abc', 1) S.sqrt('4') S.log(1) S.type(nil)
              S.setmetatable({}, nil) S.setmetatable({}, {}) S.collectgarbage() S.collectgarbage('stop') S.seek(io.stdout) S.char(255) S.char('7')")
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)))

  ;; the checks at top level: values, and the requests they raise (message without position; the
  ;; condition's `value' is C's error value, the same text at top level)
  ((test/s6-check-values _)
   (let ((L (fresh)))
     (define (req e)
       (⊦= (msg e) (lua-condition-value e))
       (list (get-condition-property e 'lua 'request #f) (get-condition-property e 'lua 'arg #f) (msg e)))
     ((lua-pushinteger 42) L)          ; 1
     ((lua-pushstring "10") L)         ; 2
     ((lua-pushnumber 2.5) L)          ; 3
     ((lua-pushstring "x\x0;y") L)     ; 4
     ((lua-pushnil) L)                 ; 5
     ((lua-pushboolean #t) L)          ; 6
     ((lua-pushstring "2.5") L)        ; 7
     ((lua-pushlstring (bytevector 255 1)) L) ; 8
     (⊦eqv 42 ((luaL-checkinteger 1) L))
     (⊦eqv 10 ((luaL-checkinteger 2) L))
     (⊦eqv 42.0 ((luaL-checknumber 1) L))
     (⊨ (flonum? ((luaL-checknumber 1) L)))
     (⊦eqv 10.0 ((luaL-checknumber 2) L))
     (⊦eqv 2.5 ((luaL-checknumber 3) L))
     (⊦= "42" ((luaL-checklstring 1) L))
     (⊦= LUA_TNUMBER ((lua-type 1) L))                               ; converted on a copy
     (⊦= "2.5" ((luaL-checkstring 3) L))
     (⊦= "x\x0;y" ((luaL-checklstring 4) L))                          ; NUL-safe
     (⊦= (bytevector 255 1) ((luaL-checklstring 8) L))                ; not UTF-8: bytes
     (⊦= "def" ((luaL-optlstring 5 "def") L))
     (⊦= "def" ((luaL-optstring 99 "def") L))                         ; none
     (⊦= #f ((luaL-optlstring 5 #f) L))
     (⊦= "10" ((luaL-optstring 2 "def") L))
     (⊦eqv 7 ((luaL-optinteger 5 7) L))                               ; nil: the default, exact
     (⊦eqv 7 ((luaL-optinteger 99 7) L))                              ; none
     (⊦eqv 42 ((luaL-optinteger 1 7) L))
     (⊦eqv 10 ((luaL-optinteger 2 7) L))                              ; a string converts
     (⊦eqv 3.0 ((luaL-optnumber 99 3) L))                             ; the default as a lua_Number
     (⊦eqv 3.0 ((luaL-optnumber 5 3) L))
     (⊦eqv 2.5 ((luaL-optnumber 3 3) L))
     (⊦eqv 42.0 ((luaL-optnumber 1 3) L))                             ; an integer, as a float
     (⊦= 'd ((luaL-opt luaL-checkinteger 5 'd) L))
     (⊦= 42 ((luaL-opt luaL-checkinteger 1 'd) L))
     ((luaL-checkany 5) L)                                            ; nil is a value
     ((luaL-checktype 5 LUA_TNIL) L)
     ((luaL-checktype 99 LUA_TNONE) L)
     ((luaL-argcheck #t 1 "never") L)
     ((luaL-argexpected #t 1 "never") L)
     ;; cond is C's int used as a boolean: #t / #f only (0 would pass with Scheme truthiness,
     ;; the opposite of C's luaL_argcheck(L, 0, ...)), refused with (exn lua api), not an argerror
     (for-each (λ (c)
                 (⊦= (list AUX_EARG 'luaL-argcheck) (api-refusal (λ () ((luaL-argcheck c 1 "m") L))))
                 (⊦= (list AUX_EARG 'luaL-argexpected) (api-refusal (λ () ((luaL-argexpected c 1 "t") L)))))
               (list 0 1 'true))
     ((luaL-checkstack 100 "unused") L)
     ((luaL-checkstack 10 #f) L)
     (⊦= 8 ((lua-gettop) L))
     ;; failures: requests carrying C's message without the position
     (⊦= '(argerror 3 "bad argument #3 (number has no integer representation)")
         (condition-case ((luaL-checkinteger 3) L) (e (exn lua) (req e))))
     (⊦= '(argerror 7 "bad argument #7 (number has no integer representation)")
         (condition-case ((luaL-checkinteger 7) L) (e (exn lua) (req e))))
     (⊦= '(typeerror 6 "bad argument #6 (number expected, got boolean)")
         (condition-case ((luaL-checkinteger 6) L) (e (exn lua) (req e))))
     (⊦= '(typeerror 4 "bad argument #4 (number expected, got string)")
         (condition-case ((luaL-checknumber 4) L) (e (exn lua) (req e))))
     (⊦eqv 1.0 ((luaL-optnumber 9 1) L))                               ; none is optional
     (⊦= '(typeerror 6 "bad argument #6 (string expected, got boolean)")
         (condition-case ((luaL-checklstring 6) L) (e (exn lua) (req e))))
     (⊦= '(typeerror 6 "bad argument #6 (string expected, got boolean)")
         (condition-case ((luaL-optlstring 6 "d") L) (e (exn lua) (req e))))
     (⊦eqv 0 ((luaL-opt luaL-checknumber 5 0) L))                     ; nil: the default as given
     (⊦= '(typeerror 6 "bad argument #6 (number expected, got boolean)")
         (condition-case ((luaL-opt luaL-checknumber 6 0) L) (e (exn lua) (req e))))
     (⊦= '(argerror 12 "bad argument #12 (value expected)")
         (condition-case ((luaL-checkany 12) L) (e (exn lua) (req e))))
     (⊦= '(typeerror 1 "bad argument #1 (table expected, got number)")
         (condition-case ((luaL-checktype 1 LUA_TTABLE) L) (e (exn lua) (req e))))
     (⊦= '(typeerror 99 "bad argument #99 (nil expected, got no value)")
         (condition-case ((luaL-checktype 99 LUA_TNIL) L) (e (exn lua) (req e))))
     ;; the value's type code above the expected one is refused too (string 4 for number 3, number 3
     ;; for nil 0, nil 0 for none -1, boolean 1 for nil 0)
     (⊦= '(typeerror 4 "bad argument #4 (number expected, got string)")
         (condition-case ((luaL-checktype 4 LUA_TNUMBER) L) (e (exn lua) (req e))))
     (⊦= '(typeerror 1 "bad argument #1 (nil expected, got number)")
         (condition-case ((luaL-checktype 1 LUA_TNIL) L) (e (exn lua) (req e))))
     (⊦= '(typeerror 5 "bad argument #5 (no value expected, got nil)")
         (condition-case ((luaL-checktype 5 LUA_TNONE) L) (e (exn lua) (req e))))
     (⊦= '(typeerror 6 "bad argument #6 (nil expected, got boolean)")
         (condition-case ((luaL-checktype 6 LUA_TNIL) L) (e (exn lua) (req e))))
     (⊦= '(argerror 2 "bad argument #2 (my message)")
         (condition-case ((luaL-argcheck #f 2 "my message") L) (e (exn lua) (req e))))
     (⊦= '(typeerror 6 "bad argument #6 (widget expected, got boolean)")
         (condition-case ((luaL-argexpected #f 6 "widget") L) (e (exn lua) (req e))))
     (⊦= 'lerror (condition-case ((luaL-checkstack 2000000 "too deep") L) (e (exn lua) (get-condition-property e 'lua 'request #f))))
     (⊦= "stack overflow (too deep)" (condition-case ((luaL-checkstack 2000000 "too deep") L) (e (exn lua) (msg e))))
     (⊦= "stack overflow (too deep)" (condition-case ((luaL-checkstack 2000000 "too deep") L) (e (exn lua) (lua-condition-value e))))
     (⊦= "stack overflow" (condition-case ((luaL-checkstack 2000000 #f) L) (e (exn lua) (msg e))))
     (⊦= "stack overflow" (condition-case ((luaL-checkstack 2000000 #f) L) (e (exn lua) (lua-condition-value e))))
     ;; checkoption: index, default, NUL cut as C's strcmp, invalid names
     ((lua-pushstring "b") L)                                         ; 9
     ((lua-pushstring "c\x0;zz") L)                                   ; 10
     (⊦= 1 ((luaL-checkoption 9 #f '("a" "b" "c")) L))
     (⊦= 0 ((luaL-checkoption 5 "a" '("a" "b" "c")) L))              ; nil: the default
     (⊦= 2 ((luaL-checkoption 99 "c" '("a" "b" "c")) L))             ; none: the default
     (⊦= 2 ((luaL-checkoption 10 #f '("a" "b" "c")) L))
     (⊦= '(argerror 9 "bad argument #9 (invalid option 'b')")
         (condition-case ((luaL-checkoption 9 #f '("x" "y")) L) (e (exn lua) (req e))))
     (⊦= '(argerror 5 "bad argument #5 (invalid option 'z')")
         (condition-case ((luaL-checkoption 5 "z" '("x" "y")) L) (e (exn lua) (req e))))
     (⊦= '(typeerror 5 "bad argument #5 (string expected, got nil)")
         (condition-case ((luaL-checkoption 5 #f '("x" "y")) L) (e (exn lua) (req e))))
     ;; api refusals, before Lua runs
     (⊦raises (exn lua api) ((luaL-checkinteger 0) L))
     (⊦raises (exn lua api) ((luaL-checknumber -20) L))
     (⊦raises (exn lua api) ((luaL-checktype 1 9) L))
     (⊦raises (exn lua api) ((luaL-checkstack -1 "m") L))
     (⊦raises (exn lua api) ((luaL-checkstack 1 "m\x0;") L))
     (⊦raises (exn lua api) ((luaL-checkoption 9 #f '("a" 1)) L))
     (⊦raises (exn lua api) ((luaL-checkoption 9 #f '("a\x0;")) L))
     (⊦raises (exn lua api) ((luaL-optinteger 5 1.5) L))
     (⊦raises (exn lua api) ((luaL-optnumber 5 'x) L))
     (⊦raises (exn lua api) ((luaL-opt 42 5 0) L))
     ;; typename, pushfail
     (⊦= "number" ((luaL-typename 1) L))
     (⊦= "string" ((luaL-typename -1) L))
     (⊦= "nil" ((luaL-typename 5) L))
     (⊦= "no value" ((luaL-typename 99) L))
     (⊦raises (exn lua api) ((luaL-typename 0) L))
     ((luaL-pushfail) L)
     (⊦= LUA_TNIL ((lua-type -1) L))
     (⊦= 11 ((lua-gettop) L))
     ((lua-settop 0) L)
     (lua-close L)))

  ((test/s6-metatables _)
   (let ((L (fresh)))
     (⊨ ((luaL-newmetatable "My.Type") L))
     (⊦= 1 ((lua-gettop) L))
     (⊦= LUA_TSTRING ((lua-getfield -1 "__name") L))
     (⊦= "My.Type" ((lua-tostring -1) L))
     ((lua-pop 1) L)
     (⊭ ((luaL-newmetatable "My.Type") L))                               ; exists: pushes the same table
     (run! L "function same(a, b) return rawequal(a, b) end")
     ((lua-getglobal "same") L) ((lua-pushvalue 1) L) ((lua-pushvalue 2) L) ((lua-call 2 1) L)
     (⊨ ((lua-toboolean -1) L))
     ((lua-settop 0) L)
     (⊦= LUA_TTABLE ((luaL-getmetatable "My.Type") L))
     (⊦= LUA_TNIL ((luaL-getmetatable "No.Such") L))
     (⊦= 2 ((lua-gettop) L))
     ((lua-settop 0) L)
     ;; setmetatable by name, on a table and on a userdata; testudata / checkudata
     (run! L "T = {} U = io.tmpfile() U:close() debug.setmetatable(U, debug.getregistry()['My.Type'])")
     ((lua-getglobal "T") L)
     ((luaL-setmetatable "My.Type") L)
     (⊦= 1 ((lua-gettop) L))                                            ; the value stays
     (run! L "N = getmetatable(T).__name")
     (⊦= "My.Type" (global L "N"))
     ((lua-getglobal "U") L)                                            ; 2
     ((lua-getglobal "io") L) ((lua-getfield -1 "stdout") L)            ; 3 4
     (⊨ (pointer? ((luaL-testudata 2 "My.Type") L)))
     (⊦= ((luaL-testudata 2 "My.Type") L) ((luaL-checkudata 2 "My.Type") L))
     (⊦= #f ((luaL-testudata 1 "My.Type") L))                           ; a table is not a userdata
     (⊦= #f ((luaL-testudata 4 "My.Type") L))                           ; another metatable
     (⊨ (pointer? ((luaL-testudata 4 "FILE*") L)))
     (⊦= #f ((luaL-testudata 99 "FILE*") L))                            ; none: NULL, as C
     (⊦= 4 ((lua-gettop) L))
     (⊦= "bad argument #4 (My.Type expected, got FILE*)"
         (condition-case ((luaL-checkudata 4 "My.Type") L) (e (exn lua) (msg e))))
     (⊦= "bad argument #1 (FILE* expected, got My.Type)"
         (condition-case ((luaL-checkudata 1 "FILE*") L) (e (exn lua) (msg e))))
     ;; setmetatable with an unknown name removes the metatable (nil), as C
     ((lua-pushvalue 1) L)
     ((luaL-setmetatable "No.Such") L)
     ((lua-pop 1) L)
     (run! L "M = getmetatable(T)")
     (⊦= 'nil (global L "M"))
     ;; a registry field that is not a table: a Lua error, never lua_setmetatable on it
     (run! L "debug.getregistry()['Bad.Type'] = 42")
     ((lua-pushvalue 1) L)
     (let ((e (condition-case ((luaL-setmetatable "Bad.Type") L) (e (exn lua) e))))
       (⊦= 'errrun (lua-condition-status e))
       (⊦= "registry field 'Bad.Type' is not a table" (msg e)))
     (⊦= 5 ((lua-gettop) L))
     ((lua-pop 1) L)
     ;; getmetafield / callmeta
     (run! L "O = setmetatable({}, {__index = function() return 'idx' end, kind = 'k1',
                                   __tostring = function(t) return 'O!' end,
                                   __call = 5, bad = function() error('in meta', 0) end})")
     ((lua-getglobal "O") L)                                            ; 5
     (⊦= LUA_TSTRING ((luaL-getmetafield 5 "kind") L))
     (⊦= "k1" ((lua-tostring -1) L))
     ((lua-pop 1) L)
     (⊦= LUA_TNUMBER ((luaL-getmetafield -1 "__call") L))
     ((lua-pop 1) L)
     (⊦= LUA_TNIL ((luaL-getmetafield 5 "missing") L))                  ; nothing pushed
     (⊦= LUA_TNIL ((luaL-getmetafield 3 "kind") L))                     ; io table: no metatable
     (⊦= LUA_TNIL ((luaL-getmetafield 77 "kind") L))                    ; none
     (⊦= LUA_TTABLE ((luaL-getmetafield 4 "__index") L))                ; FILE* methods
     ((lua-pop 1) L)
     (⊦= 5 ((lua-gettop) L))
     (⊨ ((luaL-callmeta 5 "__tostring") L))
     (⊦= "O!" ((lua-tostring -1) L))
     ((lua-pop 1) L)
     (⊭ ((luaL-callmeta 5 "missing") L))
     (⊭ ((luaL-callmeta 77 "__tostring") L))
     (⊦= 5 ((lua-gettop) L))
     (let ((e (condition-case ((luaL-callmeta 5 "bad") L) (e (exn lua) e))))
       (⊦= "in meta" (msg e))
       (⊦= 'errrun (lua-condition-status e)))
     (⊦= 5 ((lua-gettop) L))                                            ; the state goes on
     (⊦raises (exn lua api) ((luaL-getmetafield 0 "x") L))
     (⊦raises (exn lua api) ((luaL-getmetafield -9 "x") L))
     (⊦raises (exn lua api) ((luaL-getmetafield 1 "x\x0;y") L))
     (⊦raises (exn lua api) ((luaL-newmetatable (string->utf8 "bv")) L))  ; strlen strings only
     (⊦raises (exn lua api) ((luaL-testudata 0 "x") L))
     ((lua-settop 0) L)
     (⊦raises (exn lua api) ((luaL-setmetatable "My.Type") L))            ; nothing on top
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)))

  ((test/s6-tolstring-len _)
   (let ((L (fresh)))
     (run! L "NAMED = setmetatable({}, {__name = 'MyType'})
              STR = setmetatable({}, {__tostring = function() return 'custom!' end})
              NUMSTR = setmetatable({}, {__tostring = function() return 42 end})
              BADSTR = setmetatable({}, {__tostring = function() return {} end})
              RAISE = setmetatable({}, {__tostring = function() error('tostring failed', 0) end})
              L3 = setmetatable({}, {__len = function() return 3 end})
              LF = setmetatable({}, {__len = function() return 2.5 end})
              LS = setmetatable({}, {__len = function() return '7' end})")
     ((lua-pushinteger 12) L) ((lua-pushnumber 1.5) L) ((lua-pushnil) L) ((lua-pushboolean #f) L) ((lua-pushstring "s\x0;t") L)
     (⊦= "12" ((luaL-tolstring 1) L))
     (⊦= 6 ((lua-gettop) L))                                            ; pushed too
     (⊦= LUA_TSTRING ((lua-type -1) L))
     (⊦= LUA_TNUMBER ((lua-type 1) L))                                  ; the slot keeps its number
     (⊦= "1.5" ((luaL-tolstring 2) L))
     (⊦= "nil" ((luaL-tolstring 3) L))
     (⊦= "false" ((luaL-tolstring 4) L))
     (⊦= "s\x0;t" ((luaL-tolstring 5) L))
     ((lua-settop 5) L)
     (for-each (λ (g) ((lua-getglobal g) L)) '("NAMED" "STR" "NUMSTR" "BADSTR" "RAISE" "L3" "LF" "LS"))  ; 6..13
     (⊨ (string-prefix? "MyType: 0x" ((luaL-tolstring 6) L)))
     (⊦= "custom!" ((luaL-tolstring 7) L))
     (⊦= "42" ((luaL-tolstring 8) L))
     (⊦= 16 ((lua-gettop) L))
     ((lua-settop 13) L)
     (⊦= "'__tostring' must return a string" (condition-case ((luaL-tolstring 9) L) (e (exn lua) (msg e))))
     (⊦= "tostring failed" (condition-case ((luaL-tolstring 10) L) (e (exn lua) (msg e))))
     (⊦= 13 ((lua-gettop) L))
     (⊨ (string-prefix? "table: 0x" ((luaL-tolstring LUA_REGISTRYINDEX) L)))
     ((lua-pop 1) L)
     ;; luaL-len: an integer, nothing left on the stack
     (⊦= 3 ((luaL-len 5) L))                                            ; "s\0t"
     (⊦= 3 ((luaL-len 11) L))
     (⊦= 7 ((luaL-len 13) L))                                           ; '7' converts
     (⊦= 13 ((lua-gettop) L))
     (let ((e (condition-case ((luaL-len 12) L) (e (exn lua) e))))
       (⊦= "object length is not an integer" (msg e))
       (⊦= 'errrun (lua-condition-status e)))
     (⊨ (string-suffix? "attempt to get length of a number value" (condition-case ((luaL-len 1) L) (e (exn lua) (msg e)))))
     (⊦= 13 ((lua-gettop) L))
     (⊦raises (exn lua api) ((luaL-len 14) L))
     (⊦raises (exn lua api) ((luaL-tolstring 0) L))
     ((lua-settop 0) L)
     (lua-close L)))

  ((test/s6-ref _)
   (let ((L (fresh)))
     (run! L "T = {}")
     ((lua-getglobal "T") L)                                            ; 1
     ((lua-pushstring "first") L)
     (let ((r1 ((luaL-ref 1) L)))
       (⊦= 1 ((lua-gettop) L))                                          ; the value was popped
       ((lua-pushstring "second") L)
       (let ((r2 ((luaL-ref -2) L)))
         (⊨ (and (fixnum? r1) (fixnum? r2) (> r1 0) (> r2 0) (not (= r1 r2))))
         (run! L (string-append "A, B = T[" (number->string r1) "], T[" (number->string r2) "]"))
         (⊦= "first" (global L "A"))
         (⊦= "second" (global L "B"))
         ((luaL-unref 1 r1) L)
         (run! L (string-append "A = T[" (number->string r1) "]"))
         (⊭ (equal? "first" (global L "A")))
         ((lua-pushstring "third") L)
         (⊦= r1 ((luaL-ref 1) L))                                       ; the freed ref is reused
         ((luaL-unref 1 LUA_NOREF) L)                                   ; negative refs are ignored
         ((luaL-unref 1 LUA_REFNIL) L)))
     ((lua-pushnil) L)
     (⊦= LUA_REFNIL ((luaL-ref 1) L))                                   ; nil: fixed ref, nothing stored
     (⊦= 1 ((lua-gettop) L))
     ;; the registry, as the C idiom
     ((lua-pushstring "in the registry") L)
     (let ((r ((luaL-ref LUA_REGISTRYINDEX) L)))
       (⊨ (> r 0))
       ((luaL-unref LUA_REGISTRYINDEX r) L))
     ;; refusals: not a table, invalid index, nothing to ref
     ((lua-pushinteger 5) L)
     (⊦raises (exn lua api) ((luaL-ref 2) L))                           ; t is a number
     (⊦raises (exn lua api) ((luaL-unref 2 1) L))
     (⊦raises (exn lua api) ((luaL-ref 9) L))
     (⊦= 2 ((lua-gettop) L))
     ((lua-settop 0) L)
     (⊦raises (exn lua api) ((luaL-ref LUA_REGISTRYINDEX) L))           ; no value on top
     ;; a __gc on the stored value may run Scheme: the safe path
     (let ((n 0))
       ((register! "cb" (λ (C) (set! n (+ n 1)) (gc #t) 0)) L)
       (run! L "T2 = {}")
       ((lua-getglobal "T2") L)
       (do ((i 0 (+ i 1))) ((= i 200))
         (run! L "V = setmetatable({}, {__gc = function() cb() end})")
         ((lua-getglobal "V") L)
         (let ((r ((luaL-ref 1) L))) ((luaL-unref 1 r) L)))
       (run! L "V = nil collectgarbage() collectgarbage()")
       (⊨ (> n 0))
       (⊦= 1 ((lua-gettop) L)))
     (lua-close L)))

  ((test/s6-load-do _)
   (let ((L (fresh)))
     (run! L "P = os.tmpname()
              local f = assert(io.open(P, 'w')) f:write('#!/usr/bin/lua\\nlocal a, b = ...\\nreturn 1 + 2, (a or 0) + 40') f:close()
              PB = os.tmpname()
              f = assert(io.open(PB, 'wb')) f:write(string.dump(function() return 7 end)) f:close()
              PS = os.tmpname()
              f = assert(io.open(PS, 'w')) f:write('return = 1') f:close()
              PE = os.tmpname()
              f = assert(io.open(PE, 'w')) f:write('\\n\\nerror(\"at line three\")') f:close()")
     (let ((p (global L "P")) (pb (global L "PB")) (ps (global L "PS")) (pe (global L "PE")))
       (⊦= LUA_OK ((luaL-loadfilex p #f) L))
       (⊦= 1 ((lua-gettop) L))
       (⊦= LUA_TFUNCTION ((lua-type 1) L))
       ((lua-pushinteger 2) L)
       ((lua-call 1 2) L)
       (⊦= 3 ((lua-tointeger 1) L))
       (⊦= 42 ((lua-tointeger 2) L))
       ((lua-settop 0) L)
       (⊦= LUA_OK ((luaL-loadfile p) L))
       ((lua-settop 0) L)
       ;; the chunk name is "@" + the file name
       (run! L (string-append "local f = loadfile(P) local i = debug.getinfo(f, 'S') SRC = i.source"))
       (⊦= (string-append "@" p) (global L "SRC"))
       ;; modes
       (⊦= LUA_ERRSYNTAX ((luaL-loadfilex p "b") L))
       (⊨ (string-suffix? "attempt to load a text chunk (mode is 'b')" ((lua-tostring -1) L)))
       ((lua-pop 1) L)
       (⊦= LUA_ERRSYNTAX ((luaL-loadfilex pb "t") L))
       (⊨ (string-suffix? "attempt to load a binary chunk (mode is 't')" ((lua-tostring -1) L)))
       ((lua-pop 1) L)
       (⊦= LUA_OK ((luaL-loadfilex pb "b") L))
       ((lua-call 0 1) L)
       (⊦= 7 ((lua-tointeger -1) L))
       ((lua-pop 1) L)
       (⊦= LUA_OK ((luaL-loadfilex pb "bt") L))
       ((lua-pop 1) L)
       ;; statuses, the message left on the stack (C semantics)
       (⊦= LUA_ERRSYNTAX ((luaL-loadfile ps) L))
       (⊨ (string? ((lua-tostring -1) L)))
       ((lua-pop 1) L)
       (⊦= LUA_ERRFILE ((luaL-loadfile "/no/such/dir/file.lua") L))
       (⊦= "cannot open /no/such/dir/file.lua: No such file or directory" ((lua-tostring -1) L))
       ((lua-pop 1) L)
       (⊦= 'errfile (condition-case ((lua-check (luaL-loadfile "/no/such/dir/file.lua")) L) (e (exn lua) (lua-condition-status e))))
       (⊦= 0 ((lua-gettop) L))
       ;; dofile
       (⊦= LUA_OK ((luaL-dofile p) L))
       (⊦= 2 ((lua-gettop) L))                                          ; MULTRET results
       (⊦= 40 ((lua-tointeger 2) L))
       ((lua-settop 0) L)
       (⊦= LUA_ERRSYNTAX ((luaL-dofile ps) L))
       ((lua-pop 1) L)
       (⊦= LUA_ERRRUN ((luaL-dofile pe) L))
       (⊦= (string-append pe ":3: at line three") ((lua-tostring -1) L))
       ((lua-pop 1) L)
       (⊦= LUA_ERRFILE ((luaL-dofile "/no/such/file") L))
       ((lua-pop 1) L)
       (⊨ (string-suffix? ":3: at line three" (condition-case ((lua-check (luaL-dofile pe)) L) (e (exn lua) (msg e)))))
       (⊦= 0 ((lua-gettop) L))
       ;; the safe path: a Scheme callable alive in the universe, a GC during the load
       ((register! "noop" nop-callback) L)
       (run! L "setmetatable({}, {__gc = function() noop() end})")
       (⊦= LUA_OK ((luaL-loadfilex (string-copy p) "t") L))
       ((lua-pop 1) L)
       ;; refusals
       (⊦raises (exn lua api) ((luaL-loadfilex "a\x0;b" #f) L))
       (⊦raises (exn lua api) ((luaL-loadfilex (string->utf8 p) #f) L))
       (⊦raises (exn lua api) ((luaL-loadfilex p 42) L))
       (run! L "os.remove(P) os.remove(PB) os.remove(PS) os.remove(PE)")
       (⊦= 0 ((lua-gettop) L)))
     (lua-close L)))

  ((test/s6-gsub-where-traceback _)
   (let ((L (fresh)))
     (⊦= "hell0 w0rld" ((luaL-gsub "hello world" "o" "0") L))
     (⊦= "hell0 w0rld" ((lua-tostring -1) L))                            ; pushed too
     (⊦= "a.b.c" ((luaL-gsub "a::b::c" "::" ".") L))
     (⊦= "none" ((luaL-gsub "none" "zz" "y") L))
     (⊦= "" ((luaL-gsub "aaa" "a" "") L))
     (⊦= "\x3bb;\x3bb;" ((luaL-gsub "xx" "x" "\x3bb;") L))              ; UTF-8
     (⊦= 5 ((lua-gettop) L))
     (⊦raises (exn lua api) ((luaL-gsub "abc" "" "x") L))                ; C would loop for ever
     (⊦raises (exn lua api) ((luaL-gsub "a\x0;b" "a" "x") L))
     (⊦raises (exn lua api) ((luaL-gsub 'a "a" "x") L))
     ((lua-settop 0) L)
     ;; where: level 0 is the running function (a C function has no line), 1 its caller
     ((luaL-where 1) L)
     (⊦= "" ((lua-tostring -1) L))                                       ; top level: no Lua caller
     ((lua-pop 1) L)
     ((register! "W" (λ (C) ((luaL-where 1) C) ((luaL-where 0) C) ((luaL-where 2) C) 3)) L)
     (run! L "local function f() local a, b, c = W() return a, b, c end
              local w, z, main = f() WA, WZ, WM = w, z, main
              EA = debug.getinfo(1, 'S').short_src .. ':1: '
              EM = debug.getinfo(1, 'S').short_src .. ':2: '")
     (⊦= (global L "EA") (global L "WA"))
     (⊦= "" (global L "WZ"))
     (⊦= (global L "EM") (global L "WM"))
     ;; the where of luaL-error in a callback agrees
     (let ((w #f))
       ((register! "E" (λ (C) ((luaL-where 1) C) (set! w ((lua-tostring -1) C)) ((luaL-error "raised") C))) L)
       (run! L "local ok, e = pcall(function() local r = E() return r end) EE = e")
       (⊨ (string-suffix? ":1: " w))
       (⊦= (string-append w "raised") (global L "EE")))
     ;; traceback: the same text as debug.traceback from the same place
     ((register! "TB" (λ (C) ((luaL-traceback C "msg" 1) C) 1)) L)
     ((register! "TB0" (λ (C) ((luaL-traceback C #f 0) C) 1)) L)
     (run! L "function g() local a = debug.traceback('msg', 1); local b = TB(); return a, b end
              function g0() local a, b = debug.traceback(nil, 0), TB0() return a, b end
              A, B = g() A0, B0 = g0()")
     (⊦= (global L "A") (global L "B"))
     (⊨ (string-prefix? "msg\nstack traceback:\n\t" (global L "B")))
     (⊨ (string-prefix? "stack traceback:\n\t[C]: in " (global L "B0")))  ; level 0: TB0 itself
     (⊨ (string-prefix? "stack traceback:\n\t[C]: in " (global L "A0")))  ; (debug.traceback there)
     ;; from Scheme at top level: the protected frame is skipped
     ((luaL-traceback L "top" 0) L)
     (⊦= "top\nstack traceback:" ((lua-tostring -1) L))
     ((lua-pop 1) L)
     ;; another thread of the universe: no shift
     (let* ((ptr #f))
       ((register! "grab" (λ (C) (set! ptr (luaR-state C)) 0)) L)
       (run! L "CO = coroutine.create(function() grab() local x = 1
                  coroutine.yield() end)
                coroutine.resume(CO)
                TCO = debug.traceback(CO, 'co', 0) TCO1 = debug.traceback(CO, nil, 1)
                CW = debug.getinfo(CO, 1, 'S').short_src .. ':' .. debug.getinfo(CO, 1, 'l').currentline .. ': '")
       (let ((T (make-luaR ptr (luaR-u L) 'thread #f -1 0 #f)))
         ((luaL-traceback T "co" 0) L)
         (⊦= (global L "TCO") ((lua-tostring -1) L))
         ((lua-pop 1) L)
         ((luaL-traceback T #f 1) L)
         (⊦= (global L "TCO1") ((lua-tostring -1) L))
         ((lua-pop 1) L)
         ;; a suspended thread: luaL-where reads it and pushes through the helper
         (let ((top ((lua-gettop) T)))
           ((luaL-where 1) T)
           (⊦= (global L "CW") ((lua-tostring -1) T))
           ((luaL-gsub "on co" "co" "the thread") T)                     ; P* too
           (⊦= "on the thread" ((lua-tostring -1) T))
           ((luaL-newmetatable "Co.Type") T)
           (⊦= (+ top 3) ((lua-gettop) T))
           ((lua-pop 3) T)
           (⊦= top ((lua-gettop) T)))
         (⊦raises (exn lua api) ((luaL-len 1) T))                         ; P on a suspended thread
         ;; luaL-traceback is P* too: onto a suspended thread, as C (it only reads and allocates):
         ;; its own trace (no protected frame to skip there), and the main thread's (skipped)
         (let ((top ((lua-gettop) T)))
           ((luaL-traceback T "co" 0) T)
           (⊦= (global L "TCO") ((lua-tostring -1) T))
           ((luaL-traceback T #f 1) T)
           (⊦= (global L "TCO1") ((lua-tostring -1) T))
           ((luaL-traceback L "x" 0) T)
           (⊦= "x\nstack traceback:" ((lua-tostring -1) T))
           ((lua-pop 3) T)
           (⊦= top ((lua-gettop) T)))))
     ;; and onto a thread dead by an error in a Lua function, as debug.traceback reads it
     (run! L "DCO = coroutine.create(function() local a = 1; local b = nil; return a + b end)
              coroutine.resume(DCO)
              TD = debug.traceback(DCO, 'dead', 0) TD1 = debug.traceback(DCO, nil, 1)")
     ((lua-getglobal "DCO") L)
     (let ((D ((lua-tothread -1) L)))
       ((lua-pop 1) L)
       (⊨ (> ((lua-status) D) LUA_YIELD))
       ((luaL-traceback D "dead" 0) D)
       ((lua-xmove L 1) D)                                                ; a value pushed since its death
       (⊦= (global L "TD") ((lua-tostring -1) L))
       ((luaL-traceback D #f 1) D)
       ((lua-xmove L 1) D)
       (⊦= (global L "TD1") ((lua-tostring -1) L))
       ((luaL-traceback D "m" 0) L)                                       ; read from the main thread
       (⊦= (global L "TD") (string-append "dead" (substring ((lua-tostring -1) L) 1)))
       ((lua-pop 3) L))
     ;; a negative level traces no frame, as C (lua_getstack answers 0) and debug.traceback('m', -1);
     ;; tracing L itself must not shift -1 into level 0, and INT_MIN must not overflow `last - level'
     (run! L "NEG = debug.traceback('m', -1)")
     ((luaL-traceback L "m" -1) L)
     (⊦= "m\nstack traceback:" ((lua-tostring -1) L))
     (⊦= (global L "NEG") ((lua-tostring -1) L))
     ((lua-pop 1) L)
     ((luaL-traceback L #f -5) L)
     (⊦= "stack traceback:" ((lua-tostring -1) L))
     ((lua-pop 1) L)
     ((luaL-traceback L "m" -2147483648) L)
     (⊦= "m\nstack traceback:" ((lua-tostring -1) L))
     ((lua-pop 1) L)
     ((register! "TBN" (λ (C) ((luaL-traceback C "n" -1) C) 1)) L)       ; inside a callback (frames exist)
     (run! L "TN = (function() return TBN() end)()")
     (⊦= "n\nstack traceback:" (global L "TN"))
     ((luaL-traceback L "far" 2147483646) L)                              ; INT_MAX - 1: past the last level
     (⊦= "far\nstack traceback:" ((lua-tostring -1) L))
     ((lua-pop 1) L)
     (⊦raises (exn lua api) ((luaL-traceback L "x" 2147483647) L))       ; INT_MAX: C's level++ overflows
     (⊦raises (exn lua api) ((luaL-traceback L "x" -2147483649) L))      ; not an int
     (⊦raises (exn lua api) ((luaL-traceback L "x\x0;" 0) L))
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)))

  ((test/s6-libraries _)
   (let ((L (lua-make-empty)))                                           ; no library yet
     ((luaL-checkversion) L)
     ((luaL-checkversion_ 505 LUAL_NUMSIZES) L)
     (⊦= 136 LUAL_NUMSIZES)
     (let ((e (condition-case ((luaL-checkversion_ 504 LUAL_NUMSIZES) L) (e (exn lua) e))))
       (⊦= "version mismatch: app. needs 504.0, Lua core provides 505.0" (msg e))
       (⊦= 'errrun (lua-condition-status e)))
     (⊦= "core and library have incompatible numeric types"
         (condition-case ((luaL-checkversion_ 505 1) L) (e (exn lua) (msg e))))
     (⊦raises (exn lua api) ((luaL-checkversion_ 'x 1) L))
     (⊦raises (exn lua api) ((luaL-checkversion_ 505 -1) L))
     (⊦= 0 ((lua-gettop) L))
     ;; openselectedlibs: base (1) and string (128) loaded, math (32) only preloaded
     ((luaL-openselectedlibs (+ 1 128) 32) L)
     (⊦= 0 ((lua-gettop) L))
     (⊦= LUA_TTABLE ((lua-getglobal "string") L))
     (⊦= LUA_TFUNCTION ((lua-getglobal "print") L))
     (⊦= LUA_TNIL ((lua-getglobal "math") L))
     (⊦= LUA_TNIL ((lua-getglobal "io") L))
     ((lua-settop 0) L)
     (⊨ ((luaL-getsubtable LUA_REGISTRYINDEX "_PRELOAD") L))
     (⊦= LUA_TFUNCTION ((lua-getfield -1 "math") L))
     (⊦= LUA_TNIL ((lua-getfield -2 "string") L))                        ; loaded, not preloaded
     ((lua-settop 0) L)
     ;; requiref with a C opener
     ((luaL-requiref "math" luaopen-math #t) L)
     (⊦= 1 ((lua-gettop) L))
     (⊦= LUA_TNUMBER ((lua-getfield -1 "pi") L))
     (⊦= 3.141592653589793 ((lua-tonumber -1) L))
     ((lua-settop 0) L)
     (⊦= LUA_TTABLE ((lua-getglobal "math") L))
     ((lua-pop 1) L)
     ((luaL-requiref "m2" luaopen-math #f) L)                            ; not global
     ((lua-pop 1) L)
     (⊦= LUA_TNIL ((lua-getglobal "m2") L))
     ((lua-pop 1) L)
     ;; glb is C's int used as a boolean: #t / #f only (0 would set the global with Scheme
     ;; truthiness, the opposite of C's luaL_requiref(L, name, f, 0)); refused before anything runs
     (for-each (λ (g)
                 (⊦= (list AUX_EARG 'luaL-requiref) (api-refusal (λ () ((luaL-requiref "m3" luaopen-math g) L))))
                 (⊦= (list AUX_EARG 'luaL-requiref) (api-refusal (λ () ((luaL-requiref "m3" (λ (C) ((lua-newtable) C) 1) g) L)))))
               (list 0 1 'yes))
     (⊦= 0 ((lua-gettop) L))
     (⊦= LUA_TNIL ((lua-getglobal "m3") L))
     ((lua-pop 1) L)
     (⊨ ((luaL-getsubtable LUA_REGISTRYINDEX "_LOADED") L))
     (⊦= LUA_TNIL ((lua-getfield -1 "m3") L))                             ; nor opened
     ((lua-settop 0) L)
     (⊨ ((luaL-getsubtable LUA_REGISTRYINDEX "_LOADED") L))
     (⊦= LUA_TTABLE ((lua-getfield -1 "m2") L))
     ((lua-settop 0) L)
     (for-each (λ (p) (⊨ (pointer? p)))
               (list luaopen-base luaopen-package luaopen-coroutine luaopen-debug luaopen-io luaopen-math
                     luaopen-os luaopen-string luaopen-table luaopen-utf8))
     ;; every C opener opens its own library, and no other: each module (in a state of its own, with
     ;; no library) has the field only its library has, and none of the other libraries' fields
     (let ((libs `(("_G" ,luaopen-base "pcall") ("package" ,luaopen-package "searchers")
                   ("coroutine" ,luaopen-coroutine "wrap") ("debug" ,luaopen-debug "traceback")
                   ("io" ,luaopen-io "write") ("math" ,luaopen-math "pi") ("os" ,luaopen-os "clock")
                   ("string" ,luaopen-string "rep") ("table" ,luaopen-table "concat")
                   ("utf8" ,luaopen-utf8 "codepoint"))))
       (for-each (λ (lib)
                   (let ((S (lua-make-empty)))
                     ((luaL-requiref (car lib) (cadr lib) #f) S)
                     (⊦= (cons (car lib) (map (λ (other) (eq? other lib)) libs))
                         (cons (car lib) (map (λ (other)
                                                (let ((t ((lua-getfield -1 (caddr other)) S)))
                                                  ((lua-pop 1) S)
                                                  (not (= t LUA_TNIL))))
                                              libs)))
                     (⊦= 1 ((lua-gettop) S))
                     (lua-close S)))
                 libs))
     ;; requiref with a Scheme opener: called once with the module name
     (let ((calls '()))
       (define (opener C)
         (set! calls (cons ((lua-tostring 1) C) calls))
         (gc #t)
         ((luaL-newlib `(("answer" . ,(λ (C) ((lua-pushinteger 42) C) 1)) ("flag" . #f))) C)
         1)
       ((luaL-requiref (string-append "my" "mod") opener #t) L)
       (⊦= '("mymod") calls)
       (⊦= LUA_TTABLE ((lua-type -1) L))
       ((luaL-requiref "mymod" opener #t) L)                               ; already loaded
       (⊦= '("mymod") calls)
       (⊦= 2 ((lua-gettop) L))
       ((lua-settop 0) L)
       (⊦= LUA_OK ((luaL-dostring "R = mymod.answer() F = mymod.flag") L))
       (⊦= 42 (global L "R"))
       (⊦= #f (global L "F"))
       ;; an opener that fails: the error, the function consumed, nothing registered
       (let ((e (condition-case ((luaL-requiref "broken" (λ (C) ((luaL-error "cannot open %s" "broken") C)) #t) L)
                  (e (exn lua) e))))
         (⊨ (string-suffix? "cannot open broken" (msg e))))
       (⊦= 0 ((lua-gettop) L))
       (⊦= LUA_TNIL ((lua-getglobal "broken") L))
       ((lua-pop 1) L)
       (⊦raises (exn lua api) ((luaL-requiref "x" 42 #t) L))
       (⊦raises (exn lua api) ((luaL-requiref "a\x0;b" opener #t) L)))
     ;; setfuncs with shared upvalues, a C pointer, a placeholder
     ((luaL-newlibtable '(("a" . #f) ("b" . #f) ("c" . #f))) L)
     ((lua-pushinteger 10) L)
     ((lua-pushstring "up2") L)
     ((luaL-setfuncs `(("first" . ,(λ (C) ((lua-pushvalue (lua-upvalueindex 1)) C) 1))
                       ("second" . ,(λ (C) ((lua-pushvalue (lua-upvalueindex 2)) C) 1))
                       ("opener" . ,luaopen-math)
                       ("off" . #f))
                     2)
      L)
     (⊦= 1 ((lua-gettop) L))                                              ; the upvalues are popped
     ((lua-setglobal "LIB") L)
     (run! L "A, B = LIB.first(), LIB.second() OFF = LIB.off M = LIB.opener().pi")
     (⊦= 10 (global L "A"))
     (⊦= "up2" (global L "B"))
     (⊦= #f (global L "OFF"))
     (⊦= 3.141592653589793 (global L "M"))
     (⊦raises (exn lua api) ((luaL-setfuncs '(("x" . 42)) 0) L))           ; not a function
     (⊦raises (exn lua api) ((luaL-setfuncs '(("x" . #f)) 1) L))           ; no table under the upvalue
     (⊦raises (exn lua api) ((luaL-setfuncs 'nope 0) L))
     ;; setfuncs on a table whose __newindex fails: the error, the stack as before
     (run! L "RO = setmetatable({}, {__newindex = function(t, k) error('read-only ' .. k, 0) end})")
     ((lua-getglobal "RO") L)
     ((lua-pushinteger 1) L)
     (⊦= "read-only f" (condition-case ((luaL-setfuncs `(("f" . ,nop-callback)) 1) L) (e (exn lua) (msg e))))
     (⊦= 2 ((lua-gettop) L))
     ((lua-settop 0) L)
     ;; newlib
     ((luaL-newlib `(("sq" . ,(λ (C) ((lua-pushinteger (let ((n ((luaL-checkinteger 1) C))) (* n n))) C) 1)))) L)
     ((lua-setglobal "SQ") L)
     (run! L "N = SQ.sq(12)")
     (⊦= 144 (global L "N"))
     (⊦raises (exn lua api) ((luaL-newlib '(("x" . 1))) L))
     (⊦= 0 ((lua-gettop) L))
     ;; getsubtable
     (run! L "HOST = {existing = {}, scalar = 5}")
     ((lua-getglobal "HOST") L)
     (⊨ ((luaL-getsubtable 1 "existing") L))
     (⊭ ((luaL-getsubtable 1 "fresh") L))
     (⊭ ((luaL-getsubtable 1 "scalar") L))                                ; replaced by a table
     (⊦= 4 ((lua-gettop) L))
     (⊨ ((luaL-getsubtable -4 "fresh") L))
     (run! L "OK = type(HOST.fresh) == 'table' and type(HOST.scalar) == 'table'")
     (⊦= #t (global L "OK"))
     (⊦raises (exn lua api) ((luaL-getsubtable 9 "x") L))
     (⊦raises (exn lua api) ((luaL-getsubtable 1 "x\x0;") L))
     ((lua-settop 0) L)
     ;; makeseed: an unsigned int, with or without a state
     (let ((s ((luaL-makeseed) L)) (s0 ((luaL-makeseed) #f)))
       (⊨ (and (integer? s) (exact? s) (<= 0 s 4294967295)))
       (⊨ (and (integer? s0) (exact? s0) (<= 0 s0 4294967295))))
     (lua-close L)))

  ;; Scheme reached from inside protected S6 ops: string operands copied before Lua runs, errors
  ;; and escapes coming back through them
  ((test/s6-safe-path _)
   (let ((L (fresh)) (k #f) (calls 0))
     ((register! "idx" (λ (C) (set! calls (+ calls 1)) (gc #t) ((lua-pushnil) C) 1)) L)
     ((register! "esc" (λ (C) (k 'escaped))) L)
     ((register! "boom" (λ (C) (error "scheme boom"))) L)
     (run! L "H = setmetatable({}, {__index = function(t, k) return idx(t, k) end,
                                  __name = 'Hosted', m = function(o) return esc() end,
                                  e = function(o) return boom() end})")
     ((lua-getglobal "H") L)
     (do ((i 0 (+ i 1))) ((= i 200))
       (let ((name (string-append "sub-" (number->string i))))
         (⊭ ((luaL-getsubtable 1 name) L))
         ((lua-pop 1) L)))
     (⊦= 200 calls)
     (run! L "N = 0 for k, v in pairs(H) do if type(v) == 'table' then N = N + 1 end end OK = type(rawget(H, 'sub-199')) == 'table'")
     (⊦= 200 (global L "N"))
     (⊦= #t (global L "OK"))
     ;; an escape through luaL-callmeta completes, the state goes on
     (⊦= 'escaped (call/cc (λ (kk) (set! k kk) ((luaL-callmeta 1 "m") L) 'not-escaped)))
     ((lua-settop 1) L)
     ;; a Scheme error through luaL-callmeta comes back as itself
     (⊦= "scheme boom" (condition-case ((luaL-callmeta 1 "e") L) (e (exn) (msg e))))
     (⊦= 1 ((lua-gettop) L))
     (⊨ (string-prefix? "Hosted: 0x" ((luaL-tolstring 1) L)))
     ((lua-settop 0) L)
     ;; memory errors from S6 ops: raised, and the state recovers
     (run! L "M = collectgarbage('count')")
     ((lua-setmemorylimit (+ (inexact->exact (floor (* 1024 (global L "M")))) 100000)) L)
     (let ((big (make-string 300000 #\a)))
       (⊦= 'errmem (condition-case ((luaL-gsub big "a" "bb") L) (e (exn lua) (lua-condition-status e))))
       (⊦= 0 ((lua-gettop) L))
       ((lua-setmemorylimit 0) L)
       (⊦= 600000 (string-length ((luaL-gsub big "a" "bb") L))))
     ((lua-settop 0) L)
     (lua-close L)))

  ;; luaL_typeerror's message holds the whole __name, however long, as C's
  ((test/s6-typeerror-long-name _)
   (let ((L (fresh)))
     ((register! "CT" (λ (C) ((luaL-typeerror 1 "T") C))) L)
     ((register! "CU" (λ (C) ((luaL-checkudata 1 "T") C))) L)
     (for-each
       (λ (n)
         (let ((name (make-string n #\N)))
           ((lua-pushstring name) L) ((lua-setglobal "NAME") L)
           (run! L "OBJ = setmetatable({}, {__name = NAME})
                    local ok ok, C_MSG = pcall(string.rep, OBJ)
                    ok, S_MSG = pcall(CT, OBJ)
                    ok, U_MSG = pcall(CU, OBJ)")
           (⊦= (string-append "bad argument #1 to 'string.rep' (string expected, got " name ")") (global L "C_MSG"))
           (⊦= (string-append "bad argument #1 to 'CT' (T expected, got " name ")") (global L "S_MSG"))
           (⊦= (string-append "bad argument #1 to 'CU' (T expected, got " name ")") (global L "U_MSG"))
           ((lua-getglobal "OBJ") L)
           (⊦= (string-append "bad argument #1 (T expected, got " name ")")
               (condition-case ((luaL-typeerror 1 "T") L) (e (exn lua) (msg e))))
           (⊦= (string-append "bad argument #1 (number expected, got " name ")")
               (condition-case ((luaL-checkinteger 1) L) (e (exn lua) (msg e))))
           ((lua-pop 1) L)))
       '(127 128 200 5000))
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)))

  ;; the free list of luaL_ref (t[1]) overwritten: refused with a Lua error, never trusted
  ((test/s6-ref-free-list _)
   (let ((L (fresh)))
     ((lua-newtable) L)
     ((lua-pushinteger 10) L)
     (let ((r ((luaL-ref 1) L)))
       (⊨ (>= r 2))
       ((luaL-unref 1 r) L)
       (⊦raises (exn lua) ((luaL-unref 1 1) L))                         ; the free list itself
       (⊦raises (exn lua) ((luaL-unref 1 0) L))
       ((luaL-unref 1 LUA_NOREF) L)                                     ; ignored, as C
       ((luaL-unref 1 LUA_REFNIL) L))
     ((lua-newtable) L)
     (⊦raises (exn lua) ((luaL-unref -1 5) L))                          ; no free list yet
     ((lua-pop 1) L)
     ;; corrupted free lists
     ((lua-pushboolean #t) L) ((lua-rawseti 1 1) L)                     ; t[1] = true
     ((lua-pushinteger 1) L)
     (⊦raises (exn lua) ((luaL-ref 1) L))
     ((lua-pushinteger 99) L) ((lua-rawseti 1 1) L)                     ; t[1] -> t[99], a nil
     ((lua-pushinteger 1) L)
     (⊦raises (exn lua) ((luaL-ref 1) L))
     ((lua-settop 0) L)
     ;; the registry's, by Lua code: luaVs are refused, then work again once it is repaired
     ((lua-newtable) L)
     (let ((v ((lua-toref -1) L)))
       ((lua-pop 1) L)
       (run! L "local r = debug.getregistry() SAVED = r[1] r[1] = {}")
       ((lua-newtable) L)
       (⊦raises (exn lua) ((lua-toref -1) L))
       (run! L "debug.getregistry()[1] = SAVED")
       (⊨ (lua-ref? ((lua-toref -1) L)))
       ((lua-pop 1) L)
       (⊦= LUA_TTABLE ((lua-pushref v) L))
       ((lua-pop 1) L))
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)))

  ;; lauxlib reads a free list's head as `(int)lua_tointeger' and a new ref as
  ;; `(int)lua_rawlen(t) + 1': a head or a border past INT_MAX would be truncated into another
  ;; slot -- a negative ref, taken for one of the module's internal codes (a raise popping a value
  ;; of the caller, an abort), or a reserved slot of the registry.  Refused with an (exn lua) error
  ;; before anything is stored; the caller's values stay where they were.
  ((test/s6-ref-truncated-heads _)
   (let ((L (lua-newstate 0)))                                      ; seed 0: rawlen's hash search is deterministic
     (define (outcome thunk) (condition-case (begin (thunk) 'no-raise) (e (exn lua) (msg e))))
     (define corrupted "aux.lua: the reference free list of this table is corrupted")
     (define heads (list (- (expt 2 32) 1001) (+ (expt 2 32) 2) (- (expt 2 32) 2012) (- (expt 2 32) 1201)
                         (expt 2 31) (- (expt 2 63) 1)))
     ;; a table of the caller: stack keep-me t v
     (for-each
       (λ (head)
         ((lua-pushstring "keep-me") L)
         ((lua-newtable) L)
         ((lua-pushinteger head) L) ((lua-rawseti 2 1) L)
         ((lua-pushinteger 0) L) ((lua-rawseti 2 head) L)
         ((lua-pushstring "v") L)
         (⊦= corrupted (outcome (λ () ((luaL-ref 2) L))))
         (⊦= 2 ((lua-gettop) L))                                   ; v consumed, as a successful ref
         (⊦= "keep-me" ((lua-tostring 1) L))
         (⊦= head (begin ((lua-rawgeti 2 1) L) ((lua-tointeger -1) L)))  ; the list is untouched
         ((lua-pop 1) L)
         (⊦= 0 (begin ((lua-rawgeti 2 head) L) ((lua-tointeger -1) L)))
         ((lua-pop 1) L)
         (run! L "")                                                ; the state goes on
         ((lua-settop 0) L))
       heads)
     ;; the registry, through lua-toref and luaL-ref: nothing stored, not in a reserved slot either
     (let ((reg-head (λ (head)
                       ((lua-pushinteger head) L) ((lua-rawseti LUA_REGISTRYINDEX 1) L)
                       ((lua-pushinteger 0) L) ((lua-rawseti LUA_REGISTRYINDEX head) L)))
           (reg-clear (λ (head)
                        ((lua-pushnil) L) ((lua-rawseti LUA_REGISTRYINDEX head) L)
                        ((lua-pushinteger 0) L) ((lua-rawseti LUA_REGISTRYINDEX 1) L))))
       (for-each
         (λ (head)
           (reg-head head)
           ((lua-pushstring "v") L)
           (⊦= corrupted (outcome (λ () ((lua-toref -1) L))))
           (⊦= 1 ((lua-gettop) L))
           (⊦= corrupted (outcome (λ () ((luaL-ref LUA_REGISTRYINDEX) L))))
           (⊦= 0 ((lua-gettop) L))
           (⊦= LUA_TTABLE ((lua-rawgeti LUA_REGISTRYINDEX LUA_RIDX_GLOBALS) L))
           (⊦= LUA_TTHREAD ((lua-rawgeti LUA_REGISTRYINDEX LUA_RIDX_MAINTHREAD) L))
           ((lua-settop 0) L)
           (reg-clear head))
         heads)
       ;; a head naming a reserved slot (here the main thread's, an integer planted by Lua code)
       ((lua-rawgeti LUA_REGISTRYINDEX LUA_RIDX_MAINTHREAD) L)                 ; 1: saved
       ((lua-pushinteger 1) L) ((lua-rawseti LUA_REGISTRYINDEX 1) L)          ; placeholder
       (reg-head LUA_RIDX_MAINTHREAD)
       ((lua-pushstring "v") L)
       (⊦= corrupted (outcome (λ () ((lua-toref -1) L))))
       ((lua-settop 1) L)
       ((lua-rawseti LUA_REGISTRYINDEX LUA_RIDX_MAINTHREAD) L)
       ((lua-pushinteger 0) L) ((lua-rawseti LUA_REGISTRYINDEX 1) L)
       ;; the next new ref would be a reserved slot: the registry's border lowered below it
       ((lua-rawgeti LUA_REGISTRYINDEX LUA_RIDX_MAINTHREAD) L)
       ((lua-pushnil) L) ((lua-rawseti LUA_REGISTRYINDEX LUA_RIDX_MAINTHREAD) L)
       ((lua-rawgeti LUA_REGISTRYINDEX LUA_RIDX_GLOBALS) L)
       ((lua-pushstring "v") L)
       (⊦= "aux.lua: the next reference of this table would be a reserved slot (3)" (outcome (λ () ((lua-toref -1) L))))
       ((lua-pop 2) L)
       ((lua-rawseti LUA_REGISTRYINDEX LUA_RIDX_MAINTHREAD) L)
       (⊦= 0 ((lua-gettop) L))
       ;; repaired: references work again, above the reserved slots
       ((lua-pushstring "w") L)
       (let ((v ((lua-toref -1) L)))
         ((lua-pop 1) L)
         (⊦= LUA_TSTRING ((lua-pushref v) L))
         (⊦= "w" ((lua-tostring -1) L))
         ((lua-pop 1) L))
       ((lua-pushstring "w") L)
       (⊨ (> ((luaL-ref LUA_REGISTRYINDEX) L) LUA_RIDX_LAST)))
     ;; a border past INT_MAX (keys 1, 2, 4, ..., 2^31 in the hash part; 32 keys): no ref is left
     ((lua-pushstring "keep-me") L)
     ((lua-createtable 0 64) L)
     ((lua-pushinteger 0) L) ((lua-rawseti 2 1) L)
     (do ((k 1 (+ k 1))) ((> k 31)) ((lua-pushboolean #t) L) ((lua-rawseti 2 (expt 2 k)) L))
     (⊦= (expt 2 31) ((lua-rawlen 2) L))
     ((lua-pushstring "v") L)
     (⊦= "aux.lua: no reference is left in this table (its border is 2147483648)" (outcome (λ () ((luaL-ref 2) L))))
     (⊦= 2 ((lua-gettop) L))
     (⊦= "keep-me" ((lua-tostring 1) L))
     (⊦= LUA_TNIL ((lua-rawgeti 2 (- 1 (expt 2 31))) L))      ; nothing at the truncated key
     ((lua-settop 0) L)
     (lua-close L)))

  ;; luaL-loadbuffer: chunk names, binary chunks, NULs, bytevectors
  ((test/s6-loadbuffer _)
   (let ((L (fresh)))
     (⊦= LUA_OK ((luaL-loadbuffer "return 1 + 1" "=first") L))
     ((lua-call 0 1) L)
     (⊦= 2 ((lua-tointeger -1) L))
     ((lua-pop 1) L)
     (⊦= LUA_ERRSYNTAX ((luaL-loadbuffer "return +" "=my chunk") L))       ; the name names the chunk
     (⊨ (string-prefix? "my chunk:1:" ((lua-tostring -1) L)))
     ((lua-pop 1) L)
     (⊦= LUA_OK ((luaL-loadbuffer "error('x')" "@file.lua") L))
     (⊦= LUA_ERRRUN ((lua-pcall 0 0 0) L))
     (⊦= "file.lua:1: x" ((lua-tostring -1) L))
     ((lua-pop 1) L)
     ((luaL-loadbuffer "return 1" "=named") L)
     (let ((d ((lua-getinfo ">S" #f) L)))
       (⊦= "=named" (lua-debug-source d))
       (⊦= "named" (lua-debug-short-src d)))
     ((luaL-loadstring "return 'dumped'") L)                             ; mode "bt": binary chunks too
     (let ((bin ((lua-dump #t) L)))
       ((lua-pop 1) L)
       (⊦= LUA_OK ((luaL-loadbuffer bin "=bin") L))
       ((lua-call 0 1) L)
       (⊦= "dumped" ((lua-tostring -1) L))
       ((lua-pop 1) L))
     (⊦= LUA_OK ((luaL-loadbuffer "return 'a\x0;b'" "=nul") L))          ; NULs in the buffer
     ((lua-call 0 1) L)
     (⊦= 3 ((lua-rawlen -1) L))
     ((lua-pop 1) L)
     (⊦= LUA_OK ((luaL-loadbuffer (string->utf8 "return 3") "=bv") L))
     ((lua-call 0 1) L)
     (⊦= 3 ((lua-tointeger -1) L))
     ((lua-pop 1) L)
     (⊦raises (exn lua api) ((luaL-loadbuffer "return 1" "a\x0;b") L))  ; a C string name
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)))

  ;; a __name that is not UTF-8: Lua sees C's own bytes; the Scheme condition's message is a valid
  ;; string, with U+FFFD for the bytes that are not UTF-8
  ((test/s6-typeerror-name-bytes _)
   (let ((L (fresh)))
     ((register! "CT" (λ (C) ((luaL-typeerror 1 "T") C))) L)
     ((register! "CI" (λ (C) ((luaL-checkinteger 1) C))) L)
     (run! L "OBJ = setmetatable({}, {__name = 'ab\\xf4\\x90cd\\xff'})
              local ok ok, C_MSG = pcall(string.rep, OBJ)
              ok, S_MSG = pcall(CT, OBJ)
              ok, I_MSG = pcall(CI, OBJ)")
     ((lua-getglobal "C_MSG") L) ((lua-getglobal "S_MSG") L) ((lua-getglobal "I_MSG") L)
     (let ((suffix (λ (i) (let ((b ((lua-tobytevector i) L))) (bytevector-prefix-from b (- (bytevector-length b) 8))))))
       (⊦= (list (bytevector 97 98 #xf4 #x90 99 100 #xff 41)) (list (suffix 1)))   ; "ab\xf4\x90cd\xff)"
       (⊦= (suffix 1) (suffix 2))
       (⊦= (suffix 1) (suffix 3)))
     ((lua-settop 0) L)
     ((lua-getglobal "OBJ") L)
     (for-each
       (λ (m)
         (⊨ (string? m))
         (⊦= m (utf8->string (string->utf8 m)))                               ; valid UTF-8
         (⊨ (string-suffix? "got ab\xFFFD;\xFFFD;cd\xFFFD;)" m)))
       (list (condition-case ((luaL-typeerror 1 "T") L) (e (exn lua) (msg e)))
             (condition-case ((luaL-checkinteger 1) L) (e (exn lua) (msg e)))
             (condition-case ((luaL-checkudata 1 "T") L) (e (exn lua) (msg e)))))
     ((lua-pop 1) L)
     (lua-close L)))

  ;; luaL-len: the length (metamethods included) as an exact integer, the stack unchanged
  ((test/s6-len _)
   (let ((L (fresh)))
     (run! L "T3 = {1, 2, 3}
              M2 = setmetatable({}, {__len = function() return 2 end})
              MF = setmetatable({}, {__len = function() return 4.0 end})
              MX = setmetatable({}, {__len = function() return 2.5 end})")
     (for-each (λ (g n)
                 ((lua-getglobal g) L)
                 (⊦eqv n ((luaL-len 1) L))
                 (⊦= 1 ((lua-gettop) L))
                 ((lua-pop 1) L))
               '("T3" "M2" "MF")
               '(3 2 4))
     ((lua-pushstring "abcde") L)
     (⊦eqv 5 ((luaL-len -1) L))
     ((lua-getglobal "MX") L)
     (⊦= "object length is not an integer" (condition-case ((luaL-len -1) L) (e (exn lua) (msg e))))
     (⊦= 2 ((lua-gettop) L))
     ((lua-settop 0) L)
     (lua-close L)))

  ;; a positive obj above the top is the nil value, as in C: with a metatable for the type nil,
  ;; luaL_getmetafield finds its fields, and luaL_callmeta calls the metafield with the value at
  ;; obj -- which, once the metafield is pushed there, is the metafield itself (C's own behaviour)
  ((test/s6-metafield-nil-type _)
   (let ((L (fresh)))
     (⊦= LUA_TNIL ((luaL-getmetafield 1 "kind") L))
     (⊭ ((luaL-callmeta 1 "__index") L))
     (⊦= 0 ((lua-gettop) L))
     (run! L "debug.setmetatable(nil, {__index = function(x) return type(x) end, kind = 'K'})")
     ((lua-settop 0) L)
     (⊦= LUA_TSTRING ((luaL-getmetafield 1 "kind") L))
     (⊦= '(1 "K") (list ((lua-gettop) L) ((lua-tostring -1) L)))
     ((lua-settop 0) L)
     ((lua-pushinteger 5) L)
     (⊦= LUA_TSTRING ((luaL-getmetafield 3 "kind") L))
     (⊦= '(2 "K") (list ((lua-gettop) L) ((lua-tostring -1) L)))
     ((lua-settop 1) L)
     (⊦= LUA_TFUNCTION ((luaL-getmetafield 99 "__index") L))
     (⊦= 2 ((lua-gettop) L))
     ((lua-settop 1) L)
     (⊦= LUA_TNIL ((luaL-getmetafield 2 "missing") L))              ; nothing pushed
     (⊦= 1 ((lua-gettop) L))
     (⊨ ((luaL-callmeta 2 "__index") L))
     (⊦= '(2 "function") (list ((lua-gettop) L) ((lua-tostring -1) L)))
     ((lua-settop 0) L)
     (⊨ ((luaL-callmeta 1 "__index") L))
     (⊦= '(1 "function") (list ((lua-gettop) L) ((lua-tostring -1) L)))
     ((lua-settop 0) L)
     (⊭ ((luaL-callmeta 1 "missing") L))
     (⊦= 0 ((lua-gettop) L))
     ((lua-pushnil) L)                                               ; a real nil slot: the same metatable
     (⊨ ((luaL-callmeta 1 "__index") L))
     (⊦= '(2 "nil") (list ((lua-gettop) L) ((lua-tostring -1) L)))
     ((lua-settop 0) L)
     ;; luaL_typeerror names an absent argument by the __name of the type nil too (C: "got NilT")
     (run! L "debug.setmetatable(nil, {__name = 'NilT'})")
     ((lua-settop 0) L)
     (⊦= "bad argument #3 (number expected, got NilT)" (condition-case ((luaL-typeerror 3 "number") L) (e (exn lua) (msg e))))
     ((register! "f" (λ (C) ((luaL-checkinteger 2) C) 0)) L)
     ((register! "g" (λ (C) ((luaL-typeerror 3 "number") C))) L)
     (⊦= "[string \"f(1)\"]:1: bad argument #2 to 'f' (number expected, got NilT)" (condition-case (run! L "f(1)") (e (exn lua) (msg e))))
     (⊦= "[string \"g(1)\"]:1: bad argument #3 to 'g' (number expected, got NilT)" (condition-case (run! L "g(1)") (e (exn lua) (msg e))))
     (run! L "debug.setmetatable(nil, {__name = 5})")                ; not a string: "no value", as C
     (⊦= "[string \"g(1)\"]:1: bad argument #3 to 'g' (number expected, got no value)" (condition-case (run! L "g(1)") (e (exn lua) (msg e))))
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)))

  ;; lua-format / lua-pushfstring / luaL-error with many directives (no apply over the pieces:
  ;; CHICKEN's apply does not take ~32000 arguments)
  ((test/s6-format-many-directives _)
   (let* ((n 40000)
          (rep (λ (piece) (call-with-output-string (λ (o) (do ((i 0 (+ i 1))) ((= i n)) (display piece o))))))
          (fmt (rep "5%% "))
          (want (rep "5% ")))
     (⊦= want (lua-format fmt))
     (⊦= (string-length want) (string-length (lua-format (string-append fmt "%s") "")))
     (let ((L (fresh)))
       ((lua-pushfstring fmt) L)
       (⊦= want ((lua-tostring -1) L))
       ((lua-pop 1) L)
       (⊦= want (condition-case ((luaL-error fmt) L) (e (exn lua) (msg e))))
       ((register! "f" (λ (C) ((luaL-error fmt) C))) L)
       (⊦= (string-append "[string \"return f()\"]:1: " want)
           (condition-case (run! L "return f()") (e (exn lua) (msg e))))
       (⊦= 0 ((lua-gettop) L))
       (lua-close L))))

  ;; an error message that is not UTF-8: every byte >= 128 is U+FFFD in the message (the value
  ;; keeps the bytes); built without a character list, so a large binary error value is cheap
  ((test/s6-lossy-error-message _)
   (let ((L (fresh)))
     (let ((e (condition-case (run! L "error('a\\255b\\0c\\200', 0)") (e (exn lua) e))))
       (⊦= "a\xFFFD;b\x0;c\xFFFD;" (msg e))
       (⊦= (bytevector 97 255 98 0 99 200) (lua-condition-value e)))
     (let ((e (condition-case (run! L "error('\\xE2\\x82', 0)") (e (exn lua) e))))  ; a truncated sequence
       (⊦= "\xFFFD;\xFFFD;" (msg e)))
     ((lua-pushinteger 3000000) L) ((lua-setglobal "N") L)
     (let ((m (condition-case (run! L "error(string.rep('\\255', N), 0)") (e (exn lua) (msg e)))))
       (⊦= 3000000 (string-length m))
       (⊦= #\xFFFD (string-ref m 0))
       (⊦= #\xFFFD (string-ref m 2999999)))
     ;; linear: an invalid message costs about what a valid one of the same size does (a character
     ;; list took ~10x the time and ~90 bytes of heap per byte)
     ((lua-pushinteger 4000000) L) ((lua-setglobal "N") L)
     (let ((cost (λ (byte)
                   (gc #t)
                   (let ((t0 (current-process-milliseconds)))
                     (⊦= 4000000 (string-length (condition-case (run! L (string-append "error(string.rep('" byte "', N), 0)"))
                                                  (e (exn lua) (msg e)))))
                     (- (current-process-milliseconds) t0)))))
       (let* ((valid (cost "a")) (invalid (cost "\\255")))
         (⊨ (<= invalid (+ (* 3 valid) 200)))))
     (⊦= 0 ((lua-gettop) L))
     (lua-close L)))

  ;; registry[LUA_RIDX_GLOBALS] replaced by Lua code (the debug library): every binding that would
  ;; reach lapi.c's getGlobalTable (lua_load and the luaL loaders, luaL_requiref with glb, the
  ;; library openers) refuses with (exn lua api) AUX_EGLOBALS before Lua runs, nothing pushed or
  ;; consumed; an API-checked Lua aborted in its api_check, a release one made the non-table the
  ;; chunk's _ENV.  A reader that replaces it during a lua-load stops the load likewise.
  ((test/s6-registry-globals-loads _)
   (let ((L (fresh)) (file (create-temporary-file ".lua")))
     (define (code-of thunk)
       (condition-case (begin (thunk) 'no-raise)
         (e (exn lua api) (list (get-condition-property e 'lua 'code) ((condition-property-accessor 'exn 'location) e)))))
     (define (refused who thunk) (⊦= (list AUX_EGLOBALS who) (code-of thunk)) (⊦= 1 ((lua-gettop) L)))
     (define (corrupt!) (run! L "debug.getregistry()[2] = 5"))
     (define (restore!) ((lua-pushvalue 1) L) ((lua-rawseti LUA_REGISTRYINDEX LUA_RIDX_GLOBALS) L))
     (with-output-to-file file (λ () (display "return 7")))
     ((lua-rawgeti LUA_REGISTRYINDEX LUA_RIDX_GLOBALS) L)              ; 1: the globals
     (corrupt!)
     (refused 'luaL-loadstring (λ () ((luaL-loadstring "return 1") L)))
     (refused 'luaL-loadbufferx (λ () ((luaL-loadbufferx "return 1" "=chunk" "t") L)))
     (refused 'luaL-loadbufferx (λ () ((luaL-loadbuffer (string->utf8 "return 1") "=chunk") L)))
     (refused 'luaL-loadstring (λ () ((luaL-dostring "return 1") L)))
     (refused 'lua-load (λ () ((lua-load "return 1" "=chunk" #f) L)))
     (refused 'lua-load (λ () ((lua-load (let ((n 0)) (λ () (set! n (+ n 1)) (and (= n 1) "return 1"))) "=chunk" #f) L)))
     (refused 'luaL-loadfilex (λ () ((luaL-loadfilex file "t") L)))
     (refused 'luaL-loadfilex (λ () ((luaL-dofile file) L)))
     (refused 'luaL-requiref (λ () ((luaL-requiref "base2" luaopen-base #t) L)))
     (refused 'luaL-requiref (λ () ((luaL-requiref "mine" (λ (C) ((lua-newtable) C) 1) #t) L)))
     (refused 'luaL-openlibs (λ () ((luaL-openlibs) L)))
     (refused 'luaL-openselectedlibs (λ () ((luaL-openselectedlibs LUA_COLIBK 0) L)))
     ;; no global is set: fine
     ((luaL-requiref "mine" (λ (C) ((lua-newtable) C) 1) #f) L)
     ((lua-pop 1) L)
     ((luaL-openselectedlibs 0 LUA_COLIBK) L)                          ; preload only
     ;; lua_getglobal / lua_setglobal: a Lua error (indexing a number), not an abort
     (⊦raises (exn lua) ((lua-getglobal "x") L))
     ((lua-pushinteger 1) L)
     (⊦raises (exn lua) ((lua-setglobal "x") L))
     (⊦= 1 ((lua-gettop) L))
     (restore!)
     (⊦= LUA_OK ((luaL-loadfilex file "t") L))
     ((lua-call 0 1) L)
     (⊦= 7 ((lua-tointeger -1) L))
     ((lua-pop 1) L)
     ;; a luaL-requiref open function that replaces it: the op's own check raises a Lua error before
     ;; lua_setglobal (whose api_check aborts an API-checked Lua; a release one would raise
     ;; "attempt to index a number value" instead, so the text is what shows the check ran)
     (⊦= '(errrun "aux.lua: registry[LUA_RIDX_GLOBALS] is not a table")
         (condition-case ((luaL-requiref "mine2" (λ (C) (run! C "debug.getregistry()[2] = 5") ((lua-newtable) C) 1) #t) L)
           (e (exn lua) (list (lua-condition-status e) (msg e)))))
     (⊦= 1 ((lua-gettop) L))
     (⊦= LUA_TNUMBER ((lua-rawgeti LUA_REGISTRYINDEX LUA_RIDX_GLOBALS) L))  ; the open function did replace it
     ((lua-pop 1) L)
     ((lua-getfield LUA_REGISTRYINDEX LUA_LOADED_TABLE) L)
     (⊦= LUA_TTABLE ((lua-getfield -1 "mine2") L))                  ; LOADED is set first, as C
     ((lua-pop 2) L)
     (restore!)
     ;; a reader that replaces it: the load stops (the parse would read it once it ends)
     (let* ((n 0)
            (reader (λ () (set! n (+ n 1))
                         (case n ((1) (corrupt!) "return ") ((2) "1") (else #f)))))
       (refused 'lua-load (λ () ((lua-load reader "=chunk" #f) L)))
       (⊦= 1 n))                                                        ; stopped after the first piece
     (restore!)
     ;; restored: everything works again
     (⊦= LUA_OK ((luaL-loadstring "return print ~= nil") L))
     ((lua-call 0 1) L)
     (⊦= #t ((lua-toboolean -1) L))
     ((lua-pop 1) L)
     ((luaL-requiref "mine3" (λ (C) ((lua-pushinteger 3) C) 1) #t) L)
     ((lua-pop 1) L)
     (⊦= 3 (global L "mine3"))
     ;; luaL-unref never frees the registry's reserved slots: unref 2 stored an integer in the globals
     (⊦raises (exn lua) ((luaL-unref LUA_REGISTRYINDEX LUA_RIDX_GLOBALS) L))
     (⊦raises (exn lua) ((luaL-unref LUA_REGISTRYINDEX LUA_RIDX_MAINTHREAD) L))
     (⊦= LUA_TTABLE ((lua-rawgeti LUA_REGISTRYINDEX LUA_RIDX_GLOBALS) L))
     (⊦= LUA_TTHREAD ((lua-rawgeti LUA_REGISTRYINDEX LUA_RIDX_MAINTHREAD) L))
     ((lua-settop 0) L)
     (delete-file file)
     (lua-close L)))
  )

(unittest/✓ lua-auxlib-suite)
