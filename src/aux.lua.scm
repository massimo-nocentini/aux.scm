
;; No timer interrupt checks in this module: a finalizer or a thread switch cannot run between
;; the moment a binding reads a lua_State pointer and the moment its C stub uses it (see %busy-ctx).
(declare (disable-interrupts))

;; The implementation: every definition, the raw stubs included.  Private: it exists for the test
;; suite (import (aux lua) first, it loads the code), and its raw stubs skip every check the
;; public bindings make.  The public module, (aux lua), is at the end of this file.
(module (aux lua internal) *

  (import
    scheme
    (chicken base)
    (chicken foreign)
    (chicken gc)
    (chicken condition)
    (chicken fixnum)
    (chicken string)
    (only (chicken port) call-with-output-string)
    (chicken pretty-print)
    (only (chicken memory) pointer?)
    (only (chicken locative) make-weak-locative locative->object)
    (only (chicken bytevector) bytevector? make-bytevector bytevector-length bytevector-u8-ref
                               bytevector-u8-set! bytevector bytevector-append)
    (only (scheme base) utf8->string string->utf8 call/cc exact-integer?)
    (aux base))

  ;; ==========================================================================================
  ;; (aux lua): curried bindings of the Lua 5.5 C API (lua.h, lauxlib.h, lualib.h, and the
  ;; API macros of luaconf.h: lua-strlen, lua-objlen, lua-equal, lua-lessthan).
  ;;
  ;; Names: lua_foo / luaL_foo / luaopen_foo (functions and macros) become lua-foo / luaL-foo /
  ;; luaopen-foo; constants keep their C spelling (LUA_TNIL, LUA_MULTRET, ...).
  ;;
  ;; Currying: every binding whose C signature has a lua_State takes all the other arguments
  ;; first and answers a procedure of ONE argument, the state (a luaR record):
  ;;
  ;;     ((lua-pushinteger 42) L)   ((lua-getglobal "x") L)   ((lua-gettop) L)
  ;;     ((lua-xmove to n) from)    ((lua-resume from nargs) co)   ; C's first lua_State goes last
  ;;
  ;; so bindings compose as reader-monad actions (see (aux category reader)).  Three bindings
  ;; answer several values, as C's out-parameters: lua-tonumberx and lua-tointegerx (the value
  ;; and isnum) and lua-resume (the status and nresults); a reader bind keeps only the first, so
  ;; inside do/monad wrap them as (λ (L) (call-with-values (λ () ((lua-tointegerx i) L)) list)).
  ;; Not curried:
  ;; constants, luaopen-* (c-pointer values), lua-upvalueindex, the lua-debug-* accessors,
  ;; lua-type->symbol, lua-format (it returns text, no state is involved), lua-ref?,
  ;; lua-live-handles, the condition accessors, the record predicates (luaR?, luaR-kind,
  ;; luaDebug?), lua-make-empty, lua-newstate, luaL-newstate and lua-close (the same list as in
  ;; "Exports" below).
  ;;
  ;; Safety model, in one paragraph: a Lua error is a C longjmp, and a longjmp must never cross
  ;; a CHICKEN frame; a Scheme continuation or condition must never cross a Lua C frame either.
  ;; So every C call that may raise runs inside a lua_pcall made by the SAME C stub
  ;; (`aux_protect'), and a Lua error comes back to Scheme as a status that the binding turns
  ;; into an (exn lua) condition.  That trampoline is invisible to call and return hooks, as a C
  ;; API call is, so no error can come after its op's body has returned: a binding never takes a
  ;; committed op (a handle Lua now owns) for a failed one (a count hook installed with a call or
  ;; return hook restarts its count, see aux_hooks_aside).
  ;; Scheme procedures called by Lua run under a guard that stops
  ;; every exit (condition or continuation) at the callback boundary; the C trampoline performs
  ;; the corresponding Lua action (lua_error, luaL_argerror, lua_yield, ...) only after the
  ;; callback has returned.  One consequence: a Lua error raised by a binding inside a callback
  ;; (lua-call, lua-gettable, luaL-dostring under lua-check, ...) is caught at that binding,
  ;; crosses the callback as an (exn lua) condition and is raised again by the trampoline once
  ;; the callback has returned.  Its value (eq?) and status survive, but a message handler of an
  ;; enclosing xpcall or lua-pcall runs at the callback's frame, not where the error was raised:
  ;; debug.traceback there no longer shows the frames between the two.  A callback that needs
  ;; them calls with its own handler: ((lua-pcall nargs nres msgh) C).  Lua never holds a
  ;; Scheme object: only integer handles into a
  ;; Scheme vector of its universe (luaU-objects).  A callback cannot be entered with the C stack
  ;; (CHICKEN's nursery) nearly exhausted: Lua then gets its own "C stack overflow" error.
  ;;
  ;; Error rule: where C raises, the binding raises (exn lua) (status 'errrun 'errsyntax 'errmem
  ;; 'errerr 'errfile, `value' the error value: for luaL-error, luaL-argerror, luaL-typeerror and
  ;; the K checks on them, the string C raises, luaL_where and all, see %errval); where C returns a status, the binding returns it
  ;; and leaves the error value on the stack, as C ((lua-check m) turns a status into a raise).
  ;; One deliberate deviation: luaL-dostring and luaL-dofile answer the failing status (errsyntax,
  ;; errrun, errmem, errfile) where C's macros answer 1, so that lua-check can raise it (1 is
  ;; LUA_YIELD); both still answer LUA_OK on success.
  ;; A binding that refuses before Lua runs (bad index, closed state, suspended thread, NUL in a
  ;; C string, ...) raises (exn lua api): with API checks compiled out of liblua, such a call
  ;; would corrupt memory.  One registry slot is guarded that way (best-effort: see the trust
  ;; model below): registry[LUA_RIDX_GLOBALS]
  ;; takes tables only (lua-rawseti, lua-seti, lua-rawset, lua-settable refuse any other value,
  ;; code AUX_EGLOBALS), and the loaders, luaL-requiref with glb and the library openers refuse
  ;; to run while Lua code (the debug library) has left a non-table there; replaced while such a
  ;; call runs (a luaL-requiref open function, a finalizer), the call raises the Lua error
  ;; "aux.lua: registry[LUA_RIDX_GLOBALS] is not a table" before reaching it.  luaL-ref and
  ;; lua-toref raise a Lua error rather than let lauxlib truncate a free-list head or a border
  ;; past INT_MAX into another key, or hand out a reserved registry slot.  A c-pointer becomes a
  ;; lua_CFunction only when the module vouches for it (luaopen-*, or what lua-tocfunction read from
  ;; a light C function: aux_cf_vouched); any other address would be called as code.  The
  ;; metatables the module owns are not settable (AUX_EMETATABLE): a Scheme handle's (its __gc
  ;; releases the Scheme object; FILE*'s would make liolib read the handle as a stream), the
  ;; registry's (a __mode would let the collector clear registry[LUA_RIDX_GLOBALS]), nor is the
  ;; handle metatable given to anything else (a metatable, or a registry entry under a string key).
  ;;
  ;; Trust model for the registry entries of Lua's standard libraries: the bound API is exactly as
  ;; trusting as the C API.  Those libraries keep private state in the registry and never check it
  ;; (registry._IO_input / _IO_output, "FILE*" and the metatables of other library userdata,
  ;; "_UBOX*", _HOOKKEY, _CLIBS, _LOADED, _PRELOAD, LUA_RIDX_GLOBALS, LUA_RIDX_MAINTHREAD, and any
  ;; other entry a library of this or a later Lua keeps): as in C, the caller must not overwrite
  ;; them with values of another shape, and a program that does may crash the process, as the same
  ;; program in C would.  That is caller misuse, not a defect of the bindings.  The guards below
  ;; and above (registry[LUA_RIDX_GLOBALS], "FILE*", "_UBOX*", _CLIBS) are best-effort: they stop
  ;; some common forgeries through the bound API, cover no other entry, and are no guarantee.
  ;; The guarded cases: a full userdata whose metatable is registry["FILE*"] is taken for liolib's
  ;; LStream (its closef is called), one with registry["_UBOX*"] for lauxlib's buffer box (its
  ;; block is realloc'd), and a userdata at registry._CLIBS[path] for a dlopen handle (dlsym).  So
  ;; those two metatables are given to no userdata (full or light) that does not have it already,
  ;; and no metatable to registry._CLIBS (AUX_EMETATABLE); the three entries are only removed (nil)
  ;; or made fresh by luaL-newmetatable, and registry._CLIBS takes no userdata under a string key
  ;; (AUX_ELIBRARY: lua-setfield, lua-setglobal, lua-settable, lua-rawset).
  ;;
  ;; C ints used as booleans (lua-pushboolean's b, lua-dump's strip, lua-warning's tocont,
  ;; luaL-requiref's glb, luaL-argcheck's and luaL-argexpected's cond) take #t or #f only, and
  ;; refuse anything else with (exn lua api): with Scheme truthiness 0 would be true, the
  ;; opposite of C, and a ported call would silently invert.
  ;;
  ;; Binary chunks are the one input no binding can check: Lua does not verify bytecode (manual,
  ;; section 3.3.2 and load), so a corrupted or malicious binary chunk crashes the process or
  ;; corrupts its memory.  lua-load, luaL-loadbufferx, luaL-loadfilex (mode #f, "b" or "bt") and
  ;; luaL-loadbuffer, luaL-loadstring, luaL-dostring, luaL-loadfile, luaL-dofile (always "bt": a
  ;; string starting with "\27Lua" is taken as binary) accept them, as C does; load only chunks
  ;; you produced (lua-dump) or trust, and use mode "t" for anything else.  Lua code's own load,
  ;; loadfile and dofile have the same caveat.
  ;;
  ;; The debug library is another: untrusted Lua code must not get it.  A Lua __gc finalizer runs
  ;; inside any allocating API call, and from there (or from a hook) debug.setlocal rewrites the
  ;; "(C temporary)" slots of the C function below it, which that function trusts.  The module's own
  ;; op bodies check their slots again after each allocating call (aux_slot_bad: a Lua error, never
  ;; a misapplied typed call) and lua-getlocal / lua-setlocal refuse op frames, but Lua's own C code
  ;; does not check: its parser (load, lua-load, luaL-loadstring, ...) and string.format crash
  ;; the process that way, as in stock Lua 5.5, and other C functions of its libraries may too.
  ;; The registry is in the same class once it leaves the bound API: the bindings check some of
  ;; their own direct writes (above, best-effort), but not what Lua code does with the registry or registry._CLIBS once
  ;; it can reach them (debug.getregistry; a global, a field, an upvalue the bound API stored; the
  ;; registry made the globals, registry[LUA_RIDX_GLOBALS], or registry._LOADED, which require and
  ;; luaL-requiref then write), nor a write a binding makes through a metamethod (a table whose
  ;; __newindex is the registry or _CLIBS).  From there plain Lua code can make a userdata's
  ;; metatable registry["FILE*"] or put io.stdout into _CLIBS, and io.close or package.loadlib
  ;; then crashes the process.
  ;;
  ;; Execution classes of the bindings:
  ;;   D   never allocates, raises or runs Lua code: a validated direct call;
  ;;   P   may raise, allocate or run code: runs under aux_protect, through a fast stub (no
  ;;       Scheme callable is live in the universe) or a safe stub (Lua may call Scheme back);
  ;;   P*  a push-only P, which on a suspended or dead thread runs on the universe's main
  ;;       thread (always LUA_OK) and is lua_xmove'd over;
  ;;   S   a status-returning call that protects itself (lua_pcall, lua_resume, lua_load, ...);
  ;;   T   a tail request (lua-error, luaL-error, luaL-argerror, luaL-typeerror, lua-yield,
  ;;       lua-yieldk): raised in Scheme, performed by the C trampoline after the callback returned;
  ;;   K   a Scheme composition of other bindings.
  ;;
  ;; Invariants the code refers to:
  ;;   I1  no longjmp crosses a CHICKEN frame (panics are unreachable: aux_panic aborts; Lua 5.5's
  ;;       coroutine.close() of the running coroutine, which jumps to the thread's OUTERMOST
  ;;       protected call, is refused while a binding runs Lua on that thread, and always on the
  ;;       main thread, known by its address: see the barriers);
  ;;   I2  aux_reentrant is 0 whenever Scheme runs; safe stubs set it around Lua, and every C
  ;;       entry into Scheme (cfunction, k-function, hook, reader) saves, clears and restores it,
  ;;       refusing to call Scheme when it is 0 (a fast stub is running);
  ;;   I3  a fast stub is used only while the universe holds no live callable handle; the count
  ;;       is raised before Lua can reach a handle and lowered only when Scheme drains releases;
  ;;   I4  Lua never uses Scheme memory across a possible callback: operands are copied first;
  ;;   I5  no stale pointers: a 'thread luaR is anchored in a private table of the universe and
  ;;       the anchor is verified by every binding before the pointer is used (Lua code or the
  ;;       bound API may drop it: the binding then refuses); callback and hook views are
  ;;       invalidated on exit; a luaDebug checks its view and the universe's epoch;
  ;;   I6  protected ops run on an OK thread; on a suspended or dead thread only pushes (on the
  ;;       main thread) and lowering the top are allowed, the latter not when its current frame is
  ;;       a Lua function (aux_settop_d; lua-error and lua-check pop through aux_raisable, which
  ;;       admits only the error value lua-resume pushed, under lua-check; lua-resume and lua-xmove
  ;;       lower it only down to the height recorded at the hook yield or the death, aux_nk_touch);
  ;;       while a hook runs, its thread has a
  ;;       floor (the height at entry, in that frame) that NO luaR of the thread can lower, move
  ;;       or overwrite (the hooked function's registers); so has, while a Scheme callback runs,
  ;;       every NORMAL thread (LUA_OK with a frame, not running: it waits in the C function that
  ;;       resumed another coroutine, such as lcorolib's coroutine.resume, whose slot 1 holds the
  ;;       running coroutine): the height of that frame when a binding first used the thread in
  ;;       that callback (aux_touch; the frame of a Scheme closure has none: its slots are the
  ;;       caller's); a named local or an upvalue the VM
  ;;       trusts (a for loop's control variable, a vararg table: read-only in 5.5) is never
  ;;       assigned, and neither is the C state of a C closure's upvalues;
  ;;   I7  a universe is never closed under a running binding: the finalizer of a main luaR
  ;;       defers while the universe is running or is the one whose pointer a binding has just
  ;;       read (%busy-ctx), and always when it holds Scheme callables (they would run inside
  ;;       CHICKEN's finalizer loop, which an escape must not leave); a thread luaR keeps its
  ;;       main luaR alive;
  ;;   I8  escapes and conditions stop at the callback boundary and resume past the Lua frames;
  ;;       a callback body ends (returns to C) at most once, in the srfi-18 thread that entered
  ;;       it: a rewind into it after it left, or from another thread while it runs, is refused,
  ;;       and a run in another thread (a continuation invoked by a thread created inside it)
  ;;       never ends through the boundary (%guard);
  ;;   I9  frames addressed from Scheme never hold to-be-closed slots, so lowering the top is D;
  ;;   I10 the Scheme objects a universe hands to Lua live in a table of that universe (not in a
  ;;       global root), so a universe whose callbacks capture its own luaR is still collected;
  ;;   I11 C state Lua keeps on a stack is out of reach: the slots of an op's own C frame are
  ;;       never read (lua-getlocal refuses them), a lua-load reader never sees the parser (it
  ;;       works on a private thread whose 'thread luaRs are refused, kept alive by a pin that no
  ;;       Lua value references, so the debug library can neither find nor drop it), and a
  ;;       thread inside a load has a frame, so it is not idle: nothing resumes or resets it
  ;;       under the C call;
  ;;   I12 one Scheme thread at a time runs Lua that may call Scheme back: the safe path takes a
  ;;       lock (%settle! waits for it, %lua-take! takes it), so callbacks of different srfi-18
  ;;       threads never interleave on the C stack (C_callback returns to the innermost C
  ;;       frame).  Another thread waits, yielding, while a callback of the owner has switched
  ;;       threads (if that callback joins it or waits for a mutex it holds, the waiter raises
  ;;       instead; a wait on anything else is a deadlock).
  ;;       The wait comes before the binding reads any lua_State pointer (%settle!): meanwhile
  ;;       the owner may close the universe, return from the callback whose view the waiter
  ;;       holds, or let a coroutine be collected, and the binding then refuses its luaR as
  ;;       closed or invalidated;
  ;;
  ;; Exports: (aux lua), at the end of this file, exports exactly the bindings, the constants,
  ;; the condition accessors, lua-live-handles and the read-only record predicates (luaR?,
  ;; luaR-kind, luaDebug?).  No constructor, mutator or raw stub is public: they could forge the
  ;; pointers, floors and handles the checks rely on, or call the API unchecked.  Every binding
  ;; is curried with the state last, except: the constants and luaopen-* (C function pointers),
  ;; lua-upvalueindex, the lua-debug-* accessors, lua-type->symbol, lua-format (the text of
  ;; lua_pushfstring's directives, returned: no state is involved), lua-ref?, lua-live-handles,
  ;; the condition accessors, the record predicates, and lua-make-empty, lua-newstate,
  ;; luaL-newstate and lua-close.
  ;;
  ;; Sections: CORE (records, conditions, handles, callbacks, lifecycle, representative
  ;; bindings), S1 stack and type queries, constants; S2 reading and pushing values, operators;
  ;; S3 tables, userdata, metatables; S4 states, threads, calls, load/dump, warnings, gc; S5 the
  ;; debug interface; S6 lauxlib and lualib.
  ;;
  ;; Deliberately omitted (every other LUA_API / LUALIB_API / LUAMOD_API function and
  ;; function-like macro of lua.h, lauxlib.h, lualib.h and luaconf.h is bound):
  ;;   lua_atpanic             a panic handler can only abort or longjmp across CHICKEN frames;
  ;;                           aux_panic is installed and is unreachable by construction.
  ;;   lua_callk lua_pcallk    a non-NULL continuation would unwind through C_callback: use
  ;;                           lua-call / lua-pcall, and lua-yieldk from a Scheme callback.
  ;;   lua_getallocf lua_setallocf   C function pointers only; the module owns the allocator
  ;;                           (see lua-setmemorylimit).
  ;;   lua_toclose lua_closeslot   a to-be-closed slot in a frame addressed from Scheme would make
  ;;                           settop / pop run Lua code (I9); Lua-side <close> works.
  ;;   lua_getextraspace       it holds the module's per-universe context.
  ;;   lua_ident               a static C string; lua-version and LUA_VERSION are enough.
  ;;   lua_pushexternalstring  Lua would keep the memory until it calls `falloc', and Scheme
  ;;                           memory moves: a malloc'd copy is just lua-pushlstring.
  ;;   lua_pushvfstring        a va_list cannot be built from Scheme (lua-pushfstring is bound).
  ;;   luaL_fileresult luaL_execresult   they read the C errno of a call Scheme never made.
  ;;   luaL_alloc              lauxlib's default allocator: the module owns the allocator.
  ;;   luaL_intop              pure C arithmetic on lua_Integer.
  ;;   the luaL_Buffer family: luaL_buffinit luaL_buffinitsize luaL_prepbuffsize luaL_prepbuffer
  ;;     luaL_addlstring luaL_addstring luaL_addvalue luaL_addchar luaL_addsize luaL_addgsub
  ;;     luaL_buffsub luaL_bufflen luaL_buffaddr luaL_pushresult luaL_pushresultsize
  ;;                           a C-stack struct plus stack slots of the caller's frame across
  ;;                           calls: build the string in Scheme and push it once (luaL-gsub is
  ;;                           bound).
  ;;   luaL_checkint luaL_optint luaL_checklong luaL_optlong luaL_checkunsigned luaL_optunsigned
  ;;                           LUA_COMPAT_APIINTCASTS macros, not compiled in Lua 5.5.
  ;;   lua_str2number lua_strx2number lua_integer2str lua_number2strx lua_pointer2str
  ;;   lua_getlocaledecpoint   luaconf.h's configuration hooks: how Lua's own C code converts
  ;;                           numbers, formats pointers and finds the decimal point, not API
  ;;                           (lua-stringtonumber, lua-numbertocstring, lua-tolstring and
  ;;                           lua-pushfstring are the API over them).
  ;; ==========================================================================================

  #>

  #include <stdlib.h>
  #include <string.h>
  #include <stdio.h>
  #include <limits.h>
  #include <locale.h>
  #include "lua.h"
  #include "lauxlib.h"
  #include "lualib.h"

  /* ---------- internal result codes (never collide with Lua statuses or with op results) ---------- */
  /* op results are > -1000 (type codes, booleans, refs, counts, statuses);
     -1099..-1000 a Lua error whose value is on top of the thread the op ran on,
     -1299..-1200 the same, but the value is on top of the universe's main thread,
     <= -2001 the binding refused before Lua ran (an "api" error). */
  #define AUX_LUAERR(st)    (-(1000 + (st)))
  #define AUX_LUAERR_M(st)  (-(1200 + (st)))
  #define AUX_IS_LUAERR(r)  ((r) <= -1000 && (r) > -1100)
  #define AUX_EINDEX   (-2001)   /* invalid index */
  #define AUX_ESTACK   (-2002)   /* lua_checkstack failed */
  #define AUX_ENOMEM   (-2003)   /* malloc of an operand copy failed */
  #define AUX_EUNDER   (-2004)   /* fewer operands on the stack than the op consumes */
  #define AUX_ETYPE    (-2005)   /* type precondition failed (e.g. rawget on a non-table) */
  #define AUX_ETHREAD  (-2006)   /* op not allowed on a thread in this status */
  #define AUX_ERANGE   (-2007)   /* integer argument out of range */
  #define AUX_ESTALE   (-2008)   /* stale lua_Debug */
  #define AUX_EARG     (-2009)   /* bad argument (e.g. unknown option) */
  #define AUX_ERDONLY  (-2010)   /* a read-only variable the VM trusts (lua-setlocal, lua-setupvalue) */
  #define AUX_ELOADING (-2011)   /* the thread is inside a lua-load (its frame holds the parser's state) */
  #define AUX_EGLOBALS (-2012)   /* registry[LUA_RIDX_GLOBALS] is not a table (or a write would make it so) */
  #define AUX_EMETATABLE (-2013) /* the metatable of a Scheme handle, the registry or registry._CLIBS, the handle
                                    metatable, or a library's userdata metatable given to another userdata */
  #define AUX_ELIBRARY (-2014)   /* a registry entry Lua's libraries trust (FILE*, _UBOX*, _CLIBS) or a userdata
                                    in registry._CLIBS, written by a binding */
  #define AUX_ELIBKEYS (-2015)   /* internal: the key table of those entries was replaced (made anew, then asked again) */

  /* the height a suspended or dead thread whose current frame is a Lua function had when it got there (a
     hook yield, or an error), see aux_nk_touch: below it are the frame's registers */
  typedef struct { lua_State *L; int id, st, base, flags; } aux_nk_t;
  #define AUX_NK_REFINE 1   /* base is the hook's entry height (>= the height at the yield): exact at the next touch */
  #define AUX_NK_ERRVAL 2   /* the slot below base holds the error value lua-resume pushed: it may be consumed */

  /* ---------- per-universe context: *(aux_ctx**)lua_getextraspace(L); copied into every thread by lua_newthread ---------- */
  typedef struct aux_ctx {
    lua_State *main;              /* the main thread: always LUA_OK, it also runs the P* ops of non-OK threads */
    unsigned epoch;               /* ++ by lua-resume / lua-closethread: stale lua_Debug detection */
    int warnmode, wstate;         /* warnmode: 0 discard, 1 stderr (clone of lauxlib), 2 collect;
                                     wstate: 0 off, 1 on (ready for a new message), 2 continuing a message */
    char *wbuf; size_t wlen, wcap;    /* the message being continued (collect mode) */
    char **wq; size_t wn, wqcap;      /* completed messages (collect mode) */
    size_t limit, used;           /* allocator accounting; limit 0 = unlimited */
    int *rel; size_t nrel, caprel;    /* handles released by Lua (a handle's __gc), drained by Scheme */
    lua_State *rerrL; int rerrtop;    /* the last lua-resume that left its thread dead: the thread, its top (the */
    unsigned rerrepoch;               /* error value lua_resume pushed) and the epoch then (aux_raisable) */
    aux_nk_t *nk; int nnk, capnk;     /* one record per suspended or dead thread in a Lua frame (aux_nk_touch) */
  } aux_ctx;
  #define AUX_CTX(L) (*(aux_ctx **)lua_getextraspace(L))
  static size_t aux_nrel_total = 0;   /* sum of every ctx's nrel: Scheme's fast "nothing to drain" test */

  /* ---------- floors (invariant I6): the slots below a floor belong to C or to Lua, not to the caller.
     Two kinds, both keyed by the thread AND its current frame (the CallInfo, NULL for the base level): a
     frame called meanwhile lies above it and is not concerned.  Every binding that lowers, moves or
     overwrites stack slots consults them, whatever luaR it was given.
     - a hook's (aux_hook): while it runs, the slots of its thread up to the height at entry are the
       hooked function's registers;
     - a normal thread's (aux_touch): while a Scheme callback runs, every other thread with a frame and
       status LUA_OK is a NORMAL one: it waits in a C function below the callback on the C stack, the
       lua_resume of coroutine.resume / coroutine.wrap (lcorolib), the lua_closethread of
       coroutine.close, or a protected binding's frame.  That frame is C state: luaB_coresume's slot 1
       is the running coroutine itself, whose only reference it may be (lowering it, then a collection,
       frees the running lua_State).  So the first binding a callback applies to such a thread records
       the frame's height at that moment as its floor (every binding validates its luaR through %L,
       which calls aux_touch while a callback runs).  The frame of a Scheme closure (the view's own, or
       that of an outer callback on a coroutine that called lua-resume, whose lua_resume runs in an op
       frame of the main thread) has none: its slots are the caller's.  A frame that
       is current while Scheme runs lies below the innermost callback, so it outlives it: the records a
       callback made are dropped when it returns (AUX_CB_ENTER / AUX_CB_LEAVE), never before. ---------- */
  typedef struct { lua_State *L; const void *ci; int floor; } aux_floor_t;
  #define AUX_MAXFLOORS 512
  static aux_floor_t aux_floors[AUX_MAXFLOORS]; static int aux_nfloors = 0;
  /* the number of C -> Scheme entries (cfunction, continuation, hook, reader) now running: aux_touch
     records only inside one */
  static int aux_ncb = 0;
  #define AUX_CB_ENTER(nf) ((nf) = aux_nfloors, aux_ncb++)
  #define AUX_CB_LEAVE(nf) (aux_ncb--, aux_nfloors = (nf))
  /* the CallInfo of the function running at `level' of L, NULL at the base level (D) */
  static const void *aux_ci_at(lua_State *L, int level) {
    lua_Debug ar; return lua_getstack(L, level, &ar) ? (const void *)ar.i_ci : NULL; }
  /* the index of the first record of L's current frame ci, or -1 */
  static int aux_floor_find(lua_State *L, const void *ci) {
    int i; for (i = 0; i < aux_nfloors; i++) if (aux_floors[i].L == L && aux_floors[i].ci == ci) return i;
    return -1; }
  static int aux_floor(lua_State *L) {
    int i, f = 0; const void *ci;
    if (aux_nfloors == 0) return 0;
    ci = aux_ci_at(L, 0);
    for (i = 0; i < aux_nfloors; i++)
      if (aux_floors[i].L == L && aux_floors[i].ci == ci && aux_floors[i].floor > f) f = aux_floors[i].floor;
    return f; }
  /* the floor of L for a binding given the luaR floor `floor' (a hook view's own) */
  static int aux_eff_floor(lua_State *L, int floor) { int f = aux_floor(L); return f > floor ? f : floor; }
  static int aux_floor_push(lua_State *L, int floor) {
    if (aux_nfloors == AUX_MAXFLOORS) return 0;
    aux_floors[aux_nfloors].L = L; aux_floors[aux_nfloors].ci = aux_ci_at(L, 0); aux_floors[aux_nfloors].floor = floor;
    aux_nfloors++; return 1; }
  static int aux_touch_note(lua_State *L);               /* below: it needs aux_self */
  /* called by %L for every luaR a binding uses while a Scheme callback runs, before the binding does
     anything: the first time L's current frame is seen in this callback, its floor is recorded.  0, or
     AUX_ESTACK when the table is full and the frame has C state (the binding is refused) (D) */
  static int aux_touch(lua_State *L) {
    const void *ci; int f;
    if (aux_ncb == 0 || lua_status(L) != LUA_OK) return 0;   /* suspended or dead threads: aux_nk's rules */
    if ((ci = aux_ci_at(L, 0)) == NULL || aux_floor_find(L, ci) >= 0) return 0;   /* base level: the caller's */
    f = aux_touch_note(L);
    if (aux_nfloors == AUX_MAXFLOORS) return f > 0 ? AUX_ESTACK : 0;
    aux_floors[aux_nfloors].L = L; aux_floors[aux_nfloors].ci = ci; aux_floors[aux_nfloors].floor = f;
    aux_nfloors++; return 0; }

  /* ---------- parser threads (see aux_s4_load): while a lua-load reader runs, the parser works in the current
     frame of a private thread of the load; no 'thread luaR of such a thread may be used meanwhile ---------- */
  static lua_State *aux_loading[AUX_MAXFLOORS]; static int aux_nloading = 0;
  static int aux_is_loading(lua_State *L) {
    int i; for (i = 0; i < aux_nloading; i++) if (aux_loading[i] == L) return 1;
    return 0; }

  /* ---------- barriers (invariant I1): the threads a stub called from Scheme runs Lua on under a protection of
     its own (aux_protect, aux_pcall, the collector of aux_s4_gc), innermost first.  Lua 5.5's coroutine.close()
     of the RUNNING coroutine ends in luaD_throwbaselevel: a longjmp to the OUTERMOST protected call of that
     thread, skipping every one in between.  When one of those is a binding's, the jump would cross its C
     stub (and the C_callback frames of the Scheme callback that called it), or land in a protected call
     whose saved stack the reset has just shrunk.  So the coroutine.close the module installs (aux_co_close)
     refuses a thread with a barrier.  A lua-resume'd coroutine has none of its own (its resume runs on the
     main thread): closing itself unwinds to that lua_resume, as C.  The nodes live in the C frames that push
     them; nothing can jump over those frames (their own lua_pcall catches every error). ---------- */
  typedef struct aux_barrier { lua_State *L; struct aux_barrier *prev; } aux_barrier;
  static aux_barrier *aux_barriers = NULL;
  #define AUX_BARRIER_PUSH(b, T) ((b).L = (T), (b).prev = aux_barriers, aux_barriers = &(b))
  #define AUX_BARRIER_POP(b) (aux_barriers = (b).prev)
  static int aux_barred(lua_State *L) {
    const aux_barrier *b; for (b = aux_barriers; b; b = b->prev) if (b->L == L) return 1;
    return 0; }

  /* the thread running the innermost Scheme callback (a lua_CFunction, continuation, hook or reader), or
     NULL: lua-resume / lua-closethread count their C calls from it, as the C stack really grows from there */
  static lua_State *aux_cur = NULL;

  /* A Scheme callback runs on the C stack below the frames of the Lua code that called it, and that stack is
     CHICKEN's nursery: nested callbacks eat it up.  One entered with too little of it left could only
     collect garbage, for ever (every stack check fails again after the collection).  Lua's own C-call
     count does not see those frames, so a callback is refused ("C stack overflow", Lua's own error, raised
     in the calling C function before any Scheme runs) when less than this is left. */
  #define AUX_CSTACK_RESERVE (160 * 1024)
  static int aux_cstack_low(void) {
    char here; C_uword left;
  #if C_STACK_GROWS_DOWNWARD
    if ((C_uword)&here <= (C_uword)C_stack_hard_limit) return 1;
    left = (C_uword)&here - (C_uword)C_stack_hard_limit;
  #else
    if ((C_uword)&here >= (C_uword)C_stack_hard_limit) return 1;
    left = (C_uword)C_stack_hard_limit - (C_uword)&here;
  #endif
    return left < AUX_CSTACK_RESERVE; }

  /* ---------- re-entrancy flag (invariant I2) ---------- */
  static int aux_reentrant = 0;   /* 1 iff the innermost Scheme->C transition is a foreign-SAFE stub */

  /* ---------- panic: unreachable by construction; fail-stop on a binding bug ---------- */
  static int aux_panic(lua_State *L) {
    fprintf(stderr, "aux.lua: PANIC (unprotected Lua error: a bug in (aux lua)): %s\n",
            lua_type(L, -1) == LUA_TSTRING ? lua_tostring(L, -1) : lua_typename(L, lua_type(L, -1)));
    fflush(stderr); abort(); return 0;
  }

  /* ---------- Scheme bytes: string (slot 0 = NUL-terminated UTF-8 bytevector) or bytevector ---------- */
  static const char *aux_bytes(C_word x, size_t *n) {
    if (C_header_bits(x) == C_BYTEVECTOR_TYPE) { *n = C_header_size(x); return (const char *)C_data_pointer(x); }
    { C_word bv = C_block_item(x, 0); *n = C_header_size(bv) - 1; return (const char *)C_data_pointer(bv); }
  }
  static char *aux_dup(const char *s, size_t n) { char *p = malloc(n + 1); if (p) { memcpy(p, s, n); p[n] = 0; } return p; }

  /* ---------- warnings: never a Scheme procedure (they run inside the collector and inside D stubs) ---------- */
  static void aux_wappend(aux_ctx *c, const char *m) {
    size_t n = strlen(m);
    if (c->wlen + n + 1 > c->wcap) {
      size_t cap = c->wcap ? c->wcap : 64; char *p;
      while (cap < c->wlen + n + 1) cap *= 2;
      if (!(p = realloc(c->wbuf, cap))) return;
      c->wbuf = p; c->wcap = cap; }
    memcpy(c->wbuf + c->wlen, m, n); c->wlen += n; c->wbuf[c->wlen] = 0;
  }
  static void aux_wfinish(aux_ctx *c) {
    char *m;
    if (c->wn == c->wqcap) {
      size_t cap = c->wqcap ? 2 * c->wqcap : 8; char **q = realloc(c->wq, cap * sizeof *q);
      if (!q) { c->wlen = 0; return; }
      c->wq = q; c->wqcap = cap; }
    if (!(m = malloc(c->wlen + 1))) { c->wlen = 0; return; }
    if (c->wlen) memcpy(m, c->wbuf, c->wlen);
    m[c->wlen] = 0; c->wq[c->wn++] = m; c->wlen = 0;
  }
  /* lauxlib's warnfoff/warnfon/warnfcont state machine, with a selectable sink.  Mode 0 is C's NULL warning
     function (lua_newstate's, or lua_setwarnf(L, NULL, NULL)): nothing reaches it, control messages and
     pieces included, so the state machine stays as it is */
  static void aux_warnf(void *ud, const char *msg, int tocont) {
    aux_ctx *c = (aux_ctx *)ud;
    if (c->warnmode == 0) return;
    if (c->wstate != 2) {                               /* start of a message */
      if (!tocont && msg[0] == '@') {                   /* control message */
        if (strcmp(msg + 1, "off") == 0) c->wstate = 0;
        else if (strcmp(msg + 1, "on") == 0) c->wstate = 1;
        return; }
      if (c->wstate == 0) return;                       /* off: ignored, pieces included */
      if (c->warnmode == 1) fputs("Lua warning: ", stderr);
    }
    if (c->warnmode == 1) { fputs(msg, stderr); if (!tocont) { fputs("\n", stderr); fflush(stderr); } }
    else if (c->warnmode == 2) { aux_wappend(c, msg); if (!tocont) aux_wfinish(c); }
    c->wstate = tocont ? 2 : 1;
  }
  /* lua-setwarnf: a warning function is installed, as lua_setwarnf does: 0 (none) or a sink that starts ON
     and at a new message, as luaL_newstate's warnfon (an earlier "@off", or a piece left to be continued,
     belonged to the function it replaces).  A message the collect sink was building is kept as it stands. */
  static void aux_setwarnmode(aux_ctx *c, int m) {
    if (c->warnmode == 2 && c->wstate == 2) aux_wfinish(c);
    c->wlen = 0; c->warnmode = m; c->wstate = 1; }
  static void aux_wclear(aux_ctx *c) {
    size_t i; for (i = 0; i < c->wn; i++) free(c->wq[i]);
    c->wn = 0;
  }

  /* ---------- counting allocator (lua-setmemorylimit): NULL past the limit, so Lua raises LUA_ERRMEM ---------- */
  static void *aux_alloc(void *ud, void *ptr, size_t osize, size_t nsize) {
    aux_ctx *c = (aux_ctx *)ud;
    size_t old = ptr ? osize : 0;
    if (nsize == 0) { free(ptr); c->used = old > c->used ? 0 : c->used - old; return NULL; }
    if (c->limit && nsize > old && c->used + (nsize - old) > c->limit) return NULL;
    { void *p = realloc(ptr, nsize);
      if (p) c->used = (old > c->used ? 0 : c->used - old) + nsize;
      return p; }
  }

  /* ---------- handles: Scheme objects in Lua, as integers only ---------- */
  #define AUX_MAGIC 0x6175786cu
  enum { AUX_K_PROC, AUX_K_OBJECT, AUX_K_COND, AUX_K_HOOK, AUX_K_KFUNC, AUX_K_READER };
  typedef struct { unsigned magic; int handle; int kind; int nup; } aux_handle;
  /* addresses used as registry keys / sentinel.  aux_anchor_key: the table anchoring the threads of 'thread
     luaRs (integer keys chosen by Scheme; never luaL_ref, whose free list Lua code can corrupt) */
  static char aux_mt_key, aux_anchor_key, aux_hook_key, aux_escape_key;
  /* registry[&aux_libkeys_key]: the array {"FILE*", "_UBOX*", "_CLIBS"}, made by op_setup.  It keeps the three
     strings alive, so a D check reads the registry entries Lua's libraries trust (aux_libslot_push) by a raw
     get with an existing key, never interning a string (an allocation) outside a protected call. */
  static char aux_libkeys_key;
  enum { AUX_LIB_FILE, AUX_LIB_UBOX, AUX_LIB_CLIBS, AUX_NLIB };
  static const char *const aux_libnames[AUX_NLIB] = { LUA_FILEHANDLE, "_UBOX*", "_CLIBS" };
  static void aux_libkeys_make(lua_State *L);
  /* registry[&aux_kfunc_key]: a weak-keyed table, thread -> the KFUNC handle userdata of the lua-yieldk
     continuation that thread is suspended in (at most one: only the innermost frame of a coroutine can be
     a yielded Scheme closure, lua_callk / lua_pcallk being unbound).  Off the thread's stack, so a
     suspended coroutine looks as it does in C; the entry dies with the thread. */
  static char aux_kfunc_key;

  static void aux_release(aux_ctx *c, int h) {         /* a handle of c's universe is no longer used by Lua */
    if (c->nrel == c->caprel) {
      size_t n = c->caprel ? 2 * c->caprel : 64; int *p = realloc(c->rel, n * sizeof *p);
      if (!p) return;                                  /* leak the slot, never fail in __gc */
      c->rel = p; c->caprel = n; }
    c->rel[c->nrel++] = h; aux_nrel_total++; }

  static int aux_handle_gc(lua_State *L) {             /* pure C: never a callback inside Lua's collector */
    aux_handle *u = (aux_handle *)lua_touserdata(L, 1);
    if (u && lua_rawlen(L, 1) == sizeof *u && u->magic == AUX_MAGIC && u->handle >= 0) { aux_release(AUX_CTX(L), u->handle); u->handle = -1; }
    return 0; }
  /* the __tostring of the handle metatable, which Lua code reaches (debug.getmetatable, or the bound
     lua-getmetatable, which ignores __metatable) and may call with any value: lua_getiuservalue wants a
     full userdata, so anything that is not shaped as a handle is refused as luaL_checkudata refuses it
     (the metatable itself is not required: a handle keeps its text when the registry entry is replaced) */
  static int aux_handle_tostring(lua_State *L) {
    aux_handle *u = (aux_handle *)lua_touserdata(L, 1);
    if (lua_type(L, 1) != LUA_TUSERDATA || lua_rawlen(L, 1) != sizeof *u || u->magic != AUX_MAGIC)
      return luaL_typeerror(L, 1, "aux.scheme");
    if (lua_getiuservalue(L, 1, 1) != LUA_TSTRING) { lua_pop(L, 1); lua_pushliteral(L, "<scheme object>"); }
    return 1; }

  /* our handle userdata at idx, or NULL; never allocates, never raises (D-safe) */
  static aux_handle *aux_tohandle(lua_State *L, int idx) {
    aux_handle *u = NULL;
    if (idx > LUA_REGISTRYINDEX && (idx == 0 || (idx > 0 ? idx : -idx) > lua_gettop(L))) return NULL;  /* empty slot */
    idx = lua_absindex(L, idx);
    if (lua_type(L, idx) != LUA_TUSERDATA || lua_rawlen(L, idx) != sizeof(aux_handle) || !lua_checkstack(L, 2)) return NULL;
    if (lua_getmetatable(L, idx)) {
      if (lua_rawgetp(L, LUA_REGISTRYINDEX, &aux_mt_key) == LUA_TTABLE && lua_rawequal(L, -1, -2)) { u = (aux_handle *)lua_touserdata(L, idx); if (u->magic != AUX_MAGIC || u->handle < 0) u = NULL; }
      lua_pop(L, 2); }
    return u; }

  /* ---------- protected trampoline ---------- */
  typedef struct aux_op aux_op;
  struct aux_op {
    int (*fn)(lua_State *L, aux_op *o);  /* body, runs inside lua_pcall; operands are its stack slots 1..n */
    int i0, i1, i2; lua_Integer n0; lua_Number d0; size_t z0;
    const char *s0; size_t l0; const char *s1; size_t l1; const char *s2; size_t l2;
    void *p0; lua_State *L1;
    int r;                               /* int result (> -1000), returned by the stub on success */
    /* set by aux_protect / aux_tramp only: the call and return hooks of the thread, set aside while the
       trampoline itself is entered and left (hsup: they are set aside now), and the body's progress */
    lua_Hook hk; int hmask, hcount, hsup, entered, done;
  };
  /* The op travels in C globals, never on the Lua stack: Lua code may keep the trampoline itself
     (debug.getinfo, from a metamethod the body runs) to call it later.  So the trampoline runs its op only
     when it is called by aux_protect: on the same thread, from the frame aux_protect was called in.  Nested
     ops save and restore the globals. */
  static aux_op *aux_op_pending = NULL; static lua_State *aux_op_L = NULL; static const void *aux_op_caller = NULL;

  /* The trampoline is the module's own C function, not a call the program made: in C, lua_pushcclosure or
     lua_sethook fire no hook.  So the call and return hooks of the thread never see it enter or leave:
     they are set aside around its call and return, and are in force while its body runs (a metamethod
     the body calls is hooked as in C).  This matters beyond fidelity: a return hook that raised after the
     body had returned would make a committed op (a handle stored in a new userdata, a registry slot
     taken, an anchor or a hook set) look failed to the binding, which would then release what Lua still
     holds.  Setting them aside is lua_sethook, which restarts the count of a count hook installed together
     with a call or return hook (line and count hooks alone are left alone: they never fire on a C
     function's call or return). */
  #define AUX_MASKCR (LUA_MASKCALL | LUA_MASKRET)
  static void aux_hooks_aside(lua_State *L, aux_op *o) {
    int m = lua_gethookmask(L);
    o->hsup = (m & AUX_MASKCR) != 0;
    if (o->hsup) { o->hk = lua_gethook(L); o->hmask = m; o->hcount = lua_gethookcount(L); lua_sethook(L, NULL, 0, 0); } }
  static void aux_hooks_back(lua_State *L, aux_op *o) {
    if (o->hsup) { o->hsup = 0; lua_sethook(L, o->hk, o->hmask, o->hcount); } }

  static int aux_tramp(lua_State *L) {
    aux_op *o = aux_op_pending; int n;
    if (o == NULL || L != aux_op_L || aux_ci_at(L, 1) != aux_op_caller)
      return luaL_error(L, "aux.lua: a protected operation called out of its context");
    aux_op_pending = NULL;
    o->entered = 1;
    aux_hooks_back(L, o);                /* the body runs under the thread's hooks, as C code would */
    n = o->fn(L, o);
    aux_hooks_aside(L, o);               /* the hooks in force now (the body may have set new ones) */
    o->done = 1;
    return n; }

  /* refs: indices (in the CALLER's frame; negative, positive or pseudo) of non-consumed operands, copied as args 1..nrefs
     ntop: consumed operands on top, copied as args nrefs+1..nrefs+ntop, originals removed afterwards
     nres: results left by the body (LUA_MULTRET allowed)
     net stack effect: success -ntop+nres, error -ntop+1 (the error value; Scheme pops it)          */
  #define AUX_MAXREFS 8
  static int aux_protect(lua_State *L, aux_op *o, int nrefs, const int *refs, int ntop, int nres) {
    int i, st, top = lua_gettop(L), abs[AUX_MAXREFS];
    aux_op *so; lua_State *sl; const void *sc; aux_barrier b;
    if (lua_status(L) != LUA_OK) return AUX_ETHREAD;              /* lua_pcall on a non-OK thread is UB */
    if (ntop < 0 || ntop > top) return AUX_EUNDER;
    if (ntop > 0 && top - ntop < aux_floor(L)) return AUX_EINDEX;  /* consuming slots below a floor (I6) */
    if (nrefs < 0 || nrefs > AUX_MAXREFS) return AUX_ERANGE;
    if (!lua_checkstack(L, 2 + nrefs + ntop)) return AUX_ESTACK;
    for (i = 0; i < nrefs; i++) abs[i] = lua_absindex(L, refs[i]); /* before any push: pseudo-indices unchanged */
    lua_pushcfunction(L, aux_tramp);                               /* light C function: no allocation */
    for (i = 0; i < nrefs; i++) lua_pushvalue(L, abs[i]);
    for (i = ntop; i > 0; i--) lua_pushvalue(L, top - i + 1);
    so = aux_op_pending; sl = aux_op_L; sc = aux_op_caller;
    aux_op_pending = o; aux_op_L = L; aux_op_caller = aux_ci_at(L, 0);
    o->entered = o->done = 0;
    aux_hooks_aside(L, o);                                         /* D: lua_sethook never allocates */
    AUX_BARRIER_PUSH(b, L);
    st = lua_pcall(L, nrefs + ntop, nres, 0);
    AUX_BARRIER_POP(b);
    /* back in force: set aside by the trampoline's return, or still aside when it was never entered (an
       error in the call itself); after an error in the body they are in force already (hsup is 0) */
    aux_hooks_back(L, o);
    aux_op_pending = so; aux_op_L = sl; aux_op_caller = sc;
    if (st != LUA_OK && o->done) {
      /* unreachable: with no call or return hook in force, nothing raises between the body's return and
         lua_pcall's.  An error here would mean the op committed and Scheme would take it as failed. */
      fprintf(stderr, "aux.lua: FATAL (an error after a protected operation completed: a bug in (aux lua))\n");
      fflush(stderr); abort(); }
    if (ntop > 0) { lua_rotate(L, top - ntop + 1, -ntop); lua_pop(L, ntop); }   /* originals are plain values: no TBC */
    return st == LUA_OK ? o->r : AUX_LUAERR(st); }

  /* the two paths: same op, same body; 'safe' selects the discipline */
  static int aux_run(int safe, lua_State *L, aux_op *o, int nrefs, const int *refs, int ntop, int nres) {
    int r, saved; char *c0 = NULL, *c1 = NULL, *c2 = NULL;
    if (!safe) return aux_protect(L, o, nrefs, refs, ntop, nres);
    if (o->s0 && !(o->s0 = c0 = aux_dup(o->s0, o->l0))) return AUX_ENOMEM;       /* copy BEFORE any Lua call */
    if (o->s1 && !(o->s1 = c1 = aux_dup(o->s1, o->l1))) { free(c0); return AUX_ENOMEM; }
    if (o->s2 && !(o->s2 = c2 = aux_dup(o->s2, o->l2))) { free(c0); free(c1); return AUX_ENOMEM; }
    saved = aux_reentrant; aux_reentrant = 1;
    r = aux_protect(L, o, nrefs, refs, ntop, nres);
    aux_reentrant = saved;
    free(c0); free(c1); free(c2); return r; }

  /* P* ops: push-only bodies (nrefs = ntop = 0). On a thread whose status != LUA_OK the body runs on the main
     thread (always LUA_OK: it can neither yield nor die, and it lives as long as the universe) and its results
     are moved over.  No thread is kept aside for this: an anchor Lua code could remove would dangle. */
  static int aux_run_h(int safe, lua_State *L, aux_op *o, int nres) {
    lua_State *M; int r;
    if (lua_status(L) == LUA_OK) return aux_run(safe, L, o, 0, NULL, 0, nres);
    M = AUX_CTX(L)->main;
    if (nres < 0 || !lua_checkstack(L, nres)) return AUX_ESTACK;
    r = aux_run(safe, M, o, 0, NULL, 0, nres);
    if (AUX_IS_LUAERR(r)) return AUX_LUAERR_M(-r - 1000);      /* the error value stays on top of M */
    if (r <= -1000) return r;
    if (!lua_checkstack(L, nres)) { lua_pop(M, nres); return AUX_ESTACK; }   /* L may have run meanwhile */
    lua_xmove(M, L, nres); return r; }

  /* ---------- index checks: API checks are compiled out of liblua, so an invalid index is memory corruption ---------- */
  #define AUX_IX_READ   1   /* stack index, REGISTRYINDEX, or -- while the running function is a C function -- an
                               upvalue pseudo-index (reads beyond nup give nil: harmless) */
  #define AUX_IX_STACK  2   /* real stack slot only: 1 <= |idx| <= top */
  #define AUX_IX_WRITE  3   /* stack slot, or an upvalue pseudo-index <= nup+1 of the running Scheme closure (never the registry) */
  /* 1 iff the running function of L is a C function (0 at base level and in a Lua frame, e.g. a hook's):
     Lua reads an upvalue pseudo-index only there (index2value asserts "caller not a C function") */
  static int aux_cframe(lua_State *L) {
    lua_Debug ar;
    if (!lua_getstack(L, 0, &ar)) return 0;
    lua_getinfo(L, "S", &ar);                              /* D: no allocation */
    return strcmp(ar.what, "C") == 0; }
  static int aux_ix(lua_State *L, int idx, int kind, int nup) {
    int top = lua_gettop(L);
    if (idx > 0) return idx <= top;
    if (idx > LUA_REGISTRYINDEX) return idx != 0 && -idx <= top;   /* negative stack index (0 is invalid) */
    if (kind == AUX_IX_STACK) return 0;
    if (idx == LUA_REGISTRYINDEX) return kind == AUX_IX_READ;
    { int i = LUA_REGISTRYINDEX - idx;                             /* raw upvalue number, 1 = our handle */
      if (i < 1 || i > 256 || !aux_cframe(L)) return 0;
      return kind == AUX_IX_READ ? 1 : (i >= 2 && i <= nup + 1); } }
  /* query index: 1 valid, 0 acceptable but empty (a positive index above the top), -1 invalid */
  static int aux_qix(lua_State *L, int idx) {
    if (idx > 0 && idx > lua_gettop(L)) return 0;
    return aux_ix(L, idx, AUX_IX_READ, 0) ? 1 : -1; }

  /* 1 iff the current frame of L is a Lua function: a thread dead by an error raised in Lua code, or
     suspended by a hook.  The API (lua_settop's CIST_TBC assertion) only covers C frames: lowering such
     a frame could run its pending __close outside any protection, so the bindings refuse it. */
  static int aux_in_lua_frame(lua_State *L) {
    lua_Debug ar;
    if (!lua_getstack(L, 0, &ar)) return 0;               /* base frame */
    lua_getinfo(L, "S", &ar);                              /* D: no allocation */
    return strcmp(ar.what, "C") != 0; }

  /* 0, or AUX_ETHREAD: lua-error / lua-check may pop the top of L (the error value they raise).  Refused as
     aux_settop_d refuses lowering (a suspended or dead thread in a Lua frame: the top may be a register of
     that frame, a pending to-be-closed variable), except for lua-check (fresh) when the value is the one a
     lua-resume run by its m pushed on the thread it left dead: same thread and top, no resume or closethread
     since, and at least one during m (e0: the epoch before m, masked as %lua-epoch). */
  static int aux_raisable(lua_State *L, int fresh, int e0) {
    aux_ctx *c;
    if (lua_status(L) == LUA_OK || !aux_in_lua_frame(L)) return 0;
    c = AUX_CTX(L);
    if (fresh && c->rerrL == L && c->rerrtop == lua_gettop(L) && c->rerrepoch == c->epoch
        && (int)(c->epoch & 0x3fffffff) != e0) return 0;
    return AUX_ETHREAD; }

  /* ---------- settop (D): lowering never closes anything (invariant I9) ---------- */
  static int aux_settop_d(lua_State *L, int idx, int floor) {
    int top = lua_gettop(L), nt;
    floor = aux_eff_floor(L, floor);
    if (idx >= 0) nt = idx;
    else { if (idx <= LUA_REGISTRYINDEX || -idx > top + 1) return AUX_EINDEX; nt = top + idx + 1; }
    if (nt > top) { if (!lua_checkstack(L, nt - top)) return AUX_ESTACK; lua_settop(L, nt); return 0; }
    if (nt < floor) return AUX_EINDEX;                    /* hook views: the hooked function's registers */
    if (nt < top && lua_status(L) != LUA_OK && aux_in_lua_frame(L)) return AUX_ETHREAD;
    lua_settop(L, nt); return 0; }

  /* ---------- the answer of a Scheme callback, applied by the C side AFTER C_callback returned ---------- */
  int aux_lua_dispatch(lua_State *L, int h, int nup);                 /* define-external: cfunction body */
  int aux_lua_kdispatch(lua_State *L, int h, int nup, int status);    /* define-external: k-function body */
  enum { AUX_RAISE = -1, AUX_LERROR = -2, AUX_ARGERROR = -3, AUX_TYPEERROR = -4, AUX_YIELD = -5, AUX_YIELDK = -6,
         AUX_ESCAPE = -7, AUX_INTERNAL = -8, AUX_BADANSWER = -9, AUX_STALE = -10, AUX_RUNERROR = -11 };
  static int aux_req_n = 0, aux_req_arg = 0, aux_req_k = -1;          /* side channel of the last answer */
  static char *aux_req_msg = NULL; static size_t aux_req_len = 0, aux_req_cap = 0;
  static int aux_req_set(const char *s, size_t n) {
    if (n + 1 > aux_req_cap) { size_t cap = n + 64; char *p = realloc(aux_req_msg, cap); if (!p) return 0; aux_req_msg = p; aux_req_cap = cap; }
    memcpy(aux_req_msg, s, n); aux_req_msg[n] = 0; aux_req_len = n; return 1; }
  static int aux_kfunction(lua_State *L, int status, lua_KContext ctx);

  /* the handle of the running Scheme closure, or NULL. Its nup must be the closure's real upvalue
     count (debug.setupvalue can plant another closure's handle): WRITE index checks trust it. */
  static aux_handle *aux_self(lua_State *L) {
    aux_handle *u = aux_tohandle(L, lua_upvalueindex(1));
    if (u == NULL || u->kind != AUX_K_PROC || u->nup < 0 || u->nup > 254) return NULL;
    if (lua_type(L, lua_upvalueindex(u->nup + 2)) != LUA_TNONE) return NULL;
    if (u->nup > 0 && lua_type(L, lua_upvalueindex(u->nup + 1)) == LUA_TNONE) return NULL;
    return u; }
  /* the floor aux_touch records for the current frame of L (status LUA_OK, not the base level): none (0)
     for a Scheme closure's frame, whose slots are its caller's; else the frame's height (C state) */
  static int aux_touch_note(lua_State *L) { return aux_cframe(L) && aux_self(L) != NULL ? 0 : lua_gettop(L); }

  static const char *aux_typearg(lua_State *L, int arg);                /* luaL_typeerror's "got" name, D */
  /* room for the pushes of an error raised for a callback (message, position, concat: 3 slots, 4 kept):
     the callback may have filled its frame up to ci->top, which lua_checkstack alone guards, and the API
     does not check a push.  If the stack cannot grow, the callback's own values above floor are dropped
     (floor: 0 for a lua_CFunction, whose frame has no TBC slot; a hook's entry top: the hooked
     function's registers stay).  Drops only happen at the stack size limit. */
  static void aux_room(lua_State *L, int floor) {
    int t;
    if (lua_checkstack(L, 4)) return;
    t = lua_gettop(L); lua_settop(L, t - 4 > floor ? t - 4 : floor); }
  static int aux_answer(lua_State *L, int r, int floor) {   /* every longjmp below happens after C_callback returned */
    if (r >= 0) {
      if (r > lua_gettop(L)) { aux_room(L, floor); return luaL_error(L, "aux.lua: callback answered more results than it pushed"); }
      return r; }
    switch (r) {
    case AUX_RAISE:     return lua_error(L);                                             /* value on top */
    case AUX_LERROR:    aux_room(L, floor);
                        lua_pushlstring(L, aux_req_msg, aux_req_len); luaL_where(L, 1); lua_rotate(L, -2, 1);
                        lua_concat(L, 2); return lua_error(L);
    case AUX_RUNERROR:  /* luaG_runerror's text: the position of the current function when it is a Lua one (a hook's) */
                        aux_room(L, floor);
                        lua_pushlstring(L, aux_req_msg, aux_req_len); luaL_where(L, 0); lua_rotate(L, -2, 1);
                        lua_concat(L, 2); return lua_error(L);
    case AUX_ARGERROR:  aux_room(L, floor);
                        lua_pushlstring(L, aux_req_msg, aux_req_len); return luaL_argerror(L, aux_req_arg, lua_tostring(L, -1));
    case AUX_TYPEERROR: { const char *got;
                          aux_room(L, floor);                                     /* first: got may point into a metatable */
                          got = aux_typearg(L, aux_req_arg);                      /* before any push: the arg as it is */
                          lua_pushfstring(L, "%s expected, got %s", aux_req_msg, got);  /* lauxlib's typeerror text */
                          return luaL_argerror(L, aux_req_arg, lua_tostring(L, -1)); }
    case AUX_YIELD:     if (aux_req_n < 0 || aux_req_n > lua_gettop(L)) { aux_room(L, floor); return luaL_error(L, "aux.lua: bad yield count"); }
                        return lua_yield(L, aux_req_n);
    case AUX_YIELDK:    if (aux_req_n < 0 || aux_req_n > lua_gettop(L)) { aux_room(L, floor); return luaL_error(L, "aux.lua: bad yield count"); }
                        return lua_yieldk(L, aux_req_n, (lua_KContext)aux_req_k, aux_kfunction);   /* ctx: the KFUNC handle */
    case AUX_ESCAPE:    lua_settop(L, floor); lua_pushlightuserdata(L, &aux_escape_key); return lua_error(L); /* own values: no TBC */
    }
    aux_room(L, floor);
    switch (r) {
    case AUX_INTERNAL:  return luaL_error(L, "aux.lua: internal error in a Scheme callback");
    case AUX_BADANSWER: return luaL_error(L, "aux.lua: a Scheme lua_CFunction must answer a result count");
    case AUX_STALE:     return luaL_error(L, "aux.lua: stale Scheme procedure");
    }
    return luaL_error(L, "aux.lua: bad callback answer %d", r); }

  static int aux_cfunction(lua_State *L) {
    aux_handle *u = aux_self(L);
    int r, saved, nf; lua_State *cur;
    if (u == NULL) return luaL_error(L, "aux.lua: released or forged Scheme procedure");
    if (!aux_reentrant) return luaL_error(L, "aux.lua: Scheme procedure called from a non-reentrant binding");
    if (aux_cstack_low()) return luaL_error(L, "C stack overflow");
    saved = aux_reentrant; aux_reentrant = 0; cur = aux_cur; aux_cur = L; AUX_CB_ENTER(nf);
    r = aux_lua_dispatch(L, u->handle, u->nup);          /* C_callback: Scheme runs AND RETURNS here */
    AUX_CB_LEAVE(nf); aux_reentrant = saved; aux_cur = cur;
    return aux_answer(L, r, 0); }

  /* the continuation of a Scheme closure that answered ((lua-yieldk n k) C): ctx is the handle of k, whose
     userdata registry[&aux_kfunc_key][L] holds.  The entry is consumed first (the handle is released
     when the userdata is collected: the entry below stays valid until Scheme drains the release). */
  static int aux_kfunction(lua_State *L, int status, lua_KContext ctx) {
    int r, saved, h = -1;
    aux_handle *k, *u = aux_self(L);
    luaL_checkstack(L, 4, NULL);                         /* raising here is fine: no Scheme ran yet */
    if (lua_rawgetp(L, LUA_REGISTRYINDEX, &aux_kfunc_key) == LUA_TTABLE) {
      lua_pushthread(L); lua_rawget(L, -2);               /* table ud */
      k = aux_tohandle(L, -1);
      if (k && k->kind == AUX_K_KFUNC && (lua_KContext)k->handle == ctx) {
        h = k->handle;
        lua_pushthread(L); lua_pushnil(L); lua_rawset(L, -4); }   /* an existing key: no allocation */
      lua_pop(L, 1); }
    lua_pop(L, 1);                                       /* our own pushes only: the stack is as C sees it */
    if (h < 0 || u == NULL) return luaL_error(L, "aux.lua: released or forged Scheme continuation");
    if (!aux_reentrant) return luaL_error(L, "aux.lua: Scheme procedure called from a non-reentrant binding");
    if (aux_cstack_low()) return luaL_error(L, "C stack overflow");
    { lua_State *cur = aux_cur; int nf;
      saved = aux_reentrant; aux_reentrant = 0; aux_cur = L; AUX_CB_ENTER(nf);
      r = aux_lua_kdispatch(L, h, u->nup, status);
      AUX_CB_LEAVE(nf); aux_reentrant = saved; aux_cur = cur; }
    return aux_answer(L, r, 0); }

  /* ---------- universe setup and teardown ---------- */
  static int op_setup(lua_State *L, aux_op *o) {
    aux_ctx *c = AUX_CTX(L);
    lua_createtable(L, 0, 4);
    lua_pushcfunction(L, aux_handle_gc); lua_setfield(L, -2, "__gc");
    lua_pushcfunction(L, aux_handle_tostring); lua_setfield(L, -2, "__tostring");
    lua_pushboolean(L, 0); lua_setfield(L, -2, "__metatable");
    lua_pushliteral(L, "aux.scheme"); lua_setfield(L, -2, "__name");
    lua_rawsetp(L, LUA_REGISTRYINDEX, &aux_mt_key);
    lua_createtable(L, 4, 0);
    lua_rawsetp(L, LUA_REGISTRYINDEX, &aux_anchor_key);
    aux_libkeys_make(L);
    (void)c; return 0; }

  /* completes a fresh state L with its ctx c (calloc'd by the caller, possibly already the ud of
     aux_alloc for lua_newstate): c, or NULL with L closed and c freed.  warnmode is the initial sink:
     1 (stderr) for luaL_newstate, which installs lauxlib's warnfon, 0 (discard) for lua_newstate,
     which installs no warning function at all (g->warnf = NULL: lua_warning does nothing) */
  static aux_ctx *aux_setup_with(lua_State *L, aux_ctx *c, int warnmode) {
    aux_op o;
    c->main = L; c->warnmode = warnmode; c->wstate = 1;  /* the state machine starts ON, as warnfon */
    AUX_CTX(L) = c;
    lua_atpanic(L, aux_panic);
    lua_setwarnf(L, aux_warnf, c);
    memset(&o, 0, sizeof o); o.fn = op_setup;
    if (aux_protect(L, &o, 0, NULL, 0, 0) != 0) { lua_close(L); free(c); return NULL; }
    return c; }
  /* the ctx of a fresh state made by luaL_newstate, or NULL (and the state closed) */
  static aux_ctx *aux_setup(lua_State *L) {
    aux_ctx *c = calloc(1, sizeof *c);
    if (!c) { lua_close(L); return NULL; }
    return aux_setup_with(L, c, 1); }

  static void aux_close(int safe, lua_State *L) {
    aux_ctx *c = AUX_CTX(L); int saved = aux_reentrant;
    if (safe) aux_reentrant = 1;                         /* lua_close runs every __gc: they may call Scheme */
    lua_close(L);
    aux_reentrant = saved;
    if (c) { aux_wclear(c); free(c->wq); free(c->wbuf); aux_nrel_total -= c->nrel; free(c->rel); free(c->nk); free(c); } }

  /* ---------- the slots of an op's own frame, after a collection step ----------
     An API call that allocates (lua_newuserdatauv, lua_createtable, lua_pushlstring / lua_pushstring /
     lua_pushliteral, lua_pushcclosure, lua_newthread, ...) may run a collection step, hence a Lua __gc
     finalizer; and a finalizer reaches every slot of the op's frame with debug.getlocal / debug.setlocal
     at level 2 (its "(C temporary)" slots, the values the body has just pushed included), as it reaches
     those of any C function, Lua's own included.  So a body that makes values and then applies a typed
     API to them (lua_rawset wants a table, lua_setiuservalue a full userdata, lua_setmetatable a table
     on top) makes them all first, then checks each slot it will use (below: a Lua error, inside the op),
     then makes no other allocating call before it is done with them.  The check also proves that an
     object the body holds by a C pointer (a new userdata, a new thread) is still referenced, hence alive:
     a step that ran a finalizer sweeps nothing after it, so what the finalizer unreferenced is freed by a
     later step at the earliest (or by an emergency collection the finalizer's own allocation makes: the
     check then sees another value in the slot, and the pointer is never used). */
  static int aux_slot_bad(lua_State *L) {
    return luaL_error(L, "aux.lua: a slot of a protected operation was rewritten (debug.setlocal from a finalizer)"); }
  #define AUX_SLOT(L, idx, t) do { if (lua_type((L), (idx)) != (t)) aux_slot_bad(L); } while (0)
  /* the userdata p that the body has just made is still at idx, and still has no metatable (a userdata
     Lua code made at the same address, after the collection of p, has one) */
  static void aux_slot_newud(lua_State *L, int idx, const void *p) {
    if (lua_type(L, idx) != LUA_TUSERDATA || lua_touserdata(L, idx) != p || lua_getmetatable(L, idx)) aux_slot_bad(L); }
  /* the thread T is still at idx */
  static void aux_slot_thread(lua_State *L, int idx, lua_State *T) {
    if (lua_type(L, idx) != LUA_TTHREAD || lua_tothread(L, idx) != T) aux_slot_bad(L); }

  /* pushes the handle metatable, registry[&aux_mt_key].  The registry is writable (lua-rawset on
     LUA_REGISTRYINDEX, debug.getregistry): an entry of another type is refused, as lua_setmetatable
     with a non-table on top is undefined behaviour in C.  (Another table is harmless: new handles
     get it, and aux_tohandle recognizes only those.)  No allocation. */
  static void aux_push_mt(lua_State *L) {
    if (lua_rawgetp(L, LUA_REGISTRYINDEX, &aux_mt_key) != LUA_TTABLE)
      luaL_error(L, "aux.lua: the registry entry of the Scheme handle metatable is not a table"); }
  /* ---------- the registry entries Lua's own libraries trust ----------
     liolib takes a full userdata whose metatable is registry["FILE*"] (LUA_FILEHANDLE) for an LStream {FILE *f;
     lua_CFunction closef} and calls its closef; lauxlib takes one whose metatable is registry["_UBOX*"] for a
     UBox {void *box; size_t bsize} and reallocs its box; loadlib's checkclib takes registry._CLIBS[path]
     (lua_getfield: its __index too) as a dlopen handle for dlsym.  None of them checks: a userdata the bound API
     made (lua-newuserdatauv: any size, uninitialized) with such a metatable, or a userdata stored in _CLIBS,
     crashes the process from plain Lua code (io.close, f:write, package.loadlib, require).  So the bindings
     never give those metatables to a userdata (full or light: a light userdata's metatable is its type's)
     that does not already have it, never set a metatable on registry._CLIBS, never store a userdata there
     under a string key, and never change the three registry entries except to remove them (nil; luaL-newmetatable
     still makes a fresh "FILE*" or "_UBOX*", whose table no userdata has yet).  Lua code cannot do any of this
     without the debug library or the registry (debug.getregistry, or handed out by the bound API), and a write
     through a metamethod (a table whose __newindex is the registry) is not seen: see the header. */
  /* pushes registry[aux_libnames[i]] and returns 1, or pushes nothing and returns 0 when it cannot be told:
     the key table was replaced (it is a registry entry: Lua code with debug.getregistry, or the bound API, may
     rewrite it).  D: 2 free slots, no allocation (raw gets with keys that exist). */
  static int aux_libslot_push(lua_State *L, int i) {
    size_t l; const char *s;
    if (lua_rawgetp(L, LUA_REGISTRYINDEX, &aux_libkeys_key) != LUA_TTABLE) { lua_pop(L, 1); return 0; }
    if (lua_rawgeti(L, -1, i + 1) != LUA_TSTRING || (s = lua_tolstring(L, -1, &l), l != strlen(aux_libnames[i]))
        || memcmp(s, aux_libnames[i], l) != 0) { lua_pop(L, 2); return 0; }
    lua_rawget(L, LUA_REGISTRYINDEX);                     /* the key string -> the entry */
    lua_remove(L, -2); return 1; }
  /* 1 when the key table is intact.  D: 2 free slots (else 0) */
  static int aux_libkeys_ok(lua_State *L) {
    int i;
    if (!lua_checkstack(L, 2)) return 0;
    for (i = 0; i < AUX_NLIB; i++) { if (!aux_libslot_push(L, i)) return 0; lua_pop(L, 1); }
    return 1; }
  /* makes the key table (anew): allocates, so inside a protected call only (op_setup, op_libkeys, an op body) */
  static void aux_libkeys_make(lua_State *L) {
    int i;
    lua_createtable(L, AUX_NLIB, 0);
    for (i = 0; i < AUX_NLIB; i++) { lua_pushstring(L, aux_libnames[i]); lua_rawseti(L, -2, i + 1); }
    lua_rawsetp(L, LUA_REGISTRYINDEX, &aux_libkeys_key); }
  static int op_libkeys(lua_State *L, aux_op *o) { (void)o; if (!aux_libkeys_ok(L)) aux_libkeys_make(L); return 0; }
  /* 1 when the value at idx is the table registry._CLIBS, -1 when that cannot be told; D, 2 free slots */
  static int aux_is_clibs(lua_State *L, int idx) {
    int r;
    if (lua_type(L, idx) != LUA_TTABLE) return 0;
    if (!aux_libslot_push(L, AUX_LIB_CLIBS)) return -1;
    r = lua_rawequal(L, -1, idx); lua_pop(L, 1); return r; }
  /* the index of the name ks / kl in aux_libnames, or -1; aux_libname_at: of the key at kidx, -2 for a key
     that is not a string */
  static int aux_libname(const char *ks, size_t kl) {
    int i;
    for (i = 0; i < AUX_NLIB; i++) if (kl == strlen(aux_libnames[i]) && memcmp(ks, aux_libnames[i], kl) == 0) return i;
    return -1; }
  static int aux_libname_at(lua_State *L, int kidx) {
    size_t kl; const char *ks;
    if (lua_type(L, kidx) != LUA_TSTRING) return -2;
    ks = lua_tolstring(L, kidx, &kl); return aux_libname(ks, kl); }
  /* t[k] = v by a binding (lua-setfield, lua-setglobal, lua-settable, lua-rawset): 0, AUX_EMETATABLE (the handle
     metatable stored in the registry under a string key: luaL-checkudata / luaL-testudata / luaL-setmetatable of
     that name would give it away), AUX_ELIBRARY (see above), AUX_ELIBKEYS (cannot be told: the key table was
     replaced) or AUX_ESTACK.  t and v are valid indices; kname is aux_libname(_at) of the key: only string keys
     matter, as the libraries look these entries up by name.  D: no allocation, no metamethod. */
  static int aux_libwrite_code(lua_State *L, int t, int kname, int v) {
    int r;
    if (kname == -2) return 0;
    if (!lua_checkstack(L, 2)) return AUX_ESTACK;
    t = lua_absindex(L, t); v = lua_absindex(L, v);
    if (lua_rawequal(L, t, LUA_REGISTRYINDEX)) {
      if (lua_type(L, v) == LUA_TTABLE) {
        lua_rawgetp(L, LUA_REGISTRYINDEX, &aux_mt_key);
        r = lua_rawequal(L, -1, v); lua_pop(L, 1);
        if (r) return AUX_EMETATABLE; }
      if (kname < 0 || lua_isnil(L, v)) return 0;         /* another name, or removed: allowed */
      if (!aux_libslot_push(L, kname)) return AUX_ELIBKEYS;
      r = lua_rawequal(L, -1, v); lua_pop(L, 1);         /* the same value again: no change */
      return r ? 0 : AUX_ELIBRARY; }
    if (lua_touserdata(L, v) == NULL) return 0;           /* neither a full nor a light userdata */
    r = aux_is_clibs(L, t);
    return r < 0 ? AUX_ELIBKEYS : r ? AUX_ELIBRARY : 0; }
  /* a stub's check: aux_libwrite_code, and when it cannot be told, the key table made anew by a protected op
     (op_libkeys: on the main thread when L is not OK) and the check made again.  That op runs Lua (a finalizer
     may call Scheme, and Scheme data move): *s0, the key's bytes in Scheme memory, is first replaced by a
     malloc'd copy, *dup, which the caller frees.  0, a refusal code, or the op's failure (its error value on top). */
  static int aux_libwrite_stub(int safe, lua_State *L, int t, int kname, int v, const char **s0, size_t l0, char **dup) {
    int c = aux_libwrite_code(L, t, kname, v);
    aux_op o = {0};
    if (c != AUX_ELIBKEYS) return c;
    if (s0 && *s0) { if (!(*dup = aux_dup(*s0, l0))) return AUX_ENOMEM; *s0 = *dup; }
    o.fn = op_libkeys;
    if ((c = aux_run_h(safe, L, &o, 0))) return c;
    c = aux_libwrite_code(L, t, kname, v);
    return c == AUX_ELIBKEYS ? AUX_ELIBRARY : c; }
  /* inside an op: the same, raised as a Lua error (a hook or a finalizer may have rewritten the op's slots,
     or the key table: then refused) */
  static void aux_libwrite_check(lua_State *L, int t, int kidx, int v) {
    int c;
    luaL_checkstack(L, 2, NULL);
    if ((c = aux_libwrite_code(L, t, aux_libname_at(L, kidx), v)) == AUX_EMETATABLE)
      luaL_error(L, "aux.lua: the handle metatable cannot be given");
    else if (c) luaL_error(L, "aux.lua: a registry entry that Lua's libraries trust cannot be forged"); }

  /* lua-setmetatable / luaL-setmetatable: D, the stack as it was.  AUX_EMETATABLE when
     - the value at idx is a Scheme handle, by its metatable or its shape: the release of its Scheme object
       is that metatable's __gc (another one leaks the object until lua-close), and another one can make
       a library take its bytes for its own (FILE*'s: liolib would call an LStream's closef from them);
     - the value at idx is the registry and mt is not nil: a __mode there, now or added later, lets the
       collector clear registry[LUA_RIDX_GLOBALS], which lapi.c's getGlobalTable (load, lua_getglobal, ...
       in Lua's own C code too) then reads as garbage;
     - the value at idx is registry._CLIBS and mt is not nil (an __index would answer checkclib);
     - mt is the handle metatable itself (a handle's is never given to anything else);
     - mt is registry["FILE*"] or registry["_UBOX*"] and the value at idx is a full or light userdata that
       does not have it already (see above).
     mt 0: not known yet (luaL-setmetatable's stub): only the checks on the value itself are made, and the op
     checks again with the metatable in hand.  AUX_ELIBKEYS: cannot be told (the key table was replaced); the
     binding makes it anew and asks again (a second AUX_ELIBKEYS is refused as AUX_EMETATABLE).  Lua code cannot do any of this without the debug library:
     setmetatable takes tables only, and getmetatable of a handle is false. */
  static int aux_setmt_code(lua_State *L, int idx, int mt) {
    int bad = 0, t, i;
    if (!lua_checkstack(L, 4)) return AUX_ESTACK;
    idx = lua_absindex(L, idx); if (mt) mt = lua_absindex(L, mt);
    if (lua_rawequal(L, idx, LUA_REGISTRYINDEX)) return !mt || lua_isnil(L, mt) ? 0 : AUX_EMETATABLE;
    lua_rawgetp(L, LUA_REGISTRYINDEX, &aux_mt_key);
    if (mt && lua_type(L, -1) == LUA_TTABLE && lua_rawequal(L, -1, mt)) bad = 1;
    else if (lua_type(L, idx) == LUA_TUSERDATA) {
      if (lua_rawlen(L, idx) == sizeof(aux_handle) && ((aux_handle *)lua_touserdata(L, idx))->magic == AUX_MAGIC) bad = 1;
      else if (lua_getmetatable(L, idx)) { bad = lua_type(L, -2) == LUA_TTABLE && lua_rawequal(L, -1, -2); lua_pop(L, 1); } }
    lua_pop(L, 1);
    if (bad || !mt || lua_isnil(L, mt)) return bad ? AUX_EMETATABLE : 0;
    t = lua_type(L, idx);
    if (t == LUA_TTABLE) { i = aux_is_clibs(L, idx); return i < 0 ? AUX_ELIBKEYS : i ? AUX_EMETATABLE : 0; }
    if (t != LUA_TUSERDATA && t != LUA_TLIGHTUSERDATA) return 0;
    for (i = AUX_LIB_FILE; i <= AUX_LIB_UBOX; i++) {
      if (!aux_libslot_push(L, i)) return AUX_ELIBKEYS;    /* cannot be told */
      if (lua_type(L, -1) == LUA_TTABLE && lua_rawequal(L, -1, mt)) {
        bad = !lua_getmetatable(L, idx) || (lua_rawequal(L, -1, -2) ? (lua_pop(L, 1), 0) : (lua_pop(L, 1), 1)); }
      lua_pop(L, 1);
      if (bad) return AUX_EMETATABLE; }
    return 0; }
  /* registry[LUA_RIDX_GLOBALS] must be a table: lua_load (and luaL_load*, luaL_dostring, ...) and
     lua_setglobal / lua_getglobal read it through lapi.c's getGlobalTable, whose api_check aborts an
     API-checked build (a release one makes a non-table the loaded chunk's _ENV).  The bindings never
     write a non-table there (aux_globals_write_bad), and every binding reaching getGlobalTable checks the
     slot first (aux_globals_code, aux_globals_check), as Lua code with the debug library (or holding the registry) may.
     aux_globals_code: D, leaves the stack as it was: 0, AUX_EGLOBALS, or AUX_ESTACK (no free slot); 0 on
     a thread that is not OK (the op is refused anyway, AUX_ETHREAD). */
  static int aux_globals_code(lua_State *L) {
    int t;
    if (lua_status(L) != LUA_OK) return 0;
    if (!lua_checkstack(L, 1)) return AUX_ESTACK;
    t = lua_rawgeti(L, LUA_REGISTRYINDEX, LUA_RIDX_GLOBALS); lua_pop(L, 1);
    return t == LUA_TTABLE ? 0 : AUX_EGLOBALS; }
  /* inside an op (may raise): right before a call that reaches getGlobalTable */
  static void aux_globals_check(lua_State *L) {
    luaL_checkstack(L, 1, NULL);
    if (lua_rawgeti(L, LUA_REGISTRYINDEX, LUA_RIDX_GLOBALS) != LUA_TTABLE)
      luaL_error(L, "aux.lua: registry[LUA_RIDX_GLOBALS] is not a table");
    lua_pop(L, 1); }
  /* 1 when t[k] = v (t, v: valid indices; k: the key at kidx, or the integer ik when kidx is 0) would put
     a non-table into registry[LUA_RIDX_GLOBALS].  D: no allocation, no metamethod.  (A float key with an
     integral value is the integer key, as in Lua; a string "2" is another key.)  LUA_RIDX_MAINTHREAD is
     not guarded: its one reader, lcorolib's coroutine.close, is replaced by aux_co_close, which never
     trusts it, and lua_tothread of another value is NULL. */
  static int aux_globals_write_bad(lua_State *L, int t, int kidx, lua_Integer ik, int v) {
    if (!lua_rawequal(L, t, LUA_REGISTRYINDEX)) return 0;
    if (kidx != 0) {
      int isint;
      if (lua_type(L, kidx) != LUA_TNUMBER) return 0;
      ik = lua_tointegerx(L, kidx, &isint);
      if (!isint) return 0; }
    return ik == LUA_RIDX_GLOBALS && lua_type(L, v) != LUA_TTABLE; }
  /* inside an op: the same, raised (the operands were checked on the originals; a hook may rewrite the
     trampoline's slots with debug.setlocal before the body runs) */
  static void aux_globals_write_check(lua_State *L, int t, int kidx, lua_Integer ik, int v) {
    if (aux_globals_write_bad(L, t, kidx, ik, v)) luaL_error(L, "aux.lua: registry[LUA_RIDX_GLOBALS] must be a table"); }
  /* pushes registry[key], a table, made weak-keyed when the entry is missing or not a table (Lua code
     may have replaced it) */
  static void aux_push_weakk(lua_State *L, const void *key) {
    if (lua_rawgetp(L, LUA_REGISTRYINDEX, key) == LUA_TTABLE) return;
    lua_pop(L, 1);
    lua_createtable(L, 0, 2); lua_createtable(L, 0, 1);        /* t mt            (collection steps) */
    lua_pushliteral(L, "__mode"); lua_pushliteral(L, "k");     /* t mt "__mode" "k"                  */
    AUX_SLOT(L, -4, LUA_TTABLE); AUX_SLOT(L, -3, LUA_TTABLE); AUX_SLOT(L, -2, LUA_TSTRING);
    lua_rawset(L, -3); lua_setmetatable(L, -2);                /* t: raw, no metamethod runs          */
    lua_pushvalue(L, -1); lua_rawsetp(L, LUA_REGISTRYINDEX, key); }

  /* ---------- generic op bodies used by the core ---------- */
  /* i0 = C kind, i1 = user upvalues n (PROC: operands 1..n), n0 = handle, s0/l0 = __tostring text or NULL.
     The handle is stored LAST: success of the op <=> Lua owns the handle (its __gc releases it).  The
     allocations come first, then the checks of the slots (see aux_slot_bad), then the typed calls. */
  static int op_newhandle(lua_State *L, aux_op *o) {
    int n = o->i1;
    aux_handle *u = (aux_handle *)lua_newuserdatauv(L, sizeof *u, 1);   /* (a collection step) */
    if (o->s0) {
      lua_pushlstring(L, o->s0, o->l0);                                 /* (a collection step) */
      aux_slot_newud(L, -2, u); AUX_SLOT(L, -1, LUA_TSTRING);
      lua_setiuservalue(L, -2, 1); }
    else aux_slot_newud(L, -1, u);
    u->magic = AUX_MAGIC; u->handle = -1; u->kind = o->i0; u->nup = n;
    aux_push_mt(L);
    lua_setmetatable(L, -2);
    if (o->i0 == AUX_K_PROC) {
      lua_rotate(L, 1, 1); lua_pushcclosure(L, aux_cfunction, n + 1);  /* (a collection step) */
      /* the closure is still on top, u its upvalue 1: else nothing Lua owns would hold the handle */
      if (lua_tocfunction(L, -1) != aux_cfunction || lua_getupvalue(L, -1, 1) == NULL) aux_slot_bad(L);
      if (lua_touserdata(L, -1) != u) aux_slot_bad(L);
      lua_pop(L, 1); }
    u->handle = (int)o->n0;
    return 1; }
  /* n0 = handle: registry[&aux_kfunc_key][L] = a new KFUNC userdata (see aux_kfunction).  The handle is
     stored LAST: success of the op <=> Lua owns the handle. */
  static int op_kanchor(lua_State *L, aux_op *o) {
    aux_handle *u;
    aux_push_weakk(L, &aux_kfunc_key);
    lua_pushthread(L);
    u = (aux_handle *)lua_newuserdatauv(L, sizeof *u, 0);               /* (a collection step) */
    aux_slot_newud(L, -1, u); aux_slot_thread(L, -2, L); AUX_SLOT(L, -3, LUA_TTABLE);
    u->magic = AUX_MAGIC; u->handle = -1; u->kind = AUX_K_KFUNC; u->nup = 0;
    aux_push_mt(L); lua_setmetatable(L, -2);
    lua_rawset(L, -3);
    u->handle = (int)o->n0;
    return 0; }
  static int op_pushcclosure_c(lua_State *L, aux_op *o) { lua_pushcclosure(L, (lua_CFunction)o->p0, o->i1); return 1; }
  static int op_pushlstring(lua_State *L, aux_op *o) { lua_pushlstring(L, o->s0, o->l0); return 1; }
  static int op_getfield(lua_State *L, aux_op *o) { lua_pushlstring(L, o->s0, o->l0); o->r = lua_gettable(L, 1); return 1; }
  static int op_setfield(lua_State *L, aux_op *o) {                  /* args: t v */
    lua_pushlstring(L, o->s0, o->l0);                                   /* (a collection step) */
    aux_libwrite_check(L, 1, -1, 2);
    lua_pushvalue(L, 2); lua_settable(L, 1); return 0; }
  static int op_getglobal(lua_State *L, aux_op *o) {
    lua_rawgeti(L, LUA_REGISTRYINDEX, LUA_RIDX_GLOBALS); lua_pushlstring(L, o->s0, o->l0);
    o->r = lua_gettable(L, -2); lua_remove(L, -2); return 1; }
  static int op_setglobal(lua_State *L, aux_op *o) {                 /* args: v */
    lua_rawgeti(L, LUA_REGISTRYINDEX, LUA_RIDX_GLOBALS); lua_pushlstring(L, o->s0, o->l0);   /* (a collection step) */
    aux_libwrite_check(L, -2, -1, 1);                                   /* the globals may be the registry */
    lua_pushvalue(L, 1); lua_settable(L, -3); return 0; }
  /* ---------- coroutine.close (see the barriers): lcorolib's own function, called unless it would close the
     running thread while a binding runs Lua on it; that case raises a Lua error instead.  The running
     main thread is refused here, with lcorolib's own message: lcorolib recognizes it by
     registry[LUA_RIDX_MAINTHREAD], which Lua code (debug.getregistry) or the bound API may overwrite,
     and it would then close the main thread under the protected call running it (a longjmp to its
     OUTERMOST protected call, over every binding's).  The ctx's pointer cannot be forged.  lcorolib's
     function is static: it is taken from the table luaopen_coroutine builds, the only place it appears,
     and replaced there before anyone sees it. */
  static lua_CFunction aux_co_close0 = NULL;
  static int aux_co_close(lua_State *L) {
    if (lua_isnone(L, 1) || lua_tothread(L, 1) == L) {       /* closing the running thread itself */
      if (L == AUX_CTX(L)->main) return luaL_error(L, "cannot close main thread");
      if (aux_barred(L)) return luaL_error(L, "aux.lua: cannot close a running coroutine while a Scheme binding runs Lua on it"); }
    if (aux_co_close0 == NULL) return luaL_error(L, "aux.lua: coroutine.close is not available");
    return aux_co_close0(L); }
  /* luaopen-coroutine: luaopen_coroutine with coroutine.close replaced */
  static int aux_luaopen_coroutine(lua_State *L) {
    luaopen_coroutine(L);
    if (lua_getfield(L, -1, "close") == LUA_TFUNCTION && lua_iscfunction(L, -1) && lua_tocfunction(L, -1) != aux_co_close)
      aux_co_close0 = lua_tocfunction(L, -1);
    lua_pop(L, 1);
    lua_pushcfunction(L, aux_co_close); lua_setfield(L, -2, "close");
    return 1; }
  /* linit.c's luaL_openselectedlibs, with aux_luaopen_coroutine for lcorolib's opener (so neither
     luaL-openlibs, luaL-openselectedlibs nor a require of a preloaded "coroutine" installs the raw close) */
  static const luaL_Reg aux_stdlibs[] = {
    {LUA_GNAME, luaopen_base}, {LUA_LOADLIBNAME, luaopen_package}, {LUA_COLIBNAME, aux_luaopen_coroutine},
    {LUA_DBLIBNAME, luaopen_debug}, {LUA_IOLIBNAME, luaopen_io}, {LUA_MATHLIBNAME, luaopen_math},
    {LUA_OSLIBNAME, luaopen_os}, {LUA_STRLIBNAME, luaopen_string}, {LUA_TABLIBNAME, luaopen_table},
    {LUA_UTF8LIBNAME, luaopen_utf8}, {NULL, NULL} };
  /* lauxlib's luaL_requiref, with the open function openf, or (openf NULL) the value at fidx, and the
     global slot checked right before lua_setglobal (openf may have run Lua or Scheme code) */
  static void aux_requiref(lua_State *L, const char *modname, lua_CFunction openf, int fidx, int glb) {
    luaL_getsubtable(L, LUA_REGISTRYINDEX, LUA_LOADED_TABLE);
    lua_getfield(L, -1, modname);                            /* LOADED[modname] */
    if (!lua_toboolean(L, -1)) {
      lua_pop(L, 1);
      if (openf) lua_pushcfunction(L, openf); else lua_pushvalue(L, fidx);
      lua_pushstring(L, modname);
      lua_call(L, 1, 1);
      lua_pushvalue(L, -1);
      lua_setfield(L, -3, modname); }                         /* LOADED[modname] = module */
    lua_remove(L, -2);
    if (glb) { aux_globals_check(L); lua_pushvalue(L, -1); lua_setglobal(L, modname); } }
  static void aux_openselectedlibs(lua_State *L, int load, int preload) {
    int mask; const luaL_Reg *lib;
    luaL_getsubtable(L, LUA_REGISTRYINDEX, LUA_PRELOAD_TABLE);
    for (lib = aux_stdlibs, mask = 1; lib->name != NULL; lib++, mask <<= 1) {
      if (load & mask) { aux_requiref(L, lib->name, lib->func, 0, 1); lua_pop(L, 1); }
      else if (preload & mask) { lua_pushcfunction(L, lib->func); lua_setfield(L, -2, lib->name); } }
    lua_pop(L, 1); }
  static int op_openlibs(lua_State *L, aux_op *o) { aux_openselectedlibs(L, ~0, 0); return 0; }
  /* the global slot, when luaL_openselectedlibs would set a global (a library of load) */
  static int aux_openlibs_code(lua_State *L, int load) {
    const luaL_Reg *lib; int mask;
    for (lib = aux_stdlibs, mask = 1; lib->name != NULL; lib++, mask <<= 1)
      if (load & mask) return aux_globals_code(L);
    return 0; }
  /* ---------- C function pointers Scheme may push (lua-pushcclosure, luaL-setfuncs, luaL-requiref, ...) ----------
     Any other c-pointer would be called as a lua_CFunction: a table's or a userdata's address (lua-topointer,
     lua-touserdata, lua-newuserdatauv), or the function of a C closure that reads its upvalues (io.lines'
     and string.gmatch's iterators, from lua-tocfunction) pushed without them.  Vouched are the luaopen-*
     openers and every function lua-tocfunction has read from a LIGHT C function: Lua itself holds it as a
     callable value with no upvalue, so any Lua code may call it with any arguments; given upvalues, it
     ignores them.  The set only grows, and is process-wide: a light C function of a C module a universe
     loaded (require, package.loadlib) stays vouched after that universe closed and unloaded it. */
  static void **aux_cf_tab = NULL; static size_t aux_cf_cap = 0, aux_cf_cnt = 0;
  static size_t aux_cf_slot(void **t, size_t cap, void *f) {
    size_t i = (size_t)(((uintptr_t)f >> 3) * 2654435761u) & (cap - 1);
    while (t[i] && t[i] != f) i = (i + 1) & (cap - 1);
    return i; }
  static void aux_cf_vouch(void *f) {                   /* D: no Lua call; malloc failure only leaves f out */
    size_t i;
    if (f == NULL) return;
    if (2 * (aux_cf_cnt + 1) > aux_cf_cap) {
      size_t n = aux_cf_cap ? 2 * aux_cf_cap : 64; void **t = (void **)calloc(n, sizeof *t);
      if (!t) return;
      for (i = 0; i < aux_cf_cap; i++) if (aux_cf_tab[i]) t[aux_cf_slot(t, n, aux_cf_tab[i])] = aux_cf_tab[i];
      free(aux_cf_tab); aux_cf_tab = t; aux_cf_cap = n; }
    i = aux_cf_slot(aux_cf_tab, aux_cf_cap, f);
    if (!aux_cf_tab[i]) { aux_cf_tab[i] = f; aux_cf_cnt++; } }
  static int aux_cf_vouched(void *f) {
    const luaL_Reg *lib;
    if (f == NULL) return 0;
    for (lib = aux_stdlibs; lib->name != NULL; lib++) if ((void *)lib->func == f) return 1;
    return aux_cf_cap && aux_cf_tab[aux_cf_slot(aux_cf_tab, aux_cf_cap, f)] == f; }
  /* lua_tocfunction (D), vouching the function when the value is a light C function: lua_getupvalue
     answers NULL for it, and pushes the first upvalue of a C closure (which always has one) */
  static void *aux_tocfunction(lua_State *L, int idx) {
    lua_CFunction f = lua_tocfunction(L, idx);
    if (f && lua_checkstack(L, 1)) {
      idx = lua_absindex(L, idx);
      if (lua_getupvalue(L, idx, 1)) lua_pop(L, 1); else aux_cf_vouch((void *)f); }
    return (void *)f; }
  static int op_tolstring(lua_State *L, aux_op *o) { luaL_tolstring(L, 1, NULL); return 1; }   /* on a copy */
  /* luaL_ref / luaL_unref keep a free list at t[1] (lauxlib 5.5).  Lua code (debug.getregistry) or the
     bound API may overwrite it; lauxlib would then hand out a live slot, or fail an assertion.  So:
     aux_freelist_ok(L, t): t[1] is nil/false (no ref yet), or 0 (empty list), or an integer n that is a
     possible reference (2 <= n <= INT_MAX, and n > LUA_RIDX_LAST in the registry, whose reserved slots
     are never references) whose link t[n] is an integer too.  lauxlib reads the head as
     `(int)lua_tointeger', so a larger head would be truncated into another slot (a reserved one, or a
     negative ref our callers take for one of the AUX_* codes).  aux_ref raises when it is not ok (inside
     an op); aux_unref of a registry luaV only drops the value then.  t: an absolute or pseudo index. */
  static lua_Integer aux_ref_floor(lua_State *L, int t) {       /* refs are > this */
    return lua_rawequal(L, t, LUA_REGISTRYINDEX) ? LUA_RIDX_LAST : 1; }
  static int aux_freelist_ok(lua_State *L, int t) {
    int ok, top = lua_gettop(L);
    if (lua_rawgeti(L, t, 1) == LUA_TNUMBER && lua_isinteger(L, -1)) {
      lua_Integer n = lua_tointeger(L, -1);
      ok = n == 0 || (n > aux_ref_floor(L, t) && n <= INT_MAX
                      && lua_rawgeti(L, t, n) == LUA_TNUMBER && lua_isinteger(L, -1)); }
    else ok = !lua_toboolean(L, -1);
    lua_settop(L, top); return ok; }
  /* luaL_ref of the value on top, t absolute (or the registry's pseudo index).  The ref it answers is
     known beforehand: nil is LUA_REFNIL (nothing stored); else the free list's head, or, when the list is
     empty, lua_rawlen(t) + 1 (lauxlib: `(int)lua_rawlen(L, t) + 1', truncated too when the border is
     >= INT_MAX, which a table of a few dozen keys can have).  Every other outcome is refused before
     anything is stored: a ref is always > aux_ref_floor and <= INT_MAX. */
  static int aux_ref(lua_State *L, int t) {
    if (lua_isnil(L, -1)) return luaL_ref(L, t);
    luaL_checkstack(L, 2, NULL);
    if (!aux_freelist_ok(L, t)) return luaL_error(L, "aux.lua: the reference free list of this table is corrupted");
    if (lua_rawgeti(L, t, 1) != LUA_TNUMBER) {              /* nil/false: initialize it, as luaL_ref does */
      lua_pop(L, 1); lua_pushinteger(L, 0); lua_rawseti(L, t, 1); lua_pushinteger(L, 0); }
    if (lua_tointeger(L, -1) == 0) {                         /* empty list: a new ref after the border */
      lua_Unsigned len = lua_rawlen(L, t);
      if (len >= (lua_Unsigned)INT_MAX)
        return luaL_error(L, "aux.lua: no reference is left in this table (its border is %I)", (LUAI_UACINT)len);
      if ((lua_Integer)len + 1 <= aux_ref_floor(L, t))     /* the registry's border is below a reserved slot */
        return luaL_error(L, "aux.lua: the next reference of this table would be a reserved slot (%d)", (int)len + 1); }
    lua_pop(L, 1);
    return luaL_ref(L, t); }
  static int op_ref(lua_State *L, aux_op *o) { lua_pushvalue(L, 1); o->r = aux_ref(L, LUA_REGISTRYINDEX); return 0; }
  static int op_unref(lua_State *L, aux_op *o) {
    int list = lua_rawgeti(L, LUA_REGISTRYINDEX, 1) == LUA_TNUMBER && lua_isinteger(L, -1);
    lua_pop(L, 1);
    if (list && aux_freelist_ok(L, LUA_REGISTRYINDEX)) luaL_unref(L, LUA_REGISTRYINDEX, o->i0);
    else { lua_pushnil(L); lua_rawseti(L, LUA_REGISTRYINDEX, o->i0); }   /* no usable free list: release the value */
    return 0; }

  /* S ops: status-returning calls that protect themselves */
  static int aux_pcall(int safe, lua_State *L, int nargs, int nres, int msgh) {
    int top = lua_gettop(L), st, saved; aux_barrier b;
    if (lua_status(L) != LUA_OK) return AUX_ETHREAD;
    if (nargs < 0 || nargs + 1 > top) return AUX_EUNDER;
    if (top - nargs - 1 < aux_floor(L)) return AUX_EINDEX;   /* the function and its arguments are consumed (I6) */
    if (nres < LUA_MULTRET || nres > 250) return AUX_ERANGE;   /* lua_pcallk's API check: MAXRESULTS (lstate.h) */
    if (msgh != 0) {
      if (!aux_ix(L, msgh, AUX_IX_STACK, 0)) return AUX_EINDEX;
      msgh = lua_absindex(L, msgh);
      if (msgh > top - nargs - 1) return AUX_EINDEX;         /* the handler must lie below the function */
      if (lua_type(L, msgh) != LUA_TFUNCTION) return AUX_ETYPE;   /* lua_pcallk's API check */
    }
    if (nres > 0 && !lua_checkstack(L, nres)) return AUX_ESTACK;
    saved = aux_reentrant; if (safe) aux_reentrant = 1;
    AUX_BARRIER_PUSH(b, L);
    st = lua_pcall(L, nargs, nres, msgh);
    AUX_BARRIER_POP(b);
    aux_reentrant = saved; return st; }

  /* a mode Lua 5.5 accepts but the bindings refuse: "B" (fixed buffer) makes the loaded function keep
     pointers into the buffer given to lua_load, and no buffer the bindings hand over outlives the call (a
     Scheme bytevector moves, the copies are freed, luaL_loadfilex reads into its own C frame) */
  static int aux_badmode(const char *mode, size_t ml) { return mode != NULL && memchr(mode, 'B', ml) != NULL; }

  /* luaL_loadbufferx inside an op: while the parser works, its slots lie in the op's own C frame, which
     the bindings never read (see aux_dbg_getlocal), and the thread has a frame, so neither lua-resume,
     lua-closethread nor Lua's coroutine.close can reset it under the parser (a Scheme finalizer may run
     during the parse).  name and mode are always copied (a bytevector has no NUL terminator); the buffer
     only on the safe path.  A failure of the op itself (out of memory) is answered as lua_load's status,
     the message on top, as C. */
  static int op_loadbuffer(lua_State *L, aux_op *o) {
    aux_globals_check(L);                                   /* again: a finalizer may have run since */
    o->r = luaL_loadbufferx(L, o->s0, o->l0, o->s1, o->s2); return 1; }
  static int aux_loadbuffer(int safe, lua_State *L, const char *b, size_t n, const char *name, size_t nl,
                            const char *mode, size_t ml) {
    char *cb = NULL, *cn = NULL, *cm = NULL; int r, saved; aux_op o;
    if (lua_status(L) != LUA_OK) return AUX_ETHREAD;
    if (aux_badmode(mode, ml)) return AUX_EARG;
    if ((r = aux_globals_code(L))) return r;
    if ((name && !(cn = aux_dup(name, nl))) || (mode && !(cm = aux_dup(mode, ml)))) { free(cn); free(cm); return AUX_ENOMEM; }
    if (safe && !(cb = aux_dup(b, n))) { free(cn); free(cm); return AUX_ENOMEM; }
    memset(&o, 0, sizeof o); o.fn = op_loadbuffer; o.s0 = safe ? cb : b; o.l0 = n; o.s1 = cn; o.s2 = cm;
    saved = aux_reentrant; if (safe) aux_reentrant = 1;
    r = aux_protect(L, &o, 0, NULL, 0, 1);
    aux_reentrant = saved;
    free(cb); free(cn); free(cm);
    return AUX_IS_LUAERR(r) ? -r - 1000 : r; }

  /* ---------- thread anchors (invariant I5): registry[&aux_anchor_key][id] = the thread of a 'thread luaR ---------- */
  /* inside an op (may raise): the thread at tidx becomes anchor id (a table is made again if Lua code
     replaced ours: the anchors it held are lost, and the luaRs that used them are refused from then on) */
  static void aux_anchor_set(lua_State *L, int tidx, int id, lua_State *T) {   /* T: the thread at tidx */
    tidx = lua_absindex(L, tidx);
    if (lua_rawgetp(L, LUA_REGISTRYINDEX, &aux_anchor_key) != LUA_TTABLE) {
      lua_pop(L, 1); lua_createtable(L, 4, 0);                        /* (a collection step: see aux_slot_bad) */
      AUX_SLOT(L, -1, LUA_TTABLE);
      lua_pushvalue(L, -1); lua_rawsetp(L, LUA_REGISTRYINDEX, &aux_anchor_key); }
    aux_slot_thread(L, tidx, T);
    lua_pushvalue(L, tidx); lua_rawseti(L, -2, id); lua_pop(L, 1); }
  /* the records of aux_nk_t (see aux_nk_touch): only compared by address, never dereferenced */
  static int aux_nk_find(aux_ctx *c, lua_State *T) {
    int i; for (i = 0; i < c->nnk; i++) if (c->nk[i].L == T) return i;
    return -1; }
  static void aux_nk_del(aux_ctx *c, int i) { c->nk[i] = c->nk[--c->nnk]; }
  /* The id may be reused once cleared: a record made through it passes to another anchor of the same thread
     (one still holding it, so the thread is alive and its address its own), or goes. */
  static int op_unanchor(lua_State *L, aux_op *o) {
    aux_ctx *c = AUX_CTX(L); int i = 0, t = lua_rawgetp(L, LUA_REGISTRYINDEX, &aux_anchor_key);
    lua_State *T = NULL;
    if (t == LUA_TTABLE && lua_rawgeti(L, -1, o->i0) == LUA_TTHREAD) T = lua_tothread(L, -1);
    if (t == LUA_TTABLE) lua_pop(L, 1);
    while (i < c->nnk) {
      if (c->nk[i].id == o->i0) {
        int nid = -1;
        if (T != NULL && c->nk[i].L == T) {
          lua_pushnil(L);
          while (lua_next(L, -2)) {                         /* D: the keys are never modified */
            if (nid < 0 && lua_isinteger(L, -2) && lua_tointeger(L, -2) != o->i0 && lua_tointeger(L, -2) > 0
                && lua_tointeger(L, -2) <= INT_MAX && lua_type(L, -1) == LUA_TTHREAD && lua_tothread(L, -1) == T)
              nid = (int)lua_tointeger(L, -2);
            lua_pop(L, 1); } }
        if (nid < 0) { aux_nk_del(c, i); continue; }
        c->nk[i].id = nid; }
      i++; }
    if (t == LUA_TTABLE) { lua_pushnil(L); lua_rawseti(L, -2, o->i0); }
    lua_pop(L, 1); return 0; }
  /* D: the thread anchored as id, or NULL.  Read through the main thread M: a thread pointer is never
     dereferenced before it is found anchored (it may have been collected). */
  static lua_State *aux_anchor_thread(lua_State *M, int id) {
    lua_State *T = NULL; int top;
    if (!lua_checkstack(M, 2)) return NULL;
    top = lua_gettop(M);
    if (lua_rawgetp(M, LUA_REGISTRYINDEX, &aux_anchor_key) == LUA_TTABLE && lua_rawgeti(M, -1, id) == LUA_TTHREAD)
      T = lua_tothread(M, -1);
    lua_settop(M, top);                                  /* our own pushes only */
    return T; }

  /* ---------- the base of a suspended or dead thread in a Lua frame (invariant I6) ----------
     On a thread suspended by a hook's yield, or dead by an error raised in Lua code, the slots up to the
     height it had then are the registers of that Lua frame: the hooked function goes on with them (and a
     pending CALL, RETURN or SETLIST with B = 0 counts its operands up to the top), a dead frame keeps
     its to-be-closed variables there for lua-closethread.  Neither lua_resume (resume discards its
     arguments with `L->top = firstArg', resume_error with `L->top -= narg') nor lua_xmove may lower the
     top below it.  The API cannot read that height; it is known at the moment of the event, and nothing
     can push on the thread afterwards but a binding, which first gives its luaR to %L (aux_anchor_check,
     which calls aux_nk_touch): so the height is recorded there, at the first touch after the event, or
     by lua-resume itself (exact), or by the hook's yield (its entry height, an upper bound: made exact at
     the next touch).  A record belongs to the thread only while the anchor it was made through still
     holds that thread (a collected thread's address may be reused); a record that cannot be trusted is
     made again from the current top, which is never below the true limit (no binding lowers such a thread
     below its record's limit: aux_settop_d refuses, lua-check pops only what lua-resume left), so it
     only refuses more. */
  static int aux_nk_add(aux_ctx *c) {
    if (c->nnk == c->capnk) {
      int i = 0, cap; aux_nk_t *q;
      while (i < c->nnk)                                   /* first drop the records of released anchors */
        if (c->nk[i].id < 0 || aux_anchor_thread(c->main, c->nk[i].id) != c->nk[i].L) aux_nk_del(c, i); else i++;
      if (c->nnk == c->capnk) {
        cap = c->capnk ? 2 * c->capnk : 8;
        if (!(q = realloc(c->nk, cap * sizeof *q))) return 0;
        c->nk = q; c->capnk = cap; } }
    c->nnk++; return 1; }
  /* D: the record of T made (again) from its current top; errval: the value on top is the error value
     lua_resume has just pushed on a thread it left dead */
  static void aux_nk_set(lua_State *T, int id, int errval) {
    aux_ctx *c = AUX_CTX(T); int i = aux_nk_find(c, T), st = lua_status(T);
    if (st == LUA_OK || !aux_in_lua_frame(T)) { if (i >= 0) aux_nk_del(c, i); return; }
    if (i < 0) { if (!aux_nk_add(c)) return; i = c->nnk - 1; }
    c->nk[i].L = T; c->nk[i].id = id; c->nk[i].st = st; c->nk[i].base = lua_gettop(T);
    c->nk[i].flags = errval ? AUX_NK_ERRVAL : 0; }
  /* D: T is used by a binding through a luaR anchored as id (-1: a luaR made by hand, which has no anchor) */
  static void aux_nk_touch(lua_State *T, int id) {
    aux_ctx *c = AUX_CTX(T); int i, st = lua_status(T);
    if (st == LUA_OK && c->nnk == 0) return;              /* the common case */
    i = aux_nk_find(c, T);
    if (i >= 0 && st != LUA_OK && c->nk[i].st == st && aux_in_lua_frame(T)
        && (c->nk[i].id < 0 ? id < 0 : aux_anchor_thread(c->main, c->nk[i].id) == T)) {   /* -1: a luaR made by hand
                                                           (the test suite's), unchecked as its pointer is */
      if (c->nk[i].flags & AUX_NK_REFINE) { c->nk[i].base = lua_gettop(T); c->nk[i].flags = 0; }
      return; }
    aux_nk_set(T, id, 0); }
  /* D: the lowest top a binding may leave on T, a thread whose status is not LUA_OK and whose current
     frame is a Lua function: its record's base (less the error value lua-resume left there), or its top
     when no record could be made */
  static int aux_nk_lim(lua_State *T, int id) {
    aux_ctx *c = AUX_CTX(T); int i;
    aux_nk_touch(T, id);
    if ((i = aux_nk_find(c, T)) < 0) return lua_gettop(T);
    return c->nk[i].base - ((c->nk[i].flags & AUX_NK_ERRVAL) ? 1 : 0); }
  /* D: T has just yielded from a hook entered at height h (lua_yield returned): the true height is the one
     luaD_hook restores, at most h (h may include the registers a hook protects, see luaD_hook) */
  static void aux_nk_hookyield(lua_State *T, int h) {
    aux_ctx *c = AUX_CTX(T); int i = aux_nk_find(c, T);
    if (i >= 0) { c->nk[i].st = LUA_YIELD; c->nk[i].base = h; c->nk[i].flags = AUX_NK_REFINE; } }

  /* 1: T is anchored as id; 0: it is not (the luaR is refused); AUX_ELOADING: it is a parser thread */
  static int aux_anchor_check(lua_State *M, int id, lua_State *T) {
    if (T == NULL || aux_anchor_thread(M, id) != T) return 0;
    if (aux_is_loading(T)) return AUX_ELOADING;
    aux_nk_touch(T, id); return 1; }

  /* ---------- small D helpers ---------- */
  /* the name lauxlib uses for the type of the value at arg (luaL_typeerror), without allocating.  A __name
     is answered as Lua's own bytes, whole (the metatable, reachable from the value at arg, keeps them alive):
     the caller uses them at once, before anything could run a collection step. */
  static const char *aux_typearg(lua_State *L, int arg) {
    int q = aux_qix(L, arg);
    if (q < 0) return "no value";
    /* a positive arg above the top is the nil value, as in C: the __name of the type nil's metatable, if
       any, else "no value" (lua_getmetatable reads it at top+1, an acceptable index once slots are reserved) */
    arg = q == 0 ? 0 : lua_absindex(L, arg);
    if (lua_checkstack(L, 4) && lua_getmetatable(L, q == 0 ? lua_gettop(L) + 1 : arg)) {
      const char *name = NULL;
      lua_pushnil(L);
      while (lua_next(L, -2)) {                            /* D: starts from nil, keys are never modified */
        size_t kn; const char *k;
        if (!name && lua_type(L, -2) == LUA_TSTRING && lua_type(L, -1) == LUA_TSTRING
            && (k = lua_tolstring(L, -2, &kn)) && kn == 6 && memcmp(k, "__name", 6) == 0)
          name = lua_tostring(L, -1);                      /* a string value: no conversion */
        lua_pop(L, 1); }
      lua_pop(L, 1);
      if (name) return name; }
    if (q == 0) return "no value";
    if (lua_type(L, arg) == LUA_TLIGHTUSERDATA) return "light userdata";
    return lua_typename(L, lua_type(L, arg)); }

  /* ---------- the error value of luaL_error / luaL_argerror, computed without raising (P) ---------- */
  /* lauxlib's findfield and pushglobalfuncname (static there), unchanged */
  static int aux_findfield(lua_State *L, int objidx, int level) {
    if (level == 0 || !lua_istable(L, -1)) return 0;
    lua_pushnil(L);
    while (lua_next(L, -2)) {
      if (lua_type(L, -2) == LUA_TSTRING) {
        if (lua_rawequal(L, objidx, -1)) { lua_pop(L, 1); return 1; }
        else if (aux_findfield(L, objidx, level - 1)) {
          lua_pushliteral(L, "."); lua_replace(L, -3); lua_concat(L, 3); return 1; } }
      lua_pop(L, 1); }
    return 0; }
  static int aux_pushglobalfuncname(lua_State *L, lua_Debug *ar) {
    int top = lua_gettop(L);
    lua_getinfo(L, "f", ar);
    lua_getfield(L, LUA_REGISTRYINDEX, LUA_LOADED_TABLE);
    luaL_checkstack(L, 6, "not enough stack");
    if (aux_findfield(L, top + 1, 2)) {
      const char *name = lua_tostring(L, -1);
      if (strncmp(name, LUA_GNAME ".", 3) == 0) { lua_pushstring(L, name + 3); lua_remove(L, -2); }
      lua_copy(L, -1, top + 1); lua_settop(L, top + 1); return 1; }
    lua_settop(L, top); return 0; }
  /* The value C's luaL_error (i0 = 0: s0 the formatted message, kept whole) or luaL_argerror (i0 = 1: i1
     the arg, s0 the extramsg, without NUL) raises when it is called from the frame below this op's
     trampoline -- the callback's own, or the base level -- as lauxlib computes it: every level is C's
     plus one (level 0 is the trampoline).  It is pushed, not raised: the T bindings put it in their
     condition's `value', and the trampoline later raises the same text (aux_answer). */
  static int op_errval(lua_State *L, aux_op *o) {
    lua_Debug ar; const char *argword, *extramsg; int arg = o->i1;
    luaL_checkstack(L, 10, NULL);
    if (o->i0 == 0) { luaL_where(L, 2); lua_pushlstring(L, o->s0, o->l0); lua_concat(L, 2); return 1; }
    extramsg = lua_pushlstring(L, o->s0, o->l0);
    if (!lua_getstack(L, 1, &ar)) {
      luaL_where(L, 2); lua_pushfstring(L, "bad argument #%d (%s)", arg, extramsg); lua_concat(L, 2); return 1; }
    lua_getinfo(L, "nt", &ar);
    if (arg <= ar.extraargs) argword = "extra argument";
    else {
      arg -= ar.extraargs;
      if (strcmp(ar.namewhat, "method") == 0) {
        arg--;
        if (arg == 0) {
          luaL_where(L, 2); lua_pushfstring(L, "calling '%s' on bad self (%s)", ar.name, extramsg);
          lua_concat(L, 2); return 1; } }
      argword = "argument"; }
    if (ar.name == NULL) ar.name = aux_pushglobalfuncname(L, &ar) ? lua_tostring(L, -1) : "?";
    luaL_where(L, 2); lua_pushfstring(L, "bad %s #%d to '%s' (%s)", argword, arg, ar.name, extramsg);
    lua_concat(L, 2); return 1; }

  /* a float as Lua writes it (lobject.c tostringbuffFloat) */
  static const char *aux_num2str(lua_Number x) {
    static char buf[LUA_N2SBUFFSZ + 4];
    int len = snprintf(buf, LUA_N2SBUFFSZ, LUA_NUMBER_FMT, (LUAI_UACNUMBER)x);
    if (lua_str2number(buf, NULL) != x) len = snprintf(buf, LUA_N2SBUFFSZ, LUA_NUMBER_FMT_N, (LUAI_UACNUMBER)x);
    if (buf[strspn(buf, "-0123456789")] == '\0') { buf[len++] = lua_getlocaledecpoint(); buf[len++] = '0'; buf[len] = 0; }
    return buf; }

  <#

  ;; ==========================================================================================
  ;; CORE — records
  ;; ==========================================================================================

  ;; lua_State wrapper.  kind: 'main | 'thread | 'view | 'hook.  state is #f once closed/invalidated.
  ;; u: the luaU of the universe; ref: for a 'thread, the id of its anchor (a fixnum; the
  ;; universe's private anchor table holds the thread at that key), else #f;
  ;; nup: user upvalue count of the running Scheme closure for views, else -1;
  ;; floor: for 'hook views, gettop at hook entry (the top cannot be lowered below it), else 0;
  ;; main: for a 'thread, the main luaR of its universe (kept alive by the thread luaR), else #f.
  (define-record luaR state u kind ref nup floor main)

  ;; one per universe; it does NOT point back to the main luaR (only weakly, mainw), so the main
  ;; luaR can be finalized.  callables: live callable handles (proc, hook, kfunc, reader) of this
  ;; universe; active: in-flight SAFE stubs on this universe; unrefs: registry refs to luaL_unref
  ;; at the next drain point; close-pending: #t when the finalizer found the universe running.
  ;; objects / ofree / onext: the handle table (slot: #f | (obj kind . luaU)), its free slots and
  ;; its high-water mark; anext / afree: thread anchor ids (next fresh one, released ones);
  ;; unanchors: anchor ids to clear at the next drain point; mainw: a weak locative to the main luaR.
  (define-record luaU state ctx callables active unrefs close-pending objects ofree onext anext afree unanchors mainw)

  ;; a Lua value kept in the registry of universe u (tables, functions, userdata, threads, ...)
  (define-record luaV u ref)
  (define (lua-ref? x) (luaV? x))                 ; not curried: a predicate on a Scheme value

  (set-record-printer! luaR
    (λ (r out) (display (string-append "#<luaR " (symbol->string (luaR-kind r)) (if (luaR-state r) "" " closed") ">") out)))
  (set-record-printer! luaV (λ (v out) (display (string-append "#<luaV " (number->string (luaV-ref v)) ">") out)))

  ;; ==========================================================================================
  ;; CORE — constants (the ones the core needs; S1 exports the rest)
  ;; ==========================================================================================

  (define LUA_OK (foreign-value "LUA_OK" int))
  (define LUA_YIELD (foreign-value "LUA_YIELD" int))
  (define LUA_ERRRUN (foreign-value "LUA_ERRRUN" int))
  (define LUA_ERRSYNTAX (foreign-value "LUA_ERRSYNTAX" int))
  (define LUA_ERRMEM (foreign-value "LUA_ERRMEM" int))
  (define LUA_ERRERR (foreign-value "LUA_ERRERR" int))
  (define LUA_ERRFILE (foreign-value "LUA_ERRFILE" int))
  (define LUA_MULTRET (foreign-value "LUA_MULTRET" int))
  (define LUA_REGISTRYINDEX (foreign-value "LUA_REGISTRYINDEX" int))
  (define LUA_RIDX_GLOBALS (foreign-value "LUA_RIDX_GLOBALS" int))
  (define LUA_RIDX_MAINTHREAD (foreign-value "LUA_RIDX_MAINTHREAD" int))
  (define LUA_NOREF (foreign-value "LUA_NOREF" int))
  (define LUA_REFNIL (foreign-value "LUA_REFNIL" int))
  (define LUA_TNONE (foreign-value "LUA_TNONE" int))
  (define LUA_TNIL (foreign-value "LUA_TNIL" int))
  (define LUA_TBOOLEAN (foreign-value "LUA_TBOOLEAN" int))
  (define LUA_TLIGHTUSERDATA (foreign-value "LUA_TLIGHTUSERDATA" int))
  (define LUA_TNUMBER (foreign-value "LUA_TNUMBER" int))
  (define LUA_TSTRING (foreign-value "LUA_TSTRING" int))
  (define LUA_TTABLE (foreign-value "LUA_TTABLE" int))
  (define LUA_TFUNCTION (foreign-value "LUA_TFUNCTION" int))
  (define LUA_TUSERDATA (foreign-value "LUA_TUSERDATA" int))
  (define LUA_TTHREAD (foreign-value "LUA_TTHREAD" int))
  (define LUA_NUMTYPES (foreign-value "LUA_NUMTYPES" int))

  ;; internal codes, mirrored from the C prelude
  (define AUX_EINDEX (foreign-value "AUX_EINDEX" int))
  (define AUX_ESTACK (foreign-value "AUX_ESTACK" int))
  (define AUX_ENOMEM (foreign-value "AUX_ENOMEM" int))
  (define AUX_EUNDER (foreign-value "AUX_EUNDER" int))
  (define AUX_ETYPE (foreign-value "AUX_ETYPE" int))
  (define AUX_ETHREAD (foreign-value "AUX_ETHREAD" int))
  (define AUX_ERANGE (foreign-value "AUX_ERANGE" int))
  (define AUX_ESTALE (foreign-value "AUX_ESTALE" int))
  (define AUX_EARG (foreign-value "AUX_EARG" int))
  (define AUX_ERDONLY (foreign-value "AUX_ERDONLY" int))
  (define AUX_ELOADING (foreign-value "AUX_ELOADING" int))
  (define AUX_EGLOBALS (foreign-value "AUX_EGLOBALS" int))
  (define AUX_EMETATABLE (foreign-value "AUX_EMETATABLE" int))
  (define AUX_ELIBRARY (foreign-value "AUX_ELIBRARY" int))
  (define AUX_ELIBKEYS (foreign-value "AUX_ELIBKEYS" int))
  (define AUX_K_PROC (foreign-value "AUX_K_PROC" int))
  (define AUX_K_OBJECT (foreign-value "AUX_K_OBJECT" int))
  (define AUX_K_COND (foreign-value "AUX_K_COND" int))
  (define AUX_K_HOOK (foreign-value "AUX_K_HOOK" int))
  (define AUX_K_KFUNC (foreign-value "AUX_K_KFUNC" int))
  (define AUX_K_READER (foreign-value "AUX_K_READER" int))
  (define AUX_RAISE (foreign-value "AUX_RAISE" int))
  (define AUX_LERROR (foreign-value "AUX_LERROR" int))
  (define AUX_RUNERROR (foreign-value "AUX_RUNERROR" int))
  (define AUX_ARGERROR (foreign-value "AUX_ARGERROR" int))
  (define AUX_TYPEERROR (foreign-value "AUX_TYPEERROR" int))
  (define AUX_YIELD (foreign-value "AUX_YIELD" int))
  (define AUX_YIELDK (foreign-value "AUX_YIELDK" int))
  (define AUX_ESCAPE (foreign-value "AUX_ESCAPE" int))
  (define AUX_INTERNAL (foreign-value "AUX_INTERNAL" int))
  (define AUX_BADANSWER (foreign-value "AUX_BADANSWER" int))
  (define AUX_STALE (foreign-value "AUX_STALE" int))

  (define (lua-type->symbol t)
    (case t
      ((-1) 'none) ((0) 'nil) ((1) 'boolean) ((2) 'lightuserdata) ((3) 'number) ((4) 'string)
      ((5) 'table) ((6) 'function) ((7) 'userdata) ((8) 'thread)
      (else (%api-error 'lua-type->symbol "not a type code" (list t) AUX_EARG))))

  ;; ==========================================================================================
  ;; CORE — conditions
  ;; ==========================================================================================

  (define (%status->symbol st)
    (cond ((fx= st LUA_ERRRUN) 'errrun) ((fx= st LUA_ERRSYNTAX) 'errsyntax) ((fx= st LUA_ERRMEM) 'errmem)
          ((fx= st LUA_ERRERR) 'errerr) ((fx= st LUA_ERRFILE) 'errfile) ((fx= st LUA_YIELD) 'yield) (else 'ok)))

  (define (%api-message code)
    (cond ((fx= code AUX_EINDEX) "invalid stack index")
          ((fx= code AUX_ESTACK) "cannot grow the Lua stack")
          ((fx= code AUX_ENOMEM) "cannot copy an operand")
          ((fx= code AUX_EUNDER) "not enough values on the stack")
          ((fx= code AUX_ETYPE) "wrong type of value at the given index")
          ((fx= code AUX_ETHREAD) "thread is suspended or dead")
          ((fx= code AUX_ERANGE) "argument out of range")
          ((fx= code AUX_ESTALE) "stale lua_Debug")
          ((fx= code AUX_EARG) "bad argument")
          ((fx= code AUX_ERDONLY) "a read-only variable (a for loop's control variable, a vararg parameter, <const> or <close>)")
          ((fx= code AUX_ELOADING) "the thread is running a lua-load parse")
          ((fx= code AUX_EGLOBALS) "registry[LUA_RIDX_GLOBALS] must be a table")
          ((fx= code AUX_EMETATABLE) "the metatable of a Scheme handle, of the registry or of registry._CLIBS cannot be set, the handle metatable cannot be given, nor can FILE*'s or _UBOX*'s be given to another userdata")
          ((fx= code AUX_ELIBRARY) "registry[\"FILE*\"], registry[\"_UBOX*\"] and registry._CLIBS (guarded, best-effort, among the entries Lua's libraries trust) can only be removed, and registry._CLIBS takes no userdata")
          (else "refused")))

  ;; (exn lua api): the binding refused before Lua ran
  (define (%api-error who msg args #!optional (code AUX_EARG))
    (set! %busy-ctx #f)                                    ; the binding leaves: its pointer is not used again
    (abort (make-composite-condition
             (make-property-condition 'exn 'location who 'message msg 'arguments args)
             (make-property-condition 'lua 'status 'api 'code code)
             (make-property-condition 'api))))
  (define (%api-fail who code args) (%api-error who (%api-message code) args code))

  ;; (exn lua): a Lua error. props are extra 'lua properties (value, request, arg, text).
  (define (%lua-condition who msg args st . props)
    (make-composite-condition
      (make-property-condition 'exn 'location who 'message msg 'arguments args)
      (apply make-property-condition 'lua 'status (%status->symbol st) 'code st props)))

  (define lua-condition? (condition-predicate 'lua))
  (define (lua-condition-status c) (get-condition-property c 'lua 'status #f))
  (define %novalue (list 'novalue))
  (define (lua-condition-value c) (let ((v (get-condition-property c 'lua 'value %novalue))) (if (eq? v %novalue) #f v)))

  ;; the text Lua sees for a Scheme condition (tostring of the COND handle).  A string message is
  ;; kept whole; everything else (a non-string message, the 'exn 'arguments, a raised non-condition)
  ;; is written under CHICKEN's print-length limit, as print-error-message does, so a circular or
  ;; huge argument gives a text ending in "..." instead of an endless write: the text is computed
  ;; for every raised condition, even one Lua catches and never reads.
  (define %text-limit 4096)
  (define (%condition-message e)
    (let ((m (and (condition? e) (get-condition-property e 'exn 'message #f))))
      (call-with-output-string
        (λ (port)
          (when (string? m) (display m port))
          (##sys#with-print-length-limit
            %text-limit
            (λ ()
              (cond (m (unless (string? m) (display m port))
                       (let ((args (get-condition-property e 'exn 'arguments '())))
                         (when (pair? args)
                           (display ": " port)
                           ;; a circular argument list ends too: every step prints something
                           (let loop ((a args) (first? #t))
                             (when (pair? a)
                               (unless first? (display " " port))
                               (write (car a) port)
                               (loop (cdr a) #f))))))
                    ((condition? e) (display "<scheme condition>" port))
                    (else (display e port)))))))))

  ;; ==========================================================================================
  ;; CORE — argument checks (CHICKEN's `int' silently truncates: every int argument is checked)
  ;; ==========================================================================================

  (define-syntax %int!
    (syntax-rules ()
      ((_ who x ...) (begin (unless (and (fixnum? x) (fx<= x 2147483647) (fx>= x -2147483648))
                              (%api-error who "not an int" (list x) AUX_ERANGE)) ...))))
  ;; a C int used as a boolean (lua_pushboolean's b, lua_dump's strip, ...): #t or #f only.  With
  ;; Scheme truthiness 0 would be true, the opposite of C, so ported code would silently invert.
  (define-syntax %bool!
    (syntax-rules ()
      ((_ who x ...) (begin (unless (boolean? x) (%api-error who "not a boolean (#t or #f; 0 is not false here)" (list x))) ...))))
  (define-syntax %bytes!
    (syntax-rules ()
      ((_ who x ...) (begin (unless (or (string? x) (bytevector? x)) (%api-error who "not a string or bytevector" (list x))) ...))))
  (define (%nul-free? s)
    (let ((bv (if (string? s) (string->utf8 s) s)))
      (let loop ((i 0)) (cond ((fx= i (bytevector-length bv)) #t) ((fx= 0 (bytevector-u8-ref bv i)) #f) (else (loop (fx+ i 1)))))))
  ;; a C string argument that C reads with strlen: a string without NUL, or #f (NULL) when allowed
  (define (%cstring! who s false-ok?)
    (unless (or (and false-ok? (not s)) (and (string? s) (%nul-free? s)))
      (%api-error who "not a string without NUL" (list s))))
  ;; the mode of a load: #f or a C string without "B".  Lua 5.5's "B" (fixed buffer) makes the loaded
  ;; function keep pointers into the buffer given to lua_load (its code, line info, long strings), and
  ;; no buffer a binding hands over outlives the call (a moving Scheme bytevector, a copy freed at
  ;; once, luaL_loadfilex's own C frame); Lua's own `load' refuses it for Lua code likewise.
  (define (%mode! who mode)
    (%cstring! who mode #t)
    (when (and mode (memv #\B (string->list mode)))
      (%api-error who "mode \"B\" (a fixed buffer the loaded function would keep using) is refused" (list mode))))

  ;; ==========================================================================================
  ;; CORE — raw D helpers (no validation: internal use on values the core controls)
  ;; ==========================================================================================

  (define %type-raw (foreign-lambda int "lua_type" (c-pointer "lua_State") int))
  (define %gettop (foreign-lambda int "lua_gettop" (c-pointer "lua_State")))
  (define %status (foreign-lambda int "lua_status" (c-pointer "lua_State")))
  (define %pop! (foreign-lambda* void (((c-pointer "lua_State") L) (int n)) "lua_settop(L, -(n)-1);"))
  (define %typename (foreign-lambda c-string "lua_typename" (c-pointer "lua_State") int))
  ;; the floor of p (I6) for a binding given its luaR floor
  (define %eff-floor (foreign-lambda int "aux_eff_floor" (c-pointer "lua_State") int))
  (define %top-escape? (foreign-lambda* bool (((c-pointer "lua_State") L))
                         "C_return(lua_gettop(L) > 0 && lua_type(L, -1) == LUA_TLIGHTUSERDATA && lua_touserdata(L, -1) == (void *)&aux_escape_key);"))
  ;; the handle id of a COND or OBJECT handle at idx, else -1
  (define %handle-at (foreign-lambda* int (((c-pointer "lua_State") L) (int idx))
                       "aux_handle *u = aux_tohandle(L, idx);
                        C_return(u && (u->kind == AUX_K_COND || u->kind == AUX_K_OBJECT) ? u->handle : -1);"))
  (define %num2str (foreign-lambda c-string "aux_num2str" double))

  ;; bytes of the string or number at idx (numbers as Lua writes them, the slot left unchanged),
  ;; #f for other values.  Two steps, never C_alloc of a large string.
  (define %strlen (foreign-lambda* integer64 (((c-pointer "lua_State") L) (int idx))
                    "int q = aux_qix(L, idx); size_t n; char b[LUA_N2SBUFFSZ];
                     if (q < 0) C_return(AUX_EINDEX);
                     if (q == 0) C_return(-1);
                     switch (lua_type(L, idx)) {
                     case LUA_TSTRING: lua_tolstring(L, idx, &n); C_return((C_s64)n);
                     case LUA_TNUMBER: C_return((C_s64)lua_numbertocstring(L, idx, b) - 1);
                     default: C_return(-1); }"))
  (define %strcopy! (foreign-lambda* void (((c-pointer "lua_State") L) (int idx) (scheme-object bv))
                      "size_t n; char b[LUA_N2SBUFFSZ];
                       if (lua_type(L, idx) == LUA_TSTRING) { const char *s = lua_tolstring(L, idx, &n); memcpy(C_data_pointer(bv), s, n); }
                       else { n = lua_numbertocstring(L, idx, b) - 1; memcpy(C_data_pointer(bv), b, n); }"))
  (define (%bytes-at who p idx)
    (let ((n (%strlen p idx)))
      (cond ((fx= n -1) #f)
            ((< n -1000) (%api-fail who n (list idx)))
            (else (let ((bv (make-bytevector n))) (%strcopy! p idx bv) bv)))))

  ;; CHICKEN 6 strings must be valid UTF-8: other bytes stay a bytevector
  (define (%bytes->string/maybe bv) (condition-case (utf8->string bv) (_ () bv)))
  ;; every byte >= 128 of an invalid text becomes U+FFFD: built as UTF-8 bytes (at most 3 per byte) by two
  ;; linear C passes over the bytevectors (no allocation there), never as a list of characters (an error
  ;; value may be a large binary blob)
  (define %bytes-high (foreign-lambda* size_t ((scheme-object bv))
                        "const unsigned char *p = C_data_pointer(bv); size_t i, n = C_header_size(bv), k = 0;
                         for (i = 0; i < n; i++) k += p[i] >= 128;
                         C_return(k);"))
  (define %bytes-lossy! (foreign-lambda* void ((scheme-object to) (scheme-object from))
                          "const unsigned char *p = C_data_pointer(from); unsigned char *q = C_data_pointer(to);
                           size_t i, n = C_header_size(from);
                           for (i = 0; i < n; i++)
                             if (p[i] < 128) *q++ = p[i]; else { *q++ = 0xEF; *q++ = 0xBF; *q++ = 0xBD; }"))
  (define (%bytes->string/lossy bv)
    (let ((s (%bytes->string/maybe bv)))
      (if (string? s) s
        (let ((out (make-bytevector (+ (bytevector-length bv) (* 2 (%bytes-high bv))))))
          (%bytes-lossy! out bv)
          (utf8->string out)))))
  ;; the concatenation of the bytevectors of l, taken in REVERSE order (no apply: l may be long)
  (define %bytes-copy! (foreign-lambda* void ((scheme-object to) (size_t at) (scheme-object from))
                         "memcpy((char *)C_data_pointer(to) + at, C_data_pointer(from), C_header_size(from));"))
  (define (%bytes-concat-reverse l)
    (let* ((n (let loop ((l l) (n 0)) (if (null? l) n (loop (cdr l) (+ n (bytevector-length (car l)))))))
           (out (make-bytevector n)))
      (let loop ((l l) (end n))
        (if (null? l) out
          (let ((at (fx- end (bytevector-length (car l)))))
            (%bytes-copy! out at (car l))
            (loop (cdr l) at))))))
  (define (%->bytes x) (if (string? x) (string->utf8 x) x))

  ;; ==========================================================================================
  ;; CORE — handle tables: Scheme objects reachable from Lua (I10).  Each universe has its own
  ;; table (luaU-objects; slot: #f | (obj kind . luaU)), reachable only from the universe: a
  ;; callback that captures the universe's own luaR forms a cycle the collector reclaims.
  ;; ==========================================================================================

  (define %live 0)                         ; live handles of all universes
  (define %universes '())                  ; weak locatives to the open universes (lua-live-handles)
  (define (%callable-kind? kind) (memq kind '(proc hook kfunc reader)))

  (define (%register-universe! u)
    (set! %universes
      (cons (make-weak-locative u 0)
            (let loop ((l %universes))
              (cond ((null? l) '())
                    ((let ((x (locative->object (car l)))) (and x (luaU-state x))) (cons (car l) (loop (cdr l))))
                    (else (loop (cdr l))))))))

  (define (%handle-free! u h)
    (let* ((v (luaU-objects u)) (e (and (fixnum? h) (fx>= h 0) (fx< h (vector-length v)) (vector-ref v h))))
      (when e
        (when (%callable-kind? (cadr e))
          (luaU-callables-set! u (fx- (luaU-callables u) 1)))
        (vector-set! v h #f)
        (luaU-ofree-set! u (cons h (luaU-ofree u)))
        (set! %live (fx- %live 1)))))

  ;; the handles Lua released (their userdata's __gc) in universe u, freed
  (define %release-pop (foreign-lambda* int ((c-pointer c))
                         "aux_ctx *x = (aux_ctx *)c; if (x->nrel == 0) C_return(-1);
                          aux_nrel_total--; C_return(x->rel[--x->nrel]);"))
  (define (%drain-handles! u)
    (let ((ctx (luaU-ctx u)))
      (when ctx
        (let loop ((h (%release-pop ctx)))
          (unless (fx< h 0) (%handle-free! u h) (loop (%release-pop ctx)))))))

  (define (%handle-new! obj kind u)
    (%drain-handles! u)
    (let ((h (if (pair? (luaU-ofree u))
               (let ((h (car (luaU-ofree u)))) (luaU-ofree-set! u (cdr (luaU-ofree u))) h)
               (let ((n (luaU-onext u)) (v (luaU-objects u)))
                 (when (fx= n (vector-length v))
                   (let ((w (make-vector (fx* 2 n) #f)))
                     (do ((i 0 (fx+ i 1))) ((fx= i n)) (vector-set! w i (vector-ref v i)))
                     (luaU-objects-set! u w)))
                 (luaU-onext-set! u (fx+ n 1))
                 n))))
      (when (%callable-kind? kind) (luaU-callables-set! u (fx+ (luaU-callables u) 1)))
      (vector-set! (luaU-objects u) h (cons obj (cons kind u)))
      (set! %live (fx+ %live 1))
      h))

  ;; range- and kind-checked (forged handles are harmless): the entry of handle h of u, or #f
  (define (%handle-entry u h kinds)
    (let* ((v (luaU-objects u)) (e (and (fixnum? h) (fx>= h 0) (fx< h (vector-length v)) (vector-ref v h))))
      (and e (memq (cadr e) kinds) e)))

  ;; every handle of universe u, once u is closed (lua_close ran every __gc it could)
  (define (%sweep-handles! u)
    (do ((h 0 (fx+ h 1))) ((fx= h (luaU-onext u))) (%handle-free! u h)))

  ;; live handles: of all universes, or of the universe of x (a luaR or a luaU), for leak checks.
  ;; Like a binding, it first closes the dropped universes whose close was deferred.
  (define (lua-live-handles #!optional x)
    (unless (null? %pending-closes) (%close-pending!))
    (if (not x)
      (begin
        (for-each (λ (w) (let ((u (locative->object w))) (when (and u (luaU-state u)) (%drain-handles! u)))) %universes)
        %live)
      (let ((u (if (luaR? x) (luaR-u x) x)))
        (%drain-handles! u)
        (let ((v (luaU-objects u)))
          (let loop ((h 0) (n 0))
            (if (fx= h (luaU-onext u)) n (loop (fx+ h 1) (if (vector-ref v h) (fx+ n 1) n))))))))

  ;; ==========================================================================================
  ;; CORE — the fast/safe machinery
  ;; ==========================================================================================

  ;; (define-lua-stubs (%fast %safe) ((type var) ...) "C body using `safe' and `L'")
  ;; One C body, two stubs: the fast one is a non-safe foreign-lambda* (no Scheme can run inside
  ;; it), the safe one a foreign-safe-lambda* (Lua may call back into Scheme).
  (define-syntax define-lua-stubs
    (syntax-rules ()
      ((_ (fast-stub safe-stub) (args ...) body)
       (begin (define fast-stub (foreign-lambda*      int ((bool safe) ((c-pointer "lua_State") L) args ...) body))
              (define safe-stub (foreign-safe-lambda* int ((bool safe) ((c-pointer "lua_State") L) args ...) body))))))

  ;; the universe of the innermost running SAFE stub: a callback (lua_CFunction, continuation, hook,
  ;; reader) always runs inside one, and it finds its handles in that universe's table (I10)
  (define %cur-u #f)

  ;; The ctx pointer object of the universe whose lua_State a binding has just read with %L, until
  ;; the binding's stub has returned (%D / %P clear it; a safe stub sets it again on return, for the
  ;; code that reads its results).  The finalizer of a main luaR does not close that universe (I7):
  ;; between %L and the stub, the luaR the binding was given may already be garbage.
  (define %busy-ctx #f)

  ;; main luaRs whose finalizer found their universe running or busy, or holding Scheme callables
  ;; (see %lua-finalizer): closed at the entry of the next binding (%L, %U, lua-live-handles),
  ;; outside CHICKEN's finalizer loop, when no binding can be using a pointer of theirs any more -- at that
  ;; point every pointer read before is either used up or held by a running safe stub (active).
  ;; (Re-registering the finalizer instead would make (gc #t) loop for ever.)
  (define %pending-closes '())
  ;; A close may run __gc callbacks, and one may escape (resumed by %close-universe!): the luaRs not
  ;; closed yet then go back to the list, for the next binding entry.  The list is taken first, so a
  ;; universe deferred again (running: we are inside one of its callbacks) is not retried here.
  (define (%close-pending!)
    (let ((l %pending-closes))
      (set! %pending-closes '())
      (set! %busy-ctx #f)                       ; a binding entry: the mark is stale
      (dynamic-wind
        void
        (λ () (let loop () (unless (null? l) (let ((L (car l))) (set! l (cdr l)) (%lua-finalize L) (loop)))))
        (λ () (unless (null? l) (set! %pending-closes (append l %pending-closes)) (set! l '()))))))

  (define %depth 0)              ; Scheme callbacks currently running (nesting depth)
  (define %pending-escape #f)    ; (depth . rest): a continuation escape stopped at a callback boundary

  ;; One Scheme thread at a time runs Lua that may call Scheme back (I12).  A safe stub leaves Lua's
  ;; C frames on the C stack while its callbacks run, and CHICKEN's C_callback returns to the
  ;; innermost C frame: if a callback switched threads (thread-yield!, a blocking wait, a timeslice)
  ;; and another thread entered Lua with callbacks of its own, the callbacks would return out of
  ;; order, each answer reaching the other thread's Lua call.  So a safe stub is entered only by the
  ;; thread that owns the lock (taken with the first safe stub, given back when the outermost one
  ;; has returned and the module's globals are restored); another thread waits, yielding, until it
  ;; is free.  Fast stubs never call Scheme and need no lock.  The test and the set are inline, with
  ;; no procedure call in between: no thread switch can come between them.
  ;;
  ;; The wait is a thread switch: while it lasts, the owner may close a universe, return from the
  ;; callback whose view the waiter holds, or let a coroutine be collected.  So a binding waits
  ;; BEFORE it reads any lua_State pointer (%settle!, from %prepare!), and between that point and
  ;; its stub nothing switches threads (no timer interrupt in this module; no Scheme code of the
  ;; user runs outside the lock).  %run's safe path then takes the lock without waiting
  ;; (%lua-take!); finding it held by another thread would mean a stale pointer, refused.
  (define %lua-owner #f)         ; the Scheme thread inside safe stubs, or #f
  (define %lua-holds 0)          ; its nesting count
  ;; waits, yielding, until the lock is free or held by this thread (it is not taken)
  (define (%lua-wait!)
    (let ((me ##sys#current-thread))
      (let loop ()
        (cond
          ((or (not %lua-owner) (eq? %lua-owner me)) (void))
          ((memq (##sys#slot %lua-owner 3) '(dead terminated))   ; it died inside a callback: its C frames stay
           (%api-error 'lua "a Scheme thread that was running Lua callbacks is gone" (list %lua-owner) AUX_ETHREAD))
          ((and (memq (##sys#slot %lua-owner 3) '(blocked sleeping))   ; its callback waits for us: we would wait for ever
                (let ((b (##sys#slot %lua-owner 11)))
                  (or (eq? b me) (and (##sys#structure? b 'mutex) (eq? (##sys#slot b 2) me)))))
           (%api-error 'lua "deadlock: the Scheme thread running Lua callbacks waits for this one" (list %lua-owner) AUX_ETHREAD))
          (else (##sys#thread-yield!) (loop))))))
  ;; takes the lock, which must be free or this thread's: no pointer read before may be used
  ;; after a wait (the test and the set are inline, with no procedure call in between)
  (define (%lua-take!)
    (let ((me ##sys#current-thread))
      (cond
        ((not %lua-owner) (set! %lua-owner me) (set! %lua-holds 1))
        ((eq? %lua-owner me) (set! %lua-holds (fx+ %lua-holds 1)))
        (else (%api-error 'lua "internal: another Scheme thread took the Lua lock after a state pointer was read" (list %lua-owner) AUX_ETHREAD)))))
  ;; waits for the lock, then takes it (for code that reads its pointers once inside)
  (define (%lua-enter!) (%lua-wait!) (%lua-take!))
  ;; the drain point's wait: until no deferred close is pending (a close may run __gc callbacks,
  ;; which may switch threads) and the lock is free for this thread whenever the binding will take
  ;; the safe path (u holds callables, or force: the binding adds one before its stub runs)
  (define (%settle! u force)
    (let loop ()
      (unless (null? %pending-closes) (%close-pending!))
      (when (and %lua-owner (not (eq? %lua-owner ##sys#current-thread))
                 (or force (fx> (luaU-callables u) 0)))
        (%lua-wait!)
        (loop))))
  (define (%lua-leave!)
    (when (fx> %lua-holds 0)
      (set! %lua-holds (fx- %lua-holds 1))
      (when (fx= 0 %lua-holds) (set! %lua-owner #f))))

  ;; (%run u (fast safe) p arg ...): the raw result of the op, choosing the path by the universe:
  ;; fast while u holds no live Scheme callable, else safe (the thread lock taken, active counted,
  ;; the callback universe set, handles drained).
  (define-syntax %run
    (syntax-rules ()
      ((_ u (fast safe) arg ...)
       (if (fx= 0 (%callables u))
         (fast #f arg ...)
         (begin
           (%lua-take!)                            ; no wait: see %settle!
           (let ((saved %cur-u))
             (luaU-active-set! u (fx+ (luaU-active u) 1))
             (set! %cur-u u)
             (let ((r (safe #t arg ...)))
               (set! %cur-u saved)
               (luaU-active-set! u (fx- (luaU-active u) 1))
               (%lua-leave!)
               (set! %busy-ctx (##sys#slot u 2))     ; nested bindings cleared it: the caller reads on
               (%drain-handles! u)
               r)))))))

  ;; the lua_State pointer of a valid luaR, else (exn lua api).  The hottest path of every binding:
  ;; written with CHICKEN's inlined structure primitives (luaR slot 1 = state, 2 = u, 3 = kind,
  ;; 4 = ref; luaU slot 1 = state, 2 = ctx).  A 'thread luaR is checked against its anchor (I5):
  ;; Lua code, or the bound API on LUA_REGISTRYINDEX, may have removed it, and the thread may be gone.
  ;; Nothing that may run Lua code may come between %L and the use of the pointer.
  (define %luaR-tag (##sys#slot (make-luaR #f #f #f #f #f #f #f) 0))   ; define-record's (qualified) tag
  ;; unchecked reads for the hot path, valid only on records %L has just validated
  (define-inline (%u-of l) (##sys#slot l 2))              ; luaR-u
  (define-inline (%callables u) (##sys#slot u 3))         ; luaU-callables
  (define-inline (%unrefs u) (##sys#slot u 5))            ; luaU-unrefs
  (define %anchor-check (foreign-lambda int "aux_anchor_check" (c-pointer "lua_State") int (c-pointer "lua_State")))
  (define (%L-thread who x p)
    (let ((id (##sys#slot x 4)))
      (if (not (fixnum? id))                                   ; #f: built by hand
        p
        (let ((r (%anchor-check (##sys#slot (##sys#slot x 2) 1) id p)))
          (cond ((fx= r 1) p)
                ((fx= r 0) (%api-error who "the thread of this luaR is no longer anchored (it may have been collected)" (list x)))
                (else (%api-fail who r (list x))))))))
  ;; inside a callback, the floor of p's current frame when it is a normal thread's (see aux_touch)
  (define %touch (foreign-lambda int "aux_touch" (c-pointer "lua_State")))
  (define (%touch! who x p)
    (let ((r (%touch p)))
      (if (fx= r 0) p (%api-error who "too many frames of waiting threads in use in this callback" (list x) r))))
  (define-inline (%L who x)
    (unless (null? %pending-closes) (%close-pending!))
    (let ((p (and (##sys#structure? x %luaR-tag) (##sys#slot x 1))))
      (if (and p (##sys#slot (##sys#slot x 2) 1))
        (begin
          (set! %busy-ctx (##sys#slot (##sys#slot x 2) 2))
          (let ((p (if (eq? 'thread (##sys#slot x 3)) (%L-thread who x p) p)))
            (if (fx= %depth 0) p (%touch! who x p))))
        (%api-error who "closed, invalidated or not a lua state" (list x)))))
  ;; (%done e): e's value, once the binding no longer uses the pointer it read (see %busy-ctx)
  (define-syntax %done (syntax-rules () ((_ e) (let ((r e)) (set! %busy-ctx #f) r))))
  ;; validation only: x is a valid luaR (its pointer is not used)
  (define-inline (%V who x) (%L who x) (set! %busy-ctx #f))
  ;; the luaU of a valid luaR (the pointer is not read: %prepare! may run Lua code before %L)
  (define-inline (%U who x)
    (unless (null? %pending-closes) (%close-pending!))
    (if (and (##sys#structure? x %luaR-tag) (##sys#slot x 1) (##sys#slot (##sys#slot x 2) 1))
      (##sys#slot x 2)
      (%api-error who "closed, invalidated or not a lua state" (list x))))

  ;; test hook: what a binding does before its stub runs (the pointer is read, the mark is set)
  (define (%in-window L) (%L '%in-window L))

  ;; the drain point of a binding that runs a stub with %run, before it reads any pointer:
  ;; released handles, luaV refs and thread anchors of u (the last two run on the main thread),
  ;; then the wait for the thread lock (%settle!; force: the binding makes u hold a callable
  ;; before its stub runs).  After it the binding reads its pointers (%L re-validates its luaRs:
  ;; the wait may have closed or invalidated them), and nothing may switch threads until the
  ;; stub runs; what it allocates (handles, buffers) comes after %L, so a refusal leaks nothing.
  (define-inline (%prepare!/force u force)
    (unless (fx= 0 (foreign-value "((int)aux_nrel_total)" int)) (%drain-handles! u))
    (unless (and (null? (%unrefs u)) (null? (luaU-unanchors u))) (%drain-unrefs! u))
    (%settle! u force))
  (define-inline (%prepare! u) (%prepare!/force u #f))

  ;; (%P who L (fast safe) arg ...): a protected binding.  The value of the op (> -1000) or a
  ;; raise; a pending escape is resumed whatever the op answered.  The args must be variables
  ;; (they are repeated in the error path).
  (define-syntax %P
    (syntax-rules ()
      ((_ who L (fast safe) arg ...)
       (let* ((l L) (u (%U who l)))
         (%prepare! u)
         (let* ((p (%L who l)) (r (%run u (fast safe) p arg ...)))
           (if (and (fx> r -1000) (not %pending-escape))
             (begin (set! %busy-ctx #f) r)
             (%finish who u p r (list arg ...))))))))

  ;; (%D who expr arg ...): a direct (D) binding's result, or an (exn lua api) for a code <= -2000
  (define-syntax %D
    (syntax-rules ()
      ((_ who expr arg ...) (let ((r expr)) (set! %busy-ctx #f) (if (fx> r -2000) r (%api-fail who r (list arg ...)))))))

  ;; after any stub that may have run Lua code
  (define (%finish who u p r args)
    (cond
      ((and %pending-escape (fx= (car %pending-escape) (fx+ %depth 1)))
       (%drop-error! u p r)
       (let ((k (cdr %pending-escape))) (set! %pending-escape #f) (set! %busy-ctx #f) (k #f)))
      ((fx> r -1000) r)
      ((fx> r -1100) (%raise-top who u p (fx- -1000 r) args))
      ((fx> r -1300) (if (fx> r -1200) (%api-fail who AUX_EARG args) (%raise-top who u (luaU-state u) (fx- -1200 r) args)))
      (else (%api-fail who r args))))

  ;; an escape is resumed: drop the error value the op left, if any
  (define (%drop-error! u p r)
    (cond ((and (fx<= r -1000) (fx> r -1100)) (%pop! p 1))
          ((and (fx<= r -1200) (fx> r -1300)) (%pop! (luaU-state u) 1))
          ((and (fx= LUA_OK (%status p)) (%top-escape? p)) (%pop! p 1))))

  ;; resume a pending escape that belongs to the current depth (bindings that did not use %finish)
  (define (%escape-epilogue!)
    (when (and %pending-escape (fx= (car %pending-escape) (fx+ %depth 1)))
      (let ((k (cdr %pending-escape))) (set! %pending-escape #f) (set! %busy-ctx #f) (k #f))))

  ;; ==========================================================================================
  ;; CORE — Lua values as Scheme values (error values, lua-toref)
  ;; ==========================================================================================

  (define-lua-stubs (%tolstring-op/f %tolstring-op/s) ((int idx))
    "aux_op o = {0}; int refs[1]; o.fn = op_tolstring; refs[0] = idx;
     if (!aux_ix(L, idx, AUX_IX_READ, 0)) C_return(AUX_EINDEX);
     C_return(aux_run(safe, L, &o, 1, refs, 0, 1));")
  (define-lua-stubs (%ref-op/f %ref-op/s) ((int idx))
    "aux_op o = {0}; int refs[1]; o.fn = op_ref; refs[0] = idx;
     if (!aux_ix(L, idx, AUX_IX_READ, 0)) C_return(AUX_EINDEX);
     C_return(aux_run(safe, L, &o, 1, refs, 0, 0));")
  (define-lua-stubs (%unref-op/f %unref-op/s) ((int ref))
    "aux_op o = {0}; o.fn = op_unref; o.i0 = ref; C_return(aux_run(safe, L, &o, 0, NULL, 0, 0));")
  (define-lua-stubs (%unanchor-op/f %unanchor-op/s) ((int id))
    "aux_op o = {0}; o.fn = op_unanchor; o.i0 = id; C_return(aux_run(safe, L, &o, 0, NULL, 0, 0));")

  (define (%luaV-finalize v)
    (let ((u (luaV-u v)))
      (when (and (luaU-state u) (fx>= (luaV-ref v) 0))
        (luaU-unrefs-set! u (cons (luaV-ref v) (luaU-unrefs u))))))

  ;; the pending luaL_unref of dropped luaVs and the anchors of dropped thread luaRs, on the main
  ;; thread (always LUA_OK); an anchor id is reused once its slot is cleared
  (define (%drain-unrefs! u)
    (%settle! u #f)                                    ; m is read after the wait
    (let ((m (luaU-state u)) (refs (luaU-unrefs u)) (ids (luaU-unanchors u)))
      (luaU-unrefs-set! u '())
      (luaU-unanchors-set! u '())
      (unless m (set! refs '()) (set! ids '()))          ; closed meanwhile: nothing left to clear
      ;; plain loops (no library procedure, where a timer interrupt could switch threads)
      (let loop ((refs refs))
        (unless (null? refs)
          (let ((r (%run u (%unref-op/f %unref-op/s) m (car refs))))
            (when (and (fx<= r -1000) (fx> r -1100)) (%pop! m 1)))
          (loop (cdr refs))))
      (let loop ((ids ids))
        (unless (null? ids)
          (let ((r (%run u (%unanchor-op/f %unanchor-op/s) m (car ids))))
            (when (and (fx<= r -1000) (fx> r -1100)) (%pop! m 1))
            (luaU-afree-set! u (cons (car ids) (luaU-afree u))))
          (loop (cdr ids))))))

  ;; thread anchors (I5): an id for a new 'thread luaR, and the luaR itself once the op anchored it
  (define (%anchor-alloc! u)
    (if (pair? (luaU-afree u))
      (let ((id (car (luaU-afree u)))) (luaU-afree-set! u (cdr (luaU-afree u))) id)
      (let ((id (luaU-anext u))) (luaU-anext-set! u (fx+ id 1)) id)))
  (define (%thread-finalize t)             ; the anchor is cleared at the next drain point
    (let ((u (luaR-u t)) (id (luaR-ref t)))
      (when (and (luaU-state u) (fixnum? id) (fx> id 0))
        (luaU-unanchors-set! u (cons id (luaU-unanchors u))))))
  (define %anchor-thread (foreign-lambda (c-pointer "lua_State") "aux_anchor_thread" (c-pointer "lua_State") int))
  (define (%make-thread-luaR who u id)
    (set! %busy-ctx (luaU-ctx u))              ; the pointer is read below (I7)
    (let ((ptr (and (luaU-state u) (%anchor-thread (luaU-state u) id))))
      (set! %busy-ctx #f)
      (unless ptr (%api-error who "the new thread is not anchored" (list id)))
      (set-finalizer! (make-luaR ptr u 'thread id -1 0 (locative->object (luaU-mainw u))) %thread-finalize)))

  ;; a luaV for the value at idx of p (an OK thread), or #f when the registry cannot take it
  (define (%make-ref u p idx)
    (let ((r (%run u (%ref-op/f %ref-op/s) p idx)))
      (cond ((fx> r -1000) (set-finalizer! (make-luaV u r) %luaV-finalize))
            (else (when (and (fx<= r -1000) (fx> r -1100)) (%pop! p 1)) #f))))

  (define %toboolean-raw (foreign-lambda bool "lua_toboolean" (c-pointer "lua_State") int))
  (define %isinteger-raw (foreign-lambda bool "lua_isinteger" (c-pointer "lua_State") int))
  (define %tointeger-raw (foreign-lambda* integer64 (((c-pointer "lua_State") L) (int i)) "C_return(lua_tointegerx(L, i, NULL));"))
  (define %tonumber-raw (foreign-lambda* double (((c-pointer "lua_State") L) (int i)) "C_return(lua_tonumberx(L, i, NULL));"))
  (define %touserdata-raw (foreign-lambda c-pointer "lua_touserdata" (c-pointer "lua_State") int))

  ;; (values ok? v): the Scheme value of the Lua value at idx (an OK thread p)
  (define (%value-at u p idx)
    (let ((t (%type-raw p idx)))
      (cond
        ((fx= t LUA_TNIL) (values #t (void)))
        ((fx= t LUA_TBOOLEAN) (values #t (%toboolean-raw p idx)))
        ((fx= t LUA_TNUMBER) (values #t (if (%isinteger-raw p idx) (%tointeger-raw p idx) (%tonumber-raw p idx))))
        ((fx= t LUA_TSTRING) (values #t (%bytes->string/maybe (%bytes-at 'lua p idx))))
        ((fx= t LUA_TLIGHTUSERDATA) (values #t (%touserdata-raw p idx)))
        (else (let ((v (%make-ref u p idx))) (values (and v #t) v))))))

  ;; the message of the error value on top of p
  (define (%error-message u p)
    (let ((t (%type-raw p -1)))
      (if (or (fx= t LUA_TSTRING) (fx= t LUA_TNUMBER))
        (%bytes->string/lossy (%bytes-at 'lua p -1))
        (let ((r (%run u (%tolstring-op/f %tolstring-op/s) p -1)))
          (cond
            ((fx> r -1000) (let ((s (%bytes-at 'lua p -1))) (%pop! p 1) (%bytes->string/lossy s)))
            (else (when (and (fx<= r -1000) (fx> r -1100)) (%pop! p 1))
                  (string-append "(error object is a " (%typename p t) " value)")))))))

  ;; the Lua error value on top of p becomes a Scheme raise (it is popped); never returns
  (define (%raise-top who u p st args)
    (if (%top-escape? p)
      (begin (%pop! p 1) (set! %busy-ctx #f) (abort (%lua-condition who "aux.lua: stale Scheme escape" args st)))
      (let ((e (%handle-entry u (%handle-at p -1) '(cond object))))
        (if e
          (begin (%pop! p 1) (set! %busy-ctx #f) (abort (car e)))       ; the original Scheme object, identity kept
          (let ((msg (%error-message u p)))                  ; these may run Lua (__tostring, a GC step):
            (let-values (((ok? v) (%value-at u p -1)))
              (%pop! p 1)
              (set! %busy-ctx #f)
              (%escape-epilogue!)                            ; an escape they stopped wins over the raise
              (abort (if ok?
                       (%lua-condition who msg args st 'value v)
                       (%lua-condition who msg args st)))))))))

  ;; ==========================================================================================
  ;; CORE — handles in Lua
  ;; ==========================================================================================

  (define-lua-stubs (%newhandle/f %newhandle/s) ((int kind) (int n) (int h) (scheme-object msg))
    "aux_op o = {0}; o.fn = op_newhandle; o.i0 = kind; o.i1 = n; o.n0 = h;
     if (C_truep(msg)) o.s0 = aux_bytes(msg, &o.l0);
     if (n < 0 || n > 254) C_return(AUX_ERANGE);
     if (kind == AUX_K_PROC && n > 0) C_return(aux_run(safe, L, &o, 0, NULL, n, 1));
     C_return(aux_run_h(safe, L, &o, 1));")

  ;; the raw result of pushing a handle userdata (a C closure for 'proc); h is freed unless Lua owns it
  (define (%newhandle-raw u p obj kind ckind n msg)
    (let* ((h (%handle-new! obj kind u))
           (r (%run u (%newhandle/f %newhandle/s) p ckind n h msg)))
      (unless (fx> r -1000) (%handle-free! u h))
      r))

  (define (%push-handle who L obj kind ckind n msg)
    (let ((u (%U who L)))
      (%int! who n)
      (%prepare!/force u (%callable-kind? kind))
      (let* ((p (%L who L))
             (h (%handle-new! obj kind u))                 ; counted before Lua can reach it (I3)
             (r (%run u (%newhandle/f %newhandle/s) p ckind n h msg)))
        (unless (fx> r -1000) (%handle-free! u h))
        (if (and (fx> r -1000) (not %pending-escape)) (begin (set! %busy-ctx #f) (void)) (begin (%finish who u p r (list obj n)) (void))))))

  (define %pushcclosure-light (foreign-lambda* int (((c-pointer "lua_State") L) (c-pointer f))
                                "if (!lua_checkstack(L, 1)) C_return(AUX_ESTACK); lua_pushcclosure(L, (lua_CFunction)f, 0); C_return(0);"))
  (define-lua-stubs (%pushcclosure-c/f %pushcclosure-c/s) ((c-pointer f) (int n))
    "aux_op o = {0}; o.fn = op_pushcclosure_c; o.p0 = f; o.i1 = n;
     if (n < 0 || n > 255) C_return(AUX_ERANGE);
     C_return(aux_run(safe, L, &o, 0, NULL, n, 1));")

  ;; a c-pointer Scheme may hand to Lua as a lua_CFunction: a luaopen-* opener, or a function
  ;; lua-tocfunction has read from a light C function (see aux_cf_vouched); any other pointer (a
  ;; table's, a userdata's, a C closure's function without its upvalues) is (exn lua api)
  (define %cf-vouched? (foreign-lambda bool "aux_cf_vouched" c-pointer))
  (define (%cfunction-pointer? f) (and (pointer? f) (%cf-vouched? f)))
  (define (%cfunction! who f args)
    (unless (%cf-vouched? f)
      (%api-error who "not a C function pointer from luaopen-* or lua-tocfunction of a light C function" args)))

  ;; ((lua-pushcclosure f n) L): f a Scheme procedure (λ (C) answer) or a c-pointer to a lua_CFunction
  ;; (a luaopen-* opener, or what lua-tocfunction read from a light C function: see
  ;; %cfunction-pointer?); the n values on top become its upvalues (read them at
  ;; (lua-upvalueindex i)).  n is at most 255, as in C, for a C function, and at most 254 for a
  ;; Scheme procedure, whose closure keeps its handle as its first upvalue (n = 255 is refused with
  ;; (exn lua api), code AUX_ERANGE).  answer: a result count, a fixnum in [0, INT_MAX] and at most the values
  ;; on C's stack, or ((lua-yield n) C) / ((lua-yieldk n k) C); anything else is the Lua error "must
  ;; answer a result count".
  (define ((lua-pushcclosure f n) L)
    (cond
      ((procedure? f) (%push-handle 'lua-pushcclosure L f 'proc AUX_K_PROC n #f))
      ((pointer? f)
       (%cfunction! 'lua-pushcclosure f (list f n))
       (if (eq? n 0)
         (%D 'lua-pushcclosure (%pushcclosure-light (%L 'lua-pushcclosure L) f) f n)
         (begin (%int! 'lua-pushcclosure n) (%P 'lua-pushcclosure L (%pushcclosure-c/f %pushcclosure-c/s) f n)))
       (void))
      (else (%api-error 'lua-pushcclosure "not a procedure or a C function pointer" (list f n)))))
  (define ((lua-pushcfunction f) L) ((lua-pushcclosure f 0) L))

  ;; the pseudo-index of user upvalue i of the running Scheme closure (upvalue 1 is the handle), for
  ;; 1 <= i <= 255: lua_upvalueindex(i + 1), up to C's 256 (manual 4.2: an acceptable index even when
  ;; the closure has fewer upvalues, where lua-type answers LUA_TNONE and lua-isnone #t)
  (define (lua-upvalueindex i)
    (unless (and (fixnum? i) (fx>= i 1) (fx<= i 255)) (%api-error 'lua-upvalueindex "upvalue index out of range" (list i) AUX_ERANGE))
    (fx- LUA_REGISTRYINDEX (fx+ i 1)))

  ;; ((lua-pushobject obj) L): any Scheme object, as a userdata whose identity Lua keeps
  (define ((lua-pushobject obj) L) (%push-handle 'lua-pushobject L obj 'object AUX_K_OBJECT 0 #f))

  (define %toobject-h (foreign-lambda* int (((c-pointer "lua_State") L) (int idx))
                        "aux_handle *u; int q = aux_qix(L, idx);
                         if (q < 0) C_return(AUX_EINDEX); if (q == 0) C_return(-1);
                         u = aux_tohandle(L, idx);
                         C_return(u && (u->kind == AUX_K_OBJECT || u->kind == AUX_K_COND) ? u->handle : -1);"))
  ;; ((lua-toobject idx) L): the Scheme object of an object (or condition) userdata, else #f
  (define ((lua-toobject idx) L)
    (%int! 'lua-toobject idx)
    (let* ((h (%D 'lua-toobject (%toobject-h (%L 'lua-toobject L) idx) idx))
           (e (%handle-entry (luaR-u L) h '(object cond))))
      (and e (car e))))

  (define %toprocedure-h (foreign-lambda* int (((c-pointer "lua_State") L) (int idx))
                           "aux_handle *u; int h = -1, q = aux_qix(L, idx);
                            if (q < 0) C_return(AUX_EINDEX); if (q == 0) C_return(-1);
                            if (lua_tocfunction(L, idx) != aux_cfunction) C_return(-1);
                            if (!lua_checkstack(L, 1)) C_return(AUX_ESTACK);
                            lua_getupvalue(L, idx, 1);
                            u = aux_tohandle(L, -1); if (u && u->kind == AUX_K_PROC) h = u->handle;
                            lua_pop(L, 1); C_return(h);"))
  ;; ((lua-toprocedure idx) L): the Scheme procedure of a closure made by lua-pushcclosure, else #f
  (define ((lua-toprocedure idx) L)
    (%int! 'lua-toprocedure idx)
    (let* ((h (%D 'lua-toprocedure (%toprocedure-h (%L 'lua-toprocedure L) idx) idx))
           (e (%handle-entry (luaR-u L) h '(proc))))
      (and e (car e))))

  ;; ((lua-toref idx) L): a luaV keeping the value at idx alive in the registry (P); the value stays
  ;; at idx.  A Lua error when the registry's free list is corrupted (see luaL-ref).
  (define ((lua-toref idx) L)
    (%int! 'lua-toref idx)
    (let ((ref (%P 'lua-toref L (%ref-op/f %ref-op/s) idx)))
      (set-finalizer! (make-luaV (luaR-u L) ref) %luaV-finalize)))

  (define %rawgeti-reg (foreign-lambda* int (((c-pointer "lua_State") L) (int ref))
                         "if (!lua_checkstack(L, 1)) C_return(AUX_ESTACK); C_return(lua_rawgeti(L, LUA_REGISTRYINDEX, ref));"))
  ;; ((lua-pushref v) L): push the value of a luaV of the same universe; answers its type
  (define ((lua-pushref v) L)
    (let ((p (%L 'lua-pushref L)))
      (unless (and (luaV? v) (eq? (luaV-u v) (luaR-u L)))
        (%api-error 'lua-pushref "not a reference of this universe" (list v)))
      (%D 'lua-pushref (%rawgeti-reg p (luaV-ref v)) v)))

  ;; ==========================================================================================
  ;; CORE — Scheme procedures called by Lua
  ;; ==========================================================================================

  (define %raise-tag (list 'raise))
  (define %escape-tag (list 'escape))

  ;; what a Scheme lua_CFunction answers to yield: ((lua-yield n) C) / ((lua-yieldk n k) C)
  (define-record lua-yield-request n k)

  ;; the text of the COND handle Lua will get for a raised obj, or #f when obj travels otherwise (a
  ;; request, or a 'lua 'value %push-value-raw may push).  It runs user code -- `write' of the
  ;; 'exn 'arguments and `->string' reach record printers -- so %guard computes it INSIDE the
  ;; callback's extent: an escape from a printer is then an ordinary escape of the callback (the
  ;; after thunk records it), never a continuation leaving aux_lua_dispatch through Lua's C frames,
  ;; and a raise from it is stopped here, its text a fallback.
  (define (%raise-text e)
    (and (not (%request-of e))
         (let ((v (if (lua-condition? e) (get-condition-property e 'lua 'value %novalue) %novalue)))
           (or (eq? v %novalue) (luaV? v)
               (not (or (eq? v (void)) (boolean? v) (%int64? v) (real? v) (string? v) (bytevector? v) (pointer? v)))))
         (handle-exceptions _ "<scheme condition>" (%condition-message e))))

  ;; runs (thunk) stopping every exit at this boundary: its value, (raise-tag obj . text) (text: see
  ;; %raise-text) or (escape-tag . rest).
  ;; The body ends at most once, in the Scheme thread that entered it (its owner): its end returns
  ;; to C through C_callback, which can happen once only, and into the Lua call of that thread.  A
  ;; continuation captured inside the body and invoked by another srfi-18 thread would run the body
  ;; there too, and the first run to end would return the other thread's Lua call.  So:
  ;; - a rewind into the body is refused (an error raised by the rewind, before the body is entered
  ;;   again) once it has left (returned, raised or escaped), and, while it runs, in any other thread;
  ;; - a thread created inside the body inherits its dynamic extent, so invoking such a continuation
  ;;   there rewinds nothing: the body then runs in that thread, but its end (a value, a raise or an
  ;;   escape) never leaves through this boundary: a value or a raise becomes an error raised in that
  ;;   thread, and an escape leaves as an ordinary Scheme escape of that thread.  (Its bindings wait
  ;;   for the owner's Lua lock, I12, and by then the view is invalidated.)
  ;; Should the body still end twice in its owner, the process stops (%callback-fatal) rather than
  ;; return to C a second time.
  (define %foreign-run-message "a Lua callback was resumed from another Scheme thread while it runs")
  (define (%guard thunk)
    (call/cc
      (λ (stop)
        (let* ((left #f) (owner ##sys#current-thread)
               (foreign? (λ () (not (eq? ##sys#current-thread owner)))))
          (dynamic-wind
            (λ () (cond (left (error 'lua-callback "a Lua callback was resumed after it had returned"))
                        ((foreign?) (error 'lua-callback %foreign-run-message))))
            (λ () (let ((r (with-exception-handler
                             (λ (e)                                ; the text first, while an escape is still one
                               (let ((text (%raise-text e)))
                                 (when (foreign?) (error 'lua-callback %foreign-run-message))
                                 (when left (%callback-fatal))
                                 (set! left #t)                    ; then `left': no escape capture
                                 (stop (cons %raise-tag (cons e text)))))
                             thunk)))
                    (when (foreign?) (error 'lua-callback %foreign-run-message))
                    (when left (%callback-fatal))
                    (set! left #t)
                    r))
            (λ () (unless (or left (foreign?))
                    (set! left #t)
                    (call/cc (λ (rest) (stop (cons %escape-tag rest)))))))))))

;; unreachable (see %guard): a callback body that ended a second time stops the process
  (define %callback-fatal
    (foreign-lambda* void ()
      "fprintf(stderr, \"aux.lua: FATAL (a Scheme callback ended twice: its C frame is gone)\\n\"); fflush(stderr); abort();"))

  ;; a new callback while an escape is pending at this depth or above is refused
  (define (%escape-refused?)
    (and %pending-escape (fx>= (fx+ %depth 1) (car %pending-escape))))

  ;; (%safely default expr ...): expr's value, or default when it raises.  Only around code that
  ;; runs BEFORE or AFTER the guard, never around it: a handler installed around the guard would be
  ;; re-installed by the rewind of a resumed dead callback, and would then return into its dead C frame.
  (define-syntax %safely
    (syntax-rules () ((_ default expr ...) (handle-exceptions _ default expr ...))))

  ;; runs (f view) for a callback on Lp under the guard: the raw outcome (f's value,
  ;; (%raise-tag obj . text) or (%escape-tag . rest)); the view is invalidated and the depth restored.
  ;; kind: 'view ('hook for hooks, whose top cannot be lowered below floor).
  (define (%callback-outcome Lp u nup kind floor f)
    (let ((view (make-luaR Lp u kind #f nup floor #f)))
      (set! %depth (fx+ %depth 1))
      (let ((outcome (%guard (λ () (f view)))))
        (luaR-state-set! view #f)
        (set! %depth (fx- %depth 1))
        outcome)))

  ;; runs (f view) for a lua_CFunction-like callback on Lp, answering the C code of its outcome.
  ;; The outcome is computed OUTSIDE %safely (see %safely).
  (define (%run-callback Lp u nup kind f)
    (let ((outcome (%callback-outcome Lp u nup kind 0 f)))
      (%safely AUX_INTERNAL (%translate Lp u outcome))))

  ;; the text of a request: a string or a bytevector, else #f (nothing is read)
  (define %req-set! (foreign-lambda* bool ((scheme-object s))
                      "size_t n; const char *b;
                       if (C_immediatep(s) || (C_header_bits(s) != C_STRING_TYPE && C_header_bits(s) != C_BYTEVECTOR_TYPE)) C_return(0);
                       b = aux_bytes(s, &n); C_return(aux_req_set(b, n));"))
  (define %req-arg-set! (foreign-lambda* void ((int a)) "aux_req_arg = a;"))
  (define %req-n-set! (foreign-lambda* void ((int n)) "aux_req_n = n;"))
  (define %req-k-set! (foreign-lambda* void ((int h)) "aux_req_k = h;"))

  ;; the continuation k of a lua-yieldk on Lp: its handle, kept off the stack (see aux_kfunction), or
  ;; the op's failure code (<= -1000); h is freed unless Lua owns it
  (define-lua-stubs (%kanchor/f %kanchor/s) ((int h))
    "aux_op o = {0}; o.fn = op_kanchor; o.n0 = h; C_return(aux_run(safe, L, &o, 0, NULL, 0, 0));")
  (define (%kanchor-raw u p k)
    (let* ((h (%handle-new! k 'kfunc u))
           (r (%run u (%kanchor/f %kanchor/s) p h)))
      (if (fx> r -1000) h (begin (%handle-free! u h) r))))

  ;; an escape stopped at the boundary of the callback that just returned (depth already restored)
  (define (%record-escape! rest) (set! %pending-escape (cons (fx+ %depth 1) rest)))

  ;; the largest result count: the answer crosses aux_lua_dispatch's C int return, which would
  ;; truncate a larger fixnum into one of the AUX_* codes (a raise with nothing to raise, a stale
  ;; escape, a yield with a stale count...)
  (define %int-max (foreign-value "INT_MAX" int))

  ;; the C answer for an outcome; never raises (every push inspects its result code)
  (define (%translate Lp u outcome)
    (cond
      ((fixnum? outcome) (if (and (fx>= outcome 0) (fx<= outcome %int-max)) outcome AUX_BADANSWER))
      ((lua-yield-request? outcome) (%translate-yield Lp u outcome))
      ((not (pair? outcome)) AUX_BADANSWER)
      ((eq? (car outcome) %escape-tag) (%record-escape! (cdr outcome)) AUX_ESCAPE)
      ((eq? (car outcome) %raise-tag) (%translate-raise Lp u (cadr outcome) (cddr outcome)))
      (else AUX_BADANSWER)))

  (define (%translate-yield Lp u req)
    (let ((n (lua-yield-request-n req)) (k (lua-yield-request-k req)))
      (%req-n-set! n)
      (if (not k)
        AUX_YIELD
        (let ((r (%kanchor-raw u Lp k)))
          (cond ((fx>= r 0) (%req-k-set! r) AUX_YIELDK)
                ((fx> r -1100) AUX_RAISE)          ; the failed op's own error value is on top
                (else AUX_INTERNAL))))))

  ;; a request the bindings made (luaL-error, luaL-argerror, luaL-typeerror, lua-yield...), or a
  ;; condition of the same shape made by user code: its text must be a string or a bytevector, and
  ;; the arg of an argerror / typeerror a C int; any other is an ordinary Scheme condition
  (define (%request-of e)
    (let ((req (and (lua-condition? e) (get-condition-property e 'lua 'request #f))))
      (and (memq req '(argerror typeerror lerror runerror))
           (let ((text (get-condition-property e 'lua 'text "")))
             (or (string? text) (bytevector? text)))
           (or (memq req '(lerror runerror))
               (let ((arg (get-condition-property e 'lua 'arg #f)))
                 (and (fixnum? arg) (fx<= arg 2147483647) (fx>= arg -2147483648))))
           req)))

  ;; text: the COND handle's text, computed by %guard (#f for a request); runs no user code
  (define (%translate-raise Lp u e text)
    (let ((req (%request-of e)))
      (if req
        (if (%req-set! (%->bytes (get-condition-property e 'lua 'text "")))
          (begin (%req-arg-set! (if (memq req '(argerror typeerror)) (get-condition-property e 'lua 'arg 0) 0))
                 (case req ((argerror) AUX_ARGERROR) ((typeerror) AUX_TYPEERROR) ((runerror) AUX_RUNERROR) (else AUX_LERROR)))
          AUX_INTERNAL)
        (let ((v (if (lua-condition? e) (get-condition-property e 'lua 'value %novalue) %novalue)))
          (or (and (not (eq? v %novalue)) (%push-value-raw u Lp v))
              (let ((r (%newhandle-raw u Lp e 'cond AUX_K_COND 0 (or text "<scheme condition>"))))
                (if (fx> r -1100) AUX_RAISE AUX_INTERNAL)))))))

  (define %pushnil-raw (foreign-lambda* int (((c-pointer "lua_State") L)) "if (!lua_checkstack(L, 1)) C_return(AUX_ESTACK); lua_pushnil(L); C_return(0);"))
  (define %pushboolean-raw (foreign-lambda* int (((c-pointer "lua_State") L) (bool b)) "if (!lua_checkstack(L, 1)) C_return(AUX_ESTACK); lua_pushboolean(L, b); C_return(0);"))
  (define %pushinteger-raw (foreign-lambda* int (((c-pointer "lua_State") L) (integer64 n)) "if (!lua_checkstack(L, 1)) C_return(AUX_ESTACK); lua_pushinteger(L, n); C_return(0);"))
  (define %pushnumber-raw (foreign-lambda* int (((c-pointer "lua_State") L) (double x)) "if (!lua_checkstack(L, 1)) C_return(AUX_ESTACK); lua_pushnumber(L, x); C_return(0);"))
  (define %pushlightuserdata-raw (foreign-lambda* int (((c-pointer "lua_State") L) (c-pointer x)) "if (!lua_checkstack(L, 1)) C_return(AUX_ESTACK); lua_pushlightuserdata(L, x); C_return(0);"))
  (define-lua-stubs (%pushlstring/f %pushlstring/s) ((scheme-object s))
    "aux_op o = {0}; o.fn = op_pushlstring; o.s0 = aux_bytes(s, &o.l0); C_return(aux_run_h(safe, L, &o, 1));")

  (define (%int64? x) (and (exact-integer? x) (<= -9223372036854775808 x 9223372036854775807)))

  ;; push the Scheme value v back (inside a callback): AUX_RAISE when something is on top to raise,
  ;; AUX_INTERNAL when nothing could be pushed, #f when v has no Lua counterpart here
  (define (%push-value-raw u Lp v)
    (let ((r (cond
               ((eq? v (void)) (%pushnil-raw Lp))
               ((boolean? v) (%pushboolean-raw Lp v))
               ((%int64? v) (%pushinteger-raw Lp v))
               ((real? v) (%pushnumber-raw Lp (exact->inexact v)))
               ((or (string? v) (bytevector? v)) (%run u (%pushlstring/f %pushlstring/s) Lp v))
               ((pointer? v) (%pushlightuserdata-raw Lp v))
               ((and (luaV? v) (eq? (luaV-u v) u)) (%rawgeti-reg Lp (luaV-ref v)))
               (else #f))))
      (cond ((not r) #f) ((fx> r -1100) AUX_RAISE) (else AUX_INTERNAL))))

  ;; the universe of a callback on Lp: the one of the innermost safe stub (%cur-u), checked
  (define %same-ctx? (foreign-lambda* bool ((c-pointer c) ((c-pointer "lua_State") L)) "C_return(c == (void *)AUX_CTX(L));"))
  (define (%callback-universe Lp)
    (let ((u %cur-u)) (and u (luaU-ctx u) (%same-ctx? (luaU-ctx u) Lp) u)))

  (define (%dispatch Lp h nup)
    (let ((e (%safely #f (and (not (%escape-refused?)) (let ((u (%callback-universe Lp))) (and u (%handle-entry u h '(proc))))))))
      (cond
        (e (%run-callback Lp (cddr e) nup 'view (car e)))
        ((%escape-refused?) AUX_ESCAPE)
        (else AUX_STALE))))

  (define (%kdispatch Lp h nup status)
    (let ((e (%safely #f (and (not (%escape-refused?)) (let ((u (%callback-universe Lp))) (and u (%handle-entry u h '(kfunc))))))))
      (cond
        (e (%run-callback Lp (cddr e) nup 'view (λ (C) ((car e) status C))))
        ((%escape-refused?) AUX_ESCAPE)
        (else AUX_STALE))))

  (define-external (aux_lua_dispatch ((c-pointer "lua_State") Lp) (int h) (int nup)) int
    (%dispatch Lp h nup))
  (define-external (aux_lua_kdispatch ((c-pointer "lua_State") Lp) (int h) (int nup) (int status)) int
    (%kdispatch Lp h nup status))

  ;; ==========================================================================================
  ;; CORE — universe lifecycle
  ;; ==========================================================================================

  (define %luaL-newstate (foreign-lambda (c-pointer "lua_State") "luaL_newstate"))
  (define %setup (foreign-lambda c-pointer "aux_setup" (c-pointer "lua_State")))
  (define %close/f (foreign-lambda void "aux_close" bool (c-pointer "lua_State")))
  (define %close/s (foreign-safe-lambda void "aux_close" bool (c-pointer "lua_State")))

  (define (%close-universe! u)
    (let* ((safe? (fx> (luaU-callables u) 0))
           (p (begin (when safe? (%lua-enter!)) (luaU-state u))))   ; I12; it may raise: before anything is changed; the pointer is read after the wait
      (luaU-active-set! u (fx+ (luaU-active u) 1))       ; __gc callbacks may run: no nested close
      (if safe?
        (let ((saved %cur-u)) (set! %cur-u u) (%close/s #t p) (set! %cur-u saved) (%lua-leave!))
        (%close/f #f p))
      (luaU-active-set! u (fx- (luaU-active u) 1))
      (luaU-state-set! u #f)
      (luaU-ctx-set! u #f)                               ; aux_close freed it, with its pending releases
      (luaU-unrefs-set! u '())
      (luaU-unanchors-set! u '())
      (%sweep-handles! u)
      (%escape-epilogue!)))

  ; closes the state of `L', a main luaR, exactly once: the slot is cleared first, so a second
  ; close -- explicit, or the finalizer's at collection time -- does nothing.  Refused for threads
  ; and views, and while a binding on this universe is running.
  (define (lua-close L)
    (unless (luaR? L) (%api-error 'lua-close "not a lua state" (list L)))
    (unless (eq? (luaR-kind L) 'main) (%api-error 'lua-close "only a main state can be closed" (list L)))
    (let ((state (luaR-state L)) (u (luaR-u L)))
      (when state
        (when (fx> (luaU-active u) 0) (%api-error 'lua-close "closing a state while it is running" (list L)))
        (luaR-state-set! L #f)
        (%close-universe! u))))

  ;; the close of a dropped main luaR: a universe found running, or whose pointer a binding has
  ;; just read (%busy-ctx: its luaR may be garbage while its stub is yet to run), is closed at the
  ;; next binding entry instead (I7)
  (define (%defer-close! L)
    (luaU-close-pending-set! (luaR-u L) #t)
    (set! %pending-closes (cons L %pending-closes)))
  (define (%lua-finalize L)
    (when (luaR-state L)
      (let ((u (luaR-u L)))
        (if (or (fx> (luaU-active u) 0) (eq? (luaU-ctx u) %busy-ctx))
          (%defer-close! L)
          (lua-close L)))))
  ;; the finalizer of a main luaR.  It runs inside CHICKEN's finalizer loop, which must not be left
  ;; by a continuation (the loop would stay marked as running: no finalizer would ever run again,
  ;; and every (gc #t) would raise).  A universe that holds Scheme callables may run them while it
  ;; closes (its __gc metamethods), and one of them may escape; so such a universe is always
  ;; closed at the next binding entry, outside the finalizer loop.  One without callables closes
  ;; with the fast stub, where no Scheme code can run.
  (define (%lua-finalizer L)
    (when (luaR-state L)
      (if (fx> (luaU-callables (luaR-u L)) 0)
        (%defer-close! L)
        (%lua-finalize L))))

  ;; the main luaR of a new universe (state: a set-up lua_State, ctx: its aux_ctx)
  (define (%make-main-luaR state ctx)
    (let* ((u (make-luaU state ctx 0 0 '() #f (make-vector 16 #f) '() 0 1 '() '() #f))
           (L (make-luaR state u 'main #f -1 0 #f)))
      (luaU-mainw-set! u (make-weak-locative L 0))
      (%register-universe! u)
      (set-finalizer! L %lua-finalizer)))

  (define (lua-make-empty)
    (let1 (state (%luaL-newstate))  ; #f is NULL: out of memory
      (unless state (error 'lua-make-empty "luaL_newstate cannot allocate a state"))
      (let1 (ctx (%setup state))    ; #f: the state is already closed
        (unless ctx (error 'lua-make-empty "cannot set up a new state"))
        (%make-main-luaR state ctx))))

  ;; (luaL-newstate) => a new main luaR made by luaL_newstate: lauxlib's allocator, warnings on, a
  ;; luaL_makeseed seed, no library opened -- the state lua-make-empty makes.  Not curried: there
  ;; is no state yet.  Before the port it answered the bare lua_State pointer, which no binding of
  ;; this module can take (a state has a context, a finalizer and handle tables).
  (define (luaL-newstate)
    (let1 (state (%luaL-newstate))
      (unless state (error 'luaL-newstate "luaL_newstate cannot allocate a state"))
      (let1 (ctx (%setup state))
        (unless ctx (error 'luaL-newstate "cannot set up a new state"))
        (%make-main-luaR state ctx))))

  ;; ==========================================================================================
  ;; CORE — representative D bindings
  ;; ==========================================================================================

  (define lua-gettop (let ((k (λ (L) (%done (%gettop (%L 'lua-gettop L)))))) (λ () k)))

  (define %pushvalue (foreign-lambda* int (((c-pointer "lua_State") L) (int idx))
                       "if (!aux_ix(L, idx, AUX_IX_READ, 0)) C_return(AUX_EINDEX);
                        if (!lua_checkstack(L, 1)) C_return(AUX_ESTACK);
                        lua_pushvalue(L, idx); C_return(0);"))
  (define ((lua-pushvalue idx) L) (%int! 'lua-pushvalue idx) (%D 'lua-pushvalue (%pushvalue (%L 'lua-pushvalue L) idx) idx) (void))

  (define lua-pushnil (let ((k (λ (L) (%D 'lua-pushnil (%pushnil-raw (%L 'lua-pushnil L))) (void)))) (λ () k)))
  ;; ((lua-pushboolean b) L): b is #t or #f (C's int: 0 would be true with Scheme truthiness)
  (define ((lua-pushboolean b) L) (%bool! 'lua-pushboolean b) (%D 'lua-pushboolean (%pushboolean-raw (%L 'lua-pushboolean L) b) b) (void))
  ;; a refusal before Lua runs is an (exn lua api), as for every lua_Integer / lua_Number argument
  (define ((lua-pushinteger n) L)
    (unless (or (fixnum? n) (%int64? n)) (%api-error 'lua-pushinteger "not a lua_Integer" (list n) AUX_ERANGE))
    (%D 'lua-pushinteger (%pushinteger-raw (%L 'lua-pushinteger L) n) n) (void))
  (define ((lua-pushnumber x) L)
    (unless (real? x) (%api-error 'lua-pushnumber "not a number" (list x)))
    (%D 'lua-pushnumber (%pushnumber-raw (%L 'lua-pushnumber L) (exact->inexact x)) x) (void))

  (define %type (foreign-lambda* int (((c-pointer "lua_State") L) (int idx))
                  "int q = aux_qix(L, idx); if (q < 0) C_return(AUX_EINDEX); if (q == 0) C_return(LUA_TNONE);
                   C_return(lua_type(L, idx));"))
  (define ((lua-type idx) L) (%int! 'lua-type idx) (%D 'lua-type (%type (%L 'lua-type L) idx) idx))

  (define %settop (foreign-lambda int "aux_settop_d" (c-pointer "lua_State") int int))
  ;; ((lua-settop idx) L): growth fills with nil.  Lowering never runs code: a hook view cannot go
  ;; below its floor, and a thread whose current frame is a Lua function (dead by an error raised in
  ;; Lua code, or yielded by a hook) cannot be lowered at all -- lua-closethread unwinds it.
  (define ((lua-settop idx) L)
    (%int! 'lua-settop idx)
    (%D 'lua-settop (%settop (%L 'lua-settop L) idx (luaR-floor L)) idx)
    (void))
  (define ((lua-pop n) L)
    (let ((p (%L 'lua-pop L)))
      (unless (and (fixnum? n) (fx>= n 0) (fx<= n (%gettop p))) (%api-error 'lua-pop "cannot pop that many values" (list n) AUX_EUNDER))
      (%D 'lua-pop (%settop p (fx- -1 n) (luaR-floor L)) n)
      (void)))

  (define %tointegerx (foreign-lambda* integer64 (((c-pointer "lua_State") L) (int idx) ((c-pointer int) ok))
                        "int isnum = 0, q = aux_qix(L, idx); lua_Integer n;
                         if (q <= 0) { *ok = q; C_return(0); }
                         n = lua_tointegerx(L, idx, &isnum); *ok = isnum; C_return(n);"))
  (define %tonumberx (foreign-lambda* double (((c-pointer "lua_State") L) (int idx) ((c-pointer int) ok))
                       "int isnum = 0, q = aux_qix(L, idx); lua_Number x;
                        if (q <= 0) { *ok = q; C_return(0); }
                        x = lua_tonumberx(L, idx, &isnum); *ok = isnum; C_return(x);"))
  ;; ((lua-tointegerx idx) L) => (values n ok?)
  (define ((lua-tointegerx idx) L)
    (%int! 'lua-tointegerx idx)
    (let ((p (%L 'lua-tointegerx L)))
      (let-location ((ok int 0))
        (let ((n (%done (%tointegerx p idx (location ok)))))
          (if (fx< ok 0) (%api-fail 'lua-tointegerx AUX_EINDEX (list idx)) (values n (fx= ok 1)))))))
  (define ((lua-tonumberx idx) L)
    (%int! 'lua-tonumberx idx)
    (let ((p (%L 'lua-tonumberx L)))
      (let-location ((ok int 0))
        (let ((x (%done (%tonumberx p idx (location ok)))))
          (if (fx< ok 0) (%api-fail 'lua-tonumberx AUX_EINDEX (list idx)) (values x (fx= ok 1)))))))
  (define ((lua-tointeger idx) L) (let-values (((n ok?) ((lua-tointegerx idx) L))) n))
  (define ((lua-tonumber idx) L) (let-values (((x ok?) ((lua-tonumberx idx) L))) x))

  (define %toboolean (foreign-lambda* int (((c-pointer "lua_State") L) (int idx))
                       "int q = aux_qix(L, idx); if (q < 0) C_return(AUX_EINDEX); if (q == 0) C_return(0);
                        C_return(lua_toboolean(L, idx));"))
  (define ((lua-toboolean idx) L) (%int! 'lua-toboolean idx) (fx= 1 (%D 'lua-toboolean (%toboolean (%L 'lua-toboolean L) idx) idx)))

  (define lua-isyieldable (let ((k (λ (L) (%done ((foreign-lambda bool "lua_isyieldable" (c-pointer "lua_State")) (%L 'lua-isyieldable L)))))) (λ () k)))

  ;; ((lua-tobytevector idx) L): the bytes of a string or number (as Lua writes it; the slot is not
  ;; converted), #f otherwise.  ((lua-tolstring idx) L): the same as a string when they are valid UTF-8.
  (define ((lua-tobytevector idx) L) (%int! 'lua-tobytevector idx) (%done (%bytes-at 'lua-tobytevector (%L 'lua-tobytevector L) idx)))
  (define ((lua-tolstring idx) L)
    (let ((bv ((lua-tobytevector idx) L))) (and bv (%bytes->string/maybe bv))))
  (define ((lua-tostring idx) L) ((lua-tolstring idx) L))

  ;; ==========================================================================================
  ;; CORE — representative P / P* bindings
  ;; ==========================================================================================

  ;; ((lua-pushlstring s) L): s a string or a bytevector (NUL bytes allowed)
  (define ((lua-pushlstring s) L) (%bytes! 'lua-pushlstring s) (%P 'lua-pushlstring L (%pushlstring/f %pushlstring/s) s) (void))
  ;; ((lua-pushstring s) L): #f pushes nil (C's NULL)
  (define ((lua-pushstring s) L) (if s ((lua-pushlstring s) L) ((lua-pushnil) L)))

  (define-lua-stubs (%getfield/f %getfield/s) ((int idx) (scheme-object k))
    "aux_op o = {0}; int refs[1]; o.fn = op_getfield; o.s0 = aux_bytes(k, &o.l0);
     if (!aux_ix(L, idx, AUX_IX_READ, 0)) C_return(AUX_EINDEX);
     refs[0] = idx; C_return(aux_run(safe, L, &o, 1, refs, 0, 1));")
  ;; ((lua-getfield idx k) L) => the type of t[k] pushed
  (define ((lua-getfield idx k) L)
    (%int! 'lua-getfield idx) (%bytes! 'lua-getfield k)
    (%P 'lua-getfield L (%getfield/f %getfield/s) idx k))

  (define-lua-stubs (%setfield/f %setfield/s) ((int idx) (scheme-object k))
    "aux_op o = {0}; int refs[1], c; o.fn = op_setfield; o.s0 = aux_bytes(k, &o.l0);
     if (!aux_ix(L, idx, AUX_IX_READ, 0)) C_return(AUX_EINDEX);
     char *dup = NULL;
     if (lua_gettop(L) >= 1 && (c = aux_libwrite_stub(safe, L, idx, aux_libname(o.s0, o.l0), -1, &o.s0, o.l0, &dup))) { free(dup); C_return(c); }
     refs[0] = idx; c = aux_run(safe, L, &o, 1, refs, 1, 0); free(dup); C_return(c);")
  ;; ((lua-setfield idx k) L): t[k] = the value on top, popped
  (define ((lua-setfield idx k) L)
    (%int! 'lua-setfield idx) (%bytes! 'lua-setfield k)
    (%P 'lua-setfield L (%setfield/f %setfield/s) idx k) (void))

  (define-lua-stubs (%getglobal/f %getglobal/s) ((scheme-object k))
    "aux_op o = {0}; o.fn = op_getglobal; o.s0 = aux_bytes(k, &o.l0); C_return(aux_run_h(safe, L, &o, 1));")
  ;; ((lua-getglobal name) L) => the type of the global pushed
  (define ((lua-getglobal name) L) (%bytes! 'lua-getglobal name) (%P 'lua-getglobal L (%getglobal/f %getglobal/s) name))

  (define-lua-stubs (%setglobal/f %setglobal/s) ((scheme-object k))
    "aux_op o = {0}; int c = 0; char *dup = NULL; o.fn = op_setglobal; o.s0 = aux_bytes(k, &o.l0);
     if (lua_gettop(L) >= 1 && lua_status(L) == LUA_OK) {         /* else refused by aux_run (AUX_EUNDER, AUX_ETHREAD) */
       if (!lua_checkstack(L, 1)) C_return(AUX_ESTACK);
       lua_rawgeti(L, LUA_REGISTRYINDEX, LUA_RIDX_GLOBALS);        /* the globals may be the registry */
       c = aux_libwrite_stub(safe, L, -1, aux_libname(o.s0, o.l0), -2, &o.s0, o.l0, &dup);
       if (AUX_IS_LUAERR(c)) lua_remove(L, -2); else lua_pop(L, 1);   /* the globals, under a failed op's error value */
       if (c) { free(dup); C_return(c); } }
     c = aux_run(safe, L, &o, 0, NULL, 1, 0); free(dup); C_return(c);")
  ;; ((lua-setglobal name) L): the global = the value on top, popped
  (define ((lua-setglobal name) L) (%bytes! 'lua-setglobal name) (%P 'lua-setglobal L (%setglobal/f %setglobal/s) name) (void))

  (define-lua-stubs (%openlibs/f %openlibs/s) ()
    "aux_op o = {0}; int c; o.fn = op_openlibs;
     if ((c = aux_globals_code(L))) C_return(c);
     C_return(aux_run(safe, L, &o, 0, NULL, 0, 0));")
  (define luaL-openlibs (let ((k (λ (L) (%P 'luaL-openlibs L (%openlibs/f %openlibs/s)) (void)))) (λ () k)))

  ;; ==========================================================================================
  ;; CORE — representative S bindings (status-returning, self-protecting)
  ;; ==========================================================================================

  (define-lua-stubs (%pcall/f %pcall/s) ((int nargs) (int nres) (int msgh))
    "C_return(aux_pcall(safe, L, nargs, nres, msgh));")
  ;; ((lua-pcall nargs nres msgh) L) => status (the error value is left on the stack, as C).
  ;; nres: LUA_MULTRET or 0 .. 250 (Lua 5.5's limit, an API check in C), else (exn lua api)
  (define ((lua-pcall nargs nres msgh) L)
    (%int! 'lua-pcall nargs nres msgh)
    (%P 'lua-pcall L (%pcall/f %pcall/s) nargs nres msgh))
  ;; ((lua-call nargs nres) L): a Lua error is raised as an (exn lua) condition.  It is
  ;; lua-pcall without a handler: inside a callback, the error unwinds the callee to this call
  ;; before an enclosing message handler (Lua's xpcall, a lua-pcall with msgh) sees it, so that
  ;; handler runs at the callback's frame (see "Safety model"); lua-pcall with msgh runs one at
  ;; the error point
  (define ((lua-call nargs nres) L)
    (let ((st ((lua-pcall nargs nres 0) L)))
      (unless (fx= st LUA_OK) (%raise-top 'lua-call (luaR-u L) (%L 'lua-call L) st (list nargs nres)))))

  (define-lua-stubs (%loadbufferx/f %loadbufferx/s) ((scheme-object buff) (scheme-object name) (scheme-object mode))
    "size_t n, nl = 0, ml = 0; const char *b = aux_bytes(buff, &n), *nm = NULL, *md = NULL;
     if (C_truep(name)) { nm = aux_bytes(name, &nl); nl = strnlen(nm, nl); }
     if (C_truep(mode)) md = aux_bytes(mode, &ml);
     C_return(aux_loadbuffer(safe, L, b, n, nm, nl, md, ml));")
  ;; ((luaL-loadbufferx buff name mode) L) => status; pushes the chunk or the error message.  Mode
  ;; #f, "b" or "bt" accepts binary chunks, which Lua does not verify: use "t" for untrusted input.
  ;; luaL-loadbuffer, luaL-loadstring and luaL-dostring always accept them ("bt"), as C.
  (define ((luaL-loadbufferx buff name mode) L)
    (%bytes! 'luaL-loadbufferx buff)
    (%cstring! 'luaL-loadbufferx name #t)
    (%mode! 'luaL-loadbufferx mode)
    (%P 'luaL-loadbufferx L (%loadbufferx/f %loadbufferx/s) buff name mode))
  (define ((luaL-loadbuffer buff name) L) ((luaL-loadbufferx buff name #f) L))
  ;; ((luaL-loadstring s) L): the chunk name is s itself (up to a NUL, as C); the whole of s is loaded
  (define ((luaL-loadstring s) L)
    (%bytes! 'luaL-loadstring s)
    (%P 'luaL-loadstring L (%loadbufferx/f %loadbufferx/s) s s #f))
  ;; ((luaL-dostring s) L) => the failing status, or LUA_OK (C's macro answers 1 on any failure)
  (define ((luaL-dostring s) L)
    (let ((st ((luaL-loadstring s) L)))
      (if (fx= st LUA_OK) ((lua-pcall 0 LUA_MULTRET 0) L) st)))

  ;; the epoch of the universe of L (masked as aux_raisable), -1 when L is not an open luaR; read from the
  ;; main state, never through L's own pointer (a thread luaR is only validated by %L)
  (define %raisable (foreign-lambda int "aux_raisable" (c-pointer "lua_State") bool int))
  (define %main-epoch (foreign-lambda* int ((c-pointer M)) "C_return((int)(AUX_CTX((lua_State *)M)->epoch & 0x3fffffff));"))
  (define (%lua-epoch L)
    (if (##sys#structure? L %luaR-tag)
      (let ((u (##sys#slot L 2)))
        (if (and (luaU? u) (luaU-state u)) (%main-epoch (luaU-state u)) -1))
      -1))

  ;; ((lua-check m) L): runs m; a first value that is an error status raises the error value on top of L.
  ;; On a thread suspended by a hook, or dead in a Lua function, the top is one of that function's
  ;; registers (as lua-pop, refused), except the error value a lua-resume run by m has just pushed:
  ;; ((lua-check (lua-resume from n)) co).
  (define ((lua-check m) L)
    (let ((e0 (%lua-epoch L)))
      (call-with-values
        (λ () (m L))
        (λ vals
          (let ((st (and (pair? vals) (car vals))))
            (if (and (fixnum? st) (fx> st LUA_YIELD) (fx<= st LUA_ERRFILE))
              (let ((p (%L 'lua-check L)))
                (when (fx<= (%gettop p) (%eff-floor p (luaR-floor L)))    ; nothing of m's to raise (I6)
                  (%api-error 'lua-check "no error value on the stack" (list st) AUX_EUNDER))
                (let ((r (%raisable p (fx>= e0 0) e0)))
                  (unless (fx= r 0) (%api-fail 'lua-check r (list st))))
                (%raise-top 'lua-check (luaR-u L) p st '()))
              (apply values vals)))))))

  ;; ==========================================================================================
  ;; CORE — T bindings: raising from Scheme
  ;; ==========================================================================================

  ;; ((lua-error) L): raises the value on top (popped) as an (exn lua) condition whose `value' is
  ;; it; uncaught inside a callback, Lua receives the same value (table identity kept).
  (define lua-error
    (let ((k (λ (L)
               (let ((p (%L 'lua-error L)))
                 (when (fx<= (%gettop p) (%eff-floor p (luaR-floor L)))  ; a hooked function's register is not ours (I6)
                   (%api-error 'lua-error "no error value on the stack" '() AUX_EUNDER))
                 (let ((r (%raisable p #f 0)))                   ; nor one of a suspended/dead Lua frame (as lua-pop)
                   (unless (fx= r 0) (%api-fail 'lua-error r '())))
                 (%raise-top 'lua-error (luaR-u L) p LUA_ERRRUN '())))))
      (λ () k)))

  ;; Lua's lua_pushfstring directives: %% %s %f %I %p %d %c %U (unknown ones are kept, as C does).
  ;; The bytes of the result (a bytevector).  fmt and a %s argument are C strings, which C reads up
  ;; to their first NUL (strchr / strlen): a NUL in either raises (exn lua api), the rule of every
  ;; C string, rather than dropping the rest of the text; %s takes a string, a bytevector or #f
  ;; (NULL, printed "(null)" as by Lua), and nothing else (C would read any other value as a pointer).
  (define %pointer->string (foreign-lambda* c-string ((c-pointer p)) "static char b[64]; snprintf(b, sizeof b, \"%p\", p); C_return(b);"))
  (define (%utf8esc x)
    (if (fx< x #x80) (bytevector x)
      (let loop ((x x) (mfb #x3f) (acc '()))
        (let ((acc (cons (fxior #x80 (fxand x #x3f)) acc)) (x (fxshr x 6)) (mfb (fxshr mfb 1)))
          (if (fx> x mfb) (loop x mfb acc)
            (apply bytevector (cons (fxand #xff (fxior (fxshl (fxnot mfb) 1) x)) acc)))))))
  (define (%nul-free-bytes? bv)
    (let ((n (bytevector-length bv)))
      (let loop ((i 0)) (or (fx= i n) (and (not (fx= 0 (bytevector-u8-ref bv i))) (loop (fx+ i 1)))))))
  (define (%lua-format who fmt args)
    (unless (or (string? fmt) (bytevector? fmt)) (%api-error who "not a string" (list fmt)))
    (let ((f (%->bytes fmt)))
      (unless (%nul-free-bytes? f) (%api-error who "not a string without NUL" (list fmt)))
      (let loop ((i 0) (start 0) (args args) (acc '()))
        (define (piece a b) (let ((bv (make-bytevector (fx- b a)))) (do ((j a (fx+ j 1))) ((fx= j b)) (bytevector-u8-set! bv (fx- j a) (bytevector-u8-ref f j))) bv))
        (define (next-arg) (if (pair? args) (car args) (%api-error who "missing argument for a directive" (list fmt))))
        (cond
          ((fx= i (bytevector-length f)) (%bytes-concat-reverse (cons (piece start i) acc)))
          ((and (fx= (bytevector-u8-ref f i) 37) (fx< (fx+ i 1) (bytevector-length f)))
           (let* ((c (integer->char (bytevector-u8-ref f (fx+ i 1))))
                  (acc (cons (piece start i) acc))
                  (emit (λ (bv consumes?) (loop (fx+ i 2) (fx+ i 2) (if consumes? (cdr args) args) (cons bv acc)))))
             (case c
               ((#\%) (emit (bytevector 37) #f))
               ((#\s) (let* ((a (next-arg))
                             (b (cond ((not a) (string->utf8 "(null)"))
                                      ((or (string? a) (bytevector? a)) (%->bytes a))
                                      (else (%api-error who "not a string" (list a))))))
                        (unless (%nul-free-bytes? b) (%api-error who "not a string without NUL" (list a)))
                        (emit b #t)))
               ;; %d reads a C int, %I a lua_Integer: C can never print a value outside those ranges
               ((#\d) (let ((a (next-arg))) (%int! who a) (emit (string->utf8 (number->string a)) #t)))
               ((#\I) (let ((a (next-arg))) (unless (%int64? a) (%api-error who "not a lua_Integer" (list a) AUX_ERANGE)) (emit (string->utf8 (number->string a)) #t)))
               ((#\f) (let ((a (next-arg))) (unless (real? a) (%api-error who "not a number" (list a))) (emit (string->utf8 (%num2str (exact->inexact a))) #t)))
               ((#\c) (let ((a (next-arg))) (%int! who a) (emit (bytevector (fxand a #xff)) #t)))   ; an int, cast to unsigned char
               ((#\U) (let ((a (next-arg))) (unless (and (fixnum? a) (fx>= a 0) (fx<= a #x7FFFFFFF)) (%api-error who "not a code point" (list a))) (emit (%utf8esc a) #t)))
               ((#\p) (let ((a (next-arg))) (unless (or (not a) (pointer? a)) (%api-error who "not a pointer" (list a))) (emit (string->utf8 (%pointer->string a)) #t)))
               (else (emit (piece i (fx+ i 2)) #f)))))
          (else (loop (fx+ i 1) start args acc))))))
  ;; (lua-format fmt arg ...) => the formatted text as a Scheme string when it is valid UTF-8, else
  ;; a bytevector.  NOT curried: it returns the text, it does not take a state; ((lua-pushfstring
  ;; fmt arg ...) L) pushes the same text onto L.
  (define (lua-format fmt . args) (%bytes->string/maybe (%lua-format 'lua-format fmt args)))

  ;; The `value' of the conditions of luaL-error, luaL-argerror and luaL-typeerror (and so of the K
  ;; checks built on them): the string C's luaL_error / luaL_argerror would raise on L, computed
  ;; by a P op that runs lauxlib's code one level up (op_errval): the luaL_where prefix ("" at
  ;; top level; the calling Lua function's position inside a callback), and luaL_argerror's
  ;; "to 'name'", "extra argument" and "calling 'm' on bad self" forms.  Uncaught inside a
  ;; callback, Lua receives the same text.  Where the op cannot run (a suspended or dead thread,
  ;; a full stack, a memory error) the value is `fallback', the text without position.
  (define-lua-stubs (%errval/f %errval/s) ((int kind) (int arg) (scheme-object text))
    "aux_op o = {0}; o.fn = op_errval; o.i0 = kind; o.i1 = arg; o.s0 = aux_bytes(text, &o.l0);
     C_return(aux_run(safe, L, &o, 0, NULL, 0, 1));")
  (define (%errval who L kind arg text fallback)
    (let ((u (%U who L)))
      (%prepare! u)
      (let* ((p (%L who L)) (r (%run u (%errval/f %errval/s) p kind arg text)))
        (let ((v (cond ((fx> r -1000) (let ((s (%bytes-at 'lua p -1))) (%pop! p 1) (%bytes->string/maybe s)))
                       (else (when (fx> r -1100) (%pop! p 1))      ; the op's own error value
                             fallback))))
          (%escape-epilogue!)                                  ; an escape a finalizer stopped wins
          (set! %busy-ctx #f)
          v))))
  ;; the bytes of x up to its first NUL: what C's "%s" reads of a const char *
  (define (%cut-nul x)
    (let* ((bv (%->bytes x)) (n (bytevector-length bv)))
      (let loop ((i 0))
        (cond ((fx= i n) bv)
              ((fx= 0 (bytevector-u8-ref bv i))
               (let ((c (make-bytevector i))) (do ((j 0 (fx+ j 1))) ((fx= j i) c) (bytevector-u8-set! c j (bytevector-u8-ref bv j)))))
              (else (loop (fx+ i 1)))))))
  (define (%argerror-fallback arg extramsg)
    (%bytes->string/maybe (bytevector-append (string->utf8 (string-append "bad argument #" (number->string arg) " (")) extramsg (string->utf8 ")"))))

  ;; ((luaL-error fmt . args) L): raises (exn lua) with request lerror and `value' C's error value
  ;; (see %errval); uncaught inside a callback, Lua raises the message prefixed by luaL_where(L, 1),
  ;; exactly as C.
  (define ((luaL-error fmt . args) L)
    (%V 'luaL-error L)
    (let* ((bytes (%lua-format 'luaL-error fmt args))
           (v (%errval 'luaL-error L 0 0 bytes (%bytes->string/maybe bytes))))
      (abort (%lua-condition 'luaL-error (%bytes->string/lossy bytes) args LUA_ERRRUN 'request 'lerror 'text bytes 'value v))))

  ;; ((luaL-argerror arg extramsg) L): raises (exn lua) with request argerror and `value' C's error
  ;; value (see %errval); uncaught inside a callback, the real luaL_argerror runs in the callback's
  ;; frame (C-identical message).
  (define ((luaL-argerror arg extramsg) L)
    (%V 'luaL-argerror L) (%int! 'luaL-argerror arg) (%bytes! 'luaL-argerror extramsg)
    (let* ((cut (%cut-nul extramsg))
           (v (%errval 'luaL-argerror L 1 arg cut (%argerror-fallback arg cut))))
      (abort (%lua-condition 'luaL-argerror
                             (string-append "bad argument #" (number->string arg) " (" (%bytes->string/lossy (%->bytes extramsg)) ")")
                             (list arg extramsg) LUA_ERRRUN 'request 'argerror 'arg arg 'text extramsg 'value v))))

  ;; the name luaL_typeerror uses for the value at arg: Lua's own bytes (a __name need not be
  ;; UTF-8), copied in two steps (the length, then the bytes: nothing runs Lua in between)
  (define %typearg-len (foreign-lambda* int (((c-pointer "lua_State") L) (int arg))
                         "C_return((int)strlen(aux_typearg(L, arg)));"))
  (define %typearg-copy! (foreign-lambda* void (((c-pointer "lua_State") L) (int arg) (scheme-object bv))
                           "const char *s = aux_typearg(L, arg); memcpy(C_data_pointer(bv), s, C_header_size(bv));"))
  (define (%typearg-bytes p arg)
    (let ((bv (make-bytevector (%typearg-len p arg))))
      (%typearg-copy! p arg bv)
      bv))
  ;; ((luaL-typeerror arg tname) L): as luaL-argerror, with lauxlib's "T expected, got U"
  (define ((luaL-typeerror arg tname) L)
    (let ((p (%L 'luaL-typeerror L)))
      (%int! 'luaL-typeerror arg) (%bytes! 'luaL-typeerror tname)
      (let* ((got (%typearg-bytes p arg))
             (extra (bytevector-append (%cut-nul tname) (string->utf8 " expected, got ") got)))
        (set! %busy-ctx #f)
        (let ((v (%errval 'luaL-typeerror L 1 arg extra (%argerror-fallback arg extra))))
          (abort (%lua-condition 'luaL-typeerror
                               (string-append "bad argument #" (number->string arg) " ("
                                              (%bytes->string/lossy (%->bytes tname)) " expected, got " (%bytes->string/lossy got) ")")
                               (list arg tname) LUA_ERRRUN 'request 'typeerror 'arg arg 'text tname 'value v))))))

  ;; ((lua-yield n) C): the request a Scheme lua_CFunction ANSWERS to yield its top n values;
  ;; the values of the next resume become its results.
  ;; Where Lua cannot yield, the error of C's lua_yieldk: "attempt to yield from outside a
  ;; coroutine" on the main thread, "attempt to yield across a C-call boundary" on any other (a
  ;; coroutine whose callback runs under a C call without continuation: table.sort's comparator,
  ;; lua-call, a hook in such a context, ...).  Uncaught, Lua raises it as luaG_runerror does:
  ;; from a hook, prefixed by the hooked function's position (request 'runerror).
  (define %main-thread? (foreign-lambda* bool (((c-pointer "lua_State") L)) "C_return(L == AUX_CTX(L)->main);"))
  (define ((lua-yield n) C)
    (let ((p (%L 'lua-yield C)))
      (unless ((foreign-lambda bool "lua_isyieldable" (c-pointer "lua_State")) p)
        (let ((m (if (%main-thread? p) "attempt to yield from outside a coroutine" "attempt to yield across a C-call boundary")))
          (set! %busy-ctx #f)
          (abort (%lua-condition 'lua-yield m (list n) LUA_ERRRUN 'value m 'request 'runerror 'text m))))
      (unless (and (fixnum? n) (fx>= n 0) (fx<= n (%gettop p))) (%api-error 'lua-yield "bad yield count" (list n) AUX_EUNDER))
      (set! %busy-ctx #f)
      (make-lua-yield-request n #f)))
  ;; ((lua-yieldk n k) C): as lua-yield; on resume (k status C) runs as the continuation of the
  ;; function, with the resume values on top, and answers like a lua_CFunction.  The suspended
  ;; coroutine looks as in C: its stack holds what the function left, and nothing else (k is kept
  ;; in a private weak table of the universe, keyed by the thread), so the resumer may pop, move or
  ;; clear those values before it resumes.  k is released once it has run, or with the thread.
  (define ((lua-yieldk n k) C)
    (unless (procedure? k) (%api-error 'lua-yieldk "not a procedure" (list k)))
    (let ((r ((lua-yield n) C))) (lua-yield-request-k-set! r k) r))

  ;; ==========================================================================================
  ;; CORE — warnings and memory
  ;; ==========================================================================================

  (define %setwarnmode (foreign-lambda* void (((c-pointer "lua_State") L) (int m)) "aux_setwarnmode(AUX_CTX(L), m);"))
  ;; ((lua-setwarnf mode) L): mode #f / 'off (no warning function, as lua_setwarnf(L, NULL, NULL):
  ;; warnings, "@on" / "@off" and pieces alike, are dropped), 'stderr (lauxlib's), 'collect (see
  ;; lua-warnings).  As installing a warning function in C, 'stderr and 'collect start with warnings
  ;; on and at a new message, whatever an earlier function was told ("@off", a piece to continue);
  ;; the pieces 'collect had of an unfinished message are kept as one warning.
  (define ((lua-setwarnf mode) L)
    (let ((m (case mode ((#f off) 0) ((stderr) 1) ((collect) 2)
               (else (%api-error 'lua-setwarnf "unknown warning mode" (list mode))))))
      (%done (%setwarnmode (%L 'lua-setwarnf L) m))))
  (define %warn-count (foreign-lambda* int (((c-pointer "lua_State") L)) "C_return((int)AUX_CTX(L)->wn);"))
  (define %warn-len (foreign-lambda* int (((c-pointer "lua_State") L) (int i)) "C_return((int)strlen(AUX_CTX(L)->wq[i]));"))
  (define %warn-copy! (foreign-lambda* void (((c-pointer "lua_State") L) (int i) (scheme-object bv))
                        "const char *m = AUX_CTX(L)->wq[i]; memcpy(C_data_pointer(bv), m, strlen(m));"))
  (define %warn-clear! (foreign-lambda* void (((c-pointer "lua_State") L)) "aux_wclear(AUX_CTX(L));"))
  ;; ((lua-warnings) L) => the completed warnings collected so far (oldest first), clearing them
  (define lua-warnings
    (let ((k (λ (L)
               (let* ((p (%L 'lua-warnings L)) (n (%warn-count p)))
                 (let loop ((i (fx- n 1)) (acc '()))
                   (if (fx< i 0)
                     (begin (%warn-clear! p) (set! %busy-ctx #f) acc)
                     (let ((bv (make-bytevector (%warn-len p i))))
                       (%warn-copy! p i bv)
                       (loop (fx- i 1) (cons (%bytes->string/lossy bv) acc)))))))))
      (λ () k)))

  (define %setmemorylimit (foreign-lambda* void (((c-pointer "lua_State") L) (size_t limit))
                            "aux_ctx *c = AUX_CTX(L); int kb = lua_gc(L, LUA_GCCOUNT), b = lua_gc(L, LUA_GCCOUNTB);
                             void *ud = NULL;
                             c->used = (kb < 0 || b < 0) ? 0 : (size_t)kb * 1024 + (size_t)b;
                             c->limit = limit;
                             if (lua_getallocf(L, &ud) != aux_alloc) lua_setallocf(L, aux_alloc, c);"))
  ;; ((lua-setmemorylimit bytes) L): allocations past bytes fail (LUA_ERRMEM); 0 = unlimited
  (define ((lua-setmemorylimit bytes) L)
    (unless (and (exact-integer? bytes) (>= bytes 0) (<= bytes 18446744073709551615))     ; a size_t
      (%api-error 'lua-setmemorylimit "not a byte count" (list bytes) AUX_ERANGE))
    (%done (%setmemorylimit (%L 'lua-setmemorylimit L) bytes)))

  ;; ==========================================================================================
  ;; S1 — stack manipulation, type queries and constants.  Every binding here is D: it never
  ;; allocates Lua memory, never raises a Lua error and never runs Lua code; indices are
  ;; validated in C before the API is called (an invalid index is an (exn lua api)).
  ;;
  ;; Like the core (which reads an upvalue pseudo-index only inside a C function: at base level
  ;; or in a Lua frame, a hook, the API asserts "caller not a C function"), S1 is strict about
  ;; two things the release build of Lua would not catch:
  ;; - upvalue pseudo-indices are written only when the running function IS the Scheme closure
  ;;   of the view, within its real upvalue count
  ;;   (a view kept past a nested call could otherwise write another C closure's upvalues,
  ;;   e.g. a userdata some C library reads its state from, or Lua's global nil object);
  ;; - slots are never moved (rotate, insert, remove) across a to-be-closed slot: in a hook view
  ;;   only above its floor (the hooked function's registers lie below it), and never on a
  ;;   suspended or dead thread whose current frame is a Lua function.  From such a thread
  ;;   lua-xmove moves only what lies above the height it had at its hook yield or death
  ;;   (aux_nk_lim): the values pushed since, and the error value lua-resume left on a dead one.
  ;; ==========================================================================================

  #>


  /* READ index: the core's rule (an upvalue pseudo-index only inside a C function) */
  static int aux_s1_rix(lua_State *L, int idx) { return aux_ix(L, idx, AUX_IX_READ, 0); }

  /* query index: 1 valid, 0 a positive index above the top (none), -1 invalid */
  static int aux_s1_qix(lua_State *L, int idx) {
    if (idx > 0 && idx > lua_gettop(L)) return 0;
    return aux_s1_rix(L, idx) ? 1 : -1; }

  /* WRITE index: a stack slot, or a user upvalue of the running Scheme closure itself */
  static int aux_s1_wix(lua_State *L, int idx, int nup) {
    aux_handle *u;
    if (!aux_ix(L, idx, AUX_IX_WRITE, nup)) return 0;
    if (idx > LUA_REGISTRYINDEX) return 1;
    if (!aux_cframe(L) || !(u = aux_self(L))) return 0;
    return LUA_REGISTRYINDEX - idx <= u->nup + 1; }

  /* 0, or the refusal for moving the slots from absolute index a up to the top of L */
  static int aux_s1_movable(lua_State *L, int a, int floor) {
    if (a <= aux_eff_floor(L, floor)) return AUX_EINDEX;   /* a hooked function's registers */
    if (lua_status(L) != LUA_OK && aux_in_lua_frame(L)) return AUX_ETHREAD;
    return 0; }

  /* the absolute stack index (> 0), 0 for a pseudo-index (unchanged: it would collide with the
     result codes), or AUX_EINDEX.  As C, a positive index is answered unchanged, above the top too
     (an acceptable index); a negative one must be a stack slot. */
  static int aux_s1_absindex(lua_State *L, int idx) {       /* lua_absindex never reads a slot */
    if (idx > 0) return idx;
    if (idx < LUA_REGISTRYINDEX) return LUA_REGISTRYINDEX - idx <= 256 ? 0 : AUX_EINDEX;
    if (idx == LUA_REGISTRYINDEX) return 0;
    if (idx == 0 || -idx > lua_gettop(L)) return AUX_EINDEX;
    return lua_absindex(L, idx); }

  static int aux_s1_rotate(lua_State *L, int idx, int n, int floor) {
    int a, r; long long m = n < 0 ? -(long long)n : (long long)n;
    if (!aux_ix(L, idx, AUX_IX_STACK, 0)) return AUX_EINDEX;
    a = lua_absindex(L, idx);
    if (m > (long long)lua_gettop(L) - a + 1) return AUX_ERANGE;
    if ((r = aux_s1_movable(L, a, floor))) return r;
    lua_rotate(L, a, n); return 0; }

  /* a stack slot at or below the floor is not written (the registers of a hooked function: a numeric
     for loop's state would change type under the VM) */
  static int aux_s1_copy(lua_State *L, int from, int to, int nup, int floor) {
    if (!aux_s1_rix(L, from) || !aux_s1_wix(L, to, nup)) return AUX_EINDEX;
    if (to > LUA_REGISTRYINDEX && lua_absindex(L, to) <= aux_eff_floor(L, floor)) return AUX_EINDEX;
    /* a thread suspended (by a hook) or dead in a Lua function: its slots are that function's registers,
       which the VM trusts on resume (a numeric for loop's state); as lua-replace and lua-setlocal */
    if (to > LUA_REGISTRYINDEX && lua_status(L) != LUA_OK && aux_in_lua_frame(L)) return AUX_ETHREAD;
    lua_copy(L, from, to); return 0; }

  /* lua_remove: the rotate part, then the lowering step through aux_settop_d (checked first) */
  static int aux_s1_remove(lua_State *L, int idx, int floor) {
    int a, r;
    if (!aux_ix(L, idx, AUX_IX_STACK, 0)) return AUX_EINDEX;
    a = lua_absindex(L, idx);
    if ((r = aux_s1_movable(L, a, floor))) return r;       /* a > floor, so top - 1 >= floor */
    lua_rotate(L, a, -1);
    return aux_settop_d(L, -2, floor); }

  /* lua_replace: copy the top into idx, then the lowering step (checked first) */
  static int aux_s1_replace(lua_State *L, int idx, int nup, int floor) {
    int top = lua_gettop(L);
    floor = aux_eff_floor(L, floor);
    if (top < 1) return AUX_EUNDER;
    if (!aux_s1_wix(L, idx, nup)) return AUX_EINDEX;
    if (top - 1 < floor) return AUX_EINDEX;
    if (idx > LUA_REGISTRYINDEX && lua_absindex(L, idx) <= floor) return AUX_EINDEX;   /* as lua-copy */
    if (lua_status(L) != LUA_OK && aux_in_lua_frame(L)) return AUX_ETHREAD;
    lua_copy(L, -1, idx);
    return aux_settop_d(L, -2, floor); }

  static int aux_s1_checkstack(lua_State *L, int n) {
    if (n < 0) return AUX_ERANGE;
    return lua_checkstack(L, n) ? 1 : 0; }

  /* lua_xmove(from, to, n): same universe, 0 <= n <= top, room on `to'; from a non-OK thread in a Lua
     frame only what lies above the height it had at its hook yield or death (aux_nk_lim, id: from's anchor
     or -1): the values pushed since, and the error value lua-resume left on a dead one.  Below are its
     registers (a hooked function's operands, a to-be-closed slot later pushes would overwrite): repeated
     moves cannot drain them one value at a time. */
  static int aux_s1_xmove(lua_State *from, lua_State *to, int n, int floor, int id) {
    int top = lua_gettop(from), st = lua_status(from);
    if (from == to) return 0;                              /* lapi.c: before its api_checkpop, any n */
    if (AUX_CTX(from) != AUX_CTX(to)) return AUX_EARG;
    if (n < 0 || n > top) return AUX_EUNDER;
    if (n == 0) return 0;
    if (top - n < aux_eff_floor(from, floor)) return AUX_EINDEX;
    if (st != LUA_OK && aux_in_lua_frame(from) && top - n < aux_nk_lim(from, id)) return AUX_ETHREAD;
    if (!lua_checkstack(to, n)) return AUX_ESTACK;
    lua_xmove(from, to, n); return 0; }

  /* the type at a query index: LUA_TNONE above the top, AUX_EINDEX when invalid */
  static int aux_s1_type(lua_State *L, int idx) {
    int q = aux_s1_qix(L, idx);
    if (q < 0) return AUX_EINDEX;
    return q == 0 ? LUA_TNONE : lua_type(L, idx); }

  /* the lua_is* functions that are not a plain type test: 0/1, or AUX_EINDEX */
  #define AUX_S1_ISNUMBER    0
  #define AUX_S1_ISSTRING    1
  #define AUX_S1_ISCFUNCTION 2
  #define AUX_S1_ISINTEGER   3
  #define AUX_S1_ISUSERDATA  4
  static int aux_s1_is(lua_State *L, int idx, int which) {
    int q = aux_s1_qix(L, idx);
    if (q < 0) return AUX_EINDEX;
    if (q == 0) return 0;
    switch (which) {
    case AUX_S1_ISNUMBER:    return lua_isnumber(L, idx);   /* a string is converted on a TValue copy: no allocation */
    case AUX_S1_ISSTRING:    return lua_isstring(L, idx);
    case AUX_S1_ISCFUNCTION: return lua_iscfunction(L, idx);
    case AUX_S1_ISINTEGER:   return lua_isinteger(L, idx);
    default:                 return lua_isuserdata(L, idx); } }

  <#

  ;; ---------------------------------------------------------------- constants

  ;; version
  (define LUA_VERSION_NUM (foreign-value "LUA_VERSION_NUM" int))
  (define LUA_VERSION_RELEASE_NUM (foreign-value "LUA_VERSION_RELEASE_NUM" int))
  (define LUA_VERSION_MAJOR_N (foreign-value "LUA_VERSION_MAJOR_N" int))
  (define LUA_VERSION_MINOR_N (foreign-value "LUA_VERSION_MINOR_N" int))
  (define LUA_VERSION_RELEASE_N (foreign-value "LUA_VERSION_RELEASE_N" int))
  (define LUA_VERSION (foreign-value "LUA_VERSION" c-string))
  (define LUA_VERSION_MAJOR (foreign-value "LUA_VERSION_MAJOR" c-string))
  (define LUA_VERSION_MINOR (foreign-value "LUA_VERSION_MINOR" c-string))
  (define LUA_VERSION_RELEASE (foreign-value "LUA_VERSION_RELEASE" c-string))
  (define LUA_RELEASE (foreign-value "LUA_RELEASE" c-string))
  (define LUA_COPYRIGHT (foreign-value "LUA_COPYRIGHT" c-string))
  (define LUA_AUTHORS (foreign-value "LUA_AUTHORS" c-string))

  ;; stack and registry
  (define LUA_MINSTACK (foreign-value "LUA_MINSTACK" int))
  (define LUA_RIDX_LAST (foreign-value "LUA_RIDX_LAST" int))

  ;; integers
  (define LUA_MAXINTEGER (foreign-value "LUA_MAXINTEGER" integer64))
  (define LUA_MININTEGER (foreign-value "LUA_MININTEGER" integer64))

  ;; lua_arith / lua_compare operators
  (define LUA_OPADD (foreign-value "LUA_OPADD" int))
  (define LUA_OPSUB (foreign-value "LUA_OPSUB" int))
  (define LUA_OPMUL (foreign-value "LUA_OPMUL" int))
  (define LUA_OPMOD (foreign-value "LUA_OPMOD" int))
  (define LUA_OPPOW (foreign-value "LUA_OPPOW" int))
  (define LUA_OPDIV (foreign-value "LUA_OPDIV" int))
  (define LUA_OPIDIV (foreign-value "LUA_OPIDIV" int))
  (define LUA_OPBAND (foreign-value "LUA_OPBAND" int))
  (define LUA_OPBOR (foreign-value "LUA_OPBOR" int))
  (define LUA_OPBXOR (foreign-value "LUA_OPBXOR" int))
  (define LUA_OPSHL (foreign-value "LUA_OPSHL" int))
  (define LUA_OPSHR (foreign-value "LUA_OPSHR" int))
  (define LUA_OPUNM (foreign-value "LUA_OPUNM" int))
  (define LUA_OPBNOT (foreign-value "LUA_OPBNOT" int))
  (define LUA_OPEQ (foreign-value "LUA_OPEQ" int))
  (define LUA_OPLT (foreign-value "LUA_OPLT" int))
  (define LUA_OPLE (foreign-value "LUA_OPLE" int))

  ;; lua_gc options and parameters
  (define LUA_GCSTOP (foreign-value "LUA_GCSTOP" int))
  (define LUA_GCRESTART (foreign-value "LUA_GCRESTART" int))
  (define LUA_GCCOLLECT (foreign-value "LUA_GCCOLLECT" int))
  (define LUA_GCCOUNT (foreign-value "LUA_GCCOUNT" int))
  (define LUA_GCCOUNTB (foreign-value "LUA_GCCOUNTB" int))
  (define LUA_GCSTEP (foreign-value "LUA_GCSTEP" int))
  (define LUA_GCISRUNNING (foreign-value "LUA_GCISRUNNING" int))
  (define LUA_GCGEN (foreign-value "LUA_GCGEN" int))
  (define LUA_GCINC (foreign-value "LUA_GCINC" int))
  (define LUA_GCPARAM (foreign-value "LUA_GCPARAM" int))
  (define LUA_GCPMINORMUL (foreign-value "LUA_GCPMINORMUL" int))
  (define LUA_GCPMAJORMINOR (foreign-value "LUA_GCPMAJORMINOR" int))
  (define LUA_GCPMINORMAJOR (foreign-value "LUA_GCPMINORMAJOR" int))
  (define LUA_GCPPAUSE (foreign-value "LUA_GCPPAUSE" int))
  (define LUA_GCPSTEPMUL (foreign-value "LUA_GCPSTEPMUL" int))
  (define LUA_GCPSTEPSIZE (foreign-value "LUA_GCPSTEPSIZE" int))
  (define LUA_GCPN (foreign-value "LUA_GCPN" int))

  ;; debug hooks
  (define LUA_HOOKCALL (foreign-value "LUA_HOOKCALL" int))
  (define LUA_HOOKRET (foreign-value "LUA_HOOKRET" int))
  (define LUA_HOOKLINE (foreign-value "LUA_HOOKLINE" int))
  (define LUA_HOOKCOUNT (foreign-value "LUA_HOOKCOUNT" int))
  (define LUA_HOOKTAILCALL (foreign-value "LUA_HOOKTAILCALL" int))
  (define LUA_MASKCALL (foreign-value "LUA_MASKCALL" int))
  (define LUA_MASKRET (foreign-value "LUA_MASKRET" int))
  (define LUA_MASKLINE (foreign-value "LUA_MASKLINE" int))
  (define LUA_MASKCOUNT (foreign-value "LUA_MASKCOUNT" int))

  ;; miscellaneous
  (define LUA_N2SBUFFSZ (foreign-value "LUA_N2SBUFFSZ" int))
  (define LUA_GNAME (foreign-value "LUA_GNAME" c-string))
  (define LUA_LOADED_TABLE (foreign-value "LUA_LOADED_TABLE" c-string))
  (define LUA_PRELOAD_TABLE (foreign-value "LUA_PRELOAD_TABLE" c-string))
  (define LUA_FILEHANDLE (foreign-value "LUA_FILEHANDLE" c-string))
  (define LUA_SIGNATURE (foreign-value "LUA_SIGNATURE" c-string))     ; "\x1bLua": the first bytes of a binary chunk
  (define LUA_VERSUFFIX (foreign-value "LUA_VERSUFFIX" c-string))     ; "_5_5": as in LUA_PATH_5_5, LUA_INIT_5_5

  ;; standard libraries: luaL_openselectedlibs masks and module names
  (define LUA_GLIBK (foreign-value "LUA_GLIBK" int))
  (define LUA_LOADLIBK (foreign-value "LUA_LOADLIBK" int))
  (define LUA_COLIBK (foreign-value "LUA_COLIBK" int))
  (define LUA_DBLIBK (foreign-value "LUA_DBLIBK" int))
  (define LUA_IOLIBK (foreign-value "LUA_IOLIBK" int))
  (define LUA_MATHLIBK (foreign-value "LUA_MATHLIBK" int))
  (define LUA_OSLIBK (foreign-value "LUA_OSLIBK" int))
  (define LUA_STRLIBK (foreign-value "LUA_STRLIBK" int))
  (define LUA_TABLIBK (foreign-value "LUA_TABLIBK" int))
  (define LUA_UTF8LIBK (foreign-value "LUA_UTF8LIBK" int))
  (define LUA_LOADLIBNAME (foreign-value "LUA_LOADLIBNAME" c-string))
  (define LUA_COLIBNAME (foreign-value "LUA_COLIBNAME" c-string))
  (define LUA_DBLIBNAME (foreign-value "LUA_DBLIBNAME" c-string))
  (define LUA_IOLIBNAME (foreign-value "LUA_IOLIBNAME" c-string))
  (define LUA_MATHLIBNAME (foreign-value "LUA_MATHLIBNAME" c-string))
  (define LUA_OSLIBNAME (foreign-value "LUA_OSLIBNAME" c-string))
  (define LUA_STRLIBNAME (foreign-value "LUA_STRLIBNAME" c-string))
  (define LUA_TABLIBNAME (foreign-value "LUA_TABLIBNAME" c-string))
  (define LUA_UTF8LIBNAME (foreign-value "LUA_UTF8LIBNAME" c-string))

  ;; ---------------------------------------------------------------- index manipulation (D)

  (define %s1-absindex (foreign-lambda int "aux_s1_absindex" (c-pointer "lua_State") int))
  (define %s1-rotate (foreign-lambda int "aux_s1_rotate" (c-pointer "lua_State") int int int))
  (define %s1-copy (foreign-lambda int "aux_s1_copy" (c-pointer "lua_State") int int int int))
  (define %s1-remove (foreign-lambda int "aux_s1_remove" (c-pointer "lua_State") int int))
  (define %s1-replace (foreign-lambda int "aux_s1_replace" (c-pointer "lua_State") int int int))
  (define %s1-checkstack (foreign-lambda int "aux_s1_checkstack" (c-pointer "lua_State") int))
  (define %s1-xmove (foreign-lambda int "aux_s1_xmove" (c-pointer "lua_State") (c-pointer "lua_State") int int int))
  ;; the anchor id of a luaR for the C side (aux_nk_touch), -1 when it has none
  (define-inline (%anchor-id x) (let ((id (##sys#slot x 4))) (if (fixnum? id) id -1)))
  (define %s1-type (foreign-lambda int "aux_s1_type" (c-pointer "lua_State") int))
  (define %s1-is (foreign-lambda int "aux_s1_is" (c-pointer "lua_State") int int))

  ;; ((lua-absindex idx) L) => the absolute index of an acceptable index: a positive idx as it is
  ;; (above the top too, as C), a negative one counted from the top (it must be a stack slot:
  ;; -top <= idx <= -1), a pseudo-index unchanged; 0 and other negatives are refused
  (define ((lua-absindex idx) L)
    (%int! 'lua-absindex idx)
    (let* ((p (%L 'lua-absindex L)) (r (%D 'lua-absindex (%s1-absindex p idx) idx)))
      (if (fx= r 0) idx r)))

  ;; ((lua-rotate idx n) L): rotates the slots from idx to the top n positions towards the top
  ;; (n < 0: towards idx); idx a real stack slot, |n| <= its distance from the top + 1
  (define ((lua-rotate idx n) L)
    (%int! 'lua-rotate idx n)
    (let ((p (%L 'lua-rotate L)))
      (%D 'lua-rotate (%s1-rotate p idx n (luaR-floor L)) idx n)
      (void)))

  ;; ((lua-insert idx) L): moves the top value into idx, shifting the values above it up
  (define ((lua-insert idx) L)
    (%int! 'lua-insert idx)
    (let ((p (%L 'lua-insert L)))
      (%D 'lua-insert (%s1-rotate p idx 1 (luaR-floor L)) idx)
      (void)))

  ;; ((lua-copy from to) L): the value at from (any readable index) replaces the one at to (a
  ;; stack slot, or a user upvalue (lua-upvalueindex i) of the running Scheme closure).  Inside a
  ;; hook, the slots of the hooked function (up to the hook view's floor) are refused, whatever
  ;; luaR of that thread is used: the VM relies on the types of some of them.  So is any slot of a
  ;; thread suspended (by a hook) or dead in a Lua function, as with lua-replace: its slots are
  ;; that function's registers.
  (define ((lua-copy from to) L)
    (%int! 'lua-copy from to)
    (let ((p (%L 'lua-copy L)))
      (%D 'lua-copy (%s1-copy p from to (luaR-nup L) (luaR-floor L)) from to)
      (void)))

  ;; ((lua-remove idx) L): removes the value at idx, shifting the values above it down
  (define ((lua-remove idx) L)
    (%int! 'lua-remove idx)
    (let ((p (%L 'lua-remove L)))
      (%D 'lua-remove (%s1-remove p idx (luaR-floor L)) idx)
      (void)))

  ;; ((lua-replace idx) L): the top value replaces the one at idx (a WRITE index, as lua-copy's to),
  ;; and is popped
  (define ((lua-replace idx) L)
    (%int! 'lua-replace idx)
    (let ((p (%L 'lua-replace L)))
      (%D 'lua-replace (%s1-replace p idx (luaR-nup L) (luaR-floor L)) idx)
      (void)))

  ;; ((lua-checkstack n) L) => #t when the stack can take n more values (it may grow), else #f
  (define ((lua-checkstack n) L)
    (%int! 'lua-checkstack n)
    (let ((p (%L 'lua-checkstack L)))
      (fx= 1 (%D 'lua-checkstack (%s1-checkstack p n) n))))

  ;; ((lua-xmove to n) from): pops n values from `from' and pushes them, in order, onto `to';
  ;; both threads of the same universe (C's first lua_State, `from', is the curried argument).
  ;; From a thread suspended by a hook, or dead in a Lua function, only what lies above the height
  ;; it had then (the values pushed since, the error value lua-resume left): the rest is that
  ;; function's registers; more is refused with (exn lua api), code AUX_ETHREAD.
  (define ((lua-xmove to n) from)
    (%int! 'lua-xmove n)
    (let* ((p (%L 'lua-xmove from)) (q (%L 'lua-xmove to)))
      (unless (eq? (luaR-u from) (luaR-u to))
        (%api-error 'lua-xmove "moving among independent states" (list to n)))
      (%D 'lua-xmove (%s1-xmove p q n (luaR-floor from) (%anchor-id from)) to n)
      (void)))

  ;; ---------------------------------------------------------------- type queries (D)

  ;; ((lua-typename tp) L) => the name of type code tp (LUA_TNONE .. LUA_TTHREAD)
  (define ((lua-typename tp) L)
    (let ((p (%L 'lua-typename L)))
      (unless (and (fixnum? tp) (fx>= tp LUA_TNONE) (fx< tp LUA_NUMTYPES))
        (%api-error 'lua-typename "not a type code" (list tp) AUX_ERANGE))
      (%done (%typename p tp))))

  ;; (define-lua-is (name which) ...): ((name idx) L) => #t / #f through aux_s1_is
  (define-syntax define-lua-is
    (syntax-rules ()
      ((_ (name which) ...)
       (begin (define ((name idx) L)
                (%int! 'name idx)
                (let ((p (%L 'name L)))
                  (fx= 1 (%D 'name (%s1-is p idx (foreign-value which int)) idx))))
              ...))))

  (define-lua-is
    (lua-isnumber "AUX_S1_ISNUMBER")          ; a number, or a string convertible to one
    (lua-isstring "AUX_S1_ISSTRING")          ; a string or a number
    (lua-iscfunction "AUX_S1_ISCFUNCTION")    ; a C function (Scheme closures included)
    (lua-isinteger "AUX_S1_ISINTEGER")        ; a number with an integer representation (no strings)
    (lua-isuserdata "AUX_S1_ISUSERDATA"))     ; a full or light userdata

  ;; (define-lua-type-is (name test) ...): ((name idx) L) => #t / #f, `test' on the type code
  (define-syntax define-lua-type-is
    (syntax-rules ()
      ((_ (name test) ...)
       (begin (define ((name idx) L)
                (%int! 'name idx)
                (let ((p (%L 'name L)))
                  (test (%D 'name (%s1-type p idx) idx))))
              ...))))

  (define-lua-type-is
    (lua-isfunction (λ (t) (fx= t LUA_TFUNCTION)))
    (lua-istable (λ (t) (fx= t LUA_TTABLE)))
    (lua-islightuserdata (λ (t) (fx= t LUA_TLIGHTUSERDATA)))
    (lua-isnil (λ (t) (fx= t LUA_TNIL)))
    (lua-isboolean (λ (t) (fx= t LUA_TBOOLEAN)))
    (lua-isthread (λ (t) (fx= t LUA_TTHREAD)))
    (lua-isnone (λ (t) (fx= t LUA_TNONE)))
    (lua-isnoneornil (λ (t) (fx<= t LUA_TNIL))))

  ;; ==========================================================================================
  ;; S2 — reading values (lua_to*, rawlen, rawequal, numbertocstring), pushing values
  ;; (pushfstring, pushliteral, pushlightuserdata, pushthread, stringtonumber) and the operators
  ;; (arith, compare, concat, len).  The core already binds pushnil/number/integer/boolean,
  ;; pushlstring/pushstring, to{integer,number}[x], toboolean, tolstring/tostring.
  ;; Omitted (X): lua_pushexternalstring (it needs memory that stays put until Lua calls its
  ;; `falloc'; Scheme memory moves, so a malloc'd copy would just be lua-pushlstring) and
  ;; lua_pushvfstring (a va_list cannot be built from Scheme).
  ;; ==========================================================================================

  #>
  /* S2 protected op bodies: operands are the stack slots 1..n of the protected frame (copies) */
  static int op_s2_arith(lua_State *L, aux_op *o) { lua_arith(L, o->i0); return 1; }          /* ntop 2 (1 for unm/bnot), +1 */
  static int op_s2_compare(lua_State *L, aux_op *o) { o->r = lua_compare(L, 1, 2, o->i0); return 0; }   /* refs [i1 i2] */
  static int op_s2_concat(lua_State *L, aux_op *o) { lua_concat(L, o->i0); return 1; }        /* ntop n, +1 */
  static int op_s2_len(lua_State *L, aux_op *o) { lua_len(L, 1); return 1; }                  /* refs [idx], +1 */
  /* arg 1: the thread value (a copy), anchored as i0 (see aux_anchor_set) */
  static int op_s2_tothread(lua_State *L, aux_op *o) {
    if (lua_type(L, 1) != LUA_TTHREAD) return luaL_error(L, "aux.lua: not a thread");   /* a call hook's doing */
    aux_anchor_set(L, 1, o->i0, o->L1); return 0; }                 /* L1: the thread the binding read */
  <#

  ;; query-index check: #t for a valid index, #f for a positive index above the top (the answer is
  ;; the default, without calling Lua), (exn lua api) otherwise
  (define %s2-qix (foreign-lambda int "aux_qix" (c-pointer "lua_State") int))
  (define-inline (%s2-query who p idx)
    (let ((q (%s2-qix p idx)))
      (when (fx< q 0) (%api-fail who AUX_EINDEX (list idx)))
      (fx= q 1)))

  ;; ---------------------------------------------------------------- D queries

  (define %rawlen-raw (foreign-lambda unsigned-integer64 "lua_rawlen" (c-pointer "lua_State") int))
  (define %tocfunction-raw (foreign-lambda* c-pointer (((c-pointer "lua_State") L) (int idx)) "C_return(aux_tocfunction(L, idx));"))
  (define %topointer-raw (foreign-lambda* c-pointer (((c-pointer "lua_State") L) (int idx)) "C_return((void *)lua_topointer(L, idx));"))
  (define %rawequal-raw (foreign-lambda bool "lua_rawequal" (c-pointer "lua_State") int int))

  ;; ((lua-rawlen idx) L) => the raw length of a string, userdata or table (no __len), 0 otherwise
  (define (%rawlen who idx L)
    (%int! who idx)
    (let ((p (%L who L)))
      (%done (if (%s2-query who p idx) (%rawlen-raw p idx) 0))))
  (define ((lua-rawlen idx) L) (%rawlen 'lua-rawlen idx L))
  ;; ((lua-strlen idx) L), ((lua-objlen idx) L): luaconf.h's compatibility macros (Lua 5.1's names),
  ;; both lua_rawlen (K)
  (define ((lua-strlen idx) L) (%rawlen 'lua-strlen idx L))
  (define ((lua-objlen idx) L) (%rawlen 'lua-objlen idx L))

  ;; ((lua-tocfunction idx) L) => the C function pointer of a C function or C closure, else #f.
  ;; A Scheme procedure pushed with lua-pushcclosure answers the module's trampoline: use
  ;; lua-toprocedure to get the procedure back.  The function of a LIGHT C function becomes
  ;; pushable (lua-pushcfunction, luaL-setfuncs, ...: see aux_cf_vouched); a C closure's function
  ;; needs its upvalues, and is refused there.
  (define ((lua-tocfunction idx) L)
    (%int! 'lua-tocfunction idx)
    (let ((p (%L 'lua-tocfunction L)))
      (%done (and (%s2-query 'lua-tocfunction p idx) (%tocfunction-raw p idx)))))

  ;; ((lua-touserdata idx) L) => the block address of a full userdata, the pointer of a light
  ;; userdata, else #f.  A full userdata's memory belongs to Lua: valid while the userdata is reachable.
  (define ((lua-touserdata idx) L)
    (%int! 'lua-touserdata idx)
    (let ((p (%L 'lua-touserdata L)))
      (%done (and (%s2-query 'lua-touserdata p idx) (%touserdata-raw p idx)))))

  ;; ((lua-topointer idx) L) => a pointer identifying the value (tables, functions, threads,
  ;; userdata, strings), #f for values that are not objects.  Only for identity/hashing.
  (define ((lua-topointer idx) L)
    (%int! 'lua-topointer idx)
    (let ((p (%L 'lua-topointer L)))
      (%done (and (%s2-query 'lua-topointer p idx) (%topointer-raw p idx)))))

  ;; ((lua-rawequal i1 i2) L) => #t when the two values are primitively equal (no __eq); #f when an
  ;; index is a positive one above the top (C's "not valid"); other invalid indices raise (exn lua api)
  (define ((lua-rawequal i1 i2) L)
    (%int! 'lua-rawequal i1 i2)
    (let ((p (%L 'lua-rawequal L)))
      (%done (and (%s2-query 'lua-rawequal p i1) (%s2-query 'lua-rawequal p i2) (%rawequal-raw p i1 i2)))))

  ;; ((lua-numbertocstring idx) L) => the number at idx as Lua's tostring writes it, #f when the
  ;; value is not a number (strings included, as C).  The slot is not converted.
  (define ((lua-numbertocstring idx) L)
    (%int! 'lua-numbertocstring idx)
    (let ((p (%L 'lua-numbertocstring L)))
      (%done (and (%s2-query 'lua-numbertocstring p idx)
                  (fx= LUA_TNUMBER (%type-raw p idx))
                  (%bytes->string/maybe (%bytes-at 'lua-numbertocstring p idx))))))

  ;; ---------------------------------------------------------------- D pushes

  (define %stringtonumber (foreign-lambda* integer64 (((c-pointer "lua_State") L) (scheme-object s))
                            "size_t n, r; const char *b = aux_bytes(s, &n); char *c = NULL;
                             if (memchr(b, 0, n)) C_return(0);                    /* C would stop at the NUL */
                             if (!lua_checkstack(L, 1)) C_return(AUX_ESTACK);
                             if (C_header_bits(s) == C_BYTEVECTOR_TYPE) {         /* no NUL terminator */
                               if (!(c = aux_dup(b, n))) C_return(AUX_ENOMEM);
                               b = c; }
                             r = lua_stringtonumber(L, b);                        /* D: no allocation, no raise */
                             free(c); C_return((C_s64)r);"))
  ;; ((lua-stringtonumber s) L) => the size of s plus one when s (a string or bytevector) is a Lua
  ;; numeral, the number being pushed; 0 when it is not (nothing is pushed).  A NUL inside s: 0.
  (define ((lua-stringtonumber s) L)
    (%bytes! 'lua-stringtonumber s)
    (%D 'lua-stringtonumber (%stringtonumber (%L 'lua-stringtonumber L) s) s))

  ;; ((lua-pushlightuserdata p) L): p a c-pointer, or #f for NULL
  (define ((lua-pushlightuserdata ptr) L)
    (unless (or (not ptr) (pointer? ptr)) (%api-error 'lua-pushlightuserdata "not a pointer" (list ptr)))
    (%D 'lua-pushlightuserdata (%pushlightuserdata-raw (%L 'lua-pushlightuserdata L) ptr) ptr)
    (void))

  (define %pushthread (foreign-lambda* int (((c-pointer "lua_State") L))
                        "if (!lua_checkstack(L, 1)) C_return(AUX_ESTACK); C_return(lua_pushthread(L));"))
  ;; ((lua-pushthread) L) => #t when L's thread is the main thread; the thread is pushed onto itself
  (define lua-pushthread
    (let ((k (λ (L) (fx= 1 (%D 'lua-pushthread (%pushthread (%L 'lua-pushthread L)))))))
      (λ () k)))

  ;; ---------------------------------------------------------------- K pushes

  ;; ((lua-pushfstring fmt . args) L) => the formatted text (a string, or a bytevector when it is not
  ;; UTF-8), also pushed.  Lua's directives %% %s %f %I %p %d %c %U, formatted in Scheme; an unknown
  ;; directive is kept as C 5.5 does, a missing or ill-typed argument raises (exn lua api).
  (define ((lua-pushfstring fmt . args) L)
    (%bytes! 'lua-pushfstring fmt)
    (%V 'lua-pushfstring L)
    (let ((bv (%lua-format 'lua-pushfstring fmt args)))
      ((lua-pushlstring bv) L)
      (%bytes->string/maybe bv)))

  ;; ((lua-pushliteral s) L): lua_pushstring (the C macro pastes a literal)
  (define ((lua-pushliteral s) L) ((lua-pushstring s) L))

  ;; ---------------------------------------------------------------- P: threads

  (define-lua-stubs (%tothread/f %tothread/s) ((int idx) (int id))
    "aux_op o = {0}; int refs[1], r, q = aux_qix(L, idx); lua_State *M;
     if (q < 0) C_return(AUX_EINDEX);
     if (q == 0 || lua_type(L, idx) != LUA_TTHREAD) C_return(-1);
     o.fn = op_s2_tothread; o.i0 = id; o.L1 = lua_tothread(L, idx);
     if (lua_status(L) == LUA_OK) { refs[0] = idx; r = aux_run(safe, L, &o, 1, refs, 0, 0); }
     else {                                   /* a suspended or dead thread: anchor it from the main thread */
       M = AUX_CTX(L)->main;
       if (!lua_checkstack(L, 1) || !lua_checkstack(M, 1)) C_return(AUX_ESTACK);
       lua_pushvalue(L, idx); lua_xmove(L, M, 1);         /* D: xmove lowers L without closing anything */
       r = aux_run(safe, M, &o, 0, NULL, 1, 0);
       if (AUX_IS_LUAERR(r)) r = AUX_LUAERR_M(-r - 1000); }
     C_return(r);")

  ;; ((lua-tothread idx) L) => a luaR (kind 'thread) for the thread at idx, else #f.  The thread is
  ;; anchored in a private table of the universe for as long as the luaR lives, and every binding
  ;; checks the anchor first: should Lua code (or the bound API on LUA_REGISTRYINDEX) remove it, the
  ;; luaR is refused with (exn lua api), never used as a dangling pointer.  The luaR keeps the
  ;; universe's main luaR alive.
  (define ((lua-tothread idx) L)
    (%int! 'lua-tothread idx)
    (let* ((u (%U 'lua-tothread L)) (id (%anchor-alloc! u)))
      (if (fx>= (%P 'lua-tothread L (%tothread/f %tothread/s) idx id) 0)
        (%make-thread-luaR 'lua-tothread u id)
        (begin (luaU-afree-set! u (cons id (luaU-afree u))) #f))))

  ;; ---------------------------------------------------------------- P: operators

  (define-lua-stubs (%arith/f %arith/s) ((int op))
    "aux_op o = {0}; o.fn = op_s2_arith; o.i0 = op;
     if (op < LUA_OPADD || op > LUA_OPBNOT) C_return(AUX_ERANGE);
     C_return(aux_run(safe, L, &o, 0, NULL, (op == LUA_OPUNM || op == LUA_OPBNOT) ? 1 : 2, 1));")
  ;; ((lua-arith op) L): the two values on top (one for LUA_OPUNM / LUA_OPBNOT) are replaced by
  ;; the result of the operation, metamethods included.  A Lua error consumes the operands.
  (define ((lua-arith op) L)
    (%int! 'lua-arith op)
    (%P 'lua-arith L (%arith/f %arith/s) op)
    (void))

  (define-lua-stubs (%compare/f %compare/s) ((int i1) (int i2) (int op))
    "aux_op o = {0}; int refs[2], q1 = aux_qix(L, i1), q2 = aux_qix(L, i2);
     if (q1 < 0 || q2 < 0) C_return(AUX_EINDEX);
     if (op < LUA_OPEQ || op > LUA_OPLE) C_return(AUX_ERANGE);
     if (q1 == 0 || q2 == 0) C_return(0);                 /* above the top: not valid in C, 0 without calling */
     if (lua_type(L, i1) == LUA_TNONE || lua_type(L, i2) == LUA_TNONE) C_return(0);   /* e.g. a missing upvalue */
     o.fn = op_s2_compare; o.i0 = op; refs[0] = i1; refs[1] = i2;
     C_return(aux_run(safe, L, &o, 2, refs, 0, 0));")
  ;; ((lua-compare i1 i2 op) L) => #t when value i1 op value i2 (LUA_OPEQ, LUA_OPLT, LUA_OPLE),
  ;; metamethods included; #f when an index is a positive one above the top.  Nothing is popped.
  (define ((lua-compare i1 i2 op) L)
    (%int! 'lua-compare i1 i2 op)
    (fx= 1 (%P 'lua-compare L (%compare/f %compare/s) i1 i2 op)))
  ;; ((lua-equal i1 i2) L) = ((lua-compare i1 i2 LUA_OPEQ) L), ((lua-lessthan i1 i2) L) =
  ;; ((lua-compare i1 i2 LUA_OPLT) L): luaconf.h's compatibility macros (Lua 5.1's names) (K)
  (define ((lua-equal i1 i2) L)
    (%int! 'lua-equal i1 i2)
    (fx= 1 (%P 'lua-equal L (%compare/f %compare/s) i1 i2 LUA_OPEQ)))
  (define ((lua-lessthan i1 i2) L)
    (%int! 'lua-lessthan i1 i2)
    (fx= 1 (%P 'lua-lessthan L (%compare/f %compare/s) i1 i2 LUA_OPLT)))

  (define-lua-stubs (%concat/f %concat/s) ((int n))
    "aux_op o = {0}; o.fn = op_s2_concat; o.i0 = n;
     if (n < 0) C_return(AUX_ERANGE);
     C_return(aux_run(safe, L, &o, 0, NULL, n, 1));")
  ;; ((lua-concat n) L): the n values on top are replaced by their concatenation (__concat
  ;; included); n = 1 leaves the value, n = 0 pushes "".
  (define ((lua-concat n) L)
    (%int! 'lua-concat n)
    (%P 'lua-concat L (%concat/f %concat/s) n)
    (void))

  (define-lua-stubs (%len/f %len/s) ((int idx))
    "aux_op o = {0}; int refs[1]; o.fn = op_s2_len; refs[0] = idx;
     if (!aux_ix(L, idx, AUX_IX_READ, 0)) C_return(AUX_EINDEX);
     C_return(aux_run(safe, L, &o, 1, refs, 0, 1));")
  ;; ((lua-len idx) L): pushes the length of the value at idx, as the # operator (__len included)
  (define ((lua-len idx) L)
    (%int! 'lua-len idx)
    (%P 'lua-len L (%len/f %len/s) idx)
    (void))

  ;; ==========================================================================================
  ;; S3 — tables, userdata, metatables, globals
  ;;
  ;;   D  (direct, validated in C): lua-rawget lua-rawgeti lua-rawgetp lua-getmetatable
  ;;      lua-getiuservalue lua-getuservalue lua-setmetatable lua-setiuservalue lua-setuservalue
  ;;      lua-pushglobaltable
  ;;   P  (protected, may run metamethods / allocate / raise): lua-gettable lua-geti lua-settable
  ;;      lua-seti lua-rawset lua-rawseti lua-rawsetp lua-next
  ;;   P* (push-only, also on suspended/dead threads): lua-createtable lua-newuserdatauv
  ;;   K  lua-newtable lua-newuserdata lua-register
  ;;   (lua-getfield lua-setfield lua-getglobal lua-setglobal are in the core.)
  ;;
  ;; Index rules of this section: an index is a stack slot, LUA_REGISTRYINDEX, or -- only inside a
  ;; Scheme closure (a view, nup >= 0) -- an upvalue pseudo-index.  Everywhere else an upvalue
  ;; pseudo-index is refused: Lua (index2value) requires the running function to be a C function.
  ;; A binding that consumes stack values never lowers the top below a hook view's floor, and,
  ;; like lua-settop, refuses to lower a non-OK thread whose current frame is a Lua function.
  ;; ==========================================================================================

  #>
  /* S3: an index read by this section; upvalue pseudo-indices only in a Scheme closure (nup >= 0) */
  static int aux_s3_ix(lua_State *L, int idx, int nup) {
    if (idx < LUA_REGISTRYINDEX && nup < 0) return 0;
    return aux_ix(L, idx, AUX_IX_READ, 0); }
  /* 0 when idx holds a value of type t, else the refusal code */
  static int aux_s3_typed(lua_State *L, int idx, int nup, int t) {
    if (!aux_s3_ix(L, idx, nup)) return AUX_EINDEX;
    return lua_type(L, idx) == t ? 0 : AUX_ETYPE; }
  /* 0 when the n top values may be consumed (popped), else the refusal code */
  static int aux_s3_consume(lua_State *L, int n, int floor) {
    int top = lua_gettop(L);
    floor = aux_eff_floor(L, floor);
    if (n > top) return AUX_EUNDER;
    if (top - n < floor) return AUX_EINDEX;
    if (lua_status(L) != LUA_OK && aux_in_lua_frame(L)) return AUX_ETHREAD;
    return 0; }

  /* P op bodies: operands are the protected frame's slots 1..n (refs first, then consumed values) */
  static int op_s3_gettable(lua_State *L, aux_op *o) { o->r = lua_gettable(L, 1); return 1; }        /* t k */
  static int op_s3_geti(lua_State *L, aux_op *o) { o->r = lua_geti(L, 1, o->n0); return 1; }         /* t */
  static int op_s3_settable(lua_State *L, aux_op *o) {                                                /* t k v */
    aux_globals_write_check(L, 1, 2, 0, 3); aux_libwrite_check(L, 1, 2, 3); lua_settable(L, 1); return 0; }
  static int op_s3_seti(lua_State *L, aux_op *o) { aux_globals_write_check(L, 1, 0, o->n0, 2); lua_seti(L, 1, o->n0); return 0; }   /* t v */
  /* the raw ops need a table at 1: it was checked on the original, and is checked again on the copy (a call
     hook runs between the two and may rewrite the trampoline's slots with debug.setlocal) */
  static int op_s3_rawset(lua_State *L, aux_op *o) {                                                  /* t k v */
    luaL_checktype(L, 1, LUA_TTABLE); aux_globals_write_check(L, 1, 2, 0, 3); aux_libwrite_check(L, 1, 2, 3); lua_rawset(L, 1); return 0; }
  static int op_s3_rawseti(lua_State *L, aux_op *o) {                                                 /* t v */
    luaL_checktype(L, 1, LUA_TTABLE); aux_globals_write_check(L, 1, 0, o->n0, 2); lua_rawseti(L, 1, o->n0); return 0; }
  static int op_s3_rawsetp(lua_State *L, aux_op *o) { luaL_checktype(L, 1, LUA_TTABLE); lua_rawsetp(L, 1, o->p0); return 0; }   /* t v */
  static int op_s3_next(lua_State *L, aux_op *o) { luaL_checktype(L, 1, LUA_TTABLE); o->r = lua_next(L, 1); return o->r ? 2 : 0; }   /* t k; MULTRET */
  static int op_s3_createtable(lua_State *L, aux_op *o) { lua_createtable(L, o->i0, o->i1); return 1; }
  static int op_s3_newuserdatauv(lua_State *L, aux_op *o) { lua_newuserdatauv(L, o->z0, o->i0); return 1; }
  <#

  ;; (%s3-P who L (fast safe) (shown ...) arg ...): the core's %P, with the condition's
  ;; `arguments' (shown ...) kept apart from the extra state-derived stub arguments (nup, floor)
  (define-syntax %s3-P
    (syntax-rules ()
      ((_ who L (fast safe) (shown ...) arg ...)
       (let* ((l L) (u (%U who l)))
         (%prepare! u)
         (let* ((p (%L who l)) (r (%run u (fast safe) p arg ...)))
           (if (and (fx> r -1000) (not %pending-escape))
             (begin (set! %busy-ctx #f) r)
             (%finish who u p r (list shown ...))))))))

  (define (%s3-integer! who n)
    (unless (%int64? n) (%api-error who "not a lua_Integer" (list n) AUX_ERANGE)))
  (define (%s3-pointer! who x)
    (unless (or (not x) (pointer? x)) (%api-error who "not a pointer or #f" (list x))))

  ;; ---------------------------------------------------------------- P: reads through metamethods

  (define-lua-stubs (%s3-gettable/f %s3-gettable/s) ((int idx) (int nup) (int floor))
    "aux_op o = {0}; int refs[1], c; o.fn = op_s3_gettable; refs[0] = idx;
     if (!aux_s3_ix(L, idx, nup)) C_return(AUX_EINDEX);
     if ((c = aux_s3_consume(L, 1, floor))) C_return(c);
     C_return(aux_run(safe, L, &o, 1, refs, 1, 1));")
  ;; ((lua-gettable idx) L) => the type of t[k] pushed, t at idx and k on top (popped); may run __index
  (define ((lua-gettable idx) L)
    (%int! 'lua-gettable idx)
    (%s3-P 'lua-gettable L (%s3-gettable/f %s3-gettable/s) (idx) idx (luaR-nup L) (luaR-floor L)))

  (define-lua-stubs (%s3-geti/f %s3-geti/s) ((int idx) (integer64 n) (int nup))
    "aux_op o = {0}; int refs[1]; o.fn = op_s3_geti; o.n0 = n; refs[0] = idx;
     if (!aux_s3_ix(L, idx, nup)) C_return(AUX_EINDEX);
     C_return(aux_run(safe, L, &o, 1, refs, 0, 1));")
  ;; ((lua-geti idx i) L) => the type of t[i] pushed; may run __index
  (define ((lua-geti idx i) L)
    (%int! 'lua-geti idx) (%s3-integer! 'lua-geti i)
    (%s3-P 'lua-geti L (%s3-geti/f %s3-geti/s) (idx i) idx i (luaR-nup L)))

  ;; ---------------------------------------------------------------- P: writes

  (define-lua-stubs (%s3-settable/f %s3-settable/s) ((int idx) (int nup) (int floor))
    "aux_op o = {0}; int refs[1], c; o.fn = op_s3_settable; refs[0] = idx;
     if (!aux_s3_ix(L, idx, nup)) C_return(AUX_EINDEX);
     if ((c = aux_s3_consume(L, 2, floor))) C_return(c);
     if (aux_globals_write_bad(L, idx, -2, 0, -1)) C_return(AUX_EGLOBALS);
     if ((c = aux_libwrite_stub(safe, L, idx, aux_libname_at(L, -2), -1, NULL, 0, NULL))) C_return(c);
     C_return(aux_run(safe, L, &o, 1, refs, 2, 0));")
  ;; ((lua-settable idx) L): t[k] = v, t at idx, k and v on top (both popped); may run __newindex
  (define ((lua-settable idx) L)
    (%int! 'lua-settable idx)
    (%s3-P 'lua-settable L (%s3-settable/f %s3-settable/s) (idx) idx (luaR-nup L) (luaR-floor L))
    (void))

  (define-lua-stubs (%s3-seti/f %s3-seti/s) ((int idx) (integer64 n) (int nup) (int floor))
    "aux_op o = {0}; int refs[1], c; o.fn = op_s3_seti; o.n0 = n; refs[0] = idx;
     if (!aux_s3_ix(L, idx, nup)) C_return(AUX_EINDEX);
     if ((c = aux_s3_consume(L, 1, floor))) C_return(c);
     if (aux_globals_write_bad(L, idx, 0, n, -1)) C_return(AUX_EGLOBALS);
     C_return(aux_run(safe, L, &o, 1, refs, 1, 0));")
  ;; ((lua-seti idx i) L): t[i] = the value on top (popped); may run __newindex
  (define ((lua-seti idx i) L)
    (%int! 'lua-seti idx) (%s3-integer! 'lua-seti i)
    (%s3-P 'lua-seti L (%s3-seti/f %s3-seti/s) (idx i) idx i (luaR-nup L) (luaR-floor L))
    (void))

  (define-lua-stubs (%s3-rawset/f %s3-rawset/s) ((int idx) (int nup) (int floor))
    "aux_op o = {0}; int refs[1], c; o.fn = op_s3_rawset; refs[0] = idx;
     if ((c = aux_s3_typed(L, idx, nup, LUA_TTABLE))) C_return(c);
     if ((c = aux_s3_consume(L, 2, floor))) C_return(c);
     if (aux_globals_write_bad(L, idx, -2, 0, -1)) C_return(AUX_EGLOBALS);
     if ((c = aux_libwrite_stub(safe, L, idx, aux_libname_at(L, -2), -1, NULL, 0, NULL))) C_return(c);
     C_return(aux_run(safe, L, &o, 1, refs, 2, 0));")
  ;; ((lua-rawset idx) L): t[k] = v without metamethods, the table t at idx, k and v on top (popped).
  ;; A nil or NaN key raises (exn lua) "index is nil" / "index is NaN" (k and v are consumed).
  (define ((lua-rawset idx) L)
    (%int! 'lua-rawset idx)
    (%s3-P 'lua-rawset L (%s3-rawset/f %s3-rawset/s) (idx) idx (luaR-nup L) (luaR-floor L))
    (void))

  (define-lua-stubs (%s3-rawseti/f %s3-rawseti/s) ((int idx) (integer64 n) (int nup) (int floor))
    "aux_op o = {0}; int refs[1], c; o.fn = op_s3_rawseti; o.n0 = n; refs[0] = idx;
     if ((c = aux_s3_typed(L, idx, nup, LUA_TTABLE))) C_return(c);
     if ((c = aux_s3_consume(L, 1, floor))) C_return(c);
     if (aux_globals_write_bad(L, idx, 0, n, -1)) C_return(AUX_EGLOBALS);
     C_return(aux_run(safe, L, &o, 1, refs, 1, 0));")
  ;; ((lua-rawseti idx i) L): t[i] = the value on top (popped), without metamethods
  (define ((lua-rawseti idx i) L)
    (%int! 'lua-rawseti idx) (%s3-integer! 'lua-rawseti i)
    (%s3-P 'lua-rawseti L (%s3-rawseti/f %s3-rawseti/s) (idx i) idx i (luaR-nup L) (luaR-floor L))
    (void))

  (define-lua-stubs (%s3-rawsetp/f %s3-rawsetp/s) ((int idx) (c-pointer ptr) (int nup) (int floor))
    "aux_op o = {0}; int refs[1], c; o.fn = op_s3_rawsetp; o.p0 = ptr; refs[0] = idx;
     if ((c = aux_s3_typed(L, idx, nup, LUA_TTABLE))) C_return(c);
     if ((c = aux_s3_consume(L, 1, floor))) C_return(c);
     C_return(aux_run(safe, L, &o, 1, refs, 1, 0));")
  ;; ((lua-rawsetp idx p) L): t[p] = the value on top (popped), p a light userdata key (pointer or #f)
  (define ((lua-rawsetp idx p) L)
    (%int! 'lua-rawsetp idx) (%s3-pointer! 'lua-rawsetp p)
    (%s3-P 'lua-rawsetp L (%s3-rawsetp/f %s3-rawsetp/s) (idx p) idx p (luaR-nup L) (luaR-floor L))
    (void))

  ;; ---------------------------------------------------------------- P: traversal

  (define-lua-stubs (%s3-next/f %s3-next/s) ((int idx) (int nup) (int floor))
    "aux_op o = {0}; int refs[1], c; o.fn = op_s3_next; refs[0] = idx;
     if ((c = aux_s3_typed(L, idx, nup, LUA_TTABLE))) C_return(c);
     if ((c = aux_s3_consume(L, 1, floor))) C_return(c);
     C_return(aux_run(safe, L, &o, 1, refs, 1, LUA_MULTRET));")
  ;; ((lua-next idx) L) => #t with the next key and its value pushed (the key on top is replaced),
  ;; or #f with the key popped and nothing pushed.  Start with nil on top.  A key that is not in
  ;; the table raises (exn lua) "invalid key to 'next'".
  (define ((lua-next idx) L)
    (%int! 'lua-next idx)
    (fx= 1 (%s3-P 'lua-next L (%s3-next/f %s3-next/s) (idx) idx (luaR-nup L) (luaR-floor L))))

  ;; ---------------------------------------------------------------- P*: constructors

  (define-lua-stubs (%s3-createtable/f %s3-createtable/s) ((int narr) (int nrec))
    "aux_op o = {0}; o.fn = op_s3_createtable; o.i0 = narr; o.i1 = nrec;
     if (narr < 0 || nrec < 0) C_return(AUX_ERANGE);
     C_return(aux_run_h(safe, L, &o, 1));")
  ;; ((lua-createtable narr nrec) L): pushes a new table with room for narr array and nrec hash items
  (define ((lua-createtable narr nrec) L)
    (%int! 'lua-createtable narr nrec)
    (%s3-P 'lua-createtable L (%s3-createtable/f %s3-createtable/s) (narr nrec) narr nrec)
    (void))
  ;; ((lua-newtable) L)
  (define lua-newtable (let ((k (lua-createtable 0 0))) (λ () k)))

  (define-lua-stubs (%s3-newuserdatauv/f %s3-newuserdatauv/s) ((size_t size) (int nuv))
    "aux_op o = {0}; o.fn = op_s3_newuserdatauv; o.z0 = size; o.i0 = nuv;
     if (nuv < 0 || nuv >= SHRT_MAX) C_return(AUX_ERANGE);
     C_return(aux_run_h(safe, L, &o, 1));")
  ;; ((lua-newuserdatauv size nuv) L) => the address of the new block of size bytes (Lua-owned
  ;; memory, valid while the userdata is reachable); the userdata, with nuv user values
  ;; (0 <= nuv < SHRT_MAX, as lua.h checks), is pushed.
  (define ((lua-newuserdatauv size nuv) L)
    (%int! 'lua-newuserdatauv nuv)
    (unless (and (exact-integer? size) (<= 0 size 18446744073709551615))
      (%api-error 'lua-newuserdatauv "not a size" (list size nuv) AUX_ERANGE))
    (%s3-P 'lua-newuserdatauv L (%s3-newuserdatauv/f %s3-newuserdatauv/s) (size nuv) size nuv)
    (%done (%touserdata-raw (%L 'lua-newuserdatauv L) -1)))
  ;; ((lua-newuserdata size) L): one user value
  (define ((lua-newuserdata size) L) ((lua-newuserdatauv size 1) L))

  ;; ---------------------------------------------------------------- D: raw reads

  (define %s3-rawget (foreign-lambda* int (((c-pointer "lua_State") L) (int idx) (int nup) (int floor))
                       "int c = aux_s3_typed(L, idx, nup, LUA_TTABLE); if (c) C_return(c);
                        if (lua_gettop(L) < 1) C_return(AUX_EUNDER);
                        if (lua_gettop(L) - 1 < aux_eff_floor(L, floor)) C_return(AUX_EINDEX);   /* the key's slot is overwritten */
                        C_return(lua_rawget(L, idx));"))
  ;; ((lua-rawget idx) L) => the type of t[k] (no metamethods) replacing the key k on top
  (define ((lua-rawget idx) L)
    (%int! 'lua-rawget idx)
    (let ((Lp (%L 'lua-rawget L))) (%D 'lua-rawget (%s3-rawget Lp idx (luaR-nup L) (luaR-floor L)) idx)))

  (define %s3-rawgeti (foreign-lambda* int (((c-pointer "lua_State") L) (int idx) (integer64 n) (int nup))
                        "int c = aux_s3_typed(L, idx, nup, LUA_TTABLE); if (c) C_return(c);
                         if (!lua_checkstack(L, 1)) C_return(AUX_ESTACK);
                         C_return(lua_rawgeti(L, idx, n));"))
  ;; ((lua-rawgeti idx i) L) => the type of t[i] pushed (no metamethods)
  (define ((lua-rawgeti idx i) L)
    (%int! 'lua-rawgeti idx) (%s3-integer! 'lua-rawgeti i)
    (let ((Lp (%L 'lua-rawgeti L))) (%D 'lua-rawgeti (%s3-rawgeti Lp idx i (luaR-nup L)) idx i)))

  (define %s3-rawgetp (foreign-lambda* int (((c-pointer "lua_State") L) (int idx) (c-pointer ptr) (int nup))
                        "int c = aux_s3_typed(L, idx, nup, LUA_TTABLE); if (c) C_return(c);
                         if (!lua_checkstack(L, 1)) C_return(AUX_ESTACK);
                         C_return(lua_rawgetp(L, idx, ptr));"))
  ;; ((lua-rawgetp idx p) L) => the type of t[p] pushed, p a light userdata key (pointer or #f)
  (define ((lua-rawgetp idx p) L)
    (%int! 'lua-rawgetp idx) (%s3-pointer! 'lua-rawgetp p)
    (let ((Lp (%L 'lua-rawgetp L))) (%D 'lua-rawgetp (%s3-rawgetp Lp idx p (luaR-nup L)) idx p)))

  (define %s3-pushglobaltable (foreign-lambda* int (((c-pointer "lua_State") L))
                                "if (!lua_checkstack(L, 1)) C_return(AUX_ESTACK);
                                 lua_pushglobaltable(L); C_return(0);"))
  ;; ((lua-pushglobaltable) L)
  (define lua-pushglobaltable
    (let ((k (λ (L) (let ((Lp (%L 'lua-pushglobaltable L))) (%D 'lua-pushglobaltable (%s3-pushglobaltable Lp))) (void))))
      (λ () k)))

  ;; ---------------------------------------------------------------- D: metatables

  (define %s3-getmetatable (foreign-lambda* int (((c-pointer "lua_State") L) (int idx) (int nup))
                             "int q = aux_qix(L, idx);
                              if (q < 0 || (q > 0 && !aux_s3_ix(L, idx, nup))) C_return(AUX_EINDEX);
                              if (!lua_checkstack(L, 1)) C_return(AUX_ESTACK);
                              /* above the top: C reads the nil value there, whose metatable (the one of the
                                 type nil) may exist; top+1 is an acceptable index once a slot is reserved */
                              C_return(lua_getmetatable(L, q == 0 ? lua_gettop(L) + 1 : idx) ? 1 : 0);"))
  ;; ((lua-getmetatable idx) L) => #t with the metatable pushed, or #f with nothing pushed
  ;; (raw: a __metatable field is not honoured, as in C).  A positive idx above the top is the
  ;; nil value, as in C: the metatable of the type nil, if any.
  (define ((lua-getmetatable idx) L)
    (%int! 'lua-getmetatable idx)
    (fx= 1 (let ((Lp (%L 'lua-getmetatable L))) (%D 'lua-getmetatable (%s3-getmetatable Lp idx (luaR-nup L)) idx))))

  (define-lua-stubs (%libkeys/f %libkeys/s) ()
    "aux_op o = {0}; o.fn = op_libkeys; C_return(aux_run_h(safe, L, &o, 0));")
  (define %s3-setmetatable (foreign-lambda* int (((c-pointer "lua_State") L) (int idx) (int nup) (int floor))
                             "int c, t;
                              if (!aux_s3_ix(L, idx, nup)) C_return(AUX_EINDEX);
                              if ((c = aux_s3_consume(L, 1, floor))) C_return(c);
                              t = lua_type(L, -1);
                              if (t != LUA_TTABLE && t != LUA_TNIL) C_return(AUX_ETYPE);
                              if ((c = aux_setmt_code(L, idx, -1))) C_return(c);
                              lua_setmetatable(L, idx); C_return(0);"))
  ;; ((lua-setmetatable idx) L): the table or nil on top (popped) becomes the metatable of the
  ;; value at idx; for a value that is neither a table nor a full userdata, the metatable of its
  ;; whole type (C semantics).  Raw: a __metatable field is not honoured.
  ;; AUX_ELIBKEYS (the key table of the library entries was replaced): made anew by a P op, then
  ;; asked once more.
  (define ((lua-setmetatable idx) L)
    (%int! 'lua-setmetatable idx)
    (let ((r (%s3-setmetatable (%L 'lua-setmetatable L) idx (luaR-nup L) (luaR-floor L))))
      (%D 'lua-setmetatable
          (if (fx= r AUX_ELIBKEYS)
            (begin
              (set! %busy-ctx #f)
              (%P 'lua-setmetatable L (%libkeys/f %libkeys/s))
              (let ((r (%s3-setmetatable (%L 'lua-setmetatable L) idx (luaR-nup L) (luaR-floor L))))
                (if (fx= r AUX_ELIBKEYS) AUX_EMETATABLE r)))
            r)
          idx))
    (void))

  ;; ---------------------------------------------------------------- D: user values

  (define %s3-getiuservalue (foreign-lambda* int (((c-pointer "lua_State") L) (int idx) (int n) (int nup))
                              "int c = aux_s3_typed(L, idx, nup, LUA_TUSERDATA); if (c) C_return(c);
                               if (!lua_checkstack(L, 1)) C_return(AUX_ESTACK);
                               C_return(lua_getiuservalue(L, idx, n));"))
  ;; ((lua-getiuservalue idx n) L) => the type of the n-th user value pushed; LUA_TNONE (with nil
  ;; pushed) when the full userdata at idx has no such value
  (define ((lua-getiuservalue idx n) L)
    (%int! 'lua-getiuservalue idx n)
    (let ((Lp (%L 'lua-getiuservalue L))) (%D 'lua-getiuservalue (%s3-getiuservalue Lp idx n (luaR-nup L)) idx n)))
  (define ((lua-getuservalue idx) L) ((lua-getiuservalue idx 1) L))

  (define %s3-setiuservalue (foreign-lambda* int (((c-pointer "lua_State") L) (int idx) (int n) (int nup) (int floor))
                              "int c = aux_s3_typed(L, idx, nup, LUA_TUSERDATA); if (c) C_return(c);
                               if ((c = aux_s3_consume(L, 1, floor))) C_return(c);
                               C_return(lua_setiuservalue(L, idx, n) ? 1 : 0);"))
  ;; ((lua-setiuservalue idx n) L) => #t when the value on top became the n-th user value of the
  ;; full userdata at idx, #f when it has no such value; the value is popped either way
  (define ((lua-setiuservalue idx n) L)
    (%int! 'lua-setiuservalue idx n)
    (fx= 1 (let ((Lp (%L 'lua-setiuservalue L))) (%D 'lua-setiuservalue (%s3-setiuservalue Lp idx n (luaR-nup L) (luaR-floor L)) idx n))))
  (define ((lua-setuservalue idx) L) ((lua-setiuservalue idx 1) L))

  ;; ---------------------------------------------------------------- K

  ;; ((lua-register name f) L): the global name = f, a Scheme procedure (λ (C) answer) or a
  ;; c-pointer to a lua_CFunction (lua-pushcfunction then lua-setglobal)
  (define ((lua-register name f) L)
    (%bytes! 'lua-register name)
    ((lua-pushcfunction f) L)
    ((lua-setglobal name) L))

  ;; ==========================================================================================
  ;; S4 — states and threads (lua-newstate, lua-newthread, lua-closethread, lua-resetthread,
  ;; lua-resume, lua-status, lua-version), loading and dumping chunks (lua-load, lua-dump),
  ;; warnings (lua-warning) and the collector (lua-gc).  lua-pcall, lua-call, lua-yield,
  ;; lua-yieldk, lua-error, lua-isyieldable, lua-setwarnf, lua-warnings and lua-setmemorylimit
  ;; are in the core.  Omitted (see the module header): lua_atpanic, lua_callk, lua_pcallk,
  ;; lua_getallocf, lua_setallocf, lua_toclose, lua_closeslot, lua_ident, lua_getextraspace.
  ;; ==========================================================================================

  #>

  int aux_lua_readdispatch(lua_State *L, void *ctx, int h);   /* define-external: the lua_Reader body */

  /* ---------- lua_newthread (P*): the thread is pushed, and anchored as i0 (see aux_anchor_set) ---------- */
  static int op_s4_newthread(lua_State *L, aux_op *o) {
    lua_State *T = lua_newthread(L);                         /* the step comes before T is made */
    aux_anchor_set(L, -1, o->i0, T); return 1; }

  /* ---------- lua_resume (S): it runs inside a protected op on the MAIN thread, whose frame also holds
     co (arg 1) while it runs.  lua_resume itself raises outside co's own protection when resume_error
     cannot allocate its message: luaD_throw then finds no handler on co, resets it and re-throws into the
     main thread's errorJmp, which is this pcall (never a handler below CHICKEN frames).  That holds only
     when co has no handler of its own: co must be suspended, dead or not started.  A running or normal
     coroutine has one -- its active resume, below this callback's C_callback frames -- so lua_resume is
     not called for it: its resume_error is done here instead (the nargs values popped, the message
     pushed by a protected op of co's own, which catches an out-of-memory; LUA_ERRRUN).  The main thread
     is refused: this pcall runs on it.  The result is (status << 24) | nres. ---------- */
  static int op_s4_resume(lua_State *M, aux_op *o) {
    int nres = 0, st = lua_resume(o->L1, (lua_State *)o->p0, o->i0, &nres);
    if (nres < 0 || nres > 0xffffff) nres = 0xffffff;       /* impossible: LUAI_MAXSTACK is 10^6 */
    o->r = (st << 24) | nres; return 0; }
  /* co is running or normal: it has frames although its status is LUA_OK */
  static int aux_s4_active(lua_State *co) { lua_Debug ar; return lua_status(co) == LUA_OK && lua_getstack(co, 0, &ar); }
  /* lua_resume and lua_closethread only count C calls from `from': inside a callback the C stack really
     grows from the callback's thread, whatever `from' says (else a Lua -> Scheme -> lua-resume recursion
     restarts the count at every level and exhausts the C stack instead of failing with "C stack overflow") */
  static lua_State *aux_s4_from(lua_State *from) { return aux_cur ? aux_cur : from; }
  static const char aux_s4_nonsuspended[] = "cannot resume non-suspended coroutine";
  /* On a thread suspended by a hook, or dead in a Lua frame, the nargs values must lie above the frame's
     registers (aux_nk_lim): resume() resets the top to the first argument and a pending CALL, RETURN or
     SETLIST with B = 0 counts its operands up to it (a negative count writes past the stack), and
     resume_error writes its message where the top lands (over a to-be-closed variable, which
     lua-closethread would then never close); C's api_checkpop only catches the latter.  id: co's anchor
     (-1 for none).  Refused: AUX_ETHREAD, also the answer for the main thread (Scheme tells them apart). */
  static int aux_s4_resume(int safe, lua_State *co, lua_State *from, int nargs, int floor, int id) {
    aux_op o; aux_ctx *c = AUX_CTX(co); lua_State *M = c->main; int r, st0 = lua_status(co);
    if (co == M) return AUX_ETHREAD;
    if (nargs < 0 || nargs > lua_gettop(co)) return AUX_EUNDER;
    if (st0 != LUA_OK && aux_in_lua_frame(co) && lua_gettop(co) - nargs < aux_nk_lim(co, id)) return AUX_ETHREAD;
    if (aux_s4_active(co)) {                                /* lua_resume's resume_error, emulated */
      if ((r = aux_settop_d(co, -nargs - 1, floor)) != 0) return r;   /* never below a floor (a hooked function's registers) */
      memset(&o, 0, sizeof o); o.fn = op_pushlstring; o.s0 = aux_s4_nonsuspended; o.l0 = sizeof aux_s4_nonsuspended - 1;
      r = aux_run(safe, co, &o, 0, NULL, 0, 1);            /* co is LUA_OK: the op runs on it */
      return r < 0 ? r : LUA_ERRRUN << 24; }
    if (!lua_checkstack(co, 2) || !lua_checkstack(M, 1)) return AUX_ESTACK;   /* resume_error pushes its message */
    memset(&o, 0, sizeof o); o.fn = op_s4_resume; o.L1 = co; o.p0 = aux_s4_from(from); o.i0 = nargs;
    lua_pushthread(co); lua_xmove(co, M, 1);                /* co's own stack is unchanged */
    c->epoch++;
    r = aux_run(safe, M, &o, 0, NULL, 1, 0);
    if (AUX_IS_LUAERR(r)) return AUX_LUAERR_M(-r - 1000);   /* the error value is on top of main */
    if (r >= 0 && (r >> 24) > LUA_YIELD && lua_status(co) != LUA_OK) {   /* co is dead: its error value, for lua-check */
      c->rerrL = co; c->rerrtop = lua_gettop(co); c->rerrepoch = c->epoch; }
    /* the exact height of a hook yield or a death; a resume_error (a dead co, or a "C stack overflow") ran
       nothing: the frame, hence its record, is the one it had */
    if (r >= 0 && st0 <= LUA_YIELD && !(lua_status(co) == st0 && (r >> 24) > LUA_YIELD))
      aux_nk_set(co, id, lua_status(co) > LUA_YIELD);
    return r; }

  /* ---------- lua_closethread (S): self-protecting (luaD_closeprotected on co); refused for the main
     thread, the calling thread and a running (or normal) thread, whose frames it would destroy ---------- */
  static int aux_s4_closethread(int safe, lua_State *co, lua_State *from) {
    aux_ctx *c = AUX_CTX(co); int st, saved;
    if (co == c->main || co == from || aux_s4_active(co)) return AUX_ETHREAD;
    c->epoch++;
    saved = aux_reentrant; if (safe) aux_reentrant = 1;     /* __close handlers may call Scheme */
    st = lua_closethread(co, aux_s4_from(from));
    aux_reentrant = saved; return st; }

  /* ---------- lua_gc: D options never run code; COLLECT, STEP, GEN and INC run finalizers (S).  On a
     suspended or dead thread the collector runs from the main thread (always OK): it is one collector. ---------- */
  static int aux_s4_gc(int safe, lua_State *L, int what, size_t z, int a, int b) {
    int r, saved; aux_barrier br;
    if (lua_status(L) != LUA_OK) L = AUX_CTX(L)->main;
    saved = aux_reentrant; if (safe) aux_reentrant = 1;
    AUX_BARRIER_PUSH(br, L);                                /* a finalizer runs on L, in a protected call of its own */
    switch (what) {
    case LUA_GCSTEP:  r = lua_gc(L, what, z); break;
    case LUA_GCPARAM: r = lua_gc(L, what, a, b); break;
    default:          r = lua_gc(L, what); break; }
    AUX_BARRIER_POP(br);
    aux_reentrant = saved; return r; }

  /* ---------- lua_dump (P): the writer grows a malloc'd buffer that Scheme reads, copies and frees ---------- */
  typedef struct { char *b; size_t n, cap; } aux_s4_buf;
  static int aux_s4_writer(lua_State *L, const void *p, size_t sz, void *ud) {
    aux_s4_buf *w = (aux_s4_buf *)ud;
    if (sz > ((size_t)-1) / 2 - w->n) return 1;
    if (w->n + sz > w->cap) {
      size_t cap = w->cap ? w->cap : 256; char *q;
      while (cap < w->n + sz) cap *= 2;
      if (!(q = realloc(w->b, cap))) return 1;
      w->b = q; w->cap = cap; }
    if (sz) memcpy(w->b + w->n, p, sz);
    w->n += sz; return 0; }
  static int op_s4_dump(lua_State *L, aux_op *o) {           /* arg 1: a copy of the function */
    o->r = lua_dump(L, aux_s4_writer, o->p0, o->i0); return 0; }

  /* ---------- lua_load with a Scheme reader (S).  The reader calls back into Scheme with the flag
     discipline of aux_cfunction (invariant I2); each piece is copied into the load context, kept until the
     next call.  A Scheme error or escape is recorded by Scheme and answered as EOF.

     The reader is Scheme code that may use the loading thread L, run Lua code, resume coroutines...  so
     the parser never works in a frame Scheme can reach, and L is never idle meanwhile:
       op_s4_load       an op on L; it makes P, a new thread that only the load knows, and pins it (below):
                        P is in no stack slot, table or upvalue, so Lua code the reader runs on L cannot
                        find it (debug.getlocal / debug.getregistry) nor unanchor it (debug.setlocal);
       op_s4_loadframe  an op on L above it: the (empty) frame the reader sees L in; L has frames, so
                        lua-resume, lua-closethread and Lua's coroutine.resume / coroutine.close refuse it;
       op_s4_parse      an op on P: lua_load runs in P's op frame, where the parser keeps its slots (the
                        half-built closure, its anchor table).  P is a parser thread (aux_loading): a
                        'thread luaR of it is refused, a callback on it (a finalizer) runs in a frame
                        above, and the bindings never read the slots of an op's frame (aux_dbg_getlocal).
     The chunk (or the message) is moved from P onto L; the values the reader left on L are dropped with
     the reader's frame.

     The pin.  An anchor in a stack slot, the registry or any table is reachable from Lua code through the
     debug library, which can clear it: a collection would then free P under the running parser.  P is
     instead the user value of a userdata U, with a metatable of its own whose __gc is aux_pin_gc; U is
     referenced by nothing.  A collection finds U unreachable and separates it for finalization, which
     marks U, its metatable and P (lgc.c: separatetobefnz, markbeingfnz); U's __gc then sets the same
     metatable again, which registers U for finalization anew (luaC_checkfinalizer), so the next cycle
     does the same.  Finalizers run with hooks off, so no Lua code ever sees U.  When the load ends, the
     flag `done' (written through the block's C pointer: U is alive, it has not been left unregistered)
     stops the loop, and U and P are collected as usual.  lua_close ignores the re-registration.
     What is left is Lua's own: a Lua finalizer that a collection step of the parser runs on P can
     reach the parser's anchor slots with debug.setlocal, as with Lua's load() on its own thread. ---------- */
  typedef struct { int done; } aux_pin;
  static int aux_pin_gc(lua_State *L) {                    /* light C function: no allocation */
    aux_pin *pin = (aux_pin *)lua_touserdata(L, 1);        /* shaped as a pin: Lua code (debug) may call it */
    if (pin && lua_type(L, 1) == LUA_TUSERDATA && lua_rawlen(L, 1) == sizeof *pin && !pin->done && lua_getmetatable(L, 1))
      lua_setmetatable(L, 1);
    return 0; }
  /* inside an op: pins P, the thread on top (popped); the block that ends the pin (done = 1).  Every
     allocation comes first, then the checks of the slots (see aux_slot_bad: a finalizer that a step runs
     could have left P in no slot, and a later step would free P under the parser), then the typed calls,
     which allocate nothing. */
  static aux_pin *aux_pin_new(lua_State *L, lua_State *P) {
    aux_pin *pin;
    luaL_checkstack(L, 6, NULL);                            /* 5 pushes, and aux_slot_newud's */
    pin = (aux_pin *)lua_newuserdatauv(L, sizeof *pin, 1);  /* P U          (a collection step) */
    lua_createtable(L, 0, 1);                               /* P U mt       (a collection step) */
    lua_pushliteral(L, "__gc");                             /* P U mt "__gc" (a collection step) */
    lua_pushcfunction(L, aux_pin_gc);                       /* P U mt "__gc" gc */
    aux_slot_thread(L, -5, P); aux_slot_newud(L, -4, pin); AUX_SLOT(L, -3, LUA_TTABLE); AUX_SLOT(L, -2, LUA_TSTRING);
    pin->done = 1;                                          /* nothing to keep until it is complete */
    lua_rawset(L, -3);                                      /* P U mt */
    lua_pushvalue(L, -3); lua_setiuservalue(L, -3, 1);      /* P U mt: P is U's user value */
    lua_setmetatable(L, -2);                                /* P U: U is registered for finalization */
    pin->done = 0;
    lua_pop(L, 2);                                          /* U is referenced by nothing */
    return pin; }
  typedef struct { int h, done, bad; char *b; size_t n, cap; lua_State *L; } aux_s4_rctx;
  static const char *aux_s4_reader(lua_State *P, void *ud, size_t *sz) {
    aux_s4_rctx *c = (aux_s4_rctx *)ud; int r, saved, nf; lua_State *cur;
    *sz = 0;
    if (c->done || !aux_reentrant) { c->done = 1; return NULL; }
    if (aux_cstack_low()) { c->bad = 2; c->done = 1; return NULL; }        /* see aux_cstack_low */
    saved = aux_reentrant; aux_reentrant = 0; cur = aux_cur; aux_cur = c->L; AUX_CB_ENTER(nf);   /* C calls count from L */
    c->n = 0;
    r = aux_lua_readdispatch(P, c, c->h);                  /* C_callback: Scheme runs AND RETURNS here */
    AUX_CB_LEAVE(nf); aux_reentrant = saved; aux_cur = cur;
    /* the reader may have run Lua code that replaced registry[LUA_RIDX_GLOBALS]: lua_load reads it once the
       parse ends (getGlobalTable), so the load stops here, with an error of the protected parser (only
       Lua frames lie between: the callback has returned) */
    { int g = aux_globals_code(P);
      if (g) { if (g == AUX_EGLOBALS) c->bad = 4; c->done = 1; aux_globals_check(P); } }
    if (r <= 0 || c->n == 0) { c->done = 1; return NULL; }
    *sz = c->n; return c->b; }
  static int aux_s4_store(aux_s4_rctx *c, const char *s, size_t n) {
    if (n > c->cap) { char *p = realloc(c->b, n); if (!p) return 0; c->b = p; c->cap = n; }
    if (n) memcpy(c->b, s, n);
    c->n = n; return 1; }
  static int op_s4_parse(lua_State *P, aux_op *o) {          /* on P: the chunk or the message is left */
    aux_globals_check(P);
    o->r = lua_load(P, aux_s4_reader, o->p0, o->s0, o->s1); return 1; }
  static int op_s4_loadframe(lua_State *L, aux_op *o) {      /* on L: runs the parse on P = o->L1 */
    aux_op po; int r;
    memset(&po, 0, sizeof po); po.fn = op_s4_parse; po.p0 = o->p0; po.s0 = o->s0; po.s1 = o->s1;
    if (aux_nloading == AUX_MAXFLOORS) return luaL_error(L, "C stack overflow");
    aux_loading[aux_nloading++] = o->L1;
    r = aux_protect(o->L1, &po, 0, NULL, 0, 1);
    aux_nloading--;
    if (r <= -2000) return luaL_error(L, "aux.lua: the parser thread refused the load");   /* P is fresh: unreachable */
    o->r = AUX_IS_LUAERR(r) ? -r - 1000 : r;               /* a failed call: its status, the message on P */
    luaL_checkstack(L, 1, NULL);
    lua_xmove(o->L1, L, 1); return 1; }
  static int op_s4_load(lua_State *L, aux_op *o) {           /* on L: P is pinned, in no slot of this frame */
    aux_op fo; int r; aux_pin *pin;
    fo = *o; fo.fn = op_s4_loadframe; fo.L1 = lua_newthread(L);
    pin = aux_pin_new(L, fo.L1);                            /* pops P */
    ((aux_s4_rctx *)o->p0)->L = L;
    r = aux_protect(L, &fo, 0, NULL, 0, 1);
    pin->done = 1;                                          /* P is unused from here: collectable again */
    if (r <= -2000) return luaL_error(L, "aux.lua: cannot run the load");                  /* L is OK: unreachable */
    o->r = AUX_IS_LUAERR(r) ? -r - 1000 : fo.r;            /* the error value, or the chunk / message, on top */
    return 1; }
  /* name and mode are copied before lua_load: the reader may move every Scheme object.
     The status, plus 512 when the reader could not be called (the C stack is exhausted, see
     aux_cstack_low), plus 1024 when the load was stopped because registry[LUA_RIDX_GLOBALS] was no
     longer a table after the reader ran (see aux_s4_reader).  The chunk or the message is on top of L, as C. */
  static int aux_s4_load(int safe, lua_State *L, int h, const char *name, size_t nl, const char *mode, size_t ml) {
    aux_s4_rctx c; aux_op o; char *cn = NULL, *cm = NULL; int r, saved;
    if (lua_status(L) != LUA_OK) return AUX_ETHREAD;
    if (aux_badmode(mode, ml)) return AUX_EARG;
    if ((r = aux_globals_code(L))) return r;
    if ((name && !(cn = aux_dup(name, nl))) || (mode && !(cm = aux_dup(mode, ml)))) { free(cn); free(cm); return AUX_ENOMEM; }
    memset(&c, 0, sizeof c); c.h = h; c.L = L;
    memset(&o, 0, sizeof o); o.fn = op_s4_load; o.p0 = &c; o.s0 = cn; o.s1 = cm;
    saved = aux_reentrant; if (safe) aux_reentrant = 1;
    r = aux_protect(L, &o, 0, NULL, 0, 1);
    aux_reentrant = saved;
    free(c.b); free(cn); free(cm);
    if (r <= -2000) return r;
    if (AUX_IS_LUAERR(r)) r = -r - 1000;                   /* the op failed (out of memory): its message */
    return c.bad ? r + 256 * c.bad : r; }

  /* ---------- lua_newstate (K): on the counting allocator, whose ud is the ctx ---------- */
  static lua_State *aux_s4_newstate(unsigned seed) {
    aux_ctx *c = (aux_ctx *)calloc(1, sizeof *c); lua_State *L;
    if (!c) return NULL;
    if (!(L = lua_newstate(aux_alloc, c, seed))) { free(c); return NULL; }
    if (!aux_setup_with(L, c, 0)) return NULL;              /* L closed and c freed; no warning sink, as C */
    return L; }

  <#

  ;; ---------------------------------------------------------------- states

  (define %s4-newstate (foreign-lambda (c-pointer "lua_State") "aux_s4_newstate" unsigned-integer32))
  (define %s4-ctx-of (foreign-lambda* c-pointer (((c-pointer "lua_State") L)) "C_return(AUX_CTX(L));"))
  (define %s4-main? (foreign-lambda* bool (((c-pointer "lua_State") L)) "C_return(AUX_CTX(L)->main == L);"))
  (define %s4-makeseed (foreign-lambda* unsigned-integer32 () "C_return(luaL_makeseed(NULL));"))

  ;; (lua-newstate [seed]) => a new main luaR, like lua-make-empty but made by lua_newstate on the
  ;; module's counting allocator (see lua-setmemorylimit) with the given hash seed (an unsigned
  ;; 32-bit integer; #f or omitted: luaL_makeseed's).  No library is opened, and, as C's
  ;; lua_newstate installs no warning function, warnings are discarded until (lua-setwarnf 'stderr)
  ;; or (lua-setwarnf 'collect) gives them a sink (lua-make-empty / luaL-newstate start with
  ;; 'stderr, as luaL_newstate).  Not curried: there is no state yet.
  (define (lua-newstate #!optional (seed #f))
    (unless (or (not seed) (and (exact-integer? seed) (<= 0 seed 4294967295)))
      (%api-error 'lua-newstate "not an unsigned 32-bit seed" (list seed) AUX_ERANGE))
    (let ((state (%s4-newstate (or seed (%s4-makeseed)))))
      (unless state (error 'lua-newstate "lua_newstate cannot allocate a state"))
      (%make-main-luaR state (%s4-ctx-of state))))

  ;; ((lua-version) L) => the version number of the core, 505.0 (a flonum, as C's lua_Number)
  (define %s4-version (foreign-lambda double "lua_version" (c-pointer "lua_State")))
  (define lua-version (let ((k (λ (L) (%done (%s4-version (%L 'lua-version L)))))) (λ () k)))

  ;; ((lua-status) L) => LUA_OK, LUA_YIELD, or the error status of a dead thread
  (define lua-status (let ((k (λ (L) (%done (%status (%L 'lua-status L)))))) (λ () k)))

  ;; ---------------------------------------------------------------- threads

  (define-lua-stubs (%s4-newthread/f %s4-newthread/s) ((int id))
    "aux_op o = {0}; o.fn = op_s4_newthread; o.i0 = id; C_return(aux_run_h(safe, L, &o, 1));")

  ;; ((lua-newthread) L) => a luaR (kind 'thread) for a new thread, which is also pushed onto L (as
  ;; C).  As with lua-tothread, the thread stays anchored while the luaR lives (and the luaR keeps
  ;; the main luaR alive).
  (define lua-newthread
    (let ((k (λ (L)
               (let* ((u (%U 'lua-newthread L)) (id (%anchor-alloc! u)))
                 (%P 'lua-newthread L (%s4-newthread/f %s4-newthread/s) id)
                 (%make-thread-luaR 'lua-newthread u id)))))
      (λ () k)))

  ;; an optional `from' luaR must belong to universe u (its pointer is read later, with %L)
  (define (%s4-from! who from u args)
    (when from
      (unless (eq? (%U who from) u) (%api-error who "the states belong to different universes" args))))

  ;; after an S op on thread p whose error value stays on p (C semantics): an escape stopped at a
  ;; callback boundary drops the sentinels it left on p (lua_resume pushes a copy of the error value
  ;; of the dead frame) and goes on; codes <= -1000 as %finish
  (define (%s4-status-epilogue who u p r args)
    (cond
      ((fx<= r -1000) (%finish who u p r args))
      ((and %pending-escape (fx= (car %pending-escape) (fx+ %depth 1)))
       (let loop () (when (and (%top-escape? p) (fx= 0 (%settop p -2 0))) (loop)))   ; refused in a Lua frame
       (%escape-epilogue!))
      (else r)))

  (define-lua-stubs (%s4-resume/f %s4-resume/s) (((c-pointer "lua_State") from) (int nargs) (int floor) (int id))
    "C_return(aux_s4_resume(safe, L, from, nargs, floor, id));")

  ;; ((lua-resume from nargs) co) => (values status nres): starts or resumes the coroutine co with
  ;; the nargs values on its top; `from' is the resuming thread (a luaR of the same universe) or #f.
  ;; LUA_YIELD: co yielded nres values; LUA_OK: it returned nres values; an error status: the error
  ;; value is on top of co (C semantics; ((lua-check (lua-resume from n)) co) raises it instead).
  ;; A running or normal coroutine (one with active frames, such as the thread of the callback
  ;; doing the resume) answers as C: its nargs values are popped, "cannot resume non-suspended
  ;; coroutine" is pushed, and the status is LUA_ERRRUN (nres 0).  Resuming the main thread is
  ;; refused with (exn lua api): the resume itself runs on it.  On a coroutine suspended by a hook's
  ;; lua-yield, or dead by an error raised in Lua code, the nargs values must lie above the height
  ;; it had when it got there (the values pushed since, and on a dead one the error value lua-resume
  ;; left): Lua lowers the top by nargs, into that frame's registers otherwise (a pending call would
  ;; count a negative number of arguments; a to-be-closed variable would be overwritten), so more is
  ;; refused with (exn lua api), code AUX_ETHREAD.  `from' only matters for the count of nested C
  ;; calls: inside a callback the callback's thread is always used for it, so an unbounded
  ;; Lua -> Scheme -> lua-resume recursion ends with "C stack overflow" (an error status).
  (define ((lua-resume from nargs) co)
    (%int! 'lua-resume nargs)
    (let* ((u (%U 'lua-resume co)) (args (list from nargs)))
      (%s4-from! 'lua-resume from u args)
      (%prepare! u)
      (let* ((p (%L 'lua-resume co)) (fp (and from (%L 'lua-resume from)))
             (r (%run u (%s4-resume/f %s4-resume/s) p fp nargs (luaR-floor co) (%anchor-id co))))
        (when (fx= r AUX_ETHREAD)
          (%api-error 'lua-resume
                      (if (%s4-main? p) "cannot resume the main thread"
                        "thread is suspended or dead: its arguments would reach the registers of its Lua frame")
                      args AUX_ETHREAD))
        (let ((r (%s4-status-epilogue 'lua-resume u p r args)))
          (set! %busy-ctx #f)
          (values (fxshr r 24) (fxand r #xffffff))))))

  (define-lua-stubs (%s4-closethread/f %s4-closethread/s) (((c-pointer "lua_State") from))
    "C_return(aux_s4_closethread(safe, L, from));")

  ;; ((lua-closethread from) co) => status: resets the thread co (suspended, dead, or not started),
  ;; closing its pending to-be-closed variables.  LUA_OK, or an error status (the original error of
  ;; a dead thread, or one raised by a __close) with the error value on top of co.  Refused for the
  ;; main thread, for co = from, and for a running (or normal) thread.
  (define ((lua-closethread from) co)
    (let* ((u (%U 'lua-closethread co)) (args (list from)))
      (%s4-from! 'lua-closethread from u args)
      (%prepare! u)
      (let* ((p (%L 'lua-closethread co)) (fp (and from (%L 'lua-closethread from)))
             (r (%run u (%s4-closethread/f %s4-closethread/s) p fp)))
        (when (fx= r AUX_ETHREAD)
          (%api-error 'lua-closethread "cannot close the main thread, the calling thread or a running thread" args AUX_ETHREAD))
        (let ((r (%s4-status-epilogue 'lua-closethread u p r args)))
          (set! %busy-ctx #f)
          r))))

  ;; ((lua-resetthread) co) => status: lua-closethread with no `from' (the deprecated C macro)
  (define lua-resetthread (let ((k (lua-closethread #f))) (λ () k)))

  ;; ---------------------------------------------------------------- loading and dumping chunks

  (define-lua-stubs (%s4-load/f %s4-load/s) ((int h) (scheme-object name) (scheme-object mode))
    "size_t nl = 0, ml = 0; const char *nm = NULL, *md = NULL;
     if (C_truep(name)) nm = aux_bytes(name, &nl);
     if (C_truep(mode)) md = aux_bytes(mode, &ml);
     C_return(aux_s4_load(safe, L, h, nm, nl, md, ml));")
  (define %s4-reader-store! (foreign-lambda* bool ((c-pointer c) (scheme-object x))
                              "size_t n; const char *s = aux_bytes(x, &n); C_return(aux_s4_store((aux_s4_rctx *)c, s, n));"))

  ;; an (exn lua api) condition, not raised
  (define (%s4-api-condition who msg args code)
    (make-composite-condition
      (make-property-condition 'exn 'location who 'message msg 'arguments args)
      (make-property-condition 'lua 'status 'api 'code code)
      (make-property-condition 'api)))

  ;; the reader's answer: 1 a piece was stored, 0 EOF.  Its box is #(reader failed? object).
  (define (%s4-reader-answer ctx box outcome)
    (define (fail! obj) (vector-set! box 1 #t) (vector-set! box 2 obj) 0)
    (cond
      ((or (string? outcome) (bytevector? outcome))
       (if (%s4-reader-store! ctx outcome) 1 (fail! (%s4-api-condition 'lua-load (%api-message AUX_ENOMEM) '() AUX_ENOMEM))))
      ((not outcome) 0)
      ((and (pair? outcome) (eq? (car outcome) %escape-tag)) (%record-escape! (cdr outcome)) 0)
      ((and (pair? outcome) (eq? (car outcome) %raise-tag)) (fail! (cadr outcome)))
      (else (fail! (%s4-api-condition 'lua-load "a lua-load reader must answer a string, a bytevector or #f"
                                      (list outcome) AUX_EARG)))))

  ;; the body of the C reader: the guard runs the thunk (never inside a handler, see %safely)
  (define (%s4-read-dispatch Lp ctx h)
    (let ((e (%safely #f (and (not (%escape-refused?)) (let ((u (%callback-universe Lp))) (and u (%handle-entry u h '(reader))))))))
      (if (not e)
        0
        (let* ((box (car e))
               (outcome (%callback-outcome Lp (cddr e) -1 'view 0 (λ (view) ((vector-ref box 0))))))
          (%safely (begin (vector-set! box 1 #t)
                          (vector-set! box 2 (%s4-api-condition 'lua-load "internal error in a lua-load reader" '() AUX_EARG))
                          0)
                   (%s4-reader-answer ctx box outcome))))))

  (define-external (aux_lua_readdispatch ((c-pointer "lua_State") Lp) (c-pointer ctx) (int h)) int
    (%s4-read-dispatch Lp ctx h))

  ;; a load from a Scheme reader: always on the safe path (the reader is a callable handle meanwhile)
  (define (%s4-load-proc L reader chunkname mode)
    (let* ((u (%U 'lua-load L)) (box (vector reader #f #f)) (args (list reader chunkname mode)))
      (%prepare!/force u #t)
      (let* ((p (%L 'lua-load L))
             (h (%handle-new! box 'reader u))               ; counted before Lua can reach it (I3)
             (r (%run u (%s4-load/f %s4-load/s) p h chunkname mode)))
        (%handle-free! u h)
        (cond
          ((fx<= r -1000) (%finish 'lua-load u p r args))
          ((and %pending-escape (fx= (car %pending-escape) (fx+ %depth 1)))
           (%pop! p 1)                                          ; the chunk or the message lua_load pushed
           (%escape-epilogue!))
          ((vector-ref box 1) (%pop! p 1) (abort (vector-ref box 2)))
          ((fx>= r 1024) (%pop! p 1) (%api-fail 'lua-load AUX_EGLOBALS args))
          ((fx>= r 512) (%pop! p 1) (%api-error 'lua-load "C stack overflow: the reader cannot be called" args AUX_ESTACK))
          (else (set! %busy-ctx #f) r)))))

  ;; ((lua-load reader chunkname mode) L) => status, pushing the compiled chunk (LUA_OK) or the error
  ;; message (LUA_ERRSYNTAX, LUA_ERRMEM), as C.  reader: a string or a bytevector (the whole chunk),
  ;; or a thunk answering the next piece (a string or a bytevector) or #f / "" at the end.  A
  ;; condition raised by the thunk (or a bad answer) stops the load and is re-raised once lua_load
  ;; returned; a continuation escape out of it is resumed likewise.  While the thunk runs, L is
  ;; inside the load, as in a C function calling lua_load: the thunk sees L in a frame of its own
  ;; (empty on entry; what it leaves there is dropped) and L is not idle (lua-resume and
  ;; lua-closethread refuse it, Lua's coroutine.resume and coroutine.close too).  The parser itself
  ;; works on a private thread no binding can reach.  chunkname: a string or #f ("?"); mode: #f
  ;; (= "bt"), "t", "b" or "bt" (5.5's "B", a fixed buffer, is refused: see %mode!).  UNSAFE on
  ;; untrusted input unless mode is "t": Lua does not verify binary chunks, and a corrupted one
  ;; crashes the process (see the header).
  (define ((lua-load reader chunkname mode) L)
    (%cstring! 'lua-load chunkname #t)
    (%mode! 'lua-load mode)
    (cond
      ((or (string? reader) (bytevector? reader)) (%P 'lua-load L (%loadbufferx/f %loadbufferx/s) reader chunkname mode))
      ((procedure? reader) (%s4-load-proc L reader chunkname mode))
      (else (%V 'lua-load L) (%api-error 'lua-load "not a reader procedure, a string or a bytevector" (list reader chunkname mode)))))

  (define %s4-buf-new (foreign-lambda* c-pointer () "C_return(calloc(1, sizeof(aux_s4_buf)));"))
  (define %s4-buf-free! (foreign-lambda* void ((c-pointer w)) "free(((aux_s4_buf *)w)->b); free(w);"))
  (define %s4-buf-len (foreign-lambda* size_t ((c-pointer w)) "C_return(((aux_s4_buf *)w)->n);"))
  (define %s4-buf-copy! (foreign-lambda* void ((c-pointer w) (scheme-object bv))
                          "aux_s4_buf *b = (aux_s4_buf *)w; if (b->n) memcpy(C_data_pointer(bv), b->b, b->n);"))
  (define-lua-stubs (%s4-dump/f %s4-dump/s) ((c-pointer w) (bool strip))
    "aux_op o = {0}; int refs[1]; o.fn = op_s4_dump; o.p0 = w; o.i0 = strip; refs[0] = -1;
     if (lua_gettop(L) < 1) C_return(AUX_EUNDER);
     if (lua_type(L, -1) != LUA_TFUNCTION || lua_iscfunction(L, -1)) C_return(AUX_ETYPE);
     C_return(aux_run(safe, L, &o, 1, refs, 0, 0));")

  ;; ((lua-dump strip) L) => a bytevector: the binary chunk of the Lua function on top of L (it
  ;; stays there, as C).  strip: #t drops the debug information, #f keeps it (nothing else: %bool!).  lua-load reads it back (mode
  ;; #f, "b" or "bt"), with the same Lua version and number types only, and only unaltered: Lua
  ;; does not verify bytecode, so a chunk changed on its way back may crash the process.
  (define ((lua-dump strip) L)
    (%bool! 'lua-dump strip)
    (let ((u (%U 'lua-dump L)))
      (%prepare! u)
      (let* ((p (%L 'lua-dump L))
             (w (or (%s4-buf-new) (begin (set! %busy-ctx #f) (%api-fail 'lua-dump AUX_ENOMEM (list strip)))))
             (r (%run u (%s4-dump/f %s4-dump/s) p w strip))
             (bv (and (fx= r 0) (let ((bv (make-bytevector (%s4-buf-len w)))) (%s4-buf-copy! w bv) bv))))
        (%s4-buf-free! w)
        (let ((r (if (or (fx<= r -1000) %pending-escape) (%finish 'lua-dump u p r (list strip)) r)))
          (set! %busy-ctx #f)
          (if (and (fx= r 0) bv)
            bv
            (%api-fail 'lua-dump AUX_ENOMEM (list strip)))))))  ; the writer could not grow its buffer

  ;; ---------------------------------------------------------------- warnings

  (define %s4-warning (foreign-lambda* void (((c-pointer "lua_State") L) (scheme-object m) (bool tocont))
                        "size_t n; lua_warning(L, aux_bytes(m, &n), tocont);"))
  ;; ((lua-warning msg tocont) L): emits a warning, continued by the next one when tocont is #t (#f or #t only);
  ;; msg is a string without NUL.  The warning function is always the module's C one (lua-setwarnf).
  (define ((lua-warning msg tocont) L)
    (let ((p (%L 'lua-warning L)))
      (%cstring! 'lua-warning msg #f) (%bool! 'lua-warning tocont)
      (%done (%s4-warning p msg tocont))))

  ;; ---------------------------------------------------------------- the collector

  (define %s4-gc-options
    (list (cons 'stop (foreign-value "LUA_GCSTOP" int)) (cons 'restart (foreign-value "LUA_GCRESTART" int))
          (cons 'collect (foreign-value "LUA_GCCOLLECT" int)) (cons 'count (foreign-value "LUA_GCCOUNT" int))
          (cons 'countb (foreign-value "LUA_GCCOUNTB" int)) (cons 'step (foreign-value "LUA_GCSTEP" int))
          (cons 'isrunning (foreign-value "LUA_GCISRUNNING" int)) (cons 'gen (foreign-value "LUA_GCGEN" int))
          (cons 'inc (foreign-value "LUA_GCINC" int)) (cons 'param (foreign-value "LUA_GCPARAM" int))))
  (define %s4-gc-params
    (list (cons 'minormul (foreign-value "LUA_GCPMINORMUL" int)) (cons 'majorminor (foreign-value "LUA_GCPMAJORMINOR" int))
          (cons 'minormajor (foreign-value "LUA_GCPMINORMAJOR" int)) (cons 'pause (foreign-value "LUA_GCPPAUSE" int))
          (cons 'stepmul (foreign-value "LUA_GCPSTEPMUL" int)) (cons 'stepsize (foreign-value "LUA_GCPSTEPSIZE" int))))
  (define %s4-gcpn (foreign-value "LUA_GCPN" int))
  ;; a symbol of the table, or one of its codes, as the code; else #f
  (define (%s4-code x table)
    (cond ((symbol? x) (let ((a (assq x table))) (and a (cdr a))))
          ((fixnum? x) (and (memv x (map cdr table)) x))
          (else #f)))

  (define %s4-gc-d (foreign-lambda int "aux_s4_gc" bool (c-pointer "lua_State") int size_t int int))
  (define-lua-stubs (%s4-gc/f %s4-gc/s) ((int what) (size_t z) (int a) (int b))
    "C_return(aux_s4_gc(safe, L, what, z, a, b));")

  ;; ((lua-gc what . args) L) => lua_gc's int result.  what: a LUA_GC* code or its name without the
  ;; prefix, as a symbol ('stop 'restart 'collect 'count 'countb 'step 'isrunning 'gen 'inc 'param).
  ;;   'step n      n: a size_t, in BYTES (5.5; 5.4 counted Kbytes): the collector acts as if n new
  ;;                bytes had been allocated; 0 = one basic step
  ;;   'param p v   p: a LUA_GCP* code or 'minormul 'majorminor 'minormajor 'pause 'stepmul 'stepsize;
  ;;                v: the new value (an int), or a negative int to only read it; answers the old value.
  ;; 'collect 'step 'gen 'inc may run finalizers (Lua or Scheme); -1 inside a finalizer, as C.
  (define ((lua-gc what . args) L)
    (let ((w (%s4-code what %s4-gc-options)) (nargs (length args)) (all (cons what args)))
      (define (arity! n) (unless (fx= n nargs) (%api-error 'lua-gc "wrong number of arguments for this option" all)))
      (unless w (%api-error 'lua-gc "unknown option" all))
      (cond
        ((memq w (map (λ (s) (cdr (assq s %s4-gc-options))) '(stop restart count countb isrunning)))
         (arity! 0)
         (%done (%s4-gc-d #f (%L 'lua-gc L) w 0 0 0)))
        ((fx= w (cdr (assq 'param %s4-gc-options)))
         (arity! 2)
         (let ((param (%s4-code (car args) %s4-gc-params)) (value (cadr args)))
           (unless (and param (fx>= param 0) (fx< param %s4-gcpn)) (%api-error 'lua-gc "unknown collector parameter" all AUX_ERANGE))
           (%int! 'lua-gc value)
           (%done (%s4-gc-d #f (%L 'lua-gc L) w 0 param value))))
        ((fx= w (cdr (assq 'step %s4-gc-options)))
         (arity! 1)
         (let ((z (car args)) (zero 0))
           (unless (and (exact-integer? z) (>= z 0) (<= z 18446744073709551615)) (%api-error 'lua-gc "not a size" all AUX_ERANGE))
           (%P 'lua-gc L (%s4-gc/f %s4-gc/s) w z zero zero)))
        (else                                                 ; collect gen inc
         (arity! 0)
         (let ((zero 0)) (%P 'lua-gc L (%s4-gc/f %s4-gc/s) w zero zero zero))))))

  ;; ==========================================================================================
  ;; S5 — the debug interface: lua_getstack / lua_getinfo / locals / upvalues / hooks
  ;;
  ;; A lua_Debug is a luaDebug record: a malloc'd lua_Debug (freed by a finalizer), or, inside a
  ;; hook, Lua's own activation record.  lua_getinfo copies every field it fills into the record
  ;; (`fields'), so the lua-debug-* accessors never read Lua memory and keep working afterwards.
  ;;
  ;; A luaDebug that names an active function (from lua-getstack, or given to a hook) is usable
  ;; with lua-getinfo / lua-getlocal / lua-setlocal only while
  ;;   - the luaR it was obtained from is valid (a callback or hook view dies when it returns),
  ;;   - no lua-resume / lua-closethread happened since (the universe's epoch), and
  ;;   - its function is still active at the same level of the same thread (checked in C by
  ;;     walking the stack again: never a dangling CallInfo);
  ;; otherwise it is refused with (exn lua api) "stale lua_Debug".  As the output of a '>' query
  ;; (lua-getinfo), which describes the function on top and never reads the frame, any luaDebug
  ;; will do whose memory is valid: a malloc'd one always, a hook's while its hook runs.
  ;;
  ;; Hooks: ((lua-sethook proc mask count) L) with proc (λ (H event ar) answer), H a 'hook view;
  ;; while proc runs, the slots of the hooked thread up to its height at hook entry (the hooked
  ;; function's registers) cannot be lowered, moved or overwritten through H or any other luaR of
  ;; that thread (lua-setlocal assigns named locals only, and not the read-only ones the VM
  ;; trusts: a for loop's control variable, a named vararg parameter), event one of 'call 'return 'line 'count
  ;; 'tailcall, and ar a luaDebug valid during the hook only.  The answer is ignored, except
  ;; ((lua-yield 0) H), which yields the hooked coroutine (line and count events only).  A
  ;; raise inside proc becomes a Lua error in the hooked function, as for callbacks.  The hook
  ;; procedure is a callable handle: while one is set, the universe uses the safe path.  Call
  ;; and return hooks also see the (C) trampolines of protected bindings, whose slots (C state)
  ;; lua-getlocal refuses.  As in C, a coroutine
  ;; created by a hooked thread inherits the C hook but not the Scheme procedure: its hook is
  ;; inert (lua-gethook answers #f) until lua-sethook is used on it.
  ;; ==========================================================================================

  #>
  /* copies of the string fields of the last lua_getinfo (slots 0..4: name, namewhat, what, source,
     short_src) and of the last local / upvalue name (slot 5); valid[i] = 0 means NULL */
  static char *aux_dbg_s[6]; static size_t aux_dbg_n[6], aux_dbg_cap[6]; static int aux_dbg_valid[6];
  static void aux_dbg_sset(int i, const char *s, size_t n) {
    aux_dbg_valid[i] = 0;
    if (s == NULL) return;
    if (n + 1 > aux_dbg_cap[i]) {
      char *p = realloc(aux_dbg_s[i], n + 1);
      if (p == NULL) return;                              /* out of memory: the field reads as #f */
      aux_dbg_s[i] = p; aux_dbg_cap[i] = n + 1; }
    memcpy(aux_dbg_s[i], s, n); aux_dbg_s[i][n] = 0; aux_dbg_n[i] = n; aux_dbg_valid[i] = 1; }

  /* the function of ar is still active at `level' of L (never touches a dangling CallInfo) */
  static int aux_dbg_live(lua_State *L, const lua_Debug *ar, int level) {
    lua_Debug tmp;
    return level >= 0 && lua_getstack(L, level, &tmp) && tmp.i_ci == ar->i_ci; }

  /* the top of L may be lowered by one (a value popped by the call): above a hook view's floor,
     and not on a suspended/dead thread whose current frame is a Lua function (see aux_settop_d) */
  static int aux_dbg_pop_ok(lua_State *L, int floor) {
    int top = lua_gettop(L);
    floor = aux_eff_floor(L, floor);
    if (top < 1) return AUX_EUNDER;
    if (top - 1 < floor) return AUX_EINDEX;
    if (lua_status(L) != LUA_OK && aux_in_lua_frame(L)) return AUX_ETHREAD;
    return 0; }

  /* function indices of the upvalue API: READ indices; an upvalue pseudo-index only inside a
     Scheme closure (nup >= 0), where the running function is the C closure it indexes */
  static int aux_dbg_fix(lua_State *L, int fidx, int nup) {
    if (!aux_ix(L, fidx, AUX_IX_READ, 0)) return 0;
    return fidx >= LUA_REGISTRYINDEX || nup >= 0; }

  /* D part of lua_getinfo. what: ">SlnrutLf" letters, '>' first only, any length, letters
     repeated or none at all (as C).  'L' is left to the P part (aux_dbg_linetable): here the
     function is pushed instead ('f' semantics) when 'L' is asked.  The letters reach lua_getinfo
     once each (w holds at most ">Slnrutf": a repeated letter fills the same fields again in C, and
     'f' / 'L' push once however often they are asked).  Answers 1 when a function was pushed, 0
     otherwise, or a refusal code (an unknown letter: AUX_EARG, where C would return 0). */
  static int aux_dbg_getinfo(lua_State *L, lua_Debug *ar, int level, const char *what, size_t wn, int floor) {
    char w[16]; size_t i; int k = 0, gt, wantL = 0, wantf = 0;
    gt = wn > 0 && what[0] == '>';
    for (i = gt; i < wn; i++) {
      if (what[i] == 0 || strchr("SlnrutLf", what[i]) == NULL) return AUX_EARG;
      if (what[i] == 'L') wantL = 1;
      if (what[i] == 'f') wantf = 1; }
    if (gt) {
      int r = aux_dbg_pop_ok(L, floor);
      if (r) return r;
      if (lua_type(L, -1) != LUA_TFUNCTION) return AUX_ETYPE;
      w[k++] = '>'; }
    else if (!aux_dbg_live(L, ar, level)) return AUX_ESTALE;
    if (!lua_checkstack(L, 2)) return AUX_ESTACK;
    for (i = gt; i < wn; i++)
      if (what[i] != 'L' && what[i] != 'f' && memchr(w, what[i], k) == NULL) w[k++] = what[i];
    if (wantf || wantL) w[k++] = 'f';
    w[k] = 0;
    lua_getinfo(L, w, ar);                                /* D: no option left here allocates */
    for (i = gt; i < wn; i++)
      switch (what[i]) {
      case 'S': aux_dbg_sset(2, ar->what, strlen(ar->what)); aux_dbg_sset(3, ar->source, ar->srclen);
                aux_dbg_sset(4, ar->short_src, strlen(ar->short_src)); break;
      case 'n': aux_dbg_sset(0, ar->name, ar->name ? strlen(ar->name) : 0);
                aux_dbg_sset(1, ar->namewhat, ar->namewhat ? strlen(ar->namewhat) : 0); break; }
    return (wantf || wantL) ? 1 : 0; }

  static int aux_dbg_int(const lua_Debug *ar, int k) {
    switch (k) {
    case 6: return ar->currentline;  case 7: return ar->linedefined;  case 8: return ar->lastlinedefined;
    case 9: return ar->nups;         case 10: return ar->nparams;     case 11: return ar->isvararg;
    case 12: return ar->extraargs;   case 13: return ar->istailcall;  case 14: return ar->ftransfer;
    case 15: return ar->ntransfer;   default: return 0; } }

  /* P part of lua_getinfo's 'L': the function on top (pushed by the D part) becomes its table of
     valid lines (nil for a C function); with dup the function stays below it ('f' was asked).
     On failure the D part's function is removed too, so only the error value (if any) is left. */
  static int op_dbg_linetable(lua_State *L, aux_op *o) {
    lua_Debug ar; luaL_checktype(L, 1, LUA_TFUNCTION);   /* checked again on the copy: see op_s3_rawset */
    lua_getinfo(L, ">L", &ar); return 1; }
  static int aux_dbg_linetable(int safe, lua_State *L, int dup) {
    aux_op o; lua_State *H; int r, t0 = lua_gettop(L), ht;
    memset(&o, 0, sizeof o); o.fn = op_dbg_linetable;
    if (t0 < 1 || lua_type(L, -1) != LUA_TFUNCTION) return AUX_EUNDER;
    if (dup) { if (!lua_checkstack(L, 1)) { lua_settop(L, t0 - 1); return AUX_ESTACK; } lua_pushvalue(L, -1); }
    if (lua_status(L) == LUA_OK) {
      r = aux_run(safe, L, &o, 0, NULL, 1, 1);
      if (AUX_IS_LUAERR(r)) { if (dup) lua_remove(L, -2); return r; }
      if (r <= -1000) { lua_settop(L, t0 - 1); return r; }
      return 0; }
    H = AUX_CTX(L)->main; ht = lua_gettop(H);             /* suspended or dead thread: run on the main thread */
    if (!lua_checkstack(H, 1)) { lua_settop(L, t0 - 1); return AUX_ESTACK; }
    lua_xmove(L, H, 1);
    r = aux_run(safe, H, &o, 0, NULL, 1, 1);
    if (AUX_IS_LUAERR(r)) { lua_settop(L, t0 - 1); return AUX_LUAERR_M(-r - 1000); }
    if (r <= -1000) { lua_settop(H, ht); lua_settop(L, t0 - 1); return r; }
    if (!lua_checkstack(L, 1)) { lua_settop(H, ht); lua_settop(L, t0 - 1); return AUX_ESTACK; }   /* L may have run */
    lua_xmove(H, L, 1); return 0; }

  /* 1 iff ar's function is the trampoline of a protected op (a live frame): the op's C frame holds the
     copies of its operands and the C state of the call it makes -- lua_load's half-built closure and its
     anchor table, a luaL_Buffer's box, lua_dump's string table -- which C trusts and nothing else may see.
     D: 'f' pushes the function, no allocation. */
  static int aux_dbg_opframe(lua_State *L, const lua_Debug *ar) {
    lua_Debug a = *ar; int r;
    if (!lua_checkstack(L, 1)) return 1;                  /* refused when it cannot be told */
    lua_getinfo(L, "f", &a);
    r = lua_tocfunction(L, -1) == aux_tramp;
    lua_pop(L, 1); return r; }

  /* locals: 1 = a name (in slot 5), 0 = NULL, or a refusal code */
  static int aux_dbg_getlocal(lua_State *L, const lua_Debug *ar, int level, int n) {
    const char *name;
    if (ar == NULL) {                                     /* parameters of the function on top */
      if (lua_gettop(L) < 1) return AUX_EUNDER;
      name = lua_getlocal(L, NULL, n); }
    else {
      if (!aux_dbg_live(L, ar, level)) return AUX_ESTALE;
      if (n > 0 && aux_dbg_opframe(L, ar)) return AUX_EARG;
      if (!lua_checkstack(L, 1)) return AUX_ESTACK;
      name = lua_getlocal(L, ar, n); }                    /* pushes the value iff name != NULL */
    if (name == NULL) return 0;
    aux_dbg_sset(5, name, strlen(name)); return 1; }
  /* A local whose name starts with '(' (other than "(vararg)", an extra argument) is not a variable of the
     program: a temporary, a for loop's state, 5.5's "(vararg table)", a C function's slot.  The VM (or the
     C function) trusts the types it put there -- a numeric for loop's counter becomes a table under its
     feet -- so it is refused. */
  static int aux_dbg_internal(const char *name) {
    return name[0] == '(' && strcmp(name, "(vararg)") != 0; }
  /* the name of local k of ar's function, NULL if none (nothing is left pushed; the name lives in the
     function's prototype) */
  static const char *aux_dbg_localname(lua_State *L, const lua_Debug *ar, int k) {
    const char *name = lua_getlocal(L, ar, k);
    if (name) lua_pop(L, 1);
    return name; }
  /* A named local the VM trusts (5.5): a for loop's control variable, declared right after the loop's
     "(for state)" locals (read-only in 5.5; the numeric loop's is its own counter, which OP_FORLOOP reads
     as an integer or a float without checking), or the vararg parameter of `function f(...t)', local
     nparams + 1 (the vararg table, which OP_VARARG reads as a table without checking).  A main chunk is
     vararg too (lparser.c's mainfunc) but has no vararg parameter: its local nparams + 1 (= 1) is its
     first ordinary `local', assigned as C does.  (lua_getlocal(L, NULL, nparams + 1) cannot tell them
     apart: the vararg parameter is registered after OP_VARARGPREP, so it is not active at pc 0.)  The
     parser gives a main chunk, and only it, linedefined 0 (what "main"); a non-main vararg function's
     local nparams + 1 is always its vararg parameter, named or "(vararg table)".  D. */
  static int aux_dbg_readonly(lua_State *L, const lua_Debug *ar, int n) {
    const char *a, *b; lua_Debug u = *ar;
    if (n >= 3 && (a = aux_dbg_localname(L, ar, n - 1)) != NULL && strcmp(a, "(for state)") == 0
               && (b = aux_dbg_localname(L, ar, n - 2)) != NULL && strcmp(b, "(for state)") == 0) return 1;
    lua_getinfo(L, "Su", &u);                             /* no allocation */
    return u.isvararg && n == u.nparams + 1 && u.linedefined != 0; }
  static int aux_dbg_setlocal(lua_State *L, const lua_Debug *ar, int level, int n, int floor) {
    const char *name; int r;
    if (!aux_dbg_live(L, ar, level)) return AUX_ESTALE;
    if ((r = aux_dbg_pop_ok(L, floor))) return r;
    if (!lua_checkstack(L, 1)) return AUX_ESTACK;
    name = lua_getlocal(L, ar, n);                        /* its name first (pushes the value iff found) */
    if (name == NULL) return 0;
    r = aux_dbg_internal(name);
    lua_pop(L, 1);                                        /* our own push */
    if (r) return AUX_EARG;
    if (n > 0 && aux_dbg_readonly(L, ar, n)) return AUX_ERDONLY;
    name = lua_setlocal(L, ar, n);                        /* pops the value iff name != NULL (C) */
    if (name == NULL) return 0;
    aux_dbg_sset(5, name, strlen(name)); return 1; }

  /* upvalues (raw numbering: a Scheme closure's upvalue 1 is its handle) */
  static int aux_dbg_getupvalue(lua_State *L, int fidx, int n, int nup) {
    const char *name;
    if (!aux_dbg_fix(L, fidx, nup)) return AUX_EINDEX;
    if (!lua_checkstack(L, 1)) return AUX_ESTACK;
    name = lua_getupvalue(L, fidx, n);                    /* pushes the value iff name != NULL */
    if (name == NULL) return 0;
    aux_dbg_sset(5, name, strlen(name)); return 1; }
  /* n is in range for the closure at fidx (the manual's precondition of upvalueid / upvaluejoin) */
  static int aux_dbg_upvalue_ok(lua_State *L, int fidx, int n) {
    int top = lua_gettop(L);
    if (lua_getupvalue(L, fidx, n) == NULL) return 0;
    lua_settop(L, top); return 1; }                       /* drops our own push only */

  /* The kind of the variable upvalue `up' (0-based) of a Lua function is bound to, read from its binary
     chunk b[0..n) (lua_dump, stripped): 0 a regular variable, 1 <const> (a for loop's control variable
     too), 2 a named vararg parameter, 3 <close> (lparser.h's VDKREG RDKCONST RDKVAVAR RDKTOCLOSE); -1
     when the chunk cannot be read.  The public API has no other way to it.  Format (ldump.c): header,
     the upvalue count, then the function: linedefined, lastlinedefined, numparams, flag, maxstacksize,
     code (aligned), constants, upvalues (instack, idx, kind). */
  static int aux_dbg_varint(const unsigned char *b, size_t n, size_t *p, size_t *x) {
    size_t v = 0; int c;
    do { if (*p >= n || v > ((size_t)-1) >> 8) return 0; c = b[(*p)++]; v = (v << 7) | (size_t)(c & 0x7f); } while (c & 0x80);
    *x = v; return 1; }
  static int aux_dbg_upkind(const unsigned char *b, size_t n, int up) {
    size_t p = 12, k, i, x, sz[4];
    static const char data[] = "\x19\x93\r\n\x1a\n";
    if (n < 12 || memcmp(b, LUA_SIGNATURE, 4) != 0 || memcmp(b + 6, data, 6) != 0) return -1;
    for (i = 0; i < 4; i++) {                             /* sizes of int, Instruction, lua_Integer, lua_Number */
      if (p >= n) return -1;
      sz[i] = b[p]; p += 1 + sz[i]; }
    p += 1;                                               /* the upvalue count of the main closure */
    if (!aux_dbg_varint(b, n, &p, &x) || !aux_dbg_varint(b, n, &p, &x)) return -1;   /* line numbers */
    p += 3;                                               /* numparams, flag, maxstacksize */
    if (!aux_dbg_varint(b, n, &p, &k)) return -1;         /* code */
    if (sz[1] == 0 || k > n / sz[1]) return -1;
    if (p % sz[1]) p += sz[1] - p % sz[1];
    p += k * sz[1];
    if (!aux_dbg_varint(b, n, &p, &k)) return -1;         /* constants */
    for (i = 0; i < k; i++) {
      if (p >= n) return -1;
      switch (b[p++]) {
      case 0x00: case 0x01: case 0x11: break;             /* nil, false, true */
      case 0x03: if (!aux_dbg_varint(b, n, &p, &x)) return -1; break;          /* integer */
      case 0x13: p += sz[3]; break;                       /* float */
      case 0x04: case 0x14:                               /* string: a size + 1, or 0 and a reused index */
        if (!aux_dbg_varint(b, n, &p, &x)) return -1;
        if (x == 0) { if (!aux_dbg_varint(b, n, &p, &x)) return -1; }
        else { if (x > n) return -1; p += x; }
        break;
      default: return -1; } }
    if (!aux_dbg_varint(b, n, &p, &k)) return -1;         /* upvalues */
    if (up < 0 || (size_t)up >= k || p + 3 * (size_t)up + 2 >= n) return -1;
    return b[p + 3 * (size_t)up + 2]; }
  static int op_dbg_upkind(lua_State *L, aux_op *o) {     /* arg 1: a copy of the Lua function */
    aux_s4_buf *w = (aux_s4_buf *)o->p0;
    o->r = lua_dump(L, aux_s4_writer, w, 1) == 0 ? aux_dbg_upkind((const unsigned char *)w->b, w->n, o->i0) : -1;
    return 0; }
  /* P: the kind of upvalue n of the Lua function at fidx (on a suspended or dead thread the op runs on
     the main thread), or a code <= -1000 */
  static int aux_dbg_upvalkind(int safe, lua_State *L, int fidx, int n) {
    aux_op o; aux_s4_buf w; int r, refs[1];
    memset(&o, 0, sizeof o); memset(&w, 0, sizeof w);
    o.fn = op_dbg_upkind; o.p0 = &w; o.i0 = n - 1; refs[0] = fidx;
    if (lua_status(L) == LUA_OK) r = aux_run(safe, L, &o, 1, refs, 0, 0);
    else {
      lua_State *M = AUX_CTX(L)->main;
      if (!lua_checkstack(L, 1) || !lua_checkstack(M, 1)) return AUX_ESTACK;
      lua_pushvalue(L, fidx); lua_xmove(L, M, 1);
      r = aux_run(safe, M, &o, 0, NULL, 1, 0);
      if (AUX_IS_LUAERR(r)) r = AUX_LUAERR_M(-r - 1000); }
    free(w.b); return r; }
  /* The upvalues of a C closure are its C state (lstrlib's gmatch keeps a raw pointer to a userdata in
     one): only the user upvalues of a Scheme closure (raw number >= 2) may be assigned among them.  An
     upvalue of a Lua closure bound to a read-only variable (kind != 0) is refused: while it is open it
     is the variable's register, which the VM may trust (a numeric for loop's counter, a vararg table).
     The kind is read by a P op, which may run a finalizer: the function is looked up again after it.
     An upvalue that does not exist (n outside 1..nupvalues, any n of a light C function or of a value
     that is no function) is no C state: NULL, nothing popped, as C, before any refusal. */
  static int aux_dbg_setupvalue(int safe, lua_State *L, int fidx, int n, int nup, int floor) {
    const char *name; int r;
    if (!aux_dbg_fix(L, fidx, nup)) return AUX_EINDEX;
    if (lua_gettop(L) < 1) return AUX_EUNDER;             /* C's api_checknelems(L, 1), found or not */
    if (!lua_checkstack(L, 1)) return AUX_ESTACK;
    if (!aux_dbg_upvalue_ok(L, fidx, n)) return 0;        /* no such upvalue: nothing popped, as C */
    if (lua_iscfunction(L, fidx) && (lua_tocfunction(L, fidx) != aux_cfunction || n < 2)) return AUX_EARG;
    if ((r = aux_dbg_pop_ok(L, floor))) return r;
    if (lua_type(L, fidx) == LUA_TFUNCTION && !lua_iscfunction(L, fidx)) {
      const void *f = lua_topointer(L, fidx); int k;
      k = aux_dbg_upvalkind(safe, L, fidx, n);
      if (k <= -1000) return k;
      if (!aux_dbg_fix(L, fidx, nup) || lua_topointer(L, fidx) != f) return AUX_EINDEX;
      if ((r = aux_dbg_pop_ok(L, floor))) return r;
      if (k != 0) return AUX_ERDONLY; }
    name = lua_setupvalue(L, fidx, n);                    /* pops the value iff name != NULL (C) */
    if (name == NULL) return 0;
    aux_dbg_sset(5, name, strlen(name)); return 1; }
  static void *aux_dbg_upvalueid(lua_State *L, int fidx, int n, int nup, int *err) {
    *err = 0;
    if (!aux_dbg_fix(L, fidx, nup)) { *err = AUX_EINDEX; return NULL; }
    if (lua_type(L, fidx) != LUA_TFUNCTION) { *err = AUX_ETYPE; return NULL; }
    if (!lua_checkstack(L, 1)) { *err = AUX_ESTACK; return NULL; }
    if (!aux_dbg_upvalue_ok(L, fidx, n)) return NULL;
    return lua_upvalueid(L, fidx, n); }
  /* f1's upvalue n1 is made to share f2's upvalue n2.  Refused when the latter is bound to a read-only
     variable (kind != 0 in f2's prototype, read as lua-setupvalue does): f1 could then write the
     variable's register, by lua-setupvalue (which checks f1's own prototype, kind 0) or by its own
     code, and the VM trusts it (a numeric for loop's counter).  The kind is read by a P op, which may
     run a finalizer: both functions are looked up again after it. */
  static int aux_dbg_upvaluejoin(int safe, lua_State *L, int f1, int n1, int f2, int n2, int nup) {
    const void *p1, *p2; int k;
    if (!aux_dbg_fix(L, f1, nup) || !aux_dbg_fix(L, f2, nup)) return AUX_EINDEX;
    if (lua_type(L, f1) != LUA_TFUNCTION || lua_iscfunction(L, f1) ||
        lua_type(L, f2) != LUA_TFUNCTION || lua_iscfunction(L, f2)) return AUX_ETYPE;
    if (!lua_checkstack(L, 1)) return AUX_ESTACK;
    if (!aux_dbg_upvalue_ok(L, f1, n1) || !aux_dbg_upvalue_ok(L, f2, n2)) return AUX_ERANGE;
    p1 = lua_topointer(L, f1); p2 = lua_topointer(L, f2);
    k = aux_dbg_upvalkind(safe, L, f2, n2);
    if (k <= -1000) return k;
    if (!aux_dbg_fix(L, f1, nup) || !aux_dbg_fix(L, f2, nup) ||
        lua_topointer(L, f1) != p1 || lua_topointer(L, f2) != p2) return AUX_EINDEX;
    if (k != 0) return AUX_ERDONLY;
    lua_upvaluejoin(L, f1, n1, f2, n2);                   /* a GC barrier only: no allocation */
    return 0; }

  /* ---------- hooks ---------- */
  int aux_lua_hookdispatch(lua_State *L, int h, void *ar, int event, int line, int floor);   /* define-external */

  /* the HOOK handle registered for thread L in registry[&aux_hook_key] (a weak-keyed table
     thread -> handle userdata), -1 when none, or AUX_ESTACK.  D: the lookup runs on L when it is
     running or idle, else on the main thread (L's top is left as it was). */
  static int aux_hook_lookup(lua_State *L) {
    lua_State *S = lua_status(L) == LUA_OK ? L : AUX_CTX(L)->main;
    int h = -1, top; aux_handle *u;
    if (!lua_checkstack(L, 1) || !lua_checkstack(S, 4)) return AUX_ESTACK;
    top = lua_gettop(S);
    lua_pushthread(L);
    if (S != L) lua_xmove(L, S, 1);
    if (lua_rawgetp(S, LUA_REGISTRYINDEX, &aux_hook_key) == LUA_TTABLE) {
      lua_rotate(S, -2, 1);                               /* table thread */
      lua_rawget(S, -2);
      u = aux_tohandle(S, -1);
      if (u && u->kind == AUX_K_HOOK) h = u->handle; }
    lua_settop(S, top);                                   /* our own pushes only */
    return h; }

  static void aux_hook(lua_State *L, lua_Debug *ar) {
    int h, r, saved, floor, nf; lua_State *cur;
    if (!aux_reentrant) return;                           /* I2: never a callback inside a non-safe stub */
    h = aux_hook_lookup(L);
    if (h < 0) return;
    if (aux_cstack_low()) { luaL_error(L, "C stack overflow"); return; }   /* no Scheme ran: raising is fine */
    floor = lua_gettop(L);
    nf = aux_nfloors;
    if (!aux_floor_push(L, floor)) return;                /* the hooked function's registers (I6); too deep: skip */
    saved = aux_reentrant; aux_reentrant = 0; cur = aux_cur; aux_cur = L; aux_ncb++;
    r = aux_lua_hookdispatch(L, h, ar, ar->event, ar->currentline, floor);   /* C_callback returns here */
    AUX_CB_LEAVE(nf); aux_reentrant = saved; aux_cur = cur;   /* its floor, and the records of its callback */
    switch (r) {
    case 0: return;
    case AUX_YIELD: lua_yield(L, 0); aux_nk_hookyield(L, floor); return;   /* inside a hook: returns, Lua yields after it */
    case AUX_ESCAPE: lua_settop(L, floor); lua_pushlightuserdata(L, &aux_escape_key); lua_error(L); return;   /* the hook's own values dropped: LUA_MINSTACK is free */
    default: aux_answer(L, r, floor); return; } }         /* raises: every longjmp after C_callback returned */

  /* arg 1: the thread whose hook is set (i0: handle, or -1 to clear; i1 mask; i2 count; L1 the thread).
     The handle is stored LAST: success of the op <=> Lua owns the handle. */
  static int op_hook_set(lua_State *L, aux_op *o) {      /* arg 1: the thread o->L1 */
    aux_handle *u = NULL;
    aux_push_weakk(L, &aux_hook_key);
    lua_pushvalue(L, 1);
    if (o->i0 >= 0) {
      u = (aux_handle *)lua_newuserdatauv(L, sizeof *u, 0);           /* (a collection step: see aux_slot_bad) */
      aux_slot_newud(L, -1, u);
      u->magic = AUX_MAGIC; u->handle = -1; u->kind = AUX_K_HOOK; u->nup = 0;
      aux_push_mt(L); lua_setmetatable(L, -2); }
    else lua_pushnil(L);
    aux_slot_thread(L, -2, o->L1); AUX_SLOT(L, -3, LUA_TTABLE);
    lua_rawset(L, -3);
    if (u) u->handle = o->i0;
    lua_sethook(o->L1, u ? aux_hook : NULL, u ? o->i1 : 0, o->i2);    /* C keeps the count when it removes the hook */
    return 0; }
  static int aux_hook_set(int safe, lua_State *L, int h, int mask, int count) {
    aux_op o; lua_State *H;
    memset(&o, 0, sizeof o); o.fn = op_hook_set; o.i0 = h; o.i1 = mask; o.i2 = count; o.L1 = L;
    if (mask < 0 || mask > 15 || count < 0) return AUX_ERANGE;
    if (!lua_checkstack(L, 1)) return AUX_ESTACK;
    if (lua_status(L) == LUA_OK) { lua_pushthread(L); return aux_run(safe, L, &o, 0, NULL, 1, 0); }
    H = AUX_CTX(L)->main;                                 /* suspended or dead: the op runs on the main thread */
    if (!lua_checkstack(H, 1)) return AUX_ESTACK;
    lua_pushthread(L); lua_xmove(L, H, 1);
    { int r = aux_run(safe, H, &o, 0, NULL, 1, 0);
      return AUX_IS_LUAERR(r) ? AUX_LUAERR_M(-r - 1000) : r; } }
  <#

  ;; ---------- records and accessors ----------

  ;; ptr: lua_Debug* (malloc'd, or Lua's own inside a hook); u: the universe; epoch: the
  ;; universe's resume counter when obtained; view: the luaR it was obtained from; fields: the
  ;; copied fields (see lua-debug-*); level: the stack level of its function, -1 for none;
  ;; own: #t when ptr is malloc'd (valid until the finalizer), #f for a hook's (valid while its
  ;; view is).
  (define-record luaDebug ptr u epoch view fields level own)
  (set-record-printer! luaDebug
    (λ (d out) (display (string-append "#<luaDebug" (if (fx>= (luaDebug-level d) 0) (string-append " level " (number->string (luaDebug-level d))) "") ">") out)))

  ;; the fields, as copied by lua-getinfo (and, for event and currentline, at hook entry); #f
  ;; for a field no call filled yet
  (define (lua-debug-event d) (vector-ref (luaDebug-fields d) 0))           ; 'call 'return 'line 'count 'tailcall
  (define (lua-debug-name d) (vector-ref (luaDebug-fields d) 1))            ; n: string or #f
  (define (lua-debug-namewhat d) (vector-ref (luaDebug-fields d) 2))        ; n: "global" "local" "method" "field" "" ...
  (define (lua-debug-what d) (vector-ref (luaDebug-fields d) 3))            ; S: "Lua" "C" "main"
  (define (lua-debug-source d) (vector-ref (luaDebug-fields d) 4))          ; S
  (define (lua-debug-short-src d) (vector-ref (luaDebug-fields d) 5))       ; S
  (define (lua-debug-currentline d) (vector-ref (luaDebug-fields d) 6))     ; l
  (define (lua-debug-linedefined d) (vector-ref (luaDebug-fields d) 7))     ; S
  (define (lua-debug-lastlinedefined d) (vector-ref (luaDebug-fields d) 8)) ; S
  (define (lua-debug-nups d) (vector-ref (luaDebug-fields d) 9))            ; u
  (define (lua-debug-nparams d) (vector-ref (luaDebug-fields d) 10))        ; u
  (define (lua-debug-isvararg d) (vector-ref (luaDebug-fields d) 11))       ; u: boolean
  (define (lua-debug-extraargs d) (vector-ref (luaDebug-fields d) 12))      ; t
  (define (lua-debug-istailcall d) (vector-ref (luaDebug-fields d) 13))     ; t: boolean
  (define (lua-debug-ftransfer d) (vector-ref (luaDebug-fields d) 14))      ; r
  (define (lua-debug-ntransfer d) (vector-ref (luaDebug-fields d) 15))      ; r
  ;; S: srclen, the length of source in bytes as C counts it (lua-debug-source is decoded text: for a
  ;; non-ASCII chunk name its string-length is a character count)
  (define (lua-debug-srclen d) (vector-ref (luaDebug-fields d) 16))         ; S

  ;; ---------- raw helpers ----------

  (define %dbg-malloc (foreign-lambda* c-pointer () "C_return(calloc(1, sizeof(lua_Debug)));"))
  (define %dbg-free (foreign-lambda void "free" c-pointer))
  (define (%dbg-finalize d) (let ((p (luaDebug-ptr d))) (when p (luaDebug-ptr-set! d #f) (%dbg-free p))))
  (define %dbg-epoch (foreign-lambda* int (((c-pointer "lua_State") L)) "C_return((int)(AUX_CTX(L)->epoch & 0x3fffffff));"))
  ;; test hook: what lua-resume / lua-closethread do to the epoch
  (define %dbg-epoch-bump! (foreign-lambda* void (((c-pointer "lua_State") L)) "AUX_CTX(L)->epoch++;"))
  (define %dbg-int (foreign-lambda* int ((c-pointer ar) (int k)) "C_return(aux_dbg_int((lua_Debug *)ar, k));"))
  (define %dbg-slen (foreign-lambda* int ((int i)) "C_return(aux_dbg_valid[i] ? (int)aux_dbg_n[i] : -1);"))
  (define %dbg-scopy! (foreign-lambda* void ((int i) (scheme-object bv)) "memcpy(C_data_pointer(bv), aux_dbg_s[i], aux_dbg_n[i]);"))
  ;; the string copied in slot i (a bytevector when it is not UTF-8), #f for NULL
  (define (%dbg-string i)
    (let ((n (%dbg-slen i)))
      (and (fx>= n 0) (let ((bv (make-bytevector n))) (%dbg-scopy! i bv) (%bytes->string/maybe bv)))))

  ;; a fresh luaDebug (malloc'd lua_Debug, freed by the finalizer)
  (define (%dbg-new who L p level)
    (let ((ptr (%dbg-malloc)))
      (unless ptr (%api-fail who AUX_ENOMEM (list level)))
      (set-finalizer! (make-luaDebug ptr (luaR-u L) (%dbg-epoch p) L (make-vector 17 #f) level #t) %dbg-finalize)))

  ;; the checks every use of a luaDebug makes in Scheme (the frame itself is checked in C)
  (define (%dbg-check who d L p args)
    (unless (luaDebug? d) (%api-error who "not a lua_Debug" args))
    (unless (eq? (luaDebug-u d) (luaR-u L)) (%api-error who "a lua_Debug of another universe" args))
    (unless (and (luaDebug-ptr d) (luaR-state (luaDebug-view d)) (fx= (luaDebug-epoch d) (%dbg-epoch p)))
      (%api-fail who AUX_ESTALE args)))

;; a luaDebug used as the output of a '>' query: its lua_Debug memory must still be valid
  (define (%dbg-check-output who d args)
    (unless (luaDebug? d) (%api-error who "not a lua_Debug" args))
    (unless (and (luaDebug-ptr d) (or (luaDebug-own d) (luaR-state (luaDebug-view d))))
      (%api-fail who AUX_ESTALE args)))

  (define (%dbg-has-char? s c)
    (let loop ((i 0)) (and (fx< i (string-length s)) (or (char=? c (string-ref s i)) (loop (fx+ i 1))))))

  ;; copy what lua_getinfo filled for the options of `what' into the record
  (define (%dbg-read! d what)
    (let ((f (luaDebug-fields d)) (ptr (luaDebug-ptr d)))
      (define (int! k) (vector-set! f k (%dbg-int ptr k)))
      (define (bool! k) (vector-set! f k (not (fx= 0 (%dbg-int ptr k)))))
      (do ((i 0 (fx+ i 1))) ((fx= i (string-length what)))
        (case (string-ref what i)
          ((#\S) (vector-set! f 3 (%dbg-string 2)) (vector-set! f 4 (%dbg-string 3)) (vector-set! f 5 (%dbg-string 4))
                 (vector-set! f 16 (let ((n (%dbg-slen 3))) (and (fx>= n 0) n)))   ; ar->srclen, as copied
                 (int! 7) (int! 8))
          ((#\n) (vector-set! f 1 (%dbg-string 0)) (vector-set! f 2 (%dbg-string 1)))
          ((#\l) (int! 6))
          ((#\u) (int! 9) (int! 10) (bool! 11))
          ((#\t) (int! 12) (bool! 13))
          ((#\r) (int! 14) (int! 15))))))

  ;; ---------- lua_getstack / lua_getinfo ----------

  (define %dbg-getstack (foreign-lambda* int (((c-pointer "lua_State") L) (int level) (c-pointer ar))
                      "C_return(lua_getstack(L, level, (lua_Debug *)ar));"))
  ;; ((lua-getstack level) L) => a luaDebug for the function running at `level' of L (0 = the
  ;; current one), or #f when the stack is not that deep
  (define ((lua-getstack level) L)
    (%int! 'lua-getstack level)
    (let ((p (%L 'lua-getstack L)))
      (if (fx< level 0)
        #f
        (let ((d (%dbg-new 'lua-getstack L p level)))
          (if (fx= 1 (%done (%dbg-getstack p level (luaDebug-ptr d)))) d (begin (%dbg-finalize d) #f))))))

  (define %dbg-getinfo (foreign-lambda* int (((c-pointer "lua_State") L) (c-pointer ar) (int level) (scheme-object what) (int floor))
                       "size_t n; const char *w = aux_bytes(what, &n);
                        C_return(aux_dbg_getinfo(L, (lua_Debug *)ar, level, w, n, floor));"))
  (define-lua-stubs (%dbg-linetable/f %dbg-linetable/s) ((bool dup)) "C_return(aux_dbg_linetable(safe, L, dup));")

  ;; ((lua-getinfo what ar) L) => ar, filled.  what: letters of "SlnrutLf", optionally led by
  ;; ">": then the function on top is popped and described, and ar may be #f (a fresh luaDebug).
  ;; As in C, what may be of any length, repeat letters or have none ("" fills nothing, ">" only
  ;; pops the function).  The binding returns ar, not C's int: an unknown letter (or a '>' that is
  ;; not first), for which C would return 0, is refused with (exn lua api) before anything runs.
  ;; 'f' pushes the function, 'L' its table of valid lines (nil for a C function), in that order.
  ;; With '>' ar is only written, as C (its frame, if any, is neither read nor changed): any
  ;; luaDebug whose memory is valid will do, however old -- not a hook's once the hook returned.
  (define ((lua-getinfo what ar) L)
    (%cstring! 'lua-getinfo what #f)
    (let* ((p (%L 'lua-getinfo L))
           (gt? (and (fx> (string-length what) 0) (char=? #\> (string-ref what 0))))
           (d (cond ((and ar gt?) (%dbg-check-output 'lua-getinfo ar (list what ar)) ar)
                    (ar (%dbg-check 'lua-getinfo ar L p (list what ar)) ar)
                    (gt? (%dbg-new 'lua-getinfo L p -1))
                    (else (%api-error 'lua-getinfo "a lua_Debug is required unless `what' starts with >" (list what ar))))))
      (%D 'lua-getinfo (%dbg-getinfo p (luaDebug-ptr d) (luaDebug-level d) what (luaR-floor L)) what ar)
      (%dbg-read! d what)
      (when (%dbg-has-char? what #\L)
        (let ((dup (%dbg-has-char? what #\f)))
          (%P 'lua-getinfo L (%dbg-linetable/f %dbg-linetable/s) dup)))
      d))

  ;; ---------- locals ----------

  (define %dbg-getlocal (foreign-lambda* int (((c-pointer "lua_State") L) (c-pointer ar) (int level) (int n))
                      "C_return(aux_dbg_getlocal(L, (const lua_Debug *)ar, level, n));"))
  (define %dbg-setlocal (foreign-lambda* int (((c-pointer "lua_State") L) (c-pointer ar) (int level) (int n) (int floor))
                      "C_return(aux_dbg_setlocal(L, (const lua_Debug *)ar, level, n, floor));"))
  (define (%dbg-name r) (and (fx= r 1) (or (%dbg-string 5) "")))

  ;; ((lua-getlocal ar n) L) => the name of local n of ar's function, pushing its value, or #f
  ;; (nothing pushed).  n < 0: varargs.  ar #f: the name of parameter n of the Lua function on
  ;; top (nothing pushed; #f for a C function).  The slots of a protected binding's own frame (the
  ;; C trampoline every P binding runs in, which call and return hooks see) are refused with (exn
  ;; lua api): they hold C state, such as the half-built chunk of a load in progress.
  (define ((lua-getlocal ar n) L)
    (%int! 'lua-getlocal n)
    (let ((p (%L 'lua-getlocal L)))
      (when ar (%dbg-check 'lua-getlocal ar L p (list ar n)))
      (let ((r (%dbg-getlocal p (and ar (luaDebug-ptr ar)) (if ar (luaDebug-level ar) -1) n)))
        (when (fx= r AUX_EARG) (%api-error 'lua-getlocal "a slot of a protected binding's own frame (C state)" (list ar n)))
        (%dbg-name (%D 'lua-getlocal r ar n)))))

  ;; ((lua-setlocal ar n) L) => the name of local n, assigned the value on top (popped), or #f
  ;; when there is no such local (nothing popped, as in C).  Refused with (exn lua api), because the
  ;; VM (or C) trusts the types it keeps there: a local whose name starts with "(" (a temporary, a
  ;; for loop's state, a C function's slot; varargs excepted), and a for loop's control variable or
  ;; a named vararg parameter (`function f(...t)'), read-only in Lua 5.5 (code AUX_ERDONLY).
  (define ((lua-setlocal ar n) L)
    (%int! 'lua-setlocal n)
    (let ((p (%L 'lua-setlocal L)))
      (%dbg-check 'lua-setlocal ar L p (list ar n))
      (let ((r (%dbg-setlocal p (luaDebug-ptr ar) (luaDebug-level ar) n (luaR-floor L))))
        (when (fx= r AUX_EARG) (%api-error 'lua-setlocal "not a variable of the program (an internal local)" (list ar n)))
        (%dbg-name (%D 'lua-setlocal r ar n)))))

  ;; ---------- upvalues ----------

  (define %dbg-getupvalue (foreign-lambda* int (((c-pointer "lua_State") L) (int fidx) (int n) (int nup))
                        "C_return(aux_dbg_getupvalue(L, fidx, n, nup));"))
  (define-lua-stubs (%dbg-setupvalue/f %dbg-setupvalue/s) ((int fidx) (int n) (int nup) (int floor))
    "C_return(aux_dbg_setupvalue(safe, L, fidx, n, nup, floor));")
  (define %dbg-upvalueid (foreign-lambda* c-pointer (((c-pointer "lua_State") L) (int fidx) (int n) (int nup) ((c-pointer int) err))
                       "C_return(aux_dbg_upvalueid(L, fidx, n, nup, err));"))
  (define-lua-stubs (%dbg-upvaluejoin/f %dbg-upvaluejoin/s) ((int f1) (int n1) (int f2) (int n2) (int nup))
    "C_return(aux_dbg_upvaluejoin(safe, L, f1, n1, f2, n2, nup));")

  ;; ((lua-getupvalue funcindex n) L) => the name of upvalue n of the closure (pushing its
  ;; value): "" for a C closure; #f (nothing pushed) when there is none.  Raw numbering: upvalue
  ;; 1 of a Scheme closure is its handle, its user upvalue i is number i+1.
  (define ((lua-getupvalue funcindex n) L)
    (%int! 'lua-getupvalue funcindex n)
    (%dbg-name (%D 'lua-getupvalue (%dbg-getupvalue (%L 'lua-getupvalue L) funcindex n (luaR-nup L)) funcindex n)))

  ;; ((lua-setupvalue funcindex n) L) => the name of upvalue n, assigned the value on top
  ;; (popped), or #f (nothing popped, as in C) when there is none, whatever the function (a light
  ;; C function has none).  Refused with (exn lua api): of a C closure, every existing upvalue but
  ;; the user upvalues of a Scheme closure (n >= 2), C state that C code reads unchecked; of a Lua closure, an upvalue bound to a read-only variable (<const>, <close>, a for
  ;; loop's control variable, a named vararg parameter; code AUX_ERDONLY): while the variable's
  ;; function runs, the upvalue is its register, which the VM may trust.  For a Lua closure the
  ;; binding is P (the variable's kind is read from lua_dump's output), else D.
  (define ((lua-setupvalue funcindex n) L)
    (%int! 'lua-setupvalue funcindex n)
    (let* ((u (%U 'lua-setupvalue L)) (nup (luaR-nup L)) (floor (luaR-floor L)))
      (%prepare! u)
      (let* ((p (%L 'lua-setupvalue L))
             (r (%run u (%dbg-setupvalue/f %dbg-setupvalue/s) p funcindex n nup floor)))
        (cond
          ((and (fx> r -1000) (not %pending-escape)) (set! %busy-ctx #f) (%dbg-name r))
          ((and (fx= r AUX_EARG) (not %pending-escape))
           (%api-error 'lua-setupvalue "an upvalue of a C function (not a Scheme user upvalue)" (list funcindex n)))
          (else (%dbg-name (%finish 'lua-setupvalue u p r (list funcindex n))))))))

  ;; ((lua-upvalueid funcindex n) L) => a pointer identifying upvalue n (shared upvalues answer
  ;; the same one), #f when n is out of range
  (define ((lua-upvalueid funcindex n) L)
    (%int! 'lua-upvalueid funcindex n)
    (let ((p (%L 'lua-upvalueid L)))
      (let-location ((err int 0))
        (let ((id (%done (%dbg-upvalueid p funcindex n (luaR-nup L) (location err)))))
          (if (fx= err 0) id (%api-fail 'lua-upvalueid err (list funcindex n)))))))

  ;; ((lua-upvaluejoin f1 n1 f2 n2) L): upvalue n1 of the Lua closure f1 becomes upvalue n2 of
  ;; the Lua closure f2.  Refused with (exn lua api) code AUX_ERDONLY when upvalue n2 of f2 is
  ;; bound to a read-only variable (as lua-setupvalue): through f1, the variable's register could
  ;; be assigned.  P: the variable's kind is read from lua_dump's output.
  (define ((lua-upvaluejoin f1 n1 f2 n2) L)
    (%int! 'lua-upvaluejoin f1 n1 f2 n2)
    (let* ((u (%U 'lua-upvaluejoin L)) (nup (luaR-nup L)))
      (%prepare! u)
      (let* ((p (%L 'lua-upvaluejoin L))
             (r (%run u (%dbg-upvaluejoin/f %dbg-upvaluejoin/s) p f1 n1 f2 n2 nup)))
        (%finish 'lua-upvaluejoin u p r (list f1 n1 f2 n2))
        (set! %busy-ctx #f)
        (void))))

  ;; ---------- hooks ----------

  (define %hook-events '#(call return line count tailcall))
  (define %hook-yield-message "aux.lua: a hook can only yield with no values, from a line or count event")

  ;; the answer of a hook, applied by aux_hook: 0, AUX_YIELD, or a raise / escape (never raises)
  (define (%hook-answer Lp u outcome ev)
    (cond
      ((lua-yield-request? outcome)
       (if (and (memq ev '(line count)) (fx= 0 (lua-yield-request-n outcome)) (not (lua-yield-request-k outcome)))
         AUX_YIELD
         (%translate-raise Lp u (%lua-condition 'lua-sethook %hook-yield-message '() LUA_ERRRUN
                                                'request 'lerror 'text %hook-yield-message)
                           #f)))
      ((and (pair? outcome) (eq? (car outcome) %escape-tag)) (%record-escape! (cdr outcome)) AUX_ESCAPE)
      ((and (pair? outcome) (eq? (car outcome) %raise-tag)) (%translate-raise Lp u (cadr outcome) (cddr outcome)))
      (else 0)))

  ;; the luaDebug given to a hook: Lua's own record, valid while the hook view H is
  (define (%hook-debug ar u H Lp ev line)
    (let ((f (make-vector 17 #f)))
      (vector-set! f 0 ev)
      (vector-set! f 6 line)
      (make-luaDebug ar u (%dbg-epoch Lp) H f 0 #f)))

  (define (%hook-dispatch Lp h ar event line floor)
    (let ((e (%safely #f (and (not (%escape-refused?)) (let ((u (%callback-universe Lp))) (and u (%handle-entry u h '(hook))))))))
      (if (not e)
        0                                                 ; stale handle, or an escape is pending: return
        (let* ((u (cddr e)) (proc (car e))
               (ev (if (and (fx>= event 0) (fx< event 5)) (vector-ref %hook-events event) 'unknown))
               (outcome (%callback-outcome Lp u -1 'hook floor (λ (H) (proc H ev (%hook-debug ar u H Lp ev line))))))
          (%safely AUX_INTERNAL (%hook-answer Lp u outcome ev))))))

  (define-external (aux_lua_hookdispatch ((c-pointer "lua_State") Lp) (int h) (c-pointer ar) (int event) (int line) (int floor)) int
    (%hook-dispatch Lp h ar event line floor))

  (define-lua-stubs (%hook-set/f %hook-set/s) ((int h) (int mask) (int count)) "C_return(aux_hook_set(safe, L, h, mask, count));")

  ;; a mask: a fixnum in [0, 15] or a list of 'call 'return 'line 'count
  (define (%hook-mask who mask)
    (cond
      ((and (fixnum? mask) (fx>= mask 0) (fx<= mask 15)) mask)
      ((list? mask)
       (let loop ((l mask) (m 0))
         (if (null? l) m
           (loop (cdr l) (fxior m (case (car l) ((call) 1) ((return) 2) ((line) 4) ((count) 8)
                                    (else (%api-error who "unknown hook event" (list mask)))))))))
      (else (%api-error who "not a hook mask" (list mask) AUX_ERANGE))))

  ;; ((lua-sethook proc mask count) L): proc (λ (H event ar) answer) runs on the events of mask
  ;; (LUA_MASK* bits, or a list of 'call 'return 'line 'count) of thread L; count is the
  ;; instruction period of 'count events.  proc #f, or mask 0, removes the hook (count is stored
  ;; all the same, as C: lua-gethookcount answers it).
  (define ((lua-sethook proc mask count) L)
    (let ((m (%hook-mask 'lua-sethook mask)))
      (%int! 'lua-sethook count)
      (unless (fx>= count 0) (%api-error 'lua-sethook "negative count" (list count) AUX_ERANGE))
      (unless (or (not proc) (procedure? proc)) (%api-error 'lua-sethook "not a procedure" (list proc)))
      (let ((u (%U 'lua-sethook L)))
        (if (and proc (not (fx= m 0)))
          (begin
            (%prepare!/force u #t)
            (let* ((p (%L 'lua-sethook L))
                   (h (%handle-new! proc 'hook u))          ; counted before Lua can reach it (invariant I3)
                   (r (%run u (%hook-set/f %hook-set/s) p h m count)))
              (unless (fx> r -1000) (%handle-free! u h))
              (when (or (fx<= r -1000) %pending-escape) (%finish 'lua-sethook u p r (list proc mask count)))
              (set! %busy-ctx #f)))
          (let ((h -1) (z 0))                              ; C's lua_sethook stores the count all the same
            (%P 'lua-sethook L (%hook-set/f %hook-set/s) h z count)))
        (void))))

  (define %hook-get (foreign-lambda* int (((c-pointer "lua_State") L))
                       "lua_Hook f = lua_gethook(L);
                        if (f == NULL) C_return(-1);
                        if (f != aux_hook) C_return(-3);
                        C_return(aux_hook_lookup(L));"))
  ;; ((lua-gethook) L) => the Scheme procedure hooking L, #f, or 'foreign (a C hook, such as the
  ;; debug library's)
  (define lua-gethook
    (let ((k (λ (L)
               (let ((h (%D 'lua-gethook (%hook-get (%L 'lua-gethook L)))))
                 (cond ((fx= h -3) 'foreign)
                       ((fx< h 0) #f)
                       (else (let ((e (%handle-entry (luaR-u L) h '(hook)))) (and e (car e)))))))))
      (λ () k)))
  (define lua-gethookmask
    (let ((k (λ (L) (%done ((foreign-lambda int "lua_gethookmask" (c-pointer "lua_State")) (%L 'lua-gethookmask L)))))) (λ () k)))
  (define lua-gethookcount
    (let ((k (λ (L) (%done ((foreign-lambda int "lua_gethookcount" (c-pointer "lua_State")) (%L 'lua-gethookcount L)))))) (λ () k)))

  ;; ==========================================================================================
  ;; S6 — lauxlib (luaL_*) and lualib (luaopen_*, luaL_openselectedlibs)
  ;;
  ;; Already in the core: luaL-error luaL-argerror luaL-typeerror luaL-loadbufferx luaL-loadbuffer
  ;; luaL-loadstring luaL-dostring luaL-openlibs luaL-newstate (a main luaR, as lua-make-empty).
  ;; Omitted: luaL_fileresult / luaL_execresult (they read the C errno of a C call Scheme never
  ;; made); luaL_alloc (the module owns the allocator); luaL_addgsub and the whole luaL_Buffer
  ;; family (a C-stack struct plus stack slots of the caller's frame across calls: build the string
  ;; in Scheme and push it once); luaL_intop (pure C arithmetic); the LUA_COMPAT_APIINTCASTS
  ;; macros luaL_checkunsigned/optunsigned/checkint/optint/checklong/optlong (not compiled).
  ;;
  ;; The luaL-check*/opt* family is Scheme over D queries: a failed check raises the argerror /
  ;; typeerror requests of the core, so inside a callback Lua sees C's own message.
  ;; ==========================================================================================

  #>
  /* S6 op bodies: operands are the protected frame's slots 1..n (copies), see aux_protect */
  static int op_L_checkversion(lua_State *L, aux_op *o) { luaL_checkversion_(L, o->d0, o->z0); return 0; }
  static int op_L_getmetafield(lua_State *L, aux_op *o) {          /* refs [obj] or none (nil); MULTRET: 0 or 1 result */
    o->r = luaL_getmetafield(L, 1, o->s0); return o->r == LUA_TNIL ? 0 : 1; }
  static int op_L_callmeta(lua_State *L, aux_op *o) {              /* refs [obj] or none (nil); MULTRET: 0 or 1 result */
    o->r = luaL_callmeta(L, 1, o->s0); return o->r ? 1 : 0; }
  static int op_L_newmetatable(lua_State *L, aux_op *o) { o->r = luaL_newmetatable(L, o->s0); return 1; }
  static int op_L_getmetatable(lua_State *L, aux_op *o) { o->r = luaL_getmetatable(L, o->s0); return 1; }
  /* refs [-1]: the object's copy (tables and userdata are references, other types share a metatable).
     lua_setmetatable wants a table or nil on top: a registry entry of another type is refused
     (C would be undefined behaviour). */
  static int op_L_setmetatable(lua_State *L, aux_op *o) {
    int t, c;
    luaL_checkstack(L, 2, NULL);
    if (!aux_libkeys_ok(L)) aux_libkeys_make(L);                   /* (a collection step) */
    t = luaL_getmetatable(L, o->s0);
    if (t != LUA_TTABLE && t != LUA_TNIL) return luaL_error(L, "registry field '%s' is not a table", o->s0);
    luaL_checkstack(L, 2, NULL);                                    /* the metatable itself, known now */
    if ((c = aux_setmt_code(L, 1, -1))) { o->r = c == AUX_ELIBKEYS ? AUX_EMETATABLE : c; return 0; }   /* (exn lua api) */
    lua_setmetatable(L, 1); return 0; }
  /* refs [ud]: the pointer travels as a light userdata result (a pointer cannot travel in o->r) */
  static int op_L_testudata(lua_State *L, aux_op *o) { lua_pushlightuserdata(L, luaL_testudata(L, 1, o->s0)); return 1; }
  /* a table at 1: checked on the original, and again on the copy (see op_s3_rawset) */
  static int op_L_ref(lua_State *L, aux_op *o) { luaL_checktype(L, 1, LUA_TTABLE); o->r = aux_ref(L, 1); return 0; }         /* args: t v */
  /* args: t.  A ref >= 0 must be one luaL_ref gave (C: undefined behaviour otherwise): 0 and 1 never are (1 is the
     free list itself), nor is any ref of a table whose free list does not exist or is corrupted */
  static int op_L_unref(lua_State *L, aux_op *o) {
    luaL_checktype(L, 1, LUA_TTABLE);
    if (o->i0 >= 0) {
      /* the registry's reserved slots (LUA_RIDX_GLOBALS, LUA_RIDX_MAINTHREAD) are never its references:
         luaL_unref would store the free-list link, an integer, in them */
      if (o->i0 < 2 || (o->i0 <= LUA_RIDX_LAST && lua_rawequal(L, 1, LUA_REGISTRYINDEX))
          || lua_rawgeti(L, 1, 1) != LUA_TNUMBER || !lua_isinteger(L, -1) || !aux_freelist_ok(L, 1))
        return luaL_error(L, "aux.lua: %d is not a reference of this table", o->i0);
      lua_pop(L, 1); }
    luaL_unref(L, 1, o->i0); return 0; }
  /* every outcome pushes exactly one value: the chunk, or the message of a failed status */
  static int op_L_loadfilex(lua_State *L, aux_op *o) { aux_globals_check(L); o->r = luaL_loadfilex(L, o->s0, o->s1); return 1; }
  static int op_L_len(lua_State *L, aux_op *o) { lua_Integer n = luaL_len(L, 1); lua_pushinteger(L, n); return 1; }
  static int op_L_gsub(lua_State *L, aux_op *o) { luaL_gsub(L, o->s0, o->s1, o->s2); return 1; }
  static int op_L_getsubtable(lua_State *L, aux_op *o) { o->r = luaL_getsubtable(L, 1, o->s0); return 1; }
  /* the protected frame (aux_tramp) is level 0 of the thread the op runs on (L here: the pushing thread,
     or the main thread when that one is suspended or dead, aux_run_h): skip it when tracing that thread.  A negative level
     traces nothing (lua_getstack answers 0), as C: -1 for all of them, so that luaL_traceback's
     `last - level' cannot overflow; a shifted INT_MAX - 1 stays INT_MAX - 1 (as far past the last
     level, and its `level++' cannot overflow) */
  static int op_L_traceback(lua_State *L, aux_op *o) {
    int level = o->i0 < 0 ? -1 : (o->L1 == L && o->i0 < INT_MAX - 1 ? o->i0 + 1 : o->i0);
    luaL_traceback(L, o->L1, o->s0, level); return 1; }
  static int op_L_requiref(lua_State *L, aux_op *o) { aux_requiref(L, o->s0, (lua_CFunction)o->p0, 0, o->i0); return 1; }
  /* luaL_requiref with the open function given as arg 1 (a Scheme closure cannot be a light C
     function): lauxlib.c's body, with lua_pushvalue(L, 1) in place of lua_pushcfunction(L, openf) */
  static int op_L_requiref_fn(lua_State *L, aux_op *o) { aux_requiref(L, o->s0, NULL, 1, o->i0); return 1; }
  static int op_L_createtable(lua_State *L, aux_op *o) { lua_createtable(L, o->i0, o->i1); return 1; }
  static int op_L_openselectedlibs(lua_State *L, aux_op *o) { aux_openselectedlibs(L, o->i0, o->i1); return 0; }

  /* luaL_where's text, computed with D reads only (lua_getstack + lua_getinfo "Sl" never allocate):
     the byte length written into bv, 0 when there is no information (C pushes "") */
  static int aux_s6_where(lua_State *L, int lvl, char *b, int cap) {
    lua_Debug ar; int n;
    if (lvl < 0 || !lua_getstack(L, lvl, &ar)) return 0;
    lua_getinfo(L, "Sl", &ar);
    if (ar.currentline <= 0) return 0;
    n = snprintf(b, (size_t)cap, "%s:%d: ", ar.short_src, ar.currentline);
    return n < 0 ? 0 : (n >= cap ? cap - 1 : n); }
  <#

  ;; ---------------------------------------------------------------- helpers (S6-private)

  (define (%s6-noneornil? arg L) (fx<= ((lua-type arg) L) LUA_TNIL))
  (define (%s6-take-bytes bv n)
    (let ((r (make-bytevector n))) (do ((i 0 (fx+ i 1))) ((fx= i n) r) (bytevector-u8-set! r i (bytevector-u8-ref bv i)))))
  ;; the bytes before the first NUL (what C's strcmp / %s see)
  (define (%s6-until-nul bv)
    (let loop ((i 0))
      (cond ((fx= i (bytevector-length bv)) bv)
            ((fx= 0 (bytevector-u8-ref bv i)) (%s6-take-bytes bv i))
            (else (loop (fx+ i 1))))))
  ;; lauxlib's typeerror: the core's typeerror request ("T expected, got U", U as luaL_typeerror
  ;; names the argument; inside a callback Lua raises C's own message from the callback's frame)
  (define (%s6-typeerror who arg tname L)
    (%V who L)
    ((luaL-typeerror arg tname) L))
  ;; the string on top of p (a fresh Scheme string, or a bytevector when not UTF-8)
  (define (%s6-top-string who p) (%bytes->string/maybe (%bytes-at who p -1)))
  ;; a luaL_Reg array: a list of (name . f), f a procedure, a C function pointer or #f
  (define (%s6-reg! who alist)
    (let loop ((l alist))
      (cond ((null? l) #t)
            ((and (pair? l) (pair? (car l))
                  (or (string? (caar l)) (bytevector? (caar l)))
                  (let ((f (cdar l))) (or (not f) (procedure? f) (%cfunction-pointer? f))))
             (loop (cdr l)))
            (else (%api-error who "not a list of (name . procedure/pointer/#f)" (list alist))))))

  ;; ---------------------------------------------------------------- constants and C openers

  ;; the numeric-types signature luaL_checkversion_ compares (sizeof(lua_Integer)*16 + sizeof(lua_Number))
  (define LUAL_NUMSIZES (foreign-value "((int)LUAL_NUMSIZES)" int))

  ;; lua_CFunctions of the standard libraries, as C pointers for lua-pushcfunction / luaL-requiref:
  ;; never called from Scheme (they are lua_CFunctions: they may raise).  luaopen-coroutine is the
  ;; module's own opener (as luaL-openlibs / luaL-openselectedlibs use): lcorolib's library whose
  ;; close refuses to close a running coroutine while a binding runs Lua on it (see the barriers)
  (define luaopen-base (foreign-value "((void *)luaopen_base)" c-pointer))
  (define luaopen-package (foreign-value "((void *)luaopen_package)" c-pointer))
  (define luaopen-coroutine (foreign-value "((void *)aux_luaopen_coroutine)" c-pointer))   ; its close is aux_co_close
  (define luaopen-debug (foreign-value "((void *)luaopen_debug)" c-pointer))
  (define luaopen-io (foreign-value "((void *)luaopen_io)" c-pointer))
  (define luaopen-math (foreign-value "((void *)luaopen_math)" c-pointer))
  (define luaopen-os (foreign-value "((void *)luaopen_os)" c-pointer))
  (define luaopen-string (foreign-value "((void *)luaopen_string)" c-pointer))
  (define luaopen-table (foreign-value "((void *)luaopen_table)" c-pointer))
  (define luaopen-utf8 (foreign-value "((void *)luaopen_utf8)" c-pointer))

  ;; ---------------------------------------------------------------- versions

  (define-lua-stubs (%s6-checkversion/f %s6-checkversion/s) ((double ver) (size_t sz))
    "aux_op o = {0}; o.fn = op_L_checkversion; o.d0 = ver; o.z0 = sz;
     C_return(aux_run(safe, L, &o, 0, NULL, 0, 0));")
  ;; ((luaL-checkversion_ ver sz) L): raises (exn lua) on a version or numeric-types mismatch
  (define ((luaL-checkversion_ ver sz) L)
    (unless (real? ver) (%api-error 'luaL-checkversion_ "not a number" (list ver sz)))
    (unless (and (exact-integer? sz) (>= sz 0) (< sz 18446744073709551616)) (%api-error 'luaL-checkversion_ "not a size" (list ver sz) AUX_ERANGE))
    (let ((v (exact->inexact ver)))
      (%P 'luaL-checkversion_ L (%s6-checkversion/f %s6-checkversion/s) v sz))
    (void))
  (define luaL-checkversion
    (let ((k (luaL-checkversion_ (foreign-value "LUA_VERSION_NUM" int) LUAL_NUMSIZES))) (λ () k)))

  ;; ((luaL-makeseed) L) => an unsigned int of some randomness; L may be #f (C accepts NULL)
  (define %s6-makeseed (foreign-lambda unsigned-int "luaL_makeseed" (c-pointer "lua_State")))
  (define luaL-makeseed
    (let ((k (λ (L) (%done (%s6-makeseed (and L (%L 'luaL-makeseed L))))))) (λ () k)))

  ;; ---------------------------------------------------------------- metatables and metafields

  ;; obj a positive index above the top is the nil value, as in C: the op runs with no operand, so
  ;; its index 1 is above ITS top too (the metatable of the type nil, if any, is consulted; and
  ;; luaL_callmeta then calls the metafield with the value at obj -- the metafield itself, as C)
  (define-lua-stubs (%s6-getmetafield/f %s6-getmetafield/s) ((int obj) (scheme-object e))
    "aux_op o = {0}; int refs[1], q = aux_qix(L, obj); o.fn = op_L_getmetafield; o.s0 = aux_bytes(e, &o.l0); refs[0] = obj;
     if (q < 0) C_return(AUX_EINDEX);
     C_return(aux_run(safe, L, &o, q, refs, 0, LUA_MULTRET));")
  ;; ((luaL-getmetafield obj e) L) => the type of the metafield e of obj, pushed; LUA_TNIL = nothing pushed
  (define ((luaL-getmetafield obj e) L)
    (%int! 'luaL-getmetafield obj) (%cstring! 'luaL-getmetafield e #f)
    (%P 'luaL-getmetafield L (%s6-getmetafield/f %s6-getmetafield/s) obj e))

  (define-lua-stubs (%s6-callmeta/f %s6-callmeta/s) ((int obj) (scheme-object e))
    "aux_op o = {0}; int refs[1], q = aux_qix(L, obj); o.fn = op_L_callmeta; o.s0 = aux_bytes(e, &o.l0); refs[0] = obj;
     if (q < 0) C_return(AUX_EINDEX);
     C_return(aux_run(safe, L, &o, q, refs, 0, LUA_MULTRET));")
  ;; ((luaL-callmeta obj e) L) => #t with e(obj)'s result pushed, #f (nothing pushed) without metafield e
  (define ((luaL-callmeta obj e) L)
    (%int! 'luaL-callmeta obj) (%cstring! 'luaL-callmeta e #f)
    (fx= 1 (%P 'luaL-callmeta L (%s6-callmeta/f %s6-callmeta/s) obj e)))

  (define-lua-stubs (%s6-newmetatable/f %s6-newmetatable/s) ((scheme-object tname))
    "aux_op o = {0}; o.fn = op_L_newmetatable; o.s0 = aux_bytes(tname, &o.l0); C_return(aux_run_h(safe, L, &o, 1));")
  ;; ((luaL-newmetatable tname) L) => #t for a new registry.tname = {__name = tname}, #f when it existed;
  ;; pushes the metatable either way
  (define ((luaL-newmetatable tname) L)
    (%cstring! 'luaL-newmetatable tname #f)
    (fx= 1 (%P 'luaL-newmetatable L (%s6-newmetatable/f %s6-newmetatable/s) tname)))

  (define-lua-stubs (%s6-getmetatable/f %s6-getmetatable/s) ((scheme-object tname))
    "aux_op o = {0}; o.fn = op_L_getmetatable; o.s0 = aux_bytes(tname, &o.l0); C_return(aux_run_h(safe, L, &o, 1));")
  ;; ((luaL-getmetatable tname) L) => the type of registry.tname, pushed
  (define ((luaL-getmetatable tname) L)
    (%cstring! 'luaL-getmetatable tname #f)
    (%P 'luaL-getmetatable L (%s6-getmetatable/f %s6-getmetatable/s) tname))

  (define-lua-stubs (%s6-setmetatable/f %s6-setmetatable/s) ((scheme-object tname))
    "aux_op o = {0}; int refs[1], c; o.fn = op_L_setmetatable; o.s0 = aux_bytes(tname, &o.l0); refs[0] = -1;
     if (!aux_ix(L, -1, AUX_IX_STACK, 0)) C_return(AUX_EINDEX);
     if ((c = aux_setmt_code(L, -1, 0))) C_return(c);
     C_return(aux_run(safe, L, &o, 1, refs, 0, 0));")
  ;; ((luaL-setmetatable tname) L): the metatable of the value on top (not popped) = registry.tname
  (define ((luaL-setmetatable tname) L)
    (%cstring! 'luaL-setmetatable tname #f)
    (%P 'luaL-setmetatable L (%s6-setmetatable/f %s6-setmetatable/s) tname)
    (void))

  (define-lua-stubs (%s6-testudata/f %s6-testudata/s) ((int ud) (scheme-object tname))
    "aux_op o = {0}; int refs[1], q = aux_qix(L, ud); o.fn = op_L_testudata; o.s0 = aux_bytes(tname, &o.l0); refs[0] = ud;
     if (q < 0) C_return(AUX_EINDEX);
     if (q == 0) { if (!lua_checkstack(L, 1)) C_return(AUX_ESTACK); lua_pushlightuserdata(L, NULL); C_return(0); }
     C_return(aux_run(safe, L, &o, 1, refs, 0, 1));")
  ;; ((luaL-testudata ud tname) L) => the block of a full userdata whose metatable is registry.tname, else #f
  (define ((luaL-testudata ud tname) L)
    (%int! 'luaL-testudata ud) (%cstring! 'luaL-testudata tname #f)
    (%P 'luaL-testudata L (%s6-testudata/f %s6-testudata/s) ud tname)
    (let* ((p (%L 'luaL-testudata L)) (ptr (%touserdata-raw p -1))) (%pop! p 1) (set! %busy-ctx #f) ptr))
  ;; ((luaL-checkudata ud tname) L) => as luaL-testudata, else the typeerror "tname expected, got ..."
  (define ((luaL-checkudata ud tname) L)
    (or ((luaL-testudata ud tname) L) (%s6-typeerror 'luaL-checkudata ud tname L)))

  ;; ---------------------------------------------------------------- tostring, len, where, traceback

  ;; ((luaL-tolstring idx) L) => the text of any value (__tostring, __name honoured), also pushed.
  ;; A number is converted on a copy: the slot at idx keeps its number.
  (define ((luaL-tolstring idx) L)
    (%int! 'luaL-tolstring idx)
    (%P 'luaL-tolstring L (%tolstring-op/f %tolstring-op/s) idx)
    (%done (%s6-top-string 'luaL-tolstring (%L 'luaL-tolstring L))))

  (define-lua-stubs (%s6-len/f %s6-len/s) ((int idx))
    "aux_op o = {0}; int refs[1]; o.fn = op_L_len; refs[0] = idx;
     if (!aux_ix(L, idx, AUX_IX_READ, 0)) C_return(AUX_EINDEX);
     C_return(aux_run(safe, L, &o, 1, refs, 0, 1));")
  ;; ((luaL-len idx) L) => #v as an integer (__len honoured); nothing is left on the stack
  (define ((luaL-len idx) L)
    (%int! 'luaL-len idx)
    (%P 'luaL-len L (%s6-len/f %s6-len/s) idx)
    (let* ((p (%L 'luaL-len L)) (n (%tointeger-raw p -1))) (%pop! p 1) (set! %busy-ctx #f) n))

  (define %s6-where (foreign-lambda* int (((c-pointer "lua_State") L) (int lvl) (scheme-object bv))
                      "C_return(aux_s6_where(L, lvl, (char *)C_data_pointer(bv), (int)C_header_size(bv)));"))
  ;; ((luaL-where lvl) L): pushes "chunk:line: " of the function at level lvl of L, or "".  Level 0 is
  ;; the running function (inside a callback: the Scheme closure itself, as for a C function).
  ;; The text is read directly (no protected frame, hence no level shift) and pushed with lua-pushlstring,
  ;; so it also works on suspended threads.
  (define ((luaL-where lvl) L)
    (%int! 'luaL-where lvl)
    (let* ((bv (make-bytevector 256)) (n (%s6-where (%L 'luaL-where L) lvl bv)))
      ((lua-pushlstring (%s6-take-bytes bv n)) L)))

  (define-lua-stubs (%s6-traceback/f %s6-traceback/s) (((c-pointer "lua_State") L1) (scheme-object msg) (int level))
    "aux_op o = {0}; o.fn = op_L_traceback; o.L1 = L1; o.i0 = level;
     if (level > INT_MAX - 1) C_return(AUX_ERANGE);          /* C's level++ would overflow */
     if (C_truep(msg)) o.s0 = aux_bytes(msg, &o.l0);
     C_return(aux_run_h(safe, L, &o, 1));")
  ;; ((luaL-traceback L1 msg level) L): pushes onto L the traceback of L1 (a state or thread of the
  ;; same universe) from level on, after msg (#f: none).  Tracing the thread the op runs on skips the
  ;; protected frame.  P*: onto a suspended or dead L the text is built on the main thread and moved
  ;; over (it only reads L1 and allocates, as C's luaL_traceback does on such a thread); then only a
  ;; trace of the main thread has the protected frame at level 0 (op_L_traceback compares L1 with the
  ;; thread the op runs on).
  ;; A negative level traces no frame ("msg\nstack traceback:"), as C; INT_MAX is refused (C overflows).
  (define ((luaL-traceback L1 msg level) L)
    (let ((u (%U 'luaL-traceback L)))
      (unless (eq? (%U 'luaL-traceback L1) u) (%api-error 'luaL-traceback "not a state of this universe" (list L1 msg level)))
      (%cstring! 'luaL-traceback msg #t) (%int! 'luaL-traceback level)
      (%prepare! u)
      (let* ((p (%L 'luaL-traceback L)) (p1 (%L 'luaL-traceback L1))   ; after the drain point
             (r (%run u (%s6-traceback/f %s6-traceback/s) p p1 msg level)))
        (if (and (fx> r -1000) (not %pending-escape)) (set! %busy-ctx #f) (%finish 'luaL-traceback u p r (list L1 msg level)))
        (void))))

  ;; ---------------------------------------------------------------- argument checks (K)

  ;; ((luaL-checklstring arg) L) => the string (or bytevector: not UTF-8) at arg; a number is
  ;; converted on a copy (the slot keeps its number: lua_next stays safe)
  (define ((luaL-checklstring arg) L)
    (or ((lua-tolstring arg) L) (%s6-typeerror 'luaL-checklstring arg "string" L)))
  (define ((luaL-checkstring arg) L) ((luaL-checklstring arg) L))
  ;; ((luaL-optlstring arg def) L) => def when arg is none or nil, else luaL-checklstring
  (define ((luaL-optlstring arg def) L)
    (if (%s6-noneornil? arg L) def ((luaL-checklstring arg) L)))
  (define ((luaL-optstring arg def) L) ((luaL-optlstring arg def) L))

  (define ((luaL-checknumber arg) L)
    (let-values (((x ok?) ((lua-tonumberx arg) L)))
      (if ok? x (%s6-typeerror 'luaL-checknumber arg "number" L))))
  (define ((luaL-optnumber arg def) L)
    (unless (real? def) (%api-error 'luaL-optnumber "not a number" (list arg def)))
    (if (%s6-noneornil? arg L) (exact->inexact def) ((luaL-checknumber arg) L)))

  ;; lauxlib's interror: a number without an integer value is an argerror, anything else a typeerror
  (define ((luaL-checkinteger arg) L)
    (let-values (((n ok?) ((lua-tointegerx arg) L)))
      (cond (ok? n)
            ((let-values (((x num?) ((lua-tonumberx arg) L))) num?)
             ((luaL-argerror arg "number has no integer representation") L))
            (else (%s6-typeerror 'luaL-checkinteger arg "number" L)))))
  (define ((luaL-optinteger arg def) L)
    (unless (%int64? def) (%api-error 'luaL-optinteger "not a lua_Integer" (list arg def) AUX_ERANGE))
    (if (%s6-noneornil? arg L) def ((luaL-checkinteger arg) L)))

  ;; ((luaL-opt f arg def) L): def when arg is none or nil, else ((f arg) L) (f a curried checker)
  (define ((luaL-opt f arg def) L)
    (unless (procedure? f) (%api-error 'luaL-opt "not a procedure" (list f arg def)))
    (if (%s6-noneornil? arg L) def ((f arg) L)))

  (define %s6-checkstack (foreign-lambda bool "lua_checkstack" (c-pointer "lua_State") int))
  ;; ((luaL-checkstack sz msg) L): grows the stack by sz slots, else luaL-error "stack overflow (msg)"
  (define ((luaL-checkstack sz msg) L)
    (let ((p (%L 'luaL-checkstack L)))
      (%int! 'luaL-checkstack sz)
      (when (fx< sz 0) (%api-error 'luaL-checkstack "negative size" (list sz msg) AUX_ERANGE))
      (%cstring! 'luaL-checkstack msg #t)
      (unless (%done (%s6-checkstack p sz))
        (if msg ((luaL-error "stack overflow (%s)" msg) L) ((luaL-error "stack overflow") L)))
      (void)))

  (define ((luaL-checktype arg t) L)
    (unless (and (fixnum? t) (fx>= t LUA_TNONE) (fx< t LUA_NUMTYPES)) (%api-error 'luaL-checktype "not a type code" (list arg t) AUX_ERANGE))
    (unless (fx= ((lua-type arg) L) t)
      (%s6-typeerror 'luaL-checktype arg (%typename (%L 'luaL-checktype L) t) L))
    (void))

  (define ((luaL-checkany arg) L)
    (when (fx= ((lua-type arg) L) LUA_TNONE) ((luaL-argerror arg "value expected") L))
    (void))

  ;; cond: #t or #f only (C's int), as for luaL-argexpected
  (define ((luaL-argcheck cond arg extramsg) L)
    (%V 'luaL-argcheck L) (%bool! 'luaL-argcheck cond) (%int! 'luaL-argcheck arg) (%bytes! 'luaL-argcheck extramsg)
    (unless cond ((luaL-argerror arg extramsg) L))
    (void))
  (define ((luaL-argexpected cond arg tname) L)
    (%V 'luaL-argexpected L) (%bool! 'luaL-argexpected cond) (%int! 'luaL-argexpected arg) (%bytes! 'luaL-argexpected tname)
    (unless cond (%s6-typeerror 'luaL-argexpected arg tname L))
    (void))

  ;; ((luaL-checkoption arg def lst) L) => the index in lst (strings) of the string at arg (or of def,
  ;; #f or a string, when arg is none or nil); else the argerror "invalid option '...'"
  (define ((luaL-checkoption arg def lst) L)
    (%cstring! 'luaL-checkoption def #t)
    (unless (and (list? lst) (let loop ((l lst)) (or (null? l) (and (string? (car l)) (%nul-free? (car l)) (loop (cdr l))))))
      (%api-error 'luaL-checkoption "not a list of strings without NUL" (list arg def lst)))
    (let ((name (%s6-until-nul (%->bytes (if def ((luaL-optlstring arg def) L) ((luaL-checklstring arg) L))))))
      (let loop ((l lst) (i 0))
        (cond ((null? l) ((luaL-argerror arg (bytevector-append (string->utf8 "invalid option '") name (string->utf8 "'"))) L))
              ((equal? (string->utf8 (car l)) name) i)
              (else (loop (cdr l) (fx+ i 1)))))))

  ;; ((luaL-typename idx) L) => the type name of the value at idx ("no value" above the top)
  (define ((luaL-typename idx) L) (let ((t ((lua-type idx) L))) (%done (%typename (%L 'luaL-typename L) t))))

  ;; ((luaL-pushfail) L): pushes the failure value (nil)
  (define luaL-pushfail (let ((k (λ (L) (%D 'luaL-pushfail (%pushnil-raw (%L 'luaL-pushfail L))) (void)))) (λ () k)))

  ;; ---------------------------------------------------------------- references

  (define-lua-stubs (%s6-ref/f %s6-ref/s) ((int t))
    "aux_op o = {0}; int refs[1]; o.fn = op_L_ref; refs[0] = t;
     if (!aux_ix(L, t, AUX_IX_READ, 0)) C_return(AUX_EINDEX);
     if (lua_type(L, t) != LUA_TTABLE) C_return(AUX_ETYPE);
     C_return(aux_run(safe, L, &o, 1, refs, 1, 0));")
  ;; ((luaL-ref t) L) => a fresh integer key of table t holding the value on top (popped);
  ;; LUA_REFNIL for nil.  A Lua error, nothing stored (the value popped all the same), when t[1]'s
  ;; free list is corrupted: a head that is not a possible ref (<= 1, a reserved slot of the
  ;; registry, > INT_MAX: lauxlib would truncate it) or whose link is not an integer; or when the
  ;; new ref, #t + 1, would exceed INT_MAX or be a reserved slot of the registry.
  (define ((luaL-ref t) L) (%int! 'luaL-ref t) (%P 'luaL-ref L (%s6-ref/f %s6-ref/s) t))

  (define-lua-stubs (%s6-unref/f %s6-unref/s) ((int t) (int ref))
    "aux_op o = {0}; int refs[1]; o.fn = op_L_unref; o.i0 = ref; refs[0] = t;
     if (!aux_ix(L, t, AUX_IX_READ, 0)) C_return(AUX_EINDEX);
     if (lua_type(L, t) != LUA_TTABLE) C_return(AUX_ETYPE);
     C_return(aux_run(safe, L, &o, 1, refs, 0, 0));")
  ;; ((luaL-unref t ref) L): frees ref of table t for reuse (negative refs are ignored, as C)
  (define ((luaL-unref t ref) L) (%int! 'luaL-unref t ref) (%P 'luaL-unref L (%s6-unref/f %s6-unref/s) t ref) (void))

  ;; ---------------------------------------------------------------- loading

  (define-lua-stubs (%s6-loadfilex/f %s6-loadfilex/s) ((scheme-object fname) (scheme-object mode))
    "aux_op o = {0}; int c; o.fn = op_L_loadfilex;
     if (C_truep(fname)) o.s0 = aux_bytes(fname, &o.l0);
     if (C_truep(mode)) o.s1 = aux_bytes(mode, &o.l1);
     if (aux_badmode(o.s1, o.l1)) C_return(AUX_EARG);
     if (lua_status(L) == LUA_OK && (c = aux_globals_code(L))) C_return(c);
     C_return(aux_run(safe, L, &o, 0, NULL, 0, 1));")
  ;; ((luaL-loadfilex filename mode) L) => status (LUA_ERRFILE when the file cannot be opened or read);
  ;; pushes the chunk or the error message.  filename #f reads stdin; mode #f, "t", "b" or "bt"
  ;; (binary chunks are not verified by Lua: "t" for untrusted files; luaL-loadfile and luaL-dofile
  ;; accept both).
  (define ((luaL-loadfilex filename mode) L)
    (%cstring! 'luaL-loadfilex filename #t) (%mode! 'luaL-loadfilex mode)
    (%P 'luaL-loadfilex L (%s6-loadfilex/f %s6-loadfilex/s) filename mode))
  (define ((luaL-loadfile filename) L) ((luaL-loadfilex filename #f) L))
  ;; ((luaL-dofile filename) L) => the failing status (C's macro answers 1), or LUA_OK (the chunk's results are left)
  (define ((luaL-dofile filename) L)
    (let ((st ((luaL-loadfile filename) L)))
      (if (fx= st LUA_OK) ((lua-pcall 0 LUA_MULTRET 0) L) st)))

  ;; ---------------------------------------------------------------- strings, tables, libraries

  (define-lua-stubs (%s6-gsub/f %s6-gsub/s) ((scheme-object s) (scheme-object pat) (scheme-object r))
    "aux_op o = {0}; o.fn = op_L_gsub; o.s0 = aux_bytes(s, &o.l0); o.s1 = aux_bytes(pat, &o.l1); o.s2 = aux_bytes(r, &o.l2);
     C_return(aux_run_h(safe, L, &o, 1));")
  ;; ((luaL-gsub s p r) L) => s with every p replaced by r (plain strings, no patterns), also pushed.
  ;; Strings without NUL (C's strstr); an empty p is refused (C would loop for ever).
  (define ((luaL-gsub s p r) L)
    (%cstring! 'luaL-gsub s #f) (%cstring! 'luaL-gsub p #f) (%cstring! 'luaL-gsub r #f)
    (when (fx= 0 (string-length p)) (%api-error 'luaL-gsub "empty pattern" (list s p r)))
    (%P 'luaL-gsub L (%s6-gsub/f %s6-gsub/s) s p r)
    (%done (%s6-top-string 'luaL-gsub (%L 'luaL-gsub L))))

  (define-lua-stubs (%s6-getsubtable/f %s6-getsubtable/s) ((int idx) (scheme-object fname))
    "aux_op o = {0}; int refs[1]; o.fn = op_L_getsubtable; o.s0 = aux_bytes(fname, &o.l0); refs[0] = idx;
     if (!aux_ix(L, idx, AUX_IX_READ, 0)) C_return(AUX_EINDEX);
     C_return(aux_run(safe, L, &o, 1, refs, 0, 1));")
  ;; ((luaL-getsubtable idx fname) L) => #t when t[fname] was already a table, #f when it was
  ;; created; pushes it either way
  (define ((luaL-getsubtable idx fname) L)
    (%int! 'luaL-getsubtable idx) (%cstring! 'luaL-getsubtable fname #f)
    (fx= 1 (%P 'luaL-getsubtable L (%s6-getsubtable/f %s6-getsubtable/s) idx fname)))

  (define-lua-stubs (%s6-createtable/f %s6-createtable/s) ((int narr) (int nrec))
    "aux_op o = {0}; o.fn = op_L_createtable; o.i0 = narr; o.i1 = nrec;
     if (narr < 0 || nrec < 0) C_return(AUX_ERANGE);
     C_return(aux_run_h(safe, L, &o, 1));")
  ;; ((luaL-newlibtable alist) L): pushes a table sized for the entries of alist
  (define ((luaL-newlibtable alist) L)
    (%s6-reg! 'luaL-newlibtable alist)
    (let ((n (length alist)))
      (%int! 'luaL-newlibtable n)
      (%P 'luaL-newlibtable L (%s6-createtable/f %s6-createtable/s) 0 n))
    (void))

  ;; ((luaL-setfuncs alist nup) L): t[name] = f for each (name . f) of alist, t the table under the
  ;; nup values on top, which every f shares as upvalues ((lua-upvalueindex i) in a Scheme f); they
  ;; are popped at the end.  A Scheme f takes at most 254 upvalues (see lua-pushcclosure).  f: a procedure, a C function pointer, or #f (a false placeholder).
  ;; On an error the stack is as before the call (fields set before it stay set).
  (define ((luaL-setfuncs alist nup) L)
    (%s6-reg! 'luaL-setfuncs alist)
    (%int! 'luaL-setfuncs nup)
    (when (fx< nup 0) (%api-error 'luaL-setfuncs "negative upvalue count" (list alist nup) AUX_ERANGE))
    (when (fx< ((lua-gettop) L) (fx+ nup 1)) (%api-error 'luaL-setfuncs "no table under the upvalues" (list alist nup) AUX_EUNDER))
    ((luaL-checkstack nup "too many upvalues") L)
    (for-each (λ (entry)
                (let ((f (cdr entry)))
                  (if f
                    (begin
                      (do ((i 0 (fx+ i 1))) ((fx= i nup)) ((lua-pushvalue (fx- 0 nup)) L))
                      ((lua-pushcclosure f nup) L))
                    ((lua-pushboolean #f) L))
                  ((lua-setfield (fx- -2 nup) (car entry)) L)))
              alist)
    ((lua-pop nup) L))

  ;; ((luaL-newlib alist) L): luaL-checkversion, then a new table filled by luaL-setfuncs, pushed
  (define ((luaL-newlib alist) L)
    (%s6-reg! 'luaL-newlib alist)
    ((luaL-checkversion) L)
    ((luaL-newlibtable alist) L)
    ((luaL-setfuncs alist 0) L))

  (define-lua-stubs (%s6-requiref/f %s6-requiref/s) ((scheme-object name) (c-pointer f) (bool glb))
    "aux_op o = {0}; int c; o.fn = op_L_requiref; o.s0 = aux_bytes(name, &o.l0); o.p0 = f; o.i0 = glb;
     if (f == NULL) C_return(AUX_EARG);
     if (glb && (c = aux_globals_code(L))) C_return(c);
     C_return(aux_run(safe, L, &o, 0, NULL, 0, 1));")
  (define %s6-globals-code (foreign-lambda int "aux_globals_code" (c-pointer "lua_State")))   ; D
  (define-lua-stubs (%s6-requiref-fn/f %s6-requiref-fn/s) ((scheme-object name) (bool glb))
    "aux_op o = {0}; int c; o.fn = op_L_requiref_fn; o.s0 = aux_bytes(name, &o.l0); o.i0 = glb;
     if (glb && (c = aux_globals_code(L))) C_return(c);
     C_return(aux_run(safe, L, &o, 0, NULL, 1, 1));")
  ;; ((luaL-requiref modname openf glb) L): unless package.loaded[modname] is true, calls
  ;; openf(modname) and stores its result there; glb #t also sets the global modname (#f or #t only).  Pushes the
  ;; module.  openf: a C function pointer (luaopen-*: the real luaL_requiref), or a Scheme
  ;; procedure (λ (C) answer) run by the same algorithm in one protected op.
  (define ((luaL-requiref modname openf glb) L)
    (%cstring! 'luaL-requiref modname #f) (%bool! 'luaL-requiref glb)
    (cond
      ((pointer? openf)
       (%cfunction! 'luaL-requiref openf (list modname openf glb))
       (%P 'luaL-requiref L (%s6-requiref/f %s6-requiref/s) modname openf glb))
      ((procedure? openf)
       (when glb                                            ; refused before openf is pushed
         (%D 'luaL-requiref (%s6-globals-code (%L 'luaL-requiref L)) modname openf glb))
       ((lua-pushcfunction openf) L)
       (%P 'luaL-requiref L (%s6-requiref-fn/f %s6-requiref-fn/s) modname glb))
      (else (%api-error 'luaL-requiref "not a procedure or a C function pointer" (list modname openf glb))))
    (void))

  (define-lua-stubs (%s6-openselectedlibs/f %s6-openselectedlibs/s) ((int load) (int preload))
    "aux_op o = {0}; int c; o.fn = op_L_openselectedlibs; o.i0 = load; o.i1 = preload;
     if ((c = aux_openlibs_code(L, load))) C_return(c);
     C_return(aux_run(safe, L, &o, 0, NULL, 0, 0));")
  ;; ((luaL-openselectedlibs load preload) L): requires the standard libraries whose LUA_*LIBK bit
  ;; is in load, and puts those in preload (and not in load) into package.preload
  (define ((luaL-openselectedlibs load preload) L)
    (%int! 'luaL-openselectedlibs load preload)
    (%P 'luaL-openselectedlibs L (%s6-openselectedlibs/f %s6-openselectedlibs/s) load preload)
    (void))

)

;; ==========================================================================================
;; (aux lua): the public interface.  Everything here is checked: a call C would not survive is
;; refused with (exn lua api) before Lua runs.
;; ==========================================================================================
(module (aux lua)
  (
    ;; constants
    LUAL_NUMSIZES LUA_AUTHORS LUA_COLIBK LUA_COLIBNAME LUA_COPYRIGHT LUA_DBLIBK LUA_DBLIBNAME LUA_ERRERR
    LUA_ERRFILE LUA_ERRMEM LUA_ERRRUN LUA_ERRSYNTAX LUA_FILEHANDLE LUA_GCCOLLECT LUA_GCCOUNT
    LUA_GCCOUNTB LUA_GCGEN LUA_GCINC LUA_GCISRUNNING LUA_GCPARAM LUA_GCPMAJORMINOR LUA_GCPMINORMAJOR
    LUA_GCPMINORMUL LUA_GCPN LUA_GCPPAUSE LUA_GCPSTEPMUL LUA_GCPSTEPSIZE LUA_GCRESTART LUA_GCSTEP
    LUA_GCSTOP LUA_GLIBK LUA_GNAME LUA_HOOKCALL LUA_HOOKCOUNT LUA_HOOKLINE LUA_HOOKRET LUA_HOOKTAILCALL
    LUA_IOLIBK LUA_IOLIBNAME LUA_LOADED_TABLE LUA_LOADLIBK LUA_LOADLIBNAME LUA_MASKCALL LUA_MASKCOUNT
    LUA_MASKLINE LUA_MASKRET LUA_MATHLIBK LUA_MATHLIBNAME LUA_MAXINTEGER LUA_MININTEGER LUA_MINSTACK
    LUA_MULTRET LUA_N2SBUFFSZ LUA_NOREF LUA_NUMTYPES LUA_OK LUA_OPADD LUA_OPBAND LUA_OPBNOT LUA_OPBOR
    LUA_OPBXOR LUA_OPDIV LUA_OPEQ LUA_OPIDIV LUA_OPLE LUA_OPLT LUA_OPMOD LUA_OPMUL LUA_OPPOW LUA_OPSHL
    LUA_OPSHR LUA_OPSUB LUA_OPUNM LUA_OSLIBK LUA_OSLIBNAME LUA_PRELOAD_TABLE LUA_REFNIL
    LUA_REGISTRYINDEX LUA_RELEASE LUA_RIDX_GLOBALS LUA_RIDX_LAST LUA_RIDX_MAINTHREAD LUA_SIGNATURE
    LUA_STRLIBK LUA_STRLIBNAME LUA_TABLIBK LUA_TABLIBNAME LUA_TBOOLEAN LUA_TFUNCTION LUA_TLIGHTUSERDATA
    LUA_TNIL LUA_TNONE LUA_TNUMBER LUA_TSTRING LUA_TTABLE LUA_TTHREAD LUA_TUSERDATA LUA_UTF8LIBK
    LUA_UTF8LIBNAME LUA_VERSION LUA_VERSION_MAJOR LUA_VERSION_MAJOR_N LUA_VERSION_MINOR
    LUA_VERSION_MINOR_N LUA_VERSION_NUM LUA_VERSION_RELEASE LUA_VERSION_RELEASE_N
    LUA_VERSION_RELEASE_NUM LUA_VERSUFFIX LUA_YIELD
    ;; lua.h, the conditions and the module's own entries
    lua-absindex lua-arith lua-call lua-check lua-checkstack lua-close lua-closethread lua-compare
    lua-concat lua-condition-status lua-condition-value lua-condition? lua-copy lua-createtable
    lua-debug-currentline lua-debug-event lua-debug-extraargs lua-debug-ftransfer lua-debug-istailcall
    lua-debug-isvararg lua-debug-lastlinedefined lua-debug-linedefined lua-debug-name lua-debug-namewhat
    lua-debug-nparams lua-debug-ntransfer lua-debug-nups lua-debug-short-src lua-debug-source lua-debug-srclen
    lua-debug-what lua-dump lua-error lua-format lua-gc lua-getfield lua-getglobal lua-gethook
    lua-gethookcount lua-gethookmask lua-geti lua-getinfo lua-getiuservalue lua-getlocal
    lua-getmetatable lua-getstack lua-gettable lua-gettop lua-getupvalue lua-getuservalue lua-insert
    lua-isboolean lua-iscfunction lua-isfunction lua-isinteger lua-islightuserdata lua-isnil lua-isnone
    lua-isnoneornil lua-isnumber lua-isstring lua-istable lua-isthread lua-isuserdata lua-isyieldable
    lua-equal lua-len lua-lessthan lua-live-handles lua-load lua-make-empty lua-newstate lua-newtable lua-newthread
    lua-newuserdata lua-newuserdatauv lua-next lua-numbertocstring lua-objlen lua-pcall lua-pop lua-pushboolean
    lua-pushcclosure lua-pushcfunction lua-pushfstring lua-pushglobaltable lua-pushinteger
    lua-pushlightuserdata lua-pushliteral lua-pushlstring lua-pushnil lua-pushnumber lua-pushobject
    lua-pushref lua-pushstring lua-pushthread lua-pushvalue lua-rawequal lua-rawget lua-rawgeti
    lua-rawgetp lua-rawlen lua-rawset lua-rawseti lua-rawsetp lua-ref? lua-register lua-remove
    lua-replace lua-resetthread lua-resume lua-rotate lua-setfield lua-setglobal lua-sethook lua-seti
    lua-setiuservalue lua-setlocal lua-setmemorylimit lua-setmetatable lua-settable lua-settop
    lua-setupvalue lua-setuservalue lua-setwarnf lua-status lua-stringtonumber lua-strlen lua-toboolean
    lua-tobytevector lua-tocfunction lua-tointeger lua-tointegerx lua-tolstring lua-tonumber
    lua-tonumberx lua-toobject lua-topointer lua-toprocedure lua-toref lua-tostring lua-tothread
    lua-touserdata lua-type lua-type->symbol lua-typename lua-upvalueid lua-upvalueindex lua-upvaluejoin
    lua-version lua-warning lua-warnings lua-xmove lua-yield lua-yieldk
    ;; lauxlib.h
    luaL-argcheck luaL-argerror luaL-argexpected luaL-callmeta luaL-checkany luaL-checkinteger
    luaL-checklstring luaL-checknumber luaL-checkoption luaL-checkstack luaL-checkstring luaL-checktype
    luaL-checkudata luaL-checkversion luaL-checkversion_ luaL-dofile luaL-dostring luaL-error
    luaL-getmetafield luaL-getmetatable luaL-getsubtable luaL-gsub luaL-len luaL-loadbuffer
    luaL-loadbufferx luaL-loadfile luaL-loadfilex luaL-loadstring luaL-makeseed luaL-newlib
    luaL-newlibtable luaL-newmetatable luaL-newstate luaL-openlibs luaL-openselectedlibs luaL-opt
    luaL-optinteger luaL-optlstring luaL-optnumber luaL-optstring luaL-pushfail luaL-ref luaL-requiref
    luaL-setfuncs luaL-setmetatable luaL-testudata luaL-tolstring luaL-traceback luaL-typeerror
    luaL-typename luaL-unref luaL-where
    ;; lualib.h
    luaopen-base luaopen-coroutine luaopen-debug luaopen-io luaopen-math luaopen-os luaopen-package
    luaopen-string luaopen-table luaopen-utf8
    ;; records (read-only: no constructor, no mutator)
    luaDebug? luaR-kind luaR?
  )
  (import (aux lua internal)))
