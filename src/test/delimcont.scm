(import scheme (chicken base) (chicken foreign))

;; =====================================================================
;; 1. The C Runtime Implementation (Dynamic Stack)
;; =====================================================================
#>

#include <stdlib.h>

/* We store GC root handles instead of direct C_words */
void **prompt_roots = NULL; 
int prompt_capacity = 0;
int prompt_top = 0;

/* Helper to push a continuation onto the dynamic stack */
void push_prompt(C_word k) {
    /* Grow the stack if necessary */
    if (prompt_top >= prompt_capacity) {
        int new_cap = (prompt_capacity == 0) ? 16 : prompt_capacity * 2;
        prompt_roots = realloc(prompt_roots, new_cap * sizeof(void *));
        
        /* Allocate new individual GC roots for the expanded capacity */
        for(int i = prompt_capacity; i < new_cap; i++) {
            prompt_roots[i] = CHICKEN_new_gc_root();
            CHICKEN_gc_root_set(prompt_roots[i], C_SCHEME_UNDEFINED);
        }
        prompt_capacity = new_cap;
    }
    
    /* Store the continuation in the root handle */
    CHICKEN_gc_root_set(prompt_roots[prompt_top++], k);
}

/* Helper to pop a continuation and shrink the stack if it gets too empty */
C_word pop_prompt() {
    /* Retrieve the continuation */
    C_word k = CHICKEN_gc_root_ref(prompt_roots[--prompt_top]);
    
    /* Clear the root so we don't hold onto old continuations unnecessarily */
    CHICKEN_gc_root_set(prompt_roots[prompt_top], C_SCHEME_UNDEFINED);

    /* Shrink the stack if it's less than 1/4 full, keeping a minimum capacity */
    if (prompt_top < prompt_capacity / 4 && prompt_capacity > 16) {
        int new_cap = prompt_capacity / 2;
        
        /* Tell CHICKEN to stop tracking these roots before we free them */
        for(int i = new_cap; i < prompt_capacity; i++) {
            CHICKEN_delete_gc_root(prompt_roots[i]);
        }
        
        prompt_roots = realloc(prompt_roots, new_cap * sizeof(void *));
        prompt_capacity = new_cap;
    }
    
    return k;
}

/* --------------------------------------------------------------------- */
/* Control Flow Functions using push_prompt / pop_prompt                 */
/* --------------------------------------------------------------------- */

/* The boundary marker that executes when a reset block finishes */
void C_ccall my_prompt_return(C_word c, C_word closure, C_word ignored_k, C_word result) {
    C_word real_k = pop_prompt();
    C_kontinue(real_k, result);
}

/* The raw entry point for %reset */
void C_ccall my_reset(C_word c, C_word closure, C_word k, C_word thunk) {
    push_prompt(k);
    C_word *a = C_alloc(3); 
    C_word k_prompt = C_closure(&a, 2, (C_word)my_prompt_return);
    C_word thunk_proc = C_block_item(thunk, 0);
    ((C_proc)(void *)thunk_proc)(2, thunk, k_prompt);
}

/* The function that fires when the user calls a captured delimited continuation */
void C_ccall invoke_captured_k(C_word c, C_word closure, C_word caller_k, C_word arg) {
    C_word captured_k = C_block_item(closure, 1);
    push_prompt(caller_k);
    C_kontinue(captured_k, arg);
}

/* The raw entry point for %shift */
void C_ccall my_shift(C_word c, C_word closure, C_word k, C_word f) {
    C_word escape_k = pop_prompt();
    C_word *a = C_alloc(4);
    C_word delim_k_proc = C_closure(&a, 3, (C_word)invoke_captured_k, k);
    C_word f_proc = C_block_item(f, 0);
    ((C_proc3)(void *)f_proc)(3, f, escape_k, delim_k_proc);
}
<#

;; =====================================================================
;; 2. Raw Procedure Bindings
;; =====================================================================

;; Expose my_reset as a Scheme procedure (%reset thunk)
;; We allocate 2 words (header + function pointer) in the nursery and 
;; explicitly pass it to the current Scheme continuation `k`.
(define %reset
  ((foreign-primitive scheme-object ()
     "C_word *a = C_alloc(2);"
     "C_kontinue(C_k, C_closure(&a, 1, (C_word)my_reset));")))

;; Expose my_shift as a Scheme procedure (%shift function)
(define %shift
  ((foreign-primitive scheme-object ()
     "C_word *a = C_alloc(2);"
     "C_kontinue(C_k, C_closure(&a, 1, (C_word)my_shift));")))

;; =====================================================================
;; 3. Syntactic Sugar (Standard Macros)
;; =====================================================================

;; Standard `reset` macro so you don't have to wrap expressions in thunks.
;; (reset (+ 1 2))  =>  (%reset (lambda () (+ 1 2)))
(define-syntax reset
  (syntax-rules ()
    ((_ body ...)
     (%reset (lambda () body ...)))))

;; Standard `shift` macro to bind the continuation to a variable.
;; (shift k (k 2))  =>  (%shift (lambda (k) (k 2)))
(define-syntax shift
  (syntax-rules ()
    ((_ var body ...)
     (%shift (lambda (var) body ...)))))