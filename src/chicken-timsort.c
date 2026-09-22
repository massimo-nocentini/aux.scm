#include <stdio.h>
#include <assert.h>
#include <chicken/chicken.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include "timsort.c"
#include "chicken-timsort.h"

/* ------------------------------------------------------------------------
   GC safety.

   CHICKEN's collector is a copying/generational one: a major collection (or a
   heap resize) relocates every live heap object and rewrites only the roots it
   knows about -- the temporary stack, globals, the mutation stack, the saved
   callback continuations and the explicitly registered roots.  A `C_word' kept
   in a C local, in a C function parameter or in a malloc'd struct is none of
   those, so it silently becomes a pointer into the dead semispace as soon as a
   collection runs.

   `foreign-safe-lambda' protects the *continuation* of the call and nothing
   else, and this sort re-enters Scheme once per comparison, so every `C_word'
   this file touches has to be assumed stale after any callback.

   The rule this file follows: no `C_word' is ever held across a call to
   `C_callback'.  The objects to be sorted live in a Scheme vector supplied by
   the caller; the sort permutes plain C indices into that vector, and the five
   Scheme values we need (the vector, the comparator, the input list, the
   output list or vector, and the key vector) are held in GC roots and re-read
   from those roots after every point where a collection may have happened.

   `timsort_homogeneous_comparator' and the keyed comparators below are the two
   deliberate exceptions, and they earn it by proving that no callback -- hence
   no collection -- can happen at all for the duration of that sort.  Read
   their comments before touching them.

   A SECOND, UNRELATED INVARIANT, on the Scheme side, because it is invisible
   in review and costs megabytes per escape: NOTHING MAY BE NAMED AFTER THE
   FOREIGN CALL EXCEPT ITS RETURN VALUE.  Whatever the call's continuation
   captures is pinned for the life of the process when a comparator escapes
   non-locally, because CHICKEN never unwinds that continuation.  Writing
   `(vector-length src)' into the failure message of the vector wrapper --
   which is the obvious thing to write -- retained the caller's whole vector,
   elements included, on every escaped sort: measured at 4.8 MB per escape for
   300 records carrying a 2000-word payload each.  Passing the length in as an
   argument and mentioning only that argument drops it to the same 430 bytes
   the list path costs.  This is NOT the GC-safety rule above and is not
   implied by it.

   The roots themselves are recycled through a process-lifetime pool rather
   than deleted.  `CHICKEN_gc_root_set' is `C_mutate', which records the
   root's slot on the runtime's mutation stack whenever the value is still in
   the nursery; that record is only consumed by the next collection, and the
   next collection dereferences it.  Freeing the root before then (which
   `CHICKEN_delete_gc_root' does) would leave the collector reading freed
   memory.  Releasing a root by storing an immediate is a plain store, so it
   adds no new record and drops the reference.
   ------------------------------------------------------------------------ */

typedef struct timsort_roots_s
{
    struct timsort_roots_s *next; /* the free list, or the in-flight stack */
    void *vector;
    void *comparator;
    void *in;
    void *buffer;
    void *keys;
    void *ob_item;
} timsort_roots_t;

static timsort_roots_t *timsort_roots_pool = NULL;     /* free list */
static timsort_roots_t *timsort_inflight = NULL;       /* innermost sort first */
static size_t timsort_inflight_depth = 0;

static void timsort_roots_clear(timsort_roots_t *r)
{
    CHICKEN_gc_root_set(r->vector, C_SCHEME_UNDEFINED);
    CHICKEN_gc_root_set(r->comparator, C_SCHEME_UNDEFINED);
    CHICKEN_gc_root_set(r->in, C_SCHEME_UNDEFINED);
    CHICKEN_gc_root_set(r->buffer, C_SCHEME_UNDEFINED);
    CHICKEN_gc_root_set(r->keys, C_SCHEME_UNDEFINED);
}

static timsort_roots_t *timsort_roots_acquire(void)
{
    timsort_roots_t *r = timsort_roots_pool;

    if (r != NULL)
    {
        timsort_roots_pool = r->next;
    }
    else
    {
        r = (timsort_roots_t *)malloc(sizeof(timsort_roots_t));

        if (r == NULL) return NULL;

        r->vector = CHICKEN_new_gc_root();
        r->comparator = CHICKEN_new_gc_root();
        r->in = CHICKEN_new_gc_root();
        r->buffer = CHICKEN_new_gc_root();
        r->keys = CHICKEN_new_gc_root();
    }

    r->ob_item = NULL;

    r->next = timsort_inflight;
    timsort_inflight = r;
    timsort_inflight_depth++;

    return r;
}

/* The normal path: drop the Scheme references, free the working array and
   return the root set to the pool. */
static void timsort_roots_release(void)
{
    timsort_roots_t *r = timsort_inflight;

    timsort_inflight = r->next;
    timsort_inflight_depth--;

    timsort_roots_clear(r);

    free(r->ob_item);
    r->ob_item = NULL;

    r->next = timsort_roots_pool;
    timsort_roots_pool = r;
}

/* A non-local exit out of the comparator -- an error, or a continuation
   captured outside the sort -- abandons the C frames of `C_timsort': it never
   returns, so it can neither free its arrays nor drop its roots, and those
   roots would otherwise pin the whole input in the Scheme heap for the life of
   the process.  The Scheme wrapper notes the depth on the way in and calls
   `C_timsort_unwind' from the `after' thunk of a `dynamic-wind'.

   Only the sorts nested strictly inside the caller's own are unwound, so an
   outer sort that is still live is never touched.  The abandoned arrays are
   deliberately NOT freed and the root sets are NOT recycled: CHICKEN does not
   unwind the C frames of such a call either (see `callback_returned_flag' in
   runtime.c), so those frames nominally still reference them.  What is left
   behind is then exactly the C-memory leak this file had before the roots
   existed -- what the roots would have added, the retained Scheme objects, is
   released. */
size_t C_timsort_depth(void)
{
    return timsort_inflight_depth;
}

void C_timsort_unwind(size_t depth)
{
    /* Only the outermost sort unwinds.  While an outer sort is still in
       flight the runtime may resume frames belonging to an abandoned inner
       one, and clearing that inner sort's roots makes the resumed frame read
       a cleared vector root -- measured as a segfault in the nested
       inner-escape case.  Leaving them until the outermost sort finishes
       costs only the C-memory leak this file had before the roots existed. */
    if (depth != 0) return;

    while (timsort_inflight_depth > depth)
    {
        timsort_roots_t *r = timsort_inflight;

        timsort_inflight = r->next;
        timsort_inflight_depth--;

        timsort_roots_clear(r);
    }
}

typedef struct
{
    timsort_roots_t *roots;
    int primitive; /* order what we can in C, instead of always calling back */
    int widen;     /* ... and not just numbers */
    int keyed;     /* compare the KEYS at those indices, not the elements */

    /* Only set, and only read, by the keyed homogeneous comparators below,
       which provably never reach `C_callback'.  See their comment. */
    C_word keys_pinned;
} timsort_scheme_t;

/* `timsort_object_t' is `void *' and the sort treats it as completely opaque:
   it stores it, hands it to the comparator and asserts it is non-NULL (in
   `gallop_left' / `gallop_right'), and never dereferences it or requires two
   objects to differ.  I grepped every assertion in the vendored timsort.c to
   confirm that.  So the "object" can simply *be* the element's index, biased
   by one so that index 0 is not NULL -- which saves both an 8 MB array and,
   on every single comparison, a dependent load out of it.

   If a future `make timsort-fetch' ever brings in a version that dereferences
   an object or asserts distinctness, this is the line that breaks. */
#define TIMSORT_INDEX(o) ((size_t)(uintptr_t)(o) - 1)
#define TIMSORT_OBJECT_FOR_INDEX(i) ((timsort_object_t *)(uintptr_t)((i) + 1))

/* ------------------------------------------------------------------------
   Ordering primitive types in C.

   Every `C_..p' predicate in chicken.h expands to `C_mk_bool', which yields
   C_SCHEME_TRUE or C_SCHEME_FALSE -- and C_SCHEME_FALSE is 0x6, so it reads
   as *true* in C.  `if (C_fixnump(a) && C_fixnump(b))', which is what this
   file used to say, is therefore unconditionally true, and everything under
   it was dead code: non-fixnums were compared with `C_fixnum_lessp' on raw
   tagged words, i.e. ordered by heap address.  Predicates must be read
   through `C_truep', or, as here, replaced by the underlying bit test.  The
   block-header predicates (flonum/bignum/ratnum/string/symbol) dereference
   the object, so nothing may reach them before `C_immediatep' is ruled out.

   None of the primitives called below can trigger a collection.  `C_i_lessp'
   is one of the runtime's inline-callable entry points: its bignum and ratnum
   temporaries come from a C stack buffer, from `C_malloc' via
   `allocate_tmp_bignum' and from the scratch space, never from the nursery.
   The string, symbol and character comparisons only read bytes.  `aw' and
   `bw' therefore stay valid across them -- unlike across `C_callback'.
   ------------------------------------------------------------------------ */

enum timsort_kind_e
{
    /* No primitive ordering: the pair goes back to the Scheme comparator.
       Cplxnums belong here -- `<' refuses to order them, and so must we. */
    TIMSORT_KIND_OTHER = 0,
    TIMSORT_KIND_NUMBER,
    TIMSORT_KIND_STRING,
    TIMSORT_KIND_SYMBOL,
    TIMSORT_KIND_CHAR,
    TIMSORT_KIND_BOOLEAN
};

static int timsort_kind(C_word x)
{
    C_word h;

    if (x & C_FIXNUM_BIT) return TIMSORT_KIND_NUMBER;

    if (C_immediatep(x))
    {
        if ((x & C_IMMEDIATE_TYPE_BITS) == C_CHARACTER_BITS) return TIMSORT_KIND_CHAR;
        if ((x & C_IMMEDIATE_TYPE_BITS) == C_BOOLEAN_BITS) return TIMSORT_KIND_BOOLEAN;

        return TIMSORT_KIND_OTHER;
    }

    h = C_block_header(x);

    if (h == C_FLONUM_TAG || h == C_BIGNUM_TAG || h == C_RATNUM_TAG) return TIMSORT_KIND_NUMBER;
    if (h == C_SYMBOL_TAG) return TIMSORT_KIND_SYMBOL;
    if ((h & C_HEADER_BITS_MASK) == C_STRING_TYPE) return TIMSORT_KIND_STRING;

    return TIMSORT_KIND_OTHER;
}

/* Byte-wise lexicographic order over two UTF-8 byte ranges, shorter first on
   a common prefix.  UTF-8 preserves code-point order under `memcmp', and a
   character-prefix is also a byte-prefix, so this is exactly what
   `C_utf_compare' plus the length tie-break in `string<?' computes -- and
   unlike `strncmp' it does not stop at an embedded NUL, which a CHICKEN 6
   string may contain. */
/* The payload length of a NUL-terminated name bytevector.  The `?:' can never
   fire -- every such bytevector is allocated with at least the NUL -- but an
   underflow here would become an unbounded `memcmp', so it is not left to
   trust. */
static size_t timsort_name_bytes(C_word bv)
{
    size_t size = (size_t)C_header_size(bv);

    return size > 0 ? size - 1 : 0;
}

static int timsort_bytes_lessp(const void *ap, size_t an, const void *bp, size_t bn)
{
    size_t n = an < bn ? an : bn;
    int c = n == 0 ? 0 : memcmp(ap, bp, n);

    return c < 0 || (c == 0 && an < bn);
}

/* A CHICKEN 6 string is {bytevector, character count}; the bytevector carries
   a trailing NUL that is not part of the string.  `C_i_string_length' would
   give the character count, and as a *tagged* fixnum at that; ordering by it
   before the contents -- which this file used to do -- is not `string<?'. */
static int timsort_string_lessp(C_word a, C_word b)
{
    C_word abv = C_block_item(a, 0), bbv = C_block_item(b, 0);

    return timsort_bytes_lessp(C_data_pointer(abv), timsort_name_bytes(abv),
                               C_data_pointer(bbv), timsort_name_bytes(bbv));
}

/* A symbol's name is the raw UTF-8 bytevector in slot 1, also NUL-terminated.
   Scheme has no `symbol<?', so this is a defined extension of `<' rather than
   an implementation of anything standard: symbols are ordered by their names,
   consistently with `string<?'. */
static int timsort_symbol_lessp(C_word a, C_word b)
{
    C_word abv = C_symbol_name(a), bbv = C_symbol_name(b);

    return timsort_bytes_lessp(C_data_pointer(abv), timsort_name_bytes(abv),
                               C_data_pointer(bbv), timsort_name_bytes(bbv));
}

/* -1 when the pair is not one this comparison type orders in C. */
static int timsort_primitive_lessp(C_word aw, C_word bw, int widen)
{
    int ka = timsort_kind(aw);

    if (ka != timsort_kind(bw)) return -1;

    switch (ka)
    {
    /* Mixed fixnum/flonum/bignum/ratnum pairs land here too; `C_i_lessp' is
       the runtime's own `<' -- it is what the compiler inlines `(< a b)' to
       -- so NaN (never less), the infinities, -0.0 and exact/inexact
       mixtures all come out exactly as Scheme has them. */
    case TIMSORT_KIND_NUMBER:  return C_truep(C_i_lessp(aw, bw));
    case TIMSORT_KIND_STRING:  return widen ? timsort_string_lessp(aw, bw) : -1;
    case TIMSORT_KIND_SYMBOL:  return widen ? timsort_symbol_lessp(aw, bw) : -1;
    case TIMSORT_KIND_CHAR:    return widen ? C_character_code(aw) < C_character_code(bw) : -1;
    case TIMSORT_KIND_BOOLEAN: return widen ? (aw == C_SCHEME_FALSE && bw != C_SCHEME_FALSE) : -1;
    default: return -1;
    }
}

static int timsort_comparator(timsort_object_t *a, timsort_object_t *b, void *arg)
{
    C_word result;

    timsort_scheme_t *ctx = (timsort_scheme_t *)arg;

    /* Re-read the vector, and both elements out of it, on every comparison: a
       previous callback may have let a collection move all three.  On a keyed
       sort the order is decided by the parallel key vector; the elements
       themselves are never looked at until the write-back. */
    C_word vector = CHICKEN_gc_root_ref(ctx->keyed ? ctx->roots->keys : ctx->roots->vector);

    C_word aw = C_block_item(vector, TIMSORT_INDEX(a));
    C_word bw = C_block_item(vector, TIMSORT_INDEX(b));

    if (ctx->primitive)
    {
        int lt;

        /* The overwhelmingly common case, and the only one worth a branch of
           its own: two fixnums.  Tagged fixnums compare as signed C words. */
        if (aw & bw & C_FIXNUM_BIT) return (C_word)aw < (C_word)bw;

        lt = timsort_primitive_lessp(aw, bw, ctx->widen);

        if (lt >= 0) return lt;

        /* Unknown, or two different kinds: fall through to Scheme, which is
           where the error for e.g. a string against a number has to come
           from anyway. */
    }

    C_save(bw);
    C_save(aw);

    result = C_callback(CHICKEN_gc_root_ref(ctx->roots->comparator), 2);

    /* aw, bw and vector are stale from here on and must not be reused. */

    return C_truep(result) ? 1 : 0;
}

/* ------------------------------------------------------------------------
   The homogeneous fast path.

   When every element of the vector belongs to one ordering class that this
   comparison type handles in C, the dispatch above provably cannot reach
   `C_callback'.  Nothing else in this file allocates, and none of
   `C_i_lessp', `memcmp' or the flonum/character reads can collect (see the
   note above), so for the whole duration of such a sort the heap is frozen
   and a raw `C_word' may safely be held in C memory again.

   That lets the sort run over the tagged words themselves: `ob_item' holds
   the values, not indices, so a comparison is a register compare with no
   load at all, instead of a random gather out of an 8 MB vector, and the
   write-back reads the sorted array sequentially.  A specialised comparator
   per class removes the per-comparison type dispatch as well.

   The vector stays pinned in its GC root, so the values in that array stay
   live even though the collector cannot see the array itself.

   THIS IS ONE OF THE TWO PLACES IN THIS FILE THAT BREAK THE "no C_word in C
   memory" RULE -- the other is `ctx->keys_pinned', used by the keyed
   specialised comparators (see TIMSORT_KEY below) -- and the soundness of
   both rests entirely on "no callback implies no collection".  Anything added
   to the dispatch that can allocate in the nursery, or any class let through
   here that can reach the Scheme comparator, turns every word held this way
   into a pointer into the dead semispace.  If you add a third such place,
   say so here and there.
   ------------------------------------------------------------------------ */

#define TIMSORT_VALUE(o) ((C_word)(uintptr_t)(o))

static int timsort_v_fixnum(timsort_object_t *a, timsort_object_t *b, void *arg)
{
    (void)arg;
    return TIMSORT_VALUE(a) < TIMSORT_VALUE(b);
}

static int timsort_v_flonum(timsort_object_t *a, timsort_object_t *b, void *arg)
{
    (void)arg;
    /* C's `<' is false whenever an operand is a NaN, which is what `<' says. */
    return C_flonum_magnitude(TIMSORT_VALUE(a)) < C_flonum_magnitude(TIMSORT_VALUE(b));
}

static int timsort_v_number(timsort_object_t *a, timsort_object_t *b, void *arg)
{
    (void)arg;
    return C_truep(C_i_lessp(TIMSORT_VALUE(a), TIMSORT_VALUE(b)));
}

static int timsort_v_string(timsort_object_t *a, timsort_object_t *b, void *arg)
{
    (void)arg;
    return timsort_string_lessp(TIMSORT_VALUE(a), TIMSORT_VALUE(b));
}

static int timsort_v_symbol(timsort_object_t *a, timsort_object_t *b, void *arg)
{
    (void)arg;
    return timsort_symbol_lessp(TIMSORT_VALUE(a), TIMSORT_VALUE(b));
}

static int timsort_v_char(timsort_object_t *a, timsort_object_t *b, void *arg)
{
    (void)arg;
    return C_character_code(TIMSORT_VALUE(a)) < C_character_code(TIMSORT_VALUE(b));
}

static int timsort_v_boolean(timsort_object_t *a, timsort_object_t *b, void *arg)
{
    (void)arg;
    return TIMSORT_VALUE(a) == C_SCHEME_FALSE && TIMSORT_VALUE(b) != C_SCHEME_FALSE;
}

/* The one ordering class every element of `elements' belongs to, refined to
   all-fixnum / all-flonum where that holds, or TIMSORT_HOMO_NONE when the
   vector is not homogeneous enough and the callback-capable path has to be
   used.  One linear read-only pass, decided before anything is allocated. */
enum timsort_homo_e
{
    TIMSORT_HOMO_NONE = 0,
    TIMSORT_HOMO_FIXNUM,
    TIMSORT_HOMO_FLONUM,
    TIMSORT_HOMO_NUMBER,
    TIMSORT_HOMO_STRING,
    TIMSORT_HOMO_SYMBOL,
    TIMSORT_HOMO_CHAR,
    TIMSORT_HOMO_BOOLEAN
};

static int timsort_homogeneous_kind(C_word elements, size_t n, int widen)
{
    size_t i;
    int kind = -1, all_fixnum = 1, all_flonum = 1;

    for (i = 0; i < n; i++)
    {
        C_word e = C_block_item(elements, i);
        int k = timsort_kind(e);

        if (k == TIMSORT_KIND_OTHER) return TIMSORT_HOMO_NONE;
        if (!widen && k != TIMSORT_KIND_NUMBER) return TIMSORT_HOMO_NONE;
        if (kind < 0) kind = k;
        else if (k != kind) return TIMSORT_HOMO_NONE;

        /* The value goes straight into `ob_item', where the sort requires it
           to be non-NULL.  No Scheme object is ever the word 0 -- fixnums are
           odd, the other immediates carry their type bits and a block is a
           real address -- but this path must not be the place that finds out
           otherwise. */
        if (e == 0) return TIMSORT_HOMO_NONE;

        if (!(e & C_FIXNUM_BIT)) all_fixnum = 0;
        /* `C_block_header' dereferences, so immediates must be ruled out. */
        if (C_immediatep(e) || C_block_header(e) != C_FLONUM_TAG) all_flonum = 0;
    }

    switch (kind)
    {
    case TIMSORT_KIND_NUMBER:
        return all_fixnum ? TIMSORT_HOMO_FIXNUM
             : (all_flonum ? TIMSORT_HOMO_FLONUM : TIMSORT_HOMO_NUMBER);
    case TIMSORT_KIND_STRING:  return TIMSORT_HOMO_STRING;
    case TIMSORT_KIND_SYMBOL:  return TIMSORT_HOMO_SYMBOL;
    case TIMSORT_KIND_CHAR:    return TIMSORT_HOMO_CHAR;
    case TIMSORT_KIND_BOOLEAN: return TIMSORT_HOMO_BOOLEAN;
    default: return TIMSORT_HOMO_NONE;
    }
}

/* The value-carrying comparator for that class, or NULL for none. */
static threeways_comparefunc_t timsort_homogeneous_comparator(C_word elements, size_t n, int widen)
{
    switch (timsort_homogeneous_kind(elements, n, widen))
    {
    case TIMSORT_HOMO_FIXNUM:  return timsort_v_fixnum;
    case TIMSORT_HOMO_FLONUM:  return timsort_v_flonum;
    case TIMSORT_HOMO_NUMBER:  return timsort_v_number;
    case TIMSORT_HOMO_STRING:  return timsort_v_string;
    case TIMSORT_HOMO_SYMBOL:  return timsort_v_symbol;
    case TIMSORT_HOMO_CHAR:    return timsort_v_char;
    case TIMSORT_HOMO_BOOLEAN: return timsort_v_boolean;
    default: return NULL;
    }
}

/* Defined below, with the comment block explaining why its keys may be pinned:
   both entry points need it, and it sits with the other keyed comparators. */
static threeways_comparefunc_t timsort_keyed_comparator(C_word keys, size_t n, int widen);

C_word C_timsort(C_word in,
                 size_t size,
                 C_word comparator,
                 C_word buffer,
                 C_word elements,
                 C_word keys,
                 int inplace,
                 int reverse,
                 int use_ordinary_insertion_sort,
                 int unpredictable_branch_on_random_data,
                 timsort_scheme_comparison_type_t comparator_type)
{
    timsort_list_t list;
    timsort_scheme_t ctx;
    threeways_comparefunc_t specialised;
    size_t index, n, m;
    C_word cons_cell, sorted_vector, result;
    int res, carries_values;

    /* The vector is the authority on how many elements there are; `size' may
       only shrink that, never grow it past the allocation -- and a keys vector,
       when there is one, may shrink it further. */
    n = (size_t)C_unfix(C_i_vector_length(elements));
    if (size < n) n = size;

    if (keys != C_SCHEME_FALSE)
    {
        m = (size_t)C_unfix(C_i_vector_length(keys));
        if (m < n) n = m;
    }

    if (n == 0)
    {
        C_return(inplace ? C_SCHEME_UNDEFINED : buffer);
    }

    ctx.primitive = comparator_type == TIMSORT_USE_LESS_THAN
                 || comparator_type == TIMSORT_USE_NUMBER_LESS_THAN;
    ctx.widen = comparator_type == TIMSORT_USE_LESS_THAN;
    ctx.keyed = keys != C_SCHEME_FALSE;
    ctx.keys_pinned = C_SCHEME_FALSE;

    ctx.roots = timsort_roots_acquire();

    if (ctx.roots == NULL) C_return(C_SCHEME_FALSE);

    /* A keyed sort must keep the permutation, so even when its keys are
       homogeneous `ob_item' holds indices; only an unkeyed homogeneous sort can
       carry the values themselves. */
    if (ctx.keyed)
    {
        specialised = ctx.primitive ? timsort_keyed_comparator(keys, n, ctx.widen) : NULL;
        carries_values = 0;
    }
    else
    {
        specialised = ctx.primitive ? timsort_homogeneous_comparator(elements, n, ctx.widen) : NULL;
        carries_values = specialised != NULL;
    }

    list.ob_item = (timsort_object_t **)malloc(n * sizeof(timsort_object_t *));

    /* Recorded before anything can fail, so that the release below frees it
       on every path out of here. */
    ctx.roots->ob_item = list.ob_item;

    if (list.ob_item == NULL)
    {
        timsort_roots_release();
        C_return(C_SCHEME_FALSE);
    }

    /* `CHICKEN_gc_root_set' goes through `C_mutate', which is what makes the
       value survive a nursery flip as well as a major collection. */
    CHICKEN_gc_root_set(ctx.roots->vector, elements);
    CHICKEN_gc_root_set(ctx.roots->comparator, comparator);
    CHICKEN_gc_root_set(ctx.roots->in, in);
    CHICKEN_gc_root_set(ctx.roots->buffer, buffer);
    CHICKEN_gc_root_set(ctx.roots->keys, keys);

    /* Only read by the keyed specialised comparators, which cannot collect. */
    if (ctx.keyed && specialised != NULL) ctx.keys_pinned = keys;

    list.ob_size = (timsort_ssize_t)n;

    if (carries_values)
    {
        for (index = 0; index < n; index++)
        {
            list.ob_item[index] = (timsort_object_t *)(uintptr_t)C_block_item(elements, index);
        }
    }
    else
    {
        for (index = 0; index < n; index++)
        {
            list.ob_item[index] = TIMSORT_OBJECT_FOR_INDEX(index);
        }
    }

    res = list_sort_impl(&list, reverse, use_ordinary_insertion_sort,
                         unpredictable_branch_on_random_data,
                         specialised != NULL ? specialised : timsort_comparator,
                         specialised != NULL && carries_values ? NULL : (void *)&ctx);

    /* `list_sort_impl' returns -1 when `merge_getmem' cannot allocate its
       temporary run buffer.  An `assert' is not error handling: under NDEBUG
       it disappears and a PARTIALLY SORTED list would be written back and
       returned as a success.  #f is the allocation-failure answer the Scheme
       wrapper already turns into an error. */
    if (res != 0)
    {
        timsort_roots_release();
        C_return(C_SCHEME_FALSE);
    }

    /* The sort is over, so no further callback can happen: from here on it is
       safe to hold `C_word's in locals again -- but they must be re-read from
       the roots, because the parameters we were called with are stale. */
    sorted_vector = CHICKEN_gc_root_ref(ctx.roots->vector);
    cons_cell = inplace ? CHICKEN_gc_root_ref(ctx.roots->in) : CHICKEN_gc_root_ref(ctx.roots->buffer);

    for (index = 0; index < n && cons_cell != C_SCHEME_END_OF_LIST; index++)
    {
        C_word sorted = carries_values
            ? TIMSORT_VALUE(list.ob_item[index])
            : C_block_item(sorted_vector, TIMSORT_INDEX(list.ob_item[index]));

        /* `C_u_i_set_car' is `C_mutate', a real call whose whole job is the
           generational write barrier.  A barrier is only ever needed for a
           pointer, and `C_immediatep' is exactly `C_mutate's own no-op
           condition -- so for a list of fixnums, characters or booleans this
           is a plain store. */
        if (C_immediatep(sorted))
        {
            C_set_block_item(cons_cell, 0, sorted);
        }
        else
        {
            C_u_i_set_car(cons_cell, sorted);
        }

        cons_cell = C_i_cdr(cons_cell);
    }

    result = inplace ? C_SCHEME_UNDEFINED : CHICKEN_gc_root_ref(ctx.roots->buffer);

    timsort_roots_release();

    C_return(result);
}

/* ------------------------------------------------------------------------
   The keyed homogeneous comparators.

   A keyed sort permutes INDICES -- the permutation is what the write-back
   needs in order to move the elements -- so these cannot carry values in
   `ob_item' the way the unkeyed homogeneous path does.  What they can do,
   when the key vector is homogeneous, is skip the callback entirely and read
   the key straight out of a pinned vector.

   `ctx->keys_pinned' is a raw `C_word' held in C memory for the duration of
   the sort, which is exactly what the rest of this file forbids.  It is
   sound here for the same reason `timsort_homogeneous_comparator' is: the
   key vector was scanned before the sort started and every key belongs to
   one class this file orders in C, so no comparison can reach `C_callback',
   so no comparison can collect, so nothing moves.  The key vector is also
   held in `roots->keys' throughout, so it stays live.

   SAYING IT PLAINLY: if anything is ever added below that can allocate in
   the nursery or fall back to the Scheme comparator, `keys_pinned' becomes a
   pointer into the dead semispace and this must go back to re-reading the
   root on every comparison.
   ------------------------------------------------------------------------ */

#define TIMSORT_KEY(ctx, o) (C_block_item((ctx)->keys_pinned, TIMSORT_INDEX(o)))

static int timsort_k_fixnum(timsort_object_t *a, timsort_object_t *b, void *arg)
{
    timsort_scheme_t *ctx = (timsort_scheme_t *)arg;
    return (C_word)TIMSORT_KEY(ctx, a) < (C_word)TIMSORT_KEY(ctx, b);
}

static int timsort_k_flonum(timsort_object_t *a, timsort_object_t *b, void *arg)
{
    timsort_scheme_t *ctx = (timsort_scheme_t *)arg;
    return C_flonum_magnitude(TIMSORT_KEY(ctx, a)) < C_flonum_magnitude(TIMSORT_KEY(ctx, b));
}

static int timsort_k_number(timsort_object_t *a, timsort_object_t *b, void *arg)
{
    timsort_scheme_t *ctx = (timsort_scheme_t *)arg;
    return C_truep(C_i_lessp(TIMSORT_KEY(ctx, a), TIMSORT_KEY(ctx, b)));
}

static int timsort_k_string(timsort_object_t *a, timsort_object_t *b, void *arg)
{
    timsort_scheme_t *ctx = (timsort_scheme_t *)arg;
    return timsort_string_lessp(TIMSORT_KEY(ctx, a), TIMSORT_KEY(ctx, b));
}

static int timsort_k_symbol(timsort_object_t *a, timsort_object_t *b, void *arg)
{
    timsort_scheme_t *ctx = (timsort_scheme_t *)arg;
    return timsort_symbol_lessp(TIMSORT_KEY(ctx, a), TIMSORT_KEY(ctx, b));
}

static int timsort_k_char(timsort_object_t *a, timsort_object_t *b, void *arg)
{
    timsort_scheme_t *ctx = (timsort_scheme_t *)arg;
    return C_character_code(TIMSORT_KEY(ctx, a)) < C_character_code(TIMSORT_KEY(ctx, b));
}

static int timsort_k_boolean(timsort_object_t *a, timsort_object_t *b, void *arg)
{
    timsort_scheme_t *ctx = (timsort_scheme_t *)arg;
    return TIMSORT_KEY(ctx, a) == C_SCHEME_FALSE && TIMSORT_KEY(ctx, b) != C_SCHEME_FALSE;
}

static threeways_comparefunc_t timsort_keyed_comparator(C_word keys, size_t n, int widen)
{
    switch (timsort_homogeneous_kind(keys, n, widen))
    {
    case TIMSORT_HOMO_FIXNUM:  return timsort_k_fixnum;
    case TIMSORT_HOMO_FLONUM:  return timsort_k_flonum;
    case TIMSORT_HOMO_NUMBER:  return timsort_k_number;
    case TIMSORT_HOMO_STRING:  return timsort_k_string;
    case TIMSORT_HOMO_SYMBOL:  return timsort_k_symbol;
    case TIMSORT_HOMO_CHAR:    return timsort_k_char;
    case TIMSORT_HOMO_BOOLEAN: return timsort_k_boolean;
    default: return NULL;
    }
}

/* ------------------------------------------------------------------------
   The native vector entry point.  No list is built, walked or marshalled
   anywhere: the caller's vector IS the working representation.
   ------------------------------------------------------------------------ */

C_word C_timsort_vector(C_word src,
                        size_t size,
                        C_word comparator,
                        C_word dst,
                        C_word keys,
                        int reverse,
                        int use_ordinary_insertion_sort,
                        int unpredictable_branch_on_random_data,
                        timsort_scheme_comparison_type_t comparator_type)
{
    timsort_list_t list;
    timsort_scheme_t ctx;
    threeways_comparefunc_t specialised;
    size_t index, n, m;
    C_word sorted_src, out, result;
    int res, carries_values;

    n = (size_t)C_unfix(C_i_vector_length(src));
    if (size < n) n = size;

    m = (size_t)C_unfix(C_i_vector_length(dst));
    if (m < n) n = m;

    /* Every comparison on the keyed path indexes `keys' with an index the sort
       took from [0, n), and the homogeneity scan reads the same range, so `n'
       must not exceed the keys vector either.  The Scheme wrappers always pass
       a freshly made keys vector of exactly this length; the clamp is here so
       that the C entry point cannot be made to read off the end of a shorter
       one. */
    if (keys != C_SCHEME_FALSE)
    {
        m = (size_t)C_unfix(C_i_vector_length(keys));
        if (m < n) n = m;
    }

    if (n == 0) C_return(dst);

    ctx.primitive = comparator_type == TIMSORT_USE_LESS_THAN
                 || comparator_type == TIMSORT_USE_NUMBER_LESS_THAN;
    ctx.widen = comparator_type == TIMSORT_USE_LESS_THAN;
    ctx.keyed = keys != C_SCHEME_FALSE;
    ctx.keys_pinned = C_SCHEME_FALSE;

    ctx.roots = timsort_roots_acquire();

    if (ctx.roots == NULL) C_return(C_SCHEME_FALSE);

    /* A keyed sort must keep the permutation, so even when its keys are
       homogeneous `ob_item' holds indices; only an unkeyed homogeneous sort
       can carry the values themselves. */
    if (ctx.keyed)
    {
        specialised = ctx.primitive ? timsort_keyed_comparator(keys, n, ctx.widen) : NULL;
        carries_values = 0;
    }
    else
    {
        specialised = ctx.primitive ? timsort_homogeneous_comparator(src, n, ctx.widen) : NULL;
        carries_values = specialised != NULL;
    }

    list.ob_item = (timsort_object_t **)malloc(n * sizeof(timsort_object_t *));

    ctx.roots->ob_item = list.ob_item;

    if (list.ob_item == NULL)
    {
        timsort_roots_release();
        C_return(C_SCHEME_FALSE);
    }

    /* `roots->in' is the list entry point's input list and has no counterpart
       here.  It holds C_SCHEME_UNDEFINED throughout: `CHICKEN_new_gc_root_2'
       initialises a fresh root to it (runtime.c), and `timsort_roots_clear'
       restores it on every release, so it is an immediate whichever way this
       root set was obtained.  Nothing on this path reads it. */
    CHICKEN_gc_root_set(ctx.roots->vector, src);
    CHICKEN_gc_root_set(ctx.roots->comparator, comparator);
    CHICKEN_gc_root_set(ctx.roots->buffer, dst);
    CHICKEN_gc_root_set(ctx.roots->keys, keys);

    /* Only read by the keyed specialised comparators, which cannot collect. */
    if (ctx.keyed && specialised != NULL) ctx.keys_pinned = keys;

    list.ob_size = (timsort_ssize_t)n;

    if (carries_values)
    {
        for (index = 0; index < n; index++)
            list.ob_item[index] = (timsort_object_t *)(uintptr_t)C_block_item(src, index);
    }
    else
    {
        for (index = 0; index < n; index++)
            list.ob_item[index] = TIMSORT_OBJECT_FOR_INDEX(index);
    }

    res = list_sort_impl(&list, reverse, use_ordinary_insertion_sort,
                         unpredictable_branch_on_random_data,
                         specialised != NULL ? specialised : timsort_comparator,
                         specialised != NULL && carries_values ? NULL : (void *)&ctx);

    if (res != 0)
    {
        timsort_roots_release();
        C_return(C_SCHEME_FALSE);
    }

    /* The sort is over: no further callback, hence no further collection, so
       locals may hold `C_word's again -- but they must be re-read from the
       roots, because the parameters are stale.

       Every write-back below then holds `out' (and, in the in-place case, one
       displaced element) in C locals across a run of `C_mutate' calls.  That
       is sound because `C_mutate' cannot collect: it is the inline in
       chicken.h that stores an immediate directly and otherwise calls
       `C_mutate_slot', which only ever pushes the slot onto the mutation stack
       and reallocs that stack if it is full.  I read runtime.c to confirm it
       rather than assume it. */
    sorted_src = CHICKEN_gc_root_ref(ctx.roots->vector);
    out = CHICKEN_gc_root_ref(ctx.roots->buffer);

    if (carries_values)
    {
        /* `ob_item' already holds the sorted objects; `src' is never read
           again, so `out' being `src' itself is harmless. */
        for (index = 0; index < n; index++)
            C_mutate(&C_block_item(out, index), TIMSORT_VALUE(list.ob_item[index]));
    }
    else if (out != sorted_src)
    {
        for (index = 0; index < n; index++)
            C_mutate(&C_block_item(out, index),
                     C_block_item(sorted_src, TIMSORT_INDEX(list.ob_item[index])));
    }
    else
    {
        /* In place, and `ob_item' is a permutation of indices into the very
           vector being written: out[i] must become src[p[i]], and src[p[i]]
           may itself still be needed by a later i.  Walk the permutation one
           cycle at a time, saving the single element the cycle displaces.
           Each slot is read exactly once, before it is written, and a slot
           already placed is marked by clearing its `ob_item' entry -- NULL
           cannot collide with a real entry, which is a one-biased index.
           No extra memory at all. */
        for (index = 0; index < n; index++)
        {
            size_t j, k;
            C_word displaced;

            if (list.ob_item[index] == NULL) continue;

            j = index;
            displaced = C_block_item(out, index);

            for (;;)
            {
                k = TIMSORT_INDEX(list.ob_item[j]);
                list.ob_item[j] = NULL;

                if (k == index)
                {
                    C_mutate(&C_block_item(out, j), displaced);
                    break;
                }

                C_mutate(&C_block_item(out, j), C_block_item(out, k));
                j = k;
            }
        }
    }

    result = CHICKEN_gc_root_ref(ctx.roots->buffer);

    timsort_roots_release();

    C_return(result);
}
