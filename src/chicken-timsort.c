#include <stdio.h>
#include <assert.h>
#include <chicken/chicken.h>
#include <stdlib.h>
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

   The rule this file now follows: no `C_word' is ever held across a call to
   `C_callback'.  The objects to be sorted live in a Scheme vector supplied by
   the caller; the sort permutes plain C indices into that vector, and the four
   Scheme values we need (the vector, the comparator, the input list and the
   output list) are held in GC roots and re-read from those roots after every
   point where a collection may have happened.

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
    void *indices;
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
    }

    r->indices = NULL;
    r->ob_item = NULL;

    r->next = timsort_inflight;
    timsort_inflight = r;
    timsort_inflight_depth++;

    return r;
}

/* The normal path: drop the Scheme references, free the working arrays and
   return the root set to the pool. */
static void timsort_roots_release(void)
{
    timsort_roots_t *r = timsort_inflight;

    timsort_inflight = r->next;
    timsort_inflight_depth--;

    timsort_roots_clear(r);

    free(r->indices);
    free(r->ob_item);
    r->indices = NULL;
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
    timsort_scheme_comparison_type_t comparator_type;
} timsort_scheme_t;

/* The sort's "objects" are pointers into a plain C array of indices: they are
   never relocated, and are always distinct and non-NULL (which `gallop_left'
   and `gallop_right' assert). */
#define TIMSORT_INDEX(o) (*(size_t *)(o))

static int timsort_comparator(timsort_object_t *a, timsort_object_t *b, void *arg)
{
    C_word result;

    timsort_scheme_t *ctx = (timsort_scheme_t *)arg;

    /* Re-read the vector, and both elements out of it, on every comparison: a
       previous callback may have let a collection move all three. */
    C_word vector = CHICKEN_gc_root_ref(ctx->roots->vector);

    C_word aw = C_block_item(vector, TIMSORT_INDEX(a));
    C_word bw = C_block_item(vector, TIMSORT_INDEX(b));

    timsort_scheme_comparison_type_t comparator_type = ctx->comparator_type;

    if (comparator_type == TIMSORT_USE_LESS_THAN)
    {
        if (C_fixnump(aw) && C_fixnump(bw))
        {
            result = C_fixnum_lessp(aw, bw);
        }
        else if (C_flonump(aw) && C_flonump(bw))
        {
            result = C_flonum_lessp(aw, bw);
        }
        else if (C_bignump(aw) && C_bignump(bw))
        {
            result = C_i_bignum_cmp(aw, bw) < 0 ? C_SCHEME_TRUE : C_SCHEME_FALSE;
        }
        else if (C_stringp(aw) && C_stringp(bw))
        {
            C_word al = C_i_string_length(aw);
            C_word bl = C_i_string_length(bw);

            if (al == bl)
            {
                char *as = C_string_or_null(aw);
                char *bs = C_string_or_null(bw);
                result = strncmp(as, bs, al) < 0 ? C_SCHEME_TRUE : C_SCHEME_FALSE;
            }
            else if (al < bl)
            {
                result = C_SCHEME_TRUE;
            }
            else
            {
                result = C_SCHEME_FALSE;
            }
        }
        else
        {
            C_save(bw);
            C_save(aw);

            result = C_callback(CHICKEN_gc_root_ref(ctx->roots->comparator), 2);
        }
    }
    else
    {
        C_save(bw);
        C_save(aw);

        result = C_callback(CHICKEN_gc_root_ref(ctx->roots->comparator), 2);
    }

    /* aw, bw and vector are stale from here on and must not be reused. */

    return C_truep(result) ? 1 : 0;
}

C_word C_timsort(C_word in,
                 size_t size,
                 C_word comparator,
                 C_word buffer,
                 C_word elements,
                 int inplace,
                 int reverse,
                 int use_ordinary_insertion_sort,
                 int unpredictable_branch_on_random_data,
                 timsort_scheme_comparison_type_t comparator_type)
{
    timsort_list_t list;
    timsort_scheme_t ctx;
    size_t *indices;
    size_t index, n;
    C_word cons_cell, sorted_vector, result;
    int res;

    /* The vector is the authority on how many elements there are; `size' may
       only shrink that, never grow it past the allocation. */
    n = (size_t)C_unfix(C_i_vector_length(elements));
    if (size < n) n = size;

    if (n == 0)
    {
        C_return(inplace ? C_SCHEME_UNDEFINED : buffer);
    }

    ctx.roots = timsort_roots_acquire();

    if (ctx.roots == NULL) C_return(C_SCHEME_FALSE);

    list.ob_item = (timsort_object_t **)malloc(n * sizeof(timsort_object_t *));
    indices = (size_t *)malloc(n * sizeof(size_t));

    /* Recorded before anything can fail, so that the release below frees
       them on every path out of here. */
    ctx.roots->ob_item = list.ob_item;
    ctx.roots->indices = indices;

    if (list.ob_item == NULL || indices == NULL)
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

    ctx.comparator_type = comparator_type;

    list.ob_size = (timsort_ssize_t)n;

    for (index = 0; index < n; index++)
    {
        indices[index] = index;
        list.ob_item[index] = (timsort_object_t *)&indices[index];
    }

    res = list_sort_impl(&list, reverse, use_ordinary_insertion_sort, unpredictable_branch_on_random_data, timsort_comparator, &ctx);

    assert(res == 0);
    (void)res; /* `assert' compiles out under NDEBUG */

    /* The sort is over, so no further callback can happen: from here on it is
       safe to hold `C_word's in locals again -- but they must be re-read from
       the roots, because the parameters we were called with are stale. */
    sorted_vector = CHICKEN_gc_root_ref(ctx.roots->vector);
    cons_cell = inplace ? CHICKEN_gc_root_ref(ctx.roots->in) : CHICKEN_gc_root_ref(ctx.roots->buffer);

    for (index = 0; index < n && cons_cell != C_SCHEME_END_OF_LIST; index++)
    {
        C_word sorted = C_block_item(sorted_vector, TIMSORT_INDEX(list.ob_item[index]));

        C_u_i_set_car(cons_cell, sorted);

        cons_cell = C_i_cdr(cons_cell);
    }

    result = inplace ? C_SCHEME_UNDEFINED : CHICKEN_gc_root_ref(ctx.roots->buffer);

    timsort_roots_release();

    C_return(result);
}
