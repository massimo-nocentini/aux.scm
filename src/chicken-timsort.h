#ifndef CHICKEN_TIMSORT_H
#define CHICKEN_TIMSORT_H

#include <chicken/chicken.h>

typedef enum timsort_scheme_comparison_type_e
{
    /* Every comparison re-enters Scheme through `C_callback'. */
    TIMSORT_USE_COMPARATOR,

    /* `<', widened to a total order over the other classes CHICKEN can
       compare without allocating: strings and symbols by their UTF-8 bytes,
       characters by code point, #f before #t.  Anything this cannot order --
       a pair, a vector, two operands of different classes -- is handed to
       the Scheme comparator, so that is still where the error comes from. */
    TIMSORT_USE_LESS_THAN,

    /* Declared by the original interface but never implemented: they fall
       through to the Scheme comparator, exactly as they always have. */
    TIMSORT_USE_GREATER_THAN,
    TIMSORT_USE_LESS_THAN_OR_EQUAL,
    TIMSORT_USE_GREATER_THAN_OR_EQUAL,

    /* Exactly `<' and nothing more: numbers are ordered in C, and every
       other pair -- including a string against a string -- is handed to the
       Scheme comparator, which is both what `<' means and where its
       "bad argument type" error comes from. */
    TIMSORT_USE_NUMBER_LESS_THAN,
} timsort_scheme_comparison_type_t;

/* `elements' must be a Scheme vector holding the objects to sort, in the
   order they appear in `list_or_array'.  It is the only place those objects
   live while the sort runs: on the general path the sort permutes plain C
   indices into it, so that no C_word is ever cached in C memory across a
   comparator callback.  See the comment at the top of chicken-timsort.c.

   Returns the sorted buffer list (#!unspecific when `inplace'), or #f when a
   working array could not be allocated -- nothing has been written in that
   case. */
C_word C_timsort(C_word list_or_array,
                 size_t size,
                 C_word comparator,
                 C_word buffer,
                 C_word elements,
                 int inplace,
                 int reverse,
                 int use_ordinary_insertion_sort,
                 int unpredictable_branch_on_random_data,
                 timsort_scheme_comparison_type_t comparator_type);

/* ------------------------------------------------------------------------
   The native vector entry point.

   `src' is a Scheme vector holding the objects to sort; it is pinned in a GC
   root and is the only place those objects live while the sort runs.  `dst'
   is the Scheme vector the sorted objects are written into; it may be `src'
   itself, in which case the permutation is applied in place, cycle by cycle,
   with no second vector anywhere.

   `keys' is either C_SCHEME_FALSE, or a Scheme vector holding one key per
   element: the order is then decided by comparing keys rather than elements,
   and `src' is merely carried along.  That is a decorate-sort-undecorate whose
   decoration step happens in Scheme, so a sort by a computed key costs n key
   applications instead of ~n log n `C_callback's.

   `size' and the lengths of `dst' and `keys' only ever shrink the number of
   elements sorted; none of them can grow it past `src'.

   Returns `dst', or #f when a working array could not be allocated -- in
   which case nothing has been written.
   ------------------------------------------------------------------------ */
C_word C_timsort_vector(C_word src,
                        size_t size,
                        C_word comparator,
                        C_word dst,
                        C_word keys,
                        int reverse,
                        int use_ordinary_insertion_sort,
                        int unpredictable_branch_on_random_data,
                        timsort_scheme_comparison_type_t comparator_type);

/* Bracket a call to `C_timsort': note the depth before it, and unwind back to
   that depth afterwards, so that a comparator escaping non-locally does not
   leave its roots holding the sorted objects.  See chicken-timsort.c. */
size_t C_timsort_depth(void);
void C_timsort_unwind(size_t depth);

#endif
