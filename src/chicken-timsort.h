
#ifndef CHICKEN_TIMSORT_H
#define CHICKEN_TIMSORT_H

#include <chicken/chicken.h>

typedef enum timsort_scheme_comparison_type_e
{
    TIMSORT_USE_COMPARATOR,
    TIMSORT_USE_LESS_THAN,
    TIMSORT_USE_GREATER_THAN,
    TIMSORT_USE_LESS_THAN_OR_EQUAL,
    TIMSORT_USE_GREATER_THAN_OR_EQUAL,
} timsort_scheme_comparison_type_t;

/* `elements' must be a Scheme vector holding the objects to sort, in the
   order they appear in `list_or_array'.  It is the only place those objects
   live while the sort runs: the sort permutes plain C indices into it, so
   that no C_word is ever cached in C memory across a comparator callback.
   See the comment at the top of chicken-timsort.c.

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

/* Bracket a call to `C_timsort': note the depth before it, and unwind back to
   that depth afterwards, so that a comparator escaping non-locally does not
   leave its roots holding the sorted objects.  See chicken-timsort.c. */
size_t C_timsort_depth(void);
void C_timsort_unwind(size_t depth);

#endif
