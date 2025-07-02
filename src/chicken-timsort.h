
#include <chicken/chicken.h>

typedef enum timsort_scheme_comparison_type_e
{
    TIMSORT_USE_COMPARATOR,
    TIMSORT_USE_LESS_THAN,
    TIMSORT_USE_GREATER_THAN,
    TIMSORT_USE_LESS_THAN_OR_EQUAL,
    TIMSORT_USE_GREATER_THAN_OR_EQUAL,
} timsort_scheme_comparison_type_t;


C_word C_timsort(C_word list_or_array,
                 size_t size,
                 C_word comparator,
                 C_word buffer,
                 int inplace,
                 int reverse,
                 int use_ordinary_insertion_sort,
                 int unpredictable_branch_on_random_data,
                 timsort_scheme_comparison_type_t comparator_type);