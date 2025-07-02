
#include <chicken/chicken.h>
#include <timsort.h>

C_word C_timsort(C_word list_or_array,
                 size_t size,
                 C_word comparator,
                 C_word buffer,
                 int inplace,
                 int reverse,
                 int use_ordinary_insertion_sort,
                 int unpredictable_branch_on_random_data);