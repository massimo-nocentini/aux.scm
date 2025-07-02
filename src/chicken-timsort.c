
#include <stdio.h>
#include <assert.h>
#include <chicken/chicken.h>
#include <timsort.h>
#include <stdlib.h>
#include <string.h>
#include "chicken-timsort.h"

typedef struct
{
    size_t index;
    C_word scheme_object;
} timsort_scheme_t;

int timsort_comparator(timsort_object_t *a, timsort_object_t *b, void *arg)
{
    C_word result;

    C_word aw = ((timsort_scheme_t *)a)->scheme_object;
    C_word bw = ((timsort_scheme_t *)b)->scheme_object;

    C_word comparator = ((timsort_scheme_t *)arg)->scheme_object;
    timsort_scheme_comparison_type_t comparator_type = ((timsort_scheme_t *)arg)->index;

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
                result = strcmp(as, bs) < 0 ? C_SCHEME_TRUE : C_SCHEME_FALSE;
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

            result = C_callback(comparator, 2);
        }
    }
    else
    {
        C_save(bw);
        C_save(aw);

        result = C_callback(comparator, 2);
    }

    return result == C_SCHEME_TRUE ? 1 : 0;
}

C_word C_timsort(C_word in,
                 size_t size,
                 C_word comparator,
                 C_word buffer,
                 int inplace,
                 int reverse,
                 int use_ordinary_insertion_sort,
                 int unpredictable_branch_on_random_data,
                 timsort_scheme_comparison_type_t comparator_type)
{

    timsort_list_t list;
    list.ob_size = size;
    list.ob_item = malloc(size * sizeof(timsort_object_t *));

    C_word cons_cell = in;

    int index = 0;

    while (cons_cell != C_SCHEME_END_OF_LIST)
    {
        timsort_scheme_t *obj = malloc(sizeof(timsort_scheme_t));
        obj->scheme_object = C_i_car(cons_cell);
        obj->index = index;

        list.ob_item[index] = (timsort_object_t *)obj;

        index++;
        cons_cell = C_i_cdr(cons_cell);
    }

    timsort_scheme_t comparator_obj;
    comparator_obj.scheme_object = comparator;
    comparator_obj.index = comparator_type;

    int res = list_sort_impl(&list, reverse, use_ordinary_insertion_sort, unpredictable_branch_on_random_data, timsort_comparator, &comparator_obj);

    assert(res == 0);

    cons_cell = inplace ? in : buffer;
    index = 0;

    while (cons_cell != C_SCHEME_END_OF_LIST)
    {
        C_word sorted = ((timsort_scheme_t *)list.ob_item[index])->scheme_object;

        C_u_i_set_car(cons_cell, sorted);

        cons_cell = C_i_cdr(cons_cell);
        index++;

        // free(list.ob_item[index]);
    }

    free(list.ob_item);

    C_return(inplace ? C_SCHEME_UNDEFINED : buffer);
}