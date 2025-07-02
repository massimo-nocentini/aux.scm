
#include <stdio.h>
#include <assert.h>
#include <chicken/chicken.h>
#include <timsort.h>

typedef struct
{
    size_t index;
    C_word scheme_object;
} timsort_scheme_t;

int timsort_comparator(timsort_object_t *a, timsort_object_t *b, void *arg)
{
    C_word comparator = ((timsort_scheme_t *)arg)->scheme_object;

    C_save(((timsort_scheme_t *)a)->scheme_object);
    C_save(((timsort_scheme_t *)b)->scheme_object);

    C_word result = C_callback(comparator, 2);

    return C_truep(result) ? 0 : 1;
}

C_word C_timsort(C_word in,
                 size_t size,
                 C_word comparator,
                 C_word buffer,
                 int inplace,
                 int reverse,
                 int use_ordinary_insertion_sort,
                 int unpredictable_branch_on_random_data)
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
    comparator_obj.index = 0;

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