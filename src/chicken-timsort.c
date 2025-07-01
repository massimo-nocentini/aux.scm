
#include <stdio.h>
#include <assert.h>
#include <chicken/chicken.h>
#include <timsort.h>

typedef struct comp_s
{
    C_word comparison;
} comp_t;

int timsort_comparator(timsort_object_t *a, timsort_object_t *b, void *arg)
{
    comp_t *comparator = arg;

    C_save(C_i_car(((comp_t *)a->external_object)->comparison));
    C_save(C_i_car(((comp_t *)b->external_object)->comparison));

    C_word result = C_callback(comparator->comparison, 2);

    return C_unfix(result);
}

C_word C_timsort(C_word in, size_t size, C_word comparator, C_word buffer)
{
    comp_t comp = {comparator};

    timsort_list_t *list = malloc(sizeof(timsort_list_t));
    list->ob_size = size;
    list->ob_item = malloc(size * sizeof(timsort_object_t *));

    C_word each = in;

    int index = 0;

    while (each != C_SCHEME_END_OF_LIST)
    {
        list->ob_item[index] = malloc(sizeof(timsort_object_t));
        list->ob_item[index]->external_object = malloc(sizeof(comp_t));
        ((comp_t *)(list->ob_item[index]->external_object))->comparison = each;
        list->ob_item[index]->index = index;
        index++;
        each = C_i_cdr(each);
    }

    // Call the timsort function
    int res = list_sort_impl(list, 0, timsort_comparator, &comp);

    assert(res == 0);

    // C_word *e = C_alloc(C_SIZEOF_PAIR);
    // each = C_SCHEME_END_OF_LIST;
    each = buffer;
    index = 0;
    while (each != C_SCHEME_END_OF_LIST)
    {

        comp_t *sorted = list->ob_item[index]->external_object;
        printf("Resulting pair: %d %d %ld\n", index, list->ob_item[index]->index, C_unfix(C_i_car(sorted->comparison)));

        C_u_i_set_car(each, C_i_car(sorted->comparison));

        each = C_i_cdr(each);
        index++;
        // printf("Resulting pair: %ld\n", C_unfix(C_i_car(each)));

        // free(list->ob_item[index]);
    }

    printf("Timsort completed.\n");

    // free(list->ob_item);
    // free(list);

    C_return(buffer);
}