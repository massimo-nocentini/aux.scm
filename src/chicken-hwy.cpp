
#include <chicken/chicken.h>
#include <iostream>
#include "chicken-hwy.h"
#include <hwy/contrib/sort/vqsort.h>

using namespace std;

void chicken_hwy_vqsort_int32(int32_t* arr, size_t len, int ascending)
{
    if (ascending)
        hwy::VQSort(arr, len, hwy::SortAscending());
    else
        hwy::VQSort(arr, len, hwy::SortDescending());
}

void chicken_hwy_vqsort_uint32(uint32_t* arr, size_t len, int ascending)
{
    if (ascending)
        hwy::VQSort(arr, len, hwy::SortAscending());
    else
        hwy::VQSort(arr, len, hwy::SortDescending());
}

void chicken_hwy_vqsort_int64(int64_t* arr, size_t len, int ascending)
{
    if (ascending)
        hwy::VQSort(arr, len, hwy::SortAscending());
    else
        hwy::VQSort(arr, len, hwy::SortDescending());
}

void chicken_hwy_vqsort_uint64(uint64_t* arr, size_t len, int ascending)
{
    if (ascending)
        hwy::VQSort(arr, len, hwy::SortAscending());
    else
        hwy::VQSort(arr, len, hwy::SortDescending());
}

void chicken_hwy_vqsort_f32(float* arr, size_t len, int ascending)
{
    if (ascending)
        hwy::VQSort(arr, len, hwy::SortAscending());
    else
        hwy::VQSort(arr, len, hwy::SortDescending());
}

void chicken_hwy_vqsort_f64(double* arr, size_t len, int ascending)
{
    if (ascending)
        hwy::VQSort(arr, len, hwy::SortAscending());
    else
        hwy::VQSort(arr, len, hwy::SortDescending());
}