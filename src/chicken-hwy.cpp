
#include <chicken/chicken.h>
#include <iostream>
#include "chicken-hwy.h"

using namespace std;

void chicken_hwy_vqsort_int32_asc(int32_t* arr, size_t len)
{
    hwy::VQSort(arr, len, hwy::SortAscending());
}

void chicken_hwy_vqsort_int32_desc(int32_t* arr, size_t len)
{
    hwy::VQSort(arr, len, hwy::SortDescending());
}

void chicken_hwy_vqsort_uint32_asc(uint32_t* arr, size_t len)
{
    hwy::VQSort(arr, len, hwy::SortAscending());
}

void chicken_hwy_vqsort_uint32_desc(uint32_t* arr, size_t len)
{
    hwy::VQSort(arr, len, hwy::SortDescending());
}

void chicken_hwy_vqsort_int64_asc(int64_t* arr, size_t len)
{
    hwy::VQSort(arr, len, hwy::SortAscending());
}

void chicken_hwy_vqsort_int64_desc(int64_t* arr, size_t len)
{
    hwy::VQSort(arr, len, hwy::SortDescending());
}

void chicken_hwy_vqsort_uint64_asc(uint64_t* arr, size_t len)
{
    hwy::VQSort(arr, len, hwy::SortAscending());
}

void chicken_hwy_vqsort_uint64_desc(uint64_t* arr, size_t len)
{
    hwy::VQSort(arr, len, hwy::SortDescending());
}

void chicken_hwy_vqsort_f32_asc(float* arr, size_t len)
{
    hwy::VQSort(arr, len, hwy::SortAscending());
}

void chicken_hwy_vqsort_f32_desc(float* arr, size_t len)
{
    hwy::VQSort(arr, len, hwy::SortDescending());
}

void chicken_hwy_vqsort_f64_asc(double* arr, size_t len)
{
    hwy::VQSort(arr, len, hwy::SortAscending());
}

void chicken_hwy_vqsort_f64_desc(double* arr, size_t len)
{
    hwy::VQSort(arr, len, hwy::SortDescending());
}