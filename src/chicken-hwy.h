
#include <chicken/chicken.h>

#ifdef __cplusplus
#include <hwy/contrib/sort/vqsort.h>
#endif

#ifdef __cplusplus
extern "C"
{
#endif

void chicken_hwy_vqsort_int32(int32_t* arr, size_t len, int ascending);
void chicken_hwy_vqsort_uint32(uint32_t* arr, size_t len, int ascending);

void chicken_hwy_vqsort_int64(int64_t* arr, size_t len, int ascending);
void chicken_hwy_vqsort_uint64(uint64_t* arr, size_t len, int ascending);

void chicken_hwy_vqsort_f32(float* arr, size_t len, int ascending);
void chicken_hwy_vqsort_f64(double* arr, size_t len, int ascending);


#ifdef __cplusplus
}
#endif
