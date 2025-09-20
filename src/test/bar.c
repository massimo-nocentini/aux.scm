#include <stdio.h>
#include <chicken/chicken.h>

extern int callout(int, int, int);
extern int callin(C_word x);

int callout(int x, int y, int z)
{
  C_word *ptr = C_alloc(C_SIZEOF_LIST(3));
  C_word lst;

  printf("This is 'callout': %d, %d, %d\n", x, y, z);
  lst = C_list(&ptr, 3, C_fix(x), C_fix(y), C_fix(z));
  return callin(lst);  /* Note: `callin' will have GC'd the data in `ptr' */
}