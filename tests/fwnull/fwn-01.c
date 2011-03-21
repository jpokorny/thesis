#include <stdlib.h>

static void test0(void) {
    void **x = NULL;
    *x = (void *)&x;
}
