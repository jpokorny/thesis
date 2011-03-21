#include <stdlib.h>

static void test0(void) {
    void **x = NULL;
    *x = (void *)&x;
}

static void test1(void **ptr) {
    if (ptr)
        test0();

    *ptr = NULL;
}
