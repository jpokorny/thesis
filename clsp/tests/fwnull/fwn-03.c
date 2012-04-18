#include <stdlib.h>

static void test0(void) {
    void **x = NULL;
    *x = (void *)&x;
}

static void test2(void **ptr) {
    *ptr = NULL;

    if (ptr)
        test0();
}
