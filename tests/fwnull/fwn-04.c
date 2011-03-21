#include <stdlib.h>

static void test3(void **ptr) {
    if (!ptr || !*ptr)
        return;

    *ptr = NULL;
}
