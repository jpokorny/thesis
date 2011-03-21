#include <stdlib.h>

struct str {
    int num;
};

static int test4(struct str *c)
{
    if ((NULL == c) || (0 == c->num))
        return 0;

    if (c)
        return 1;

    return 0;
}
