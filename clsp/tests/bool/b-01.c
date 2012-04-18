#include <stdbool.h>

static bool f01(bool a, bool b)
{
    bool c, d;
    c = a && b;
    d = a || b;
    return c || !d;
}
