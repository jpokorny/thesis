#include <stdbool.h>

union u_01 {
    bool b;
    int i;
};

static bool handle_bool(bool arg)
{
    return !arg;
}

static int handle_int(int arg)
{
    return -arg;
}

static union u_01 handle_union(union u_01 arg, int which_item)
{
    switch (which_item) {
        case 0:
            arg.b = handle_bool(arg.b);
            break;
        default:
            arg.i = handle_int(arg.i);
            break;
    }
    return arg;
}
