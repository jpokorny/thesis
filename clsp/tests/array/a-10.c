typedef int three_int_arrays[][3][4];

static void f01(three_int_arrays arrs)
{
    arrs[1][1][1] = 42;
    //return arrs;
}
