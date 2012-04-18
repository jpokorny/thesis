struct as_01 {
    int a;
    int b;
};

typedef struct as_01 three_as_01_arrays[][3][4];

static void f01(three_as_01_arrays arrs)
{
    arrs[1][1][1].b = 42;
    //return arrs;
}
