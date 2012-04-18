struct as_01 {
    int a;
    int b;
};

static void f01(struct as_01 arr[])
{
    arr[1].b = 42;
    //return arrs;
}
