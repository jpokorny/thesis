struct s2_01 {
    char c1;
    int  n1;
};

static int return_int(struct s2_01 arg)
{
    return arg.n1;
}
