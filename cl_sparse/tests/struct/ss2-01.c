struct s_01 {
    int n1;
};

struct ss2_01 {
    int num;
    struct s_01 s1;
};


static int return_int(struct ss2_01 arg)
{
    return arg.s1.n1;
}
