struct s_03 {
    int n1;
};

struct ss_03 {
    struct s_03 s1;
};


static int return_int(struct ss_03 arg)
{
    return arg.s1.n1;
}
