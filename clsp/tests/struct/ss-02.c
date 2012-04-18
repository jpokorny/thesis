struct s_02 {
    int n1;
};

struct ss_02 {
    struct s_02 s1;
};


static struct s_02 return_struct(struct ss_02 arg)
{
    return arg.s1;
}

