struct s_01 {
    int n1;
};

struct ss_01 {
    struct s_01 s1;
};


static struct ss_01 return_sstruct(struct ss_01 arg)
{
    return arg;
}
