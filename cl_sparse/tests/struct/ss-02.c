struct s01 {
    int n1;
};

struct ss01 {
    struct s01 s1;
};


static struct ss01 f01(struct ss01 arg)
{
    return arg;
}
