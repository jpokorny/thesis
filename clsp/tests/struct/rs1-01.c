struct rs_01 {
    int           n1;
    struct rs_01  *next;
};

static struct rs_01 return_next(struct rs_01 arg)
{
    return *arg.next;
}
