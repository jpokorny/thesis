struct rs_02 {
    struct rs_02  *head;
    struct rs_02  **tail;
};

static struct rs_02 head_to_tail(struct rs_02 arg)
{
    arg.tail = &arg.head;
    return arg;
}
