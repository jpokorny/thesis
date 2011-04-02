struct rs_03 {
    struct rs_03  *head;
    struct rs_03  **tail;
};

static struct rs_03* head_to_tail(struct rs_03 *arg)
{
    arg->tail = &arg->head;
    return arg;
}
