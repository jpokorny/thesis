// this used to trigger a problem with ptrcast instruction
// (type adjustment in `insn_assignment_mod_rhs')

static int *ptr01(char *a)
{
    return (int *) a;
}
