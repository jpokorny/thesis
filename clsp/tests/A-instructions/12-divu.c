
static unsigned
binop_unsigned (unsigned a, unsigned b)
{
  return (a) / (b);
}

static int
binop_int_int_uint (int a, unsigned b)
{
  return (a) / (b);
}
/*
    clsp-options:   -d 1848
    simdiff-limit:  1.0

    vim: ft=c:
 */