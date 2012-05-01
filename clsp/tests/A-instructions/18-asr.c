
static int
binop_int (int a, int b)
{
  return (a) >> (b);
}

static int
binop_int_int_uint (int a, unsigned b)
{
  return (a) >> (b);
}

static int
binop_int_zero (int a)
{
  return (a) >> (0);
}

static int
binop_int_one (int a)
{
  return (a) >> (1);
}
/*
    clsp-options:   -d 1848
    simdiff-limit:  1.0

    vim: ft=c:
 */
