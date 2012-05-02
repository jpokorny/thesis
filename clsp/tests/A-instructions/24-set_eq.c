
static int
binop_int_zero (int a)
{
  return (a) == (0);
}

static int
binop_int_one (int a)
{
  return (a) == (1);
}

static _Bool
binop_bool_char_char (char a, char b)
{
  return (a) == (b);
}

static _Bool
binop_bool_int_int (int a, int b)
{
  return (a) == (b);
}

static int
binop_int_int_int (int a, int b)
{
  return (a) == (b);
}

static _Bool
binop_bool_uint_uint (unsigned a, unsigned b)
{
  return (a) == (b);
}

static _Bool
binop_bool_double_double (double a, double b)
{
  return (a) == (b);
}

static _Bool
binop_bool_ptr_ptr (void *a, void *b)
{
  return (a) == (b);
}
/*
    clsp-options:   -d 1848

    vim: ft=c:
 */
