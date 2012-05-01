
static char
binop_char (char a, char b)
{
  return (a) + (b);
}

static int
binop_int (int a, int b)
{
  return (a) + (b);
}

static unsigned
binop_unsigned (unsigned a, unsigned b)
{
  return (a) + (b);
}

static float
binop_float (float a, float b)
{
  return (a) + (b);
}

static double
binop_double (double a, double b)
{
  return (a) + (b);
}

static int
binop_int_int_uint (int a, unsigned b)
{
  return (a) + (b);
}

static double
binop_double_double_int (double a, int b)
{
  return (a) + (b);
}

static void *
binop_ptr_ptr_int (void *a, int b)
{
  return (a) + (b);
}

static void *
binop_ptr_int_ptr (int a, void *b)
{
  return (a) + (b);
}

static int
binop_int_zero (int a)
{
  return (a) + (0);
}

static int
binop_int_one (int a)
{
  return (a) + (1);
}
/*
    clsp-options:   -d 1848
    simdiff-limit:  1.0

    vim: ft=c:
 */
