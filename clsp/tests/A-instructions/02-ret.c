
static int i;
static void
ret_void (void)
{
  return;;
}

static int
ret_int_constant (void)
{
  return 42;
}

static double
ret_double_constant (void)
{
  return 4.2;
}

static int
ret_expression (void)
{
  return i + 42;
}

static const char *
ret_string (void)
{
  return "foo";
}

static int
ret_arg (int a)
{
  return a;
}
/*
    clsp-options:   -d 1848
    vim: ft=c:
 */
