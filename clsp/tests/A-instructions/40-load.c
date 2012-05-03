
static int i;
static int j[] = { 0, 1 };

static int
load_from_sym (int arg)
{
  return i;
}

static int
load_from_arg (int *arg)
{
  return arg[1];
}

static int
load_from_reg (int arg)
{
  return j[arg];
}
/*
    clsp-options:   -d 1848
    vim: ft=c:
 */
