
typedef struct
{
  char c;
  int i;
} struct_t;
static struct_t s;
static int
call_int (int a)
{
  return a;
}

static double
call_double (double a)
{
  return a;
}

static const char *
call_string (const char *a)
{
  return a;
}

static struct_t
call_struct (struct_t a)
{
  return a;
}

int
main (int argc, char *argv[])
{
  int ret_int = call_int (42);
  double ret_double = call_double (4.2);
  const char *ret_string = call_string ("foo");
  struct_t ret_struct = call_struct (((struct_t) { 'c', 1 }));
}

/*
    clsp-options:   -d 1848
    makeinv:        yes

    vim: ft=c:
 */
