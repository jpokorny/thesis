
static int i;
static int aux;
static int *ptr = &i;
static int **ptrptr = &ptr;
static struct
{
  int first;
  int second;
} s;
static void
sym_to_sym (int **arg)
{
  ptr = &aux;
}

static void
arg_to_sym (int *arg, int arg_aux)
{
  i = arg_aux;
}

static void
reg_to_sym (int *arg)
{
  i = aux + 1;
}

static void
val_to_sym (int *arg)
{
  i = 42;
}

static void
sym_to_reg (int **arg)
{
  *ptrptr = &aux;
}

static void
arg_to_reg (int *arg, int arg_aux)
{
  *ptr = arg_aux;
}

static void
reg_to_reg (int *arg)
{
  *ptr = aux + 1;
}

static void
val_to_reg (int *arg)
{
  *ptr = 42;
}

static void
sym_to_arg (int **arg)
{
  *arg = &aux;
}

static void
arg_to_arg (int *arg, int arg_aux)
{
  *arg = arg_aux;
}

static void
reg_to_arg (int *arg)
{
  *arg = aux + 1;
}

static void
val_to_arg (int *arg)
{
  *arg = 42;
}

static void
sym_to_val (int **arg)
{
  *((int **) 0) = &aux;
}

static void
arg_to_val (int *arg, int arg_aux)
{
  *((int *) 0) = arg_aux;
}

static void
reg_to_val (int *arg)
{
  *((int *) 0) = aux + 1;
}

static void
val_to_val (int *arg)
{
  *((int *) 0) = 42;
}

/*
    clsp-options:   -d 1848

    to be added to FROM_SYM_TO:

    static void sym_offset_to_##name(int **arg) { *(&target+1) = &VAR_AUX; }

    generating something like "store.64 aux -> 8[ptr]" and via this:

    static void
    sym_offset_to_sym (int **arg)
    {
      *(&ptr + 1) = &aux;
    }

    but currently does not work (pretends the addition is not here)

    vim: ft=c:
 */
