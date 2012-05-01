
static int i;
static void
sel_1 (void)
{
  i = i ? 0 : i;
}
/*
    clsp-options:   -d 1848
    simdiff-limit:  1.0

    vim: ft=c:
 */
