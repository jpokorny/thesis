
static int i;
static void
br_1 (void)
{
  if (i)
    goto done;
  i++;
done:
  return;
}

static void
br_2 (void)
{
  while (42 > i)
    i++;
  return;
}
/*
    clsp-options:   -d 1848
    makeinv:        yes

    br_1 also example of named PSEUDO_REG:  load.32     %r1(i) <- 0[i]

    vim: ft=c:
 */
