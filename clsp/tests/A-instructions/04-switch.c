
static int i;
static void
switch_1 (void)
{
  switch (i)
    {
    case 0:
      {
	break;
      }
    case 1:
      {
	i++;
	break;
      }
    case 2:
      {
      }
    case 3:
      {
	break;
      }
    case 8 ... 10:
      {
	i++;
	break;
      }
    case 5 ... 6:
      {
	break;
      }
    default:
      {
	i++;
	break;
      }
    }
}

static void
switch_2 (void)
{
  switch (i)
    {
    case 12:
      {
	i++;
	break;
      }
    default:
      switch (i)
	{
	case 0:
	  {
	    break;
	  }
	case 1:
	  {
	    i++;
	    break;
	  }
	case 2:
	  {
	  }
	case 3:
	  {
	    break;
	  }
	case 8 ... 10:
	  {
	    i++;
	    break;
	  }
	case 5 ... 6:
	  {
	    break;
	  }
	default:
	  {
	    i++;
	    break;
	  }
	}
    }
}
/*
    clsp-options:   -d 1848
    simdiff-limit:  0.9950

    vim: ft=c:
 */
