static int test_switch_01(int a, int b, int sel)
{
    int retval;

    switch (sel) {
        case 0:
            retval = a + b;
            break;
        case 1:
            retval = a - b;
            break;
        case 2:
            retval = a * b;
            break;
        case 3:
            retval = a / b;
            break;
        default:
            retval = 0;
            break;
    }

    return retval;
}
