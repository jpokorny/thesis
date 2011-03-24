#include <stdlib.h>
#include <stdio.h>

static long int factorial(long int arg)
{
    if (arg < 0)
        return -1;
    else if (arg == 0)
        return 1;
    else
        return arg * factorial(arg - 1);
}
static int factorial_from_string(char *str)
{
    if (!str) {
        printf("Error in argument");
        return 77;
    }
    long int num = factorial(atoi(str));
    if (num <= 0) {
        printf("Bad argument for factorial: %s\n", str);
        return 99;
    }
    printf("%s factorial is %li\n", str, num);
    return 0;
}
int main(int argc, char *argv[])
{
    int rc = 0;
    if (argc <= 1)
        printf("Usage: %s <num arg for factorial>\n", argv[0]);
    else
        rc = factorial_from_string(argv[1]);
    return rc;
}
