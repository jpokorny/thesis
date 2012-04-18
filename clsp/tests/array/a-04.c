typedef int three_int_arrays[][3];

static int f01(int row, int column)
{
    int a[][3] = { {1,2,3},{4,5,6} };
    int b[][3] = { {4,5,6}, {7,8,9} };
    int row_b =  a[row][column];
    return b[row_b][column];
}
