static int bo_01(int a, int b, int c)
{
    int d, e, f, g;
    d = a & b;
    e = a | b;
    f = d ^ c;
    g = e ^ (~c);
    return f | g;
}
