#define DO_BINOP_SIMPLE(t) \
    static t binop_##t(t a, t b) { return BINOP(a, b); }

#define DO_BINOP(name, ret, x, y) \
    static ret binop_##name(x a, y b) { return BINOP(a, b); }

#define DO_BINOP_IMPLICIT(name, t, implicit) \
    static t binop_##name(t a) { return BINOP(a, implicit); }


#ifdef USE_BOOL
DO_BINOP_SIMPLE(bool)
#endif

#ifdef USE_CHAR
DO_BINOP_SIMPLE(char)
#endif

#ifdef USE_INT
DO_BINOP_SIMPLE(int)
#endif

#ifdef USE_UINT
DO_BINOP_SIMPLE(unsigned)
#endif

#ifdef USE_FLOAT
DO_BINOP_SIMPLE(float)
#endif

#ifdef USE_DOUBLE
DO_BINOP_SIMPLE(double)
#endif


#ifdef USE_INT_INT_UINT
DO_BINOP(int_int_uint, int, int, unsigned)
#endif

#ifdef USE_DOUBLE_DOUBLE_INT
DO_BINOP(double_double_int, double, double, int)
#endif

#ifdef USE_PTR_PTR_INT
DO_BINOP(ptr_ptr_int, void *, void *, int)
#endif

#ifdef USE_PTR_INT_PTR
DO_BINOP(ptr_int_ptr, void *, int, void *)
#endif

/*
    with 08-add.c.raw:
        warning: incorrect type in return expression (different base types)
            expected void *
            got long
*/
#ifdef USE_PTR_PTR_PTR
DO_BINOP(ptr_ptr_ptr, void *, void *, void *)
#endif

#ifdef USE_INT_ZERO
DO_BINOP_IMPLICIT(int_zero, int, 0)
#endif

#ifdef USE_INT_ONE
DO_BINOP_IMPLICIT(int_one, int, 1)
#endif
