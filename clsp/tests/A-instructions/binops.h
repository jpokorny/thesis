#define DO_BINOP(name, ret, x, y) \
    static ret binop_##name(x a, y b) { return BINOP(a, b); }

/* could be DO_BINOP wrapper, but protect "bool" verbatim */
#define DO_BINOP_SIMPLE(t) \
    static t binop_##t(t a, t b) { return BINOP(a, b); }

#define DO_BINOP_IMPLICIT(name, t, implicit) \
    static t binop_##name(t a) { return BINOP(a, implicit); }


#ifdef USE_BOOL
#include <stdbool.h>
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

#ifdef USE_PTRINT_PTRINT_INT
DO_BINOP(ptrint_ptrint_int, int *, int *, int)
#endif

#ifdef USE_LONGLONG_PTR_PTR
DO_BINOP(longlong_ptr_ptr, long long, void *, void *)
#endif

#ifdef USE_INT_ZERO
DO_BINOP_IMPLICIT(int_zero, int, 0)
#endif

#ifdef USE_INT_ONE
DO_BINOP_IMPLICIT(int_one, int, 1)
#endif

#ifdef USE_PTR_ONE
DO_BINOP_IMPLICIT(ptr_one, int *, 1)
#endif

#ifdef USE_INTPTR_ONE
DO_BINOP_IMPLICIT(intptr_one, int *, 1)
#endif


/* comparisons */

#ifdef USE_CMP_BOOL
#include <stdbool.h>
DO_BINOP(bool_bool_bool, bool, bool, bool)
#endif


#ifdef USE_CMP_CHAR
#include <stdbool.h>
DO_BINOP(bool_char_char, bool, char, char)
#endif

#ifdef USE_CMP_INT
#include <stdbool.h>
DO_BINOP(bool_int_int, bool, int, int)
DO_BINOP(int_int_int, int, int, int)
#endif

#ifdef USE_CMP_UINT
#include <stdbool.h>
DO_BINOP(bool_uint_uint, bool, unsigned, unsigned)
#endif

#ifdef USE_CMP_DOUBLE
#include <stdbool.h>
DO_BINOP(bool_double_double, bool, double, double)
#endif

#ifdef USE_CMP_PTR
#include <stdbool.h>
DO_BINOP(bool_ptr_ptr, bool, void *, void *)
#endif
