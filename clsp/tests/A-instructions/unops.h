#define DO_UNOP(name, t) \
    static t unop_##name(t a) { return UNOP(a); }

#ifdef USE_CHAR
DO_UNOP(char, char)
#endif

#ifdef USE_INT
DO_UNOP(int, int)
#endif

#ifdef USE_UINT
DO_UNOP(unsigned, unsigned)
#endif
