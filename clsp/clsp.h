/*
 * Copyright (C) 2009 Kamil Dudka <kdudka@redhat.com>
 * Copyright (C) 2012 Jan Pokorny <pokorny_jan@seznam.cz>
 *
 * This file is part of predator.
 *
 * predator is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * predator is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with predator.  If not, see <http://www.gnu.org/licenses/>.
 */
#ifndef CLSP_H_GUARD
#define CLSP_H_GUARD

/*
    Common base to be included to the modules, but always as a first import!
 */

#define _POSIX_C_SOURCE 200809L  /* fileno, open_memstream */

#include <stdio.h>   /* FILE, fprintf, fileno */
#include <stdlib.h>  /* malloc, exit */
#include <string.h>  /* strerror */
#include <errno.h>   /* errno */
#include <unistd.h>  /* dup, close */

#define USE_INT3_AS_BRK
#include "trap.h"

#include "clsp-macros.h"
#include "clsp-output.h"
#include "clsp-enum-color.h"
#include "clsp-enum-ec.h"
#include "clsp-use-cl.h"
#include "clsp-use-sparse.h"
#include "type_enumerator.h"

extern const char *GIT_SHA1;


/** object representing globals ******************************************/


/**
    Enumeration of used streams.
 */
enum streams {
    stream_first,
    stream_out = stream_first,
    stream_err,
    stream_warn,
    stream_debug,
    /* worker only */
    stream_worker_first,
    stream_sp = stream_worker_first,
    stream_cl,
    stream_cl_debug,
    stream_worker_last,
    stream_last = stream_worker_last
};

struct outstream {
    FILE           *stream;
    const char     *clr_norm;
    const char     *clr_high;
    const char     *clr_end;
    int            deferred_dest;
    bool           isatty;
};

extern struct globals {
    struct outstream  stream[stream_last];
    int               debug;
    /* TODO typedb */
    struct cl_code_listener  *cl;

    /* API functions resolved in compile-/run-time set here */
    struct g_cl_api {
#define CL_DECL(prefix, item, cnt)                  \
    APPLY(API_CL_RET, API_PROPS(prefix, item))      \
    (*item)                                         \
    APPLY(API_CL_ARGDECL, API_PROPS(prefix, item));
        API_PROCEED(CL, CL_DECL)
#undef CL_DECL
    } cl_api;

    /* handles to dynamically opened libraries */
    struct g_cl_libs {
        size_t        cnt;
        void          **handles;  // !HAS_CL -> first is the main one
    } cl_libs;

    /* unexposed, but run-modifying options (e.g., for testing) */
    struct {
        bool          register_atexit;
    } unexposed;

} globals;

/* convenient macros for accessing parts of globals */
#define GLOBALS(what)  (globals.what)
#ifndef STREAM  /* due to test_swapfd.c */
# define STREAMSTRUCT(which)   GLOBALS(stream)[stream_##which]
# define STREAM(which)         GLOBALS(stream)[stream_##which].stream
# define STREAMCLRNORM(which)  GLOBALS(stream)[stream_##which].clr_norm
# define STREAMCLRHIGH(which)  GLOBALS(stream)[stream_##which].clr_high
# define STREAMCLREND(which)   GLOBALS(stream)[stream_##which].clr_end
#endif



//
// pointer and array DB, for building pointer* and array hierarchy in order to
// prevent having two semantically same pointers/arrays as two different types
//

struct arr_db_item {
    int             arr_size;
    struct cl_type  *clt;
};

struct ptr_db_item {
    struct cl_type      *clt;
    struct ptr_db_item  *next;
    size_t              arr_cnt;
    struct arr_db_item  **arr;
    bool                free_type;  ///< whether we are responsible for clt
};

struct ptr_db_arr {
    size_t              alloc_size;
    size_t              last;
    struct ptr_db_item  *heads;
};


extern struct type_ptr_db {
    int                 last_base_type_uid;
    struct typen_data   *type_db;
    struct ptr_db_arr   ptr_db;
} type_ptr_db;



/*
    Pre-mortem print macro for forced exit.

    Usage: DIE( WRAPPER(wrapper_args, [fmt, [...]]) )
           where WRAPPER is either nothing or one of macros defined below

    Note:  fmt should not start with any of DCHR_* characters (internal only)
    Note:  no fancy highlighting here + common printf format specifications
 */

#define DCHR_ERRNO      "@"
#define DCHR_ECODE      "$"
#define DCHR_ERRNOCODE  "#"

#define ERRNO(...)             DCHR_ERRNO __VA_ARGS__
/* Note: code (exit ~) can be [0,10), i.e., containing single digit */
#define ECODE_(flag,c,...)  flag TOSTRING(ECNUM(c)) __VA_ARGS__
#define ECODE(...)          ECODE_(DCHR_ECODE, __VA_ARGS__)
#define ERRNOCODE(...)      ECODE_(DCHR_ERRNOCODE, __VA_ARGS__)

#define DIE(...)         DIE_(__VA_ARGS__, "")
#define DIE_(fmt, ...)   DIE__(fmt "%s", __VA_ARGS__)
#define DIE__(fmt, ...)                                                        \
    ((*fmt != *DCHR_ERRNO && *fmt != *DCHR_ERRNOCODE)                          \
        ? PUT__(err, *fmt == *DCHR_ECODE ? 2 : 0, fmt "\n" LOCFMT,             \
             __VA_ARGS__, __func__)                                            \
        : (((fmt)[(*fmt == *DCHR_ERRNOCODE ? 2 : 1)])                          \
            ? PUT__(err, *fmt == *DCHR_ERRNOCODE ? 2 : 1, fmt": %s\n"LOCFMT,   \
                    __VA_ARGS__, strerror(errno), __func__)                    \
            : PUT__(err, *fmt == *DCHR_ERRNOCODE ? 2 : 1, fmt"die: %s\n"LOCFMT,\
                    __VA_ARGS__, strerror(errno), __func__))                   \
    , ((*fmt == *DCHR_ERRNOCODE || *fmt == *DCHR_ECODE)                        \
        ? exit((int) (fmt[1] - '0'))                                           \
        : exit(ec_general)))


/*
    Debugging
 */

#define DVALUE(arg)    (1 << arg)

#define D_OPTIONS      options,    "dump gathered options"
#define D_STREAM       stream,     "print messages regarding streams"
#define D_PLUGIN       plugin,     "print diagnostics regarding plugins"
#define D_FILE         file,       "print current file being proceeded"
#define D_FNC          fnc,        "print current function being proceeded"
#define D_SYMBOL       symbol,     "print current symbol being proceeded"
#define D_INSTRUCTION  instruction,"print instruction being processed"
#define D_TYPE         type,       "print type being processed"
#define D_INSERT_TYPE  insert_type,"print type being inserted into type DB"
#define D_INSN_OP      insn_op,    "print instruction operands details"
#define DLIST(x)            \
    APPLY(x, D_OPTIONS    ) \
    APPLY(x, D_STREAM     ) \
    APPLY(x, D_PLUGIN     ) \
    APPLY(x, D_FILE       ) \
    APPLY(x, D_FNC        ) \
    APPLY(x, D_SYMBOL     ) \
    APPLY(x, D_INSTRUCTION) \
    APPLY(x, D_TYPE       ) \
    APPLY(x, D_INSERT_TYPE) \
    APPLY(x, D_INSN_OP    )

enum {
#define X(name,desc)  d_##name,
    DLIST(X)
#undef X
    d_last,
    d_first = 0
};

/* two forms of usage... */
#define DLOG(level, ...)             \
    (GLOBALS(debug) & DVALUE(level)) \
        ? PUT(debug, __VA_ARGS__)    \
        : 0                          \

#define WITH_DEBUG_LEVEL(level)                              \
    for (int i_=0; 0==i_ && (GLOBALS(debug) & DVALUE(level)) \
         ? 1                                                 \
         : 0                                                 \
         ; i_++)


/*
    Basic predefine output functions
 */


/* Warning to error stream */
#define WARN(...)                                                         \
    do {                                                                  \
        PUT(warn, "\t" HIGHLIGHT("warning") ": internal: " __VA_ARGS__); \
        PUT(warn, LOCFMT_1, __func__);                                    \
    } while (0)


/*
    Stream mangling
 */

static inline void
swap_stream(FILE *f1, FILE *f2) {
    int fd1, fd2;

    if (f1 == f2 || (fd1 = fileno(f1)) == (fd2 = fileno(f2)))
        return;

    DLOG(d_stream, "swapping: "_1(p)" ("_2(d)" - "_3(p)" ("_4(d)")",
                   (void *)f1, fd1, (void*)f2, fd2);

    /* this is always needed due to (at the very least) using colors (?) */
    fflush(f1); fflush(f2);

    int temp = dup(fd1);

    if (-1 == temp
     || -1 == close(fd1)
     || -1 == dup2(fd2, fd1)
     || -1 == dup2(temp, fd2)
     || -1 == close(temp)
    )
        DIE( ERRNO("swap_stream") );
}


/** allocations ***********************************************************/


/*  NOTE: use "MEM_NEW(foo)" for direct access ("MEM_NEW(foo).bar = 42")
          and "(MEM_NEW(foo))" as a function parameter ("bar((MEM_NEW(foo)))")
 */
#define MEM_NEW(var)                        \
   (((var) = malloc(sizeof(*(var))))        \
     ? (void) 0  /* NOOP */                 \
     : DIE( ERRNOCODE(MEM, "MEM_NEW") )     \
   ) , (var)

#define MEM_ARR_RESIZE(arr, newcnt)                      \
    (((arr) = realloc((arr), sizeof(*(arr)) * (newcnt))) \
      ? (void) 0 /* NOOP */                              \
      : DIE( ERRNOCODE(MEM, "MEM_ARR_RESIZE") )          \
    ) , (arr)

/* NOTE: one-liner for item append: *(MEM_ARR_APPEN(arr,size)) = item */
#define MEM_ARR_APPEND(arr, oldcnt)                          \
    (((arr) = realloc((arr), sizeof(*(arr)) * (++(oldcnt)))) \
      ? (void) 0 /* NOOP */                                  \
      : DIE( ERRNOCODE(MEM, "MEM_ARR_APPEND") )              \
    ) , (arr + oldcnt - 1)



#endif
