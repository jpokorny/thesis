/*
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
#ifndef CLSP_OUT_EXT_H_GUARD
#define CLSP_OUT_EXT_H_GUARD
/**
    Extending basic output to stream from @c clsp-output module

    User of this header might be required to directy or indirectly provide
    following function-like macros:
    - STREAM
    - STREAMSTRUCT
    - STREAMCLRNORM
    - STREAMCLRHIGH
    - STREAMCLREND
 */

#include "clsp.h"
#if !defined(STREAM)         \
 || !defined(STREAMSTRUCT)   \
 || !defined(STREAMCLRNORM)  \
 || !defined(STREAMCLRHIGH)  \
 || !defined(STREAMCLREND)
char error_ = "missing some macros that should be defined already"[-1];
#endif


#include <stdlib.h>           /* exit */
#include <stdio.h>            /* FILE* */
#include <stdbool.h>
#include <string.h>           /* strerror */
#include <errno.h>
#include <unistd.h>           /* dup, close, ... */

#include "clsp-macros.h"      /* APPLY */
#include "clsp-out-base.h"
#include "clsp-ret.h"


/*
    Print format helpers
 */

#define LOCFMT \
    __FILE__ ":" STRINGIFY(__LINE__) ": note: from %s [internal location]\n"

#define LOCFMT_1 \
    __FILE__ ":" STRINGIFY(__LINE__) ": note: from "_1(s)" [internal location]"

#define GET_YN(b)  (b) ? 'Y' : 'N'

/* warning to "warn" stream */
#define WARN(...)                                                        \
    do {                                                                 \
        PUT(warn, "\t" HIGHLIGHT("warning") ": internal: " __VA_ARGS__); \
        PUT(warn, LOCFMT_1, __func__);                                   \
    } while (0)


/* for iterative construction of a string in the buffer */
#define ITER_SNPRINTF(ptr, buf, ...) \
    (ptr += snprintf(ptr, sizeof(buf)+buf-ptr, __VA_ARGS__))


/** debug print-outs ******************************************************/


#define DVALUE(arg)    (1 << arg)

#define D_OPTIONS        opts, "dump gathered options"
#define D_STREAM         strm, "print messages regarding streams"
#define D_PLUGIN         plug, "print diagnostics regarding plugins"
#define D_FILE           file, "print current file being proceeded"
#define D_FUNCTION       func, "print current function being proceeded"
#define D_BASICBLOCK     bblk, "print event of opening new basic block"
#define D_SYMBOL         symb, "print current symbol being considered"
#define D_TYPE           type, "print type being processed"
#define D_INSTRUCTION    insn, "print instruction being proceeded"
#define D_OPERAND        oper, "print instruction operands details"
#define D_INITIALIZATOR  init, "print initializator being proceeded"
#define D_CACHE_HIT      chit, "print event of cache hit"
#define D_INSERT_TYPE    tins, "print type being inserted into type DB"
#define D_SP_ALLOC       allo, "print final allocators state"
#define D_MISC           misc, "random print-outs, work in progress"
/*keep this last*/
#define D_NONDETERM      ndtm, "allow extra, yet nondeterministic info"
#define DLIST(x)              \
    APPLY(x, D_OPTIONS      ) \
    APPLY(x, D_STREAM       ) \
    APPLY(x, D_PLUGIN       ) \
    APPLY(x, D_FILE         ) \
    APPLY(x, D_FUNCTION     ) \
    APPLY(x, D_BASICBLOCK   ) \
    APPLY(x, D_SYMBOL       ) \
    APPLY(x, D_TYPE         ) \
    APPLY(x, D_INSTRUCTION  ) \
    APPLY(x, D_OPERAND      ) \
    APPLY(x, D_INITIALIZATOR) \
    APPLY(x, D_CACHE_HIT    ) \
    APPLY(x, D_INSERT_TYPE  ) \
    APPLY(x, D_SP_ALLOC     ) \
    APPLY(x, D_MISC         ) \
    APPLY(x, D_NONDETERM    )


enum debugs {
#define X(name,desc)  debug_##name,
    DLIST(X)
#undef X
    debug_cnt,
    debug_last  = debug_cnt - 1,
    debug_first = 0,
};

enum debugs_use {
#define X(name,desc)  d_##name = DVALUE(debug_##name),
    DLIST(X)
#undef X
    d_none  = 0
};

extern const char *const debug_str[debug_cnt];

/* two forms of usage... */
#define DLOG(level, ...)          \
    (GLOBALS(debug) & (level))    \
        ? PUT(debug, __VA_ARGS__) \
        : 0

#define WITH_DEBUG_LEVEL(level)                        \
    for (int i_=0; 0==i_ && (GLOBALS(debug) & (level)) \
         ? 1                                           \
         : 0                                           \
         ; i_++)


/** stream manipulations/decoration ***************************************/


static inline void
swap_stream(FILE *f1, FILE *f2) {
    int fd1, fd2;

    if (f1 == f2 || (fd1 = fileno(f1)) == (fd2 = fileno(f2)))
        return;

#ifndef NLOG
    DLOG(d_strm,
         "\t" HIGHLIGHT("stream") ": swap: " _1(p)" ("_2(d)") - "_3(p)" ("
         _4(d)")",
         (void *)f1, fd1, (void*)f2, fd2);
#endif

    /* this is always needed due to (at the very least) using colors (?) */
    fflush(f1); fflush(f2);

    int temp = dup(fd1);

    if (-1 == temp
     || -1 == close(fd1)
     || -1 == dup2(fd2, fd1)
     || -1 == dup2(temp, fd2)
     || -1 == close(temp)
    )
        DIE( ERRNO("swapping stream (%d, %d)", fd1, fd2) );
}


/*
    Context managers performing nested statement(s) with swapped streams
 */

/*
    Simple swap
    
    Arguments are unordered but the convention (important with others) is:

    s1: fd of this stream will be the real target when the s2 stream is used
    s2: this is a stream used by the nested statement(s)
 */
#define WITH_SWAPPED_STREAM(s1, s2)                 \
    for (int i_=0; 0==i_                            \
         ? (swap_stream(STREAM(s1), STREAM(s2)), 1) \
         : (swap_stream(STREAM(s2), STREAM(s1)), 0) \
         ; i_++)

/*
    Swap with normal colorizing as per the stream whose fd is a real target
    
    Arguments are ordered:

    s1: fd of this stream will be the real target when the s2 stream is used
        and the output will be colorized as per normal color of s1
    s2: this is a stream used by the nested statement(s)
 */
#define WITH_SWAPPED_STREAM_NORM(s1, s2)                 \
    WITH_SWAPPED_STREAM(s1, s2)                          \
        for (int i_=0; 0==i_                             \
             ? (fputs(STREAMCLRNORM(s1), STREAM(s2)), 1) \
             : (fputs(STREAMCLREND(s1), STREAM(s2)), 0)  \
             ; i_++)

/*
    Swap with highlight colorizing as per the stream whose fd is a real target
    
    Arguments are ordered:

    s1: fd of this stream will be the real target when the s2 stream is used
        and the output will be colorized as per highlight color of s1
    s2: this is a stream used by the nested statement(s)
 */
#define WITH_SWAPPED_STREAM_HIGH(s1, s2)                 \
    WITH_SWAPPED_STREAM(s1, s2)                          \
        for (int i_=0; 0==i_                             \
             ? (fputs(STREAMCLRHIGH(s1), STREAM(s2)), 1) \
             : (fputs(STREAMCLREND(s1), STREAM(s2)), 0)  \
             ; i_++)

/*
    Swap with highlight colorizing as per the third stream (if available)
    
    Arguments are ordered:

    s1: fd of this stream will be the real target when the s2 stream is used
        and provided that it accepts colors (isatty), the output will be
        colorized as per highlight color of s3
    s2: this is a stream used by the nested statement(s)
    s3: its highlight color is used
 */
#define WITH_SWAPPED_STREAM_HIGH_AS(s1, s2, s3)                       \
    WITH_SWAPPED_STREAM(s1, s2)                                       \
        for (int i_=2 * (STREAMSTRUCT(s1).isatty); 0==i_              \
             ? 1                                                      \
             : (1==i_                                                 \
                ? 0                                                   \
                : (2==i_                                              \
                   ? (fputs(clr_codes[STREAMSTRUCT(s3).palette.high], \
                            STREAM(s2)), 1)                           \
                   : (fputs(clr_codes[STREAMSTRUCT(s3).palette.high   \
                                      ? clr_terminate                 \
                                      : clr_none], STREAM(s2)), 0)    \
                  )                                                   \
               )                                                      \
             ; i_++)


#endif
