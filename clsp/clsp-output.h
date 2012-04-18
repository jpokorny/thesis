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
#ifndef CLSP_OUTPUT_H_GUARD
#define CLSP_OUTPUT_H_GUARD
/**
    User of this header might be required to directy or indirectly provide
    following function-like macros:
    - STREAM
    - STREAMSTRUCT
    - STREAMCLRNORM
    - STREAMCLRHIGH
    - STREAMCLREND
    - swap_stream

    Defining @c C99STRICT affects @c PUT macro (see its description).
 */

#include "clsp-macros.h"  /* APPLY */


/*
    Universal print macro with implicit newline (and ability for highlighting)

    usage: PUT(stream_spec, fmt [...])

    Highlighting is enabled only for "fmt", use macro @c HIGHLIGHT, e.g.:

        PUT(out, "This is" HIGHLIGHT("important") "otherwise "_1(s), what)

    This also demonstrates the need for using specially formatted argument
    specifications: first one is decorated with @c _1 macro, second with
                    @c _2, etc.

    Please note that the highlighting only works if @c C99STRICT is *not*
    defined (on the other hand, this will supress compiler warning and
    allow proper work with non-POSIX'ed standard C library, where
    printf function family does not understand "%n$" notation).

    NOTE: fmt has to be compile-time constant
          (use PUT(out, _1(s), mystring) for variable string)
 */


#define PUT__(which, skip, fmt, ...) \
    fprintf(STREAM(which), &(fmt "\n")[skip], __VA_ARGS__)

#ifndef C99STRICT

# define _1(fmt)   "%3$" #fmt
# define _2(fmt)   "%4$" #fmt
# define _3(fmt)   "%5$" #fmt
# define _4(fmt)   "%6$" #fmt
# define _5(fmt)   "%7$" #fmt
# define _6(fmt)   "%8$" #fmt
# define _7(fmt)   "%9$" #fmt
# define _8(fmt)  "%10$" #fmt

# define HIGHLIGHT(what)   "%1$s" what "%2$s"

# define PUTx(which, endref, fmt, ...)                             \
    PUT__(which, 0, "%1$s%2$s" fmt endref,                         \
          STREAMCLRHIGH(which), STREAMCLRNORM(which), __VA_ARGS__)

#else

# define _1(fmt)  "%" #fmt
# define _2(fmt)  "%" #fmt
# define _3(fmt)  "%" #fmt
# define _4(fmt)  "%" #fmt
# define _5(fmt)  "%" #fmt
# define _6(fmt)  "%" #fmt
# define _7(fmt)  "%" #fmt
# define _8(fmt)  "%" #fmt

# define HIGHLIGHT(what)  what
/*# define HIGHLIGHT(what)  "|:" what ":|"*/

# define PUTx(which, endref, fmt, ...)       \
    PUT__(which, 0, "%s" fmt endref,         \
          STREAMCLRNORM(which), __VA_ARGS__)

#endif

#define PUT_(cnt, ...)     PUT##cnt(__VA_ARGS__)

/* (public) */
#define PUT(...)           APPLY(PUT_, VA_ARGS_CNT(__VA_ARGS__), __VA_ARGS__)
#define PUT1(which)        PUT2(which, "")
#define PUT2(which, what)  PUTx(which, _1(s), what,        STREAMCLREND(which))
#define PUT3(which, ...)   PUTx(which, _2(s), __VA_ARGS__, STREAMCLREND(which))
#define PUT4(which, ...)   PUTx(which, _3(s), __VA_ARGS__, STREAMCLREND(which))
#define PUT5(which, ...)   PUTx(which, _4(s), __VA_ARGS__, STREAMCLREND(which))
#define PUT6(which, ...)   PUTx(which, _5(s), __VA_ARGS__, STREAMCLREND(which))
#define PUT7(which, ...)   PUTx(which, _6(s), __VA_ARGS__, STREAMCLREND(which))
#define PUT8(which, ...)   PUTx(which, _7(s), __VA_ARGS__, STREAMCLREND(which))
#define PUT9(which, ...)   PUTx(which, _8(s), __VA_ARGS__, STREAMCLREND(which))


/*
    Internal location printing
 */


#define LOCFMT \
    __FILE__ ":" TOSTRING(__LINE__) ": note: from %s [internal location]"

#define LOCFMT_1 \
    __FILE__ ":" TOSTRING(__LINE__) ": note: from "_1(s)" [internal location]"


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
#define WITH_SWAPPED_STREAM_HIGH_AS(s1, s2, s3)                \
    WITH_SWAPPED_STREAM(s1, s2)                                \
        for (int i_=2 * (STREAMSTRUCT(s1).isatty); 0==i_       \
             ? 1                                               \
             : (1==i_                                          \
                ? 0                                            \
                : (2==i_                                       \
                   ? (fputs(STREAMCLRHIGH(s3), STREAM(s2)), 1) \
                   : (fputs(STREAMCLREND(s3), STREAM(s2)), 0)  \
                  )                                            \
               )                                               \
             ; i_++)


/*
    Miscellaneous
 */

#define GET_YN(b)  (b) ? 'Y' : 'N'


#endif
