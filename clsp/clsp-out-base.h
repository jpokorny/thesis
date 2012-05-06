/*
 * Copyright 2012 Jan Pokorny <xpokor04@stud.fit.vutbr.cz,
 *                             pokorny_jan@seznam.cz>
 *
 * This file is part of clsp/predator.
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
#ifndef CLSP_OUT_BASE_H_GUARD
#define CLSP_OUT_BASE_H_GUARD
/**
    Output stream and base mean of its use (for more see @c clsp-output-ext)

    User of this header might be required to directy or indirectly provide
    following function-like macros:
    - STREAM
    - STREAMCLRNORM
    - STREAMCLRHIGH
    - STREAMCLREND

    Defining @c C99STRICT affects @c PUT macro (see its description).
 */

#include <stdbool.h>
#include <stdio.h>        /* snprintf, FILE* */
#include <limits.h>       /* INT_{MIN,MAX} */

#include "clsp-macros.h"  /* APPLY */
#include "clsp-color.h"   /* struct palette */


/** Special values for file descriptors */
enum fd_extra {
    fd_undef            = -1,       /**< Undefined descriptor */
    fd_deferred_unspec  = INT_MIN,  /**< Unspecified deferred descriptor */
    fd_deferred_restore = INT_MAX   /**< Used to restore from deferred */
};


/* Xlist for output streams */
#define STREAM_OUT      out
#define STREAM_ERR      err
#define STREAM_WARN     warn
#define STREAM_DEBUG    debug
#define STREAM_SP       sp
#define STREAM_CL       cl
#define STREAM_CL_DEBUG cl_debug
#define STREAMLIST(x)         \
    APPLY(x, STREAM_OUT     ) \
    APPLY(x, STREAM_ERR     ) \
    APPLY(x, STREAM_WARN    ) \
    APPLY(x, STREAM_DEBUG   ) \
    APPLY(x, STREAM_SP      ) \
    APPLY(x, STREAM_CL      ) \
    APPLY(x, STREAM_CL_DEBUG)

/** Enumeration of output streams */
enum outstreams {
#define X(name) outstream_##name,
    STREAMLIST(X)
#undef X
    outstream_cnt,

    outstream_first = 0,
    outstream_last = outstream_cnt - 1,

    outstream_first_base = outstream_first,
    outstream_last_base = outstream_err,

    outstream_first_custom = outstream_last_base + 1,
    outstream_last_custom = outstream_last
};

/** Stringified list of output streams */
extern const char *const outstream_str[outstream_cnt];


/**
    Output stream + state
 */
struct outstream {
    FILE            *stream;
    const char      *clr_norm;
    const char      *clr_high;
    const char      *clr_end;
    int             deferred_dest;
    struct palette  palette;        /**< for restoring in deferred case */
    bool            isatty;
};

/** Summary representation of output streams */
typedef struct outstream outstreams[outstream_cnt];


/**
    Output stream properties as exposed via command-line
 */
struct outstream_props {
    int             fd;
    struct palette  palette;
};

/* "constructor" */
#define OUTSTREAM_PROPS(fd_, palette_) \
    (struct outstream_props) { .fd = fd_, .palette = palette_ }


/**
    Advanced wrapper for fdopen, safely short-circuiting for stdout and stderr

    @param[in,out] outstreams  Output streams array
    @param[in]     which       Selects the right stream in @c outstreams
    @param[in]     props       Aggregation of FD to fdopen'd and palette.
    @return   FILE pointer for target stream (never NULL, dies instead)

    @note When @c props.fd is @c fd_deferred_restore, restore fd and palette
          from the saved state (thus @c props.palette is ignored in this case).
 */
FILE *stream_setup(outstreams outstreams, enum outstreams which,
                   struct outstream_props props);

/**
    Output deferred stream (auto-closed) to predefined target file (returned)

    @param[in,out] outstreams  Output streams array
    @param[in]     which       Selects the right stream in @c outstreams
    @return   FILE pointer for just-written stream (never NULL, dies instead)

    @note The actual parameters driving this function are stream-specific.
    @note Returned file is to be closed by the caller (whether deferred stream
          was used or not).
 */
FILE *stream_output_deferred(outstreams outstreams, enum outstreams which);


/** print to output stream ************************************************/


/*
    Universal print macro with implicit newline (and ability for highlighting)

    usage: PUT(stream_spec, fmt [...])

    Highlighting is enabled only for "fmt", use macro @c HIGHLIGHT, e.g.:

        PUT(out, "This is" HIGHLIGHT("important") "otherwise "_1(s), what)

    This also demonstrates the need for using specially formatted argument
    specifications: first one is decorated with @c _1 function-like macro,
                    second with @c _2, etc.

    Please note that the highlighting only works if @c C99STRICT is *not*
    defined (on the other hand, this will supress compiler warning and
    allow proper work with non-POSIX standard C library, where printf
    function family does not understand "%n$" notation).

    note: fmt has to be compile-time constant
          (use PUT(out, _1(s), mystring) for variable string)
 */

#define PUT__(which, skip, fmt, ...) \
    fprintf(STREAM(which), &(fmt)[skip], __VA_ARGS__)

#ifndef C99STRICT

# define PUTx(which, endref, fmt, ...)                             \
    PUT__(which, 0, "%1$s%2$s" fmt endref "\n",                    \
          STREAMCLRHIGH(which), STREAMCLRNORM(which), __VA_ARGS__)

/* (public) */
# define HIGHLIGHT(what) \
    "%1$s" what "%2$s"
# define _1(fmt)   "%3$" #fmt
# define _2(fmt)   "%4$" #fmt
# define _3(fmt)   "%5$" #fmt
# define _4(fmt)   "%6$" #fmt
# define _5(fmt)   "%7$" #fmt
# define _6(fmt)   "%8$" #fmt
# define _7(fmt)   "%9$" #fmt
# define _8(fmt)  "%10$" #fmt
# define _9(fmt)  "%11$" #fmt
# define _10(fmt) "%12$" #fmt
# define _11(fmt) "%13$" #fmt
# define _12(fmt) "%14$" #fmt

# define INDENTFMT  "%2$*1$s"
# define __1(fmt)   "%4$*3$" #fmt

#else

# define PUTx(which, endref, fmt, ...)       \
    PUT__(which, 0, "%s" fmt endref "\n",    \
          STREAMCLRNORM(which), __VA_ARGS__)

/* (public) */
# define HIGHLIGHT(what) \
    what  /*"|:" what ":|"*/
# define _1(fmt)  "%" #fmt
# define _2(fmt)  "%" #fmt
# define _3(fmt)  "%" #fmt
# define _4(fmt)  "%" #fmt
# define _5(fmt)  "%" #fmt
# define _6(fmt)  "%" #fmt
# define _7(fmt)  "%" #fmt
# define _8(fmt)  "%" #fmt
# define _9(fmt)  "%" #fmt
# define _10(fmt) "%" #fmt
# define _11(fmt) "%" #fmt
# define _12(fmt) "%" #fmt

# define INDENTFMT  "%*s"
# define __1(fmt)   "%*" #fmt

#endif


#define PUT_(cnt, ...)     PUT##cnt(__VA_ARGS__)


/* (public) */
#define PUT(...)           APPLY(PUT_, VA_ARGS_CNT(__VA_ARGS__), __VA_ARGS__)
#define PUT1(which)        PUT2(which, "")
#define PUT2(which, what)  PUTx(which, _1(s),  what,        STREAMCLREND(which))
#define PUT3(which, ...)   PUTx(which, _2(s),  __VA_ARGS__, STREAMCLREND(which))
#define PUT4(which, ...)   PUTx(which, _3(s),  __VA_ARGS__, STREAMCLREND(which))
#define PUT5(which, ...)   PUTx(which, _4(s),  __VA_ARGS__, STREAMCLREND(which))
#define PUT6(which, ...)   PUTx(which, _5(s),  __VA_ARGS__, STREAMCLREND(which))
#define PUT7(which, ...)   PUTx(which, _6(s),  __VA_ARGS__, STREAMCLREND(which))
#define PUT8(which, ...)   PUTx(which, _7(s),  __VA_ARGS__, STREAMCLREND(which))
#define PUT9(which, ...)   PUTx(which, _8(s),  __VA_ARGS__, STREAMCLREND(which))
#define PUT10(which, ...)  PUTx(which, _9(s),  __VA_ARGS__, STREAMCLREND(which))
#define PUT11(which, ...)  PUTx(which, _10(s), __VA_ARGS__, STREAMCLREND(which))
#define PUT12(which, ...)  PUTx(which, _11(s), __VA_ARGS__, STREAMCLREND(which))
#define PUT13(which, ...)  PUTx(which, _12(s), __VA_ARGS__, STREAMCLREND(which))


/*
    Context managers performing nested statement(s) with colorized stream
 */

/*
    Normal colorizing as per another stream
    
    Arguments are ordered:

    s1: this is a stream used by the nested statement(s) and such
        output will be colorized as per highlight color of s2
    s2: provides normal color
 */
#define WITH_STREAM_NORM_AS(s1, s2)                               \
    for (int i_=2 * (STREAMSTRUCT(s1).isatty); 0==i_              \
         ? 1                                                      \
         : (1==i_                                                 \
            ? 0                                                   \
            : (2==i_                                              \
               ? (fputs(clr_codes[STREAMSTRUCT(s2).palette.norm], \
                        STREAM(s1)), 1)                           \
               : (fputs(clr_codes[STREAMSTRUCT(s2).palette.norm   \
                                  ? clr_terminate                 \
                                  : clr_none], STREAM(s1)), 0)    \
              )                                                   \
           )                                                      \
         ; i_++)

/*
    Highlight colorizing as per another stream
    
    Arguments are ordered:

    s1: this is a stream used by the nested statement(s) and such
        output will be colorized as per highlight color of s2
    s2: provides highlight color
 */
#define WITH_STREAM_HIGH_AS(s1, s2)                               \
    for (int i_=2 * (STREAMSTRUCT(s1).isatty); 0==i_              \
         ? 1                                                      \
         : (1==i_                                                 \
            ? 0                                                   \
            : (2==i_                                              \
               ? (fputs(clr_codes[STREAMSTRUCT(s2).palette.high], \
                        STREAM(s1)), 1)                           \
               : (fputs(clr_codes[STREAMSTRUCT(s2).palette.high   \
                                  ? clr_terminate                 \
                                  : clr_none], STREAM(s1)), 0)    \
              )                                                   \
           )                                                      \
         ; i_++)


/* Print indented (fmt + at least one arg only) */

#define INDENT_MULT    4  /* should be one of 1,2,4,8 */
#define INDENT(level)  INDENT_MULT*(level), ""

/* NOTE: format specifiers start with _3(), but only with this */
#define PUTI(which, level, fmt, ...) \
    PUT(which, __1(s) fmt, INDENT(level), __VA_ARGS__)

/* with normal color as per s2 */
#define PUTNI(which, s2, level, fmt, ...)                               \
    WITH_STREAM_NORM_AS(which, s2)                                      \
        PUT__(which, 0, INDENTFMT fmt "\n", INDENT(level), __VA_ARGS__)

/* with highlight color as per s2 */
#define PUTHI(which, s2, level, fmt, ...)                               \
    WITH_STREAM_HIGH_AS(which, s2)                                      \
        PUT__(which, 0, INDENTFMT fmt "\n", INDENT(level), __VA_ARGS__)

/* similarly, but no indentation */
#define PUTN(which, s2, fmt, ...)  PUTNI(which, s2, 0, fmt, __VA_ARGS__)
#define PUTH(which, s2, fmt, ...)  PUTHI(which, s2, 0, fmt, __VA_ARGS__)

#endif
