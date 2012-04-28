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
#ifndef CLSP_DEFAULTS_H_GUARD
#define CLSP_DEFAULTS_H_GUARD
/**
    Centralization of various default values

    For "hard" defaults, see clsp-options.c: @c options_initialize
    and 
*/

#include <unistd.h>       /* STD*_FILENO */
#include "clsp-macros.h"  /* APPLY */


/** command-line options defaults (clsp_options) **************************/


/*
    output streams
 */

#define DEF_OUTSTREAM_WARN      warn    ,STDOUT_FILENO, CLR_DARKSOME ,CLR_DARKGRAY
#define DEF_OUTSTREAM_DEBUG     debug   ,STDOUT_FILENO, CLR_GREEN    ,CLR_BOLDGREEN
#define DEF_OUTSTREAM_SP        sp      ,STDERR_FILENO, CLR_PURPLE   ,CLR_BROWN
#define DEF_OUTSTREAM_CL        cl      ,STDERR_FILENO, CLR_RED      ,CLR_BOLDRED
#define DEF_OUTSTREAM_CL_DEBUG  cl_debug,STDOUT_FILENO, CLR_LIGHTGRAY,CLR_BLUE

#define DEF_OUTSTREAMLIST(x)         \
    APPLY(x, DEF_OUTSTREAM_WARN    ) \
    APPLY(x, DEF_OUTSTREAM_DEBUG   ) \
    APPLY(x, DEF_OUTSTREAM_SP      ) \
    APPLY(x, DEF_OUTSTREAM_CL      ) \
    APPLY(x, DEF_OUTSTREAM_CL_DEBUG)

#define DEF_OUTSTREAM_FD_(name, fd, nnorm, nhigh, ncode, hnorm, hhigh, hcode) \
    fd
#define DEF_OUTSTREAM_FD(which) \
    APPLY_INNER(DEF_OUTSTREAM_FD_, DEF_OUTSTREAM_##which)
#define DEF_OUTSTREAM_FD_STR(which)  STRINGIFY(DEF_OUTSTREAM_FD(which))

#define DEF_OUTSTREAM_PALETTE_(name, fd, nnorm, nhigh, ncode, hnorm, hhigh, hcode) \
    nnorm, nhigh, ncode, hnorm, hhigh, hcode
#define DEF_OUTSTREAM_PALETTE(which) \
    APPLY_INNER(DEF_OUTSTREAM_PALETTE_, DEF_OUTSTREAM_##which)

#define DEF_OUTSTREAM_PALETTE_STR(what) \
    APPLY(PALETTE_PRETTY, DEF_OUTSTREAM_PALETTE(what))


#endif
