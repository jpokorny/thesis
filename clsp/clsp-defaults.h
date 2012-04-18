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
*/

#include "clsp-macros.h"  /* APPLY */

/** command-line options defaults (clsp_options) **************************/


/*
    file descriptor defaults
 */

#define DEF_FD_WARN       STDOUT_FILENO
#define DEF_FD_DEBUG      STDOUT_FILENO
#define DEF_FD_SPARSE     STDERR_FILENO
#define DEF_FD_CL         STDERR_FILENO
#define DEF_FD_CL_DEBUG   STDOUT_FILENO

#define DEF_FD_VAL(what)  DEF_FD_##what
#define DEF_FD_STR(what)  STRINGIFY(DEF_FD_##what)


/*
    palette defaults
 */

#define DEF_PLT_WARN       CLR_DARK     ,CLR_DARKGRAY
#define DEF_PLT_DEBUG      CLR_GREEN    ,CLR_BOLDGREEN
#define DEF_PLT_SP         CLR_PURPLE   ,CLR_BROWN
#define DEF_PLT_CL         CLR_RED      ,CLR_BOLDRED
#define DEF_PLT_CL_DEBUG   CLR_LIGHTGRAY,CLR_DARKGRAY

#define DEF_PLT_(nnorm, nhigh, ncode, hnorm, hhigh, hcode) \
    PALETTE(nnorm, hnorm)
#define DEF_PLT(what)      APPLY(DEF_PLT_, DEF_PLT_##what)
#define DEF_PLT_STR(what)  APPLY(PALETTE_PRETTY, DEF_PLT_##what)


#endif
