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
#ifndef CLSP_ENUM_EC_H_GUARD
#define CLSP_ENUM_EC_H_GUARD

#include <stdlib.h>       /* EXIT_SUCCESS */
#include "clsp_macros.h"  /* APPLY, IDENTITY */

#define ECNUM(c)              APPLY(ECNUM_, EC_##c)
#define ECNUM_(num,name,desc) num
#define ECVALUE               IDENTITY

/* numbering as macro needed due to "DIE( ECODE() )" internals */
#define EC_OK       0,ok,     "run finished, no unrecoverable error encountered"
#define EC_SPARSE   1,sparse, "sparse has not finished successfully"
#define EC_GENERAL  2,general,"something general has failed"
#define EC_OPT      3,opt,    "incorrect command-line"
#define EC_MEM      4,mem,    "memory handling has failed (probably OOM)"
#define EC_TDB      5,tdb,    "internal type database handling has failed"
#define EC_CL       6,cl,     "Code Listener run has been aborted"
#define ECLIST(x)        \
    APPLY(x, EC_OK     ) \
    APPLY(x, EC_SPARSE ) \
    APPLY(x, EC_GENERAL) \
    APPLY(x, EC_OPT    ) \
    APPLY(x, EC_MEM    ) \
    APPLY(x, EC_TDB    ) \
    APPLY(x, EC_CL     )

enum {
    /* compliance note: only POSIX guarantees 0 == EXIT_SUCCESS */
    ec_first = EXIT_SUCCESS,
#define X(num,name,desc)  ec_##name = num,
    ECLIST(X)
#undef X
    ec_last
};

extern const char *ec_str[ec_last];

#endif
