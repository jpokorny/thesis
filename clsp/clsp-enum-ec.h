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
#include "clsp-macros.h"  /* APPLY, IDENTITY */


/** function return value level *******************************************/


/**
    Generalized common return values.
 */
enum retval {
    ret_fail     =  1,   /**< Denotes failure-based exit. */
    ret_continue =  0,  /**< Denotes continuing in the run. */
    ret_bye      = -1,  /**< Denotes immediate graceful exit. */
};


/** program exit value level **********************************************/


#define ECNUM(c)              APPLY(ECNUM_, EC_##c)
#define ECNUM_(num,name,desc) num
#define ECVALUE               IDENTITY

/* numbering as macro needed due to "DIE( ECODE() )" internals */
#define EC_OK           0,ok,          "run was successful (e.g., Code Listener fed)"
#define EC_SPARSE_FATAL 1,sparse_fatal,"sparse fatal error"
#define EC_SPARSE_CODE  2,sparse_code, "sparse detected error in the code"
#define EC_GENERAL      3,general,     "general failure"
#define EC_OPT          4,opt,         "incorrect command-line"
#define EC_MEM          5,mem,         "memory handling failed (probably OOM)"
#define EC_TDB          6,tdb,         "internal type database handling failed"
#define EC_CL           7,cl,          "Code Listener run has been aborted"
#define ECLIST(x)        \
    APPLY(x, EC_OK     ) \
    APPLY(x, EC_SPARSE_FATAL ) \
    APPLY(x, EC_SPARSE_CODE  ) \
    APPLY(x, EC_GENERAL      ) \
    APPLY(x, EC_OPT          ) \
    APPLY(x, EC_MEM          ) \
    APPLY(x, EC_TDB          ) \
    APPLY(x, EC_CL           )

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
