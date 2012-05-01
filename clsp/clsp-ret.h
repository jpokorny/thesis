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
    ret_escape   = -1,  /**< Denotes soon exit (context-local or global) */
    ret_negative =  0,  /**< Denotes failure or disagreement */
    ret_positive =  1,  /**< Denotes success or agreement */
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
#define ECLIST(x)              \
    APPLY(x, EC_OK           ) \
    APPLY(x, EC_SPARSE_FATAL ) \
    APPLY(x, EC_SPARSE_CODE  ) \
    APPLY(x, EC_GENERAL      ) \
    APPLY(x, EC_OPT          ) \
    APPLY(x, EC_MEM          ) \
    APPLY(x, EC_TDB          ) \
    APPLY(x, EC_CL           )

enum {
#define X(num,name,desc)  ec_##name = num,
    ECLIST(X)
#undef X
    ec_cnt,
    ec_last = ec_cnt - 1,
    /* compliance note: only POSIX guarantees 0 == EXIT_SUCCESS */
    ec_first = 1/!(EXIT_SUCCESS) - 1
};


/** die with explaining farewell ******************************************/


/* for "hack" with prepending a control letter and skipping it afterwards */
#define DCHR_ERRNO      "@"
#define DCHR_ECODE      "$"
#define DCHR_ERRNOCODE  "#"

#define ECODE_(flag, c, ...)  flag STRINGIFY(ECNUM(c)) __VA_ARGS__

/* these are the wrappers for exitus emergence (see below) */
#define ERRNO(...)      DCHR_ERRNO __VA_ARGS__
/* NOTE: code (exit ~) can be [0,10), i.e., containing single digit */
#define ECODE(...)      ECODE_(DCHR_ECODE, __VA_ARGS__)
#define ERRNOCODE(...)  ECODE_(DCHR_ERRNOCODE, __VA_ARGS__)

/*
    Pre-mortem print macro for forced exit

    usage: DIE( WRAPPER(wrapper_args, [fmt, [...]]) )
           where WRAPPER is either nothing or one of macros defined above

    NOTE:  fmt should not start with any of DCHR_* characters (internal only)
    NOTE:  no fancy highlighting here + common printf format specifications
 */
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
    run-time assets
 */

extern const char *const ec_str[ec_cnt];


#endif
