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
#ifndef CLSP_USE_CL_H_GUARD
#define CLSP_USE_CL_H_GUARD
/**
    Customize Code Listener API (subset of it) to our needs

    Ties closely with GLOBALS function-like macro as per @c clsp.h
    (master header file for this one).
 */

#include "clsp-api-cl.h"
#include "clsp-output.h"  /* _1(), ... */

/*
    Code Listener API
    NOTE: relying on availability of GLOBALS(cl/cl_api), see above
 */

/* global API of Code Listener:  API_CL(item, ...) */
#define API_CL(...)           API_USE(CL,__VA_ARGS__,API_CL_GLOBALS)
#define API_CL_GLOBALS(item)  GLOBALS(cl_api).item

/*
    per-listener part of Code Listener API:  EMIT(item, ...)
    NOTE: injecting "self" as a first argument skipped by outer call
    NOTE: works only until there is non-function or returning function
 */
#define API_EMIT(...)           API_EMIT_(__VA_ARGS__, API_EMIT_GLOBALS)
#define API_EMIT_(item, ...)    API_USE(CLOBJ, item, GLOBALS(cl), __VA_ARGS__)
#define API_EMIT_GLOBALS(item)  GLOBALS(cl)->item


/* helpers */

#define CLPOSFMT   "%s:%d:%d"
#define CLPOSFMT_1 _1(s)":"_2(d)":"_3(d)
#define CLPOS(p)   (p).file, (p).line, (p).column


#if (API_TEST > 0)
/*
    simple test
    run as: c99 -DAPI_TEST=1 -E clsp_api_cl.h
 */
# define API_CL(...)           API_USE(CL,__VA_ARGS__,API_CL_GLOBALS)
# define API_CL_GLOBALS(item)  GLOBALS(cl_api)->item
PRAGMA_MSGSTR("following should be: ret = GLOBALS(cl_api)->chain_create( );");
API_CL(chain_create, ret);

# define API_EMIT(...)       API_EMIT_(__VA_ARGS__,API_EMIT_GLOBALS)
# define API_EMIT_(item,...) API_USE(CLOBJ,item,GLOBALS(cl),__VA_ARGS__)
# define API_EMIT_GLOBALS(item)  GLOBALS(cl)->item
PRAGMA_MSGSTR("following should be: GLOBALS(cl)->file_open(GLOBALS(cl),"foo.c" );");
API_EMIT(file_open,"foo.c");

# define CL_DECL(prefix,item,cnt) \
    APPLY(API_CL_RET,API_PROPS(prefix,item)) (*item) APPLY(API_CL_ARGDECL,API_PROPS(prefix,item))
API_PROCEED(CL, CL_DECL);
#endif

#endif
