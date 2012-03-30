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
#ifndef CLSP_API_SPARSE_H_GUARD
#define CLSP_API_SPARSE_H_GUARD
/**
    Sparse API (subset of it) internal mirroring
*/

#include "clsp_apis.h"

#include "lib.h"        /* sparse_initialize, sparse, */
#include "expression.h" /* expand_symbol */
#include "linearize.h"  /* linearize_symbol, unssa, show_instruction */
#include "storage.h"    /* set_up_storage, free_storage  */
#include "symbol.h"     /* show_symbol, show_typename  */
#include "token.h"      /* stream_name, input_streams */
#include "ptrlist.h"    /* ptr_list_empty  */
#include "target.h"     /* bits_in_pointer, bits_in_char  */
#include "scope.h"      /* *_scope  */
#if !defined(ptr_list_empty)
# pragma message "Missing some macros that should be defined already"
#endif


#define API_SPARSE_NAME   sparse
#define API_SPARSE_FQ     IDENTITY
#if 1
#define API_SPARSE_KIND(type,                                               \
                        kind,                                               \
                        argcnt,                                             \
                        out /*E=uses stderr, D=uses stdout for debug msgs*/ \
                       )  kind##argcnt
#endif
#define API_SPARSE_sparse_initialize  A,R,3,E
#define API_SPARSE_sparse             A,R,1,E
#define API_SPARSE_expand_symbol      A,N,1,E
#define API_SPARSE_linearize_symbol   A,R,1,E
#define API_SPARSE_unssa              A,N,1,E
#define API_SPARSE_set_up_storage     A,N,1,E
#define API_SPARSE_free_storage       A,N,0,X
#define API_SPARSE_show_symbol        A,N,1,D
#define API_SPARSE_show_typename      A,N,1,X  /* lying about returning */
#define API_SPARSE_show_instruction   A,N,1,X  /* lying about returning */
#define API_SPARSE_stream_name        A,R,1,X
#define API_SPARSE_LAST               Z,_,_,_
#define API_SPARSE_LIST               \
    sparse_initialize,                \
    sparse,                           \
    expand_symbol,                    \
    linearize_symbol,                 \
    unssa,                            \
    set_up_storage,                   \
    free_storage,                     \
    show_symbol,                      \
    show_typename,                    \
    show_instruction

/* access other properties */
#define API_SPARSE_OUT(type,kind,argcnt,out)  out

#if (API_SHOW > 0)
API_PRAGMA_OVERVIEW(SPARSE)
# if (API_SHOW > 1)
API_PRAGMA_DETAILS(SPARSE)
# endif
#endif



#if (API_TEST > 0)
/*
    simple test
    run as: c99 -DAPI_TEST=1 -E clsp_api_sparse.h
 */
# define API_SPARSE(...)           API_SPARSE_MAP(__VA_ARGS__,IDENTITY)
# define API_SPARSE_(...)          API_USE(SPARSE,__VA_ARGS__)
# define API_SPARSE_MAP(item,...)  API_SPARSE_HOW(item)(item,__VA_ARGS__)
# define API_SPARSE_HOW(item)      API_SPARSE_HOW_(APPLY(API_SPARSE_OUT,API_PROPS(SPARSE,item)))
# define API_SPARSE_HOW_(out)      JOIN(API_SPARSE_USE_,out)
# define API_SPARSE_USE_X          API_SPARSE_
# define API_SPARSE_USE_D(fnc,...)    \
    WITH_SWAPPED_STREAM(out,debug)    \
        API_SPARSE_(fnc,__VA_ARGS__)
# define API_SPARSE_USE_E(fnc,...)    \
    WITH_SWAPPED_STREAM(sparse,err)   \
        API_SPARSE_(fnc,__VA_ARGS__)
PRAGMA_MSGSTR("following should be: free_storage( );")
API_SPARSE(free_storage);
PRAGMA_MSGSTR("following should be: WITH_SWAPPED_STREAM(sparse,err) ret = linearize_symbol(sym );")
API_SPARSE(linearize_symbol, ret, sym);
#endif


#endif
/* vim:set ts=4 sts=4 sw=4 et: */
