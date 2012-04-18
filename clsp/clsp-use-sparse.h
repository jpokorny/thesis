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
#ifndef CLSP_USE_SPARSE_H_GUARD
#define CLSP_USE_SPARSE_H_GUARD
/**
    Customize sparse API (subset of it) to our needs
 */

#include "clsp-api-sparse.h"
#include "clsp-output.h"      /* WITH_SWAPPED... */

/*
    sparse API:  SP(item, ...)
 */

#define SP(...)           SP_MAP(__VA_ARGS__, IDENTITY)
#define SP_MAP(item,...)  SP_HOW(item)(item, __VA_ARGS__)
#define SP_(...)          API_USE(SPARSE, __VA_ARGS__)
#define SP_HOW(item)  SP_HOW_(APPLY(API_SPARSE_OUT, API_PROPS(SPARSE, item)))
#define SP_HOW_(out)  JOIN(SP_USE_, out)

#define SP_USE_X(fnc, ...)  SP_(fnc, __VA_ARGS__)
#define SP_USE_D(fnc, ...)                      \
    WITH_SWAPPED_STREAM_HIGH_AS(debug, out, sp) \
        SP_(fnc, __VA_ARGS__)
#define SP_USE_E(fnc, ...)            \
    WITH_SWAPPED_STREAM_NORM(sp, err) \
        SP_(fnc, __VA_ARGS__)

/* helpers */

#define SPARSEPOSFMT    "%s:%d:%d"

#define SPARSEPOSFMT_1  _1(s)":"_2(d)":"_3(d)
#define SPARSEPOSFMT_2  _2(s)":"_3(d)":"_4(d)

#define SPARSEPOS(p)    SP(stream_name, (p).stream), (p).line, (p).pos
#define SPARSEPOS(p)    SP(stream_name, (p).stream), (p).line, (p).pos


#if (API_TEST > 0)
/*
    simple test
    run as: c99 -DAPI_TEST=1 -E clsp_api_sparse.h
 */
# define SP(...)           SP_MAP(__VA_ARGS__,IDENTITY)
# define SP_(...)          API_USE(SPARSE,__VA_ARGS__)
# define SP_MAP(item,...)  SP_HOW(item)(item,__VA_ARGS__)
# define SP_HOW(item)      SP_HOW_(APPLY(API_SPARSE_OUT,API_PROPS(SPARSE,item)))
# define SP_HOW_(out)      JOIN(SP_USE_,out)
# define SP_USE_X          SP_
# define SP_USE_D(fnc,...)         \
    WITH_SWAPPED_STREAM(debug,out) \
        SP_(fnc,__VA_ARGS__)
# define SP_USE_E(fnc,...)          \
    WITH_SWAPPED_STREAM(sparse,err) \
        SP_(fnc,__VA_ARGS__)
PRAGMA_MSGSTR("following should be: free_storage( );")
SP(free_storage);
PRAGMA_MSGSTR("following should be: WITH_SWAPPED_STREAM(sparse,err) ret = linearize_symbol(sym );")
SP(linearize_symbol, ret, sym);
#endif


#endif
