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

/*
    Expose stub-symbols required (but unused) when facing with GCC plug-in.
 */
    
#include "clsp_apis.h"
#include "clsp_api_gccplug.h"

#define GCCPLUG_RETVAL(prefix, item) \
    GCCPLUG_RETVAL_(APPLY(API_GCCPLUG_KIND,API_PROPS(prefix,item)),prefix,item)
#define GCCPLUG_RETVAL_(kind, prefix, item) \
    GCCPLUG_RETVAL__(kind, prefix, item)
#define GCCPLUG_RETVAL__(kind, prefix, item) \
    APPLY(GCCPLUG_RETVAL_##kind, prefix, item)
#define GCCPLUG_RET_(props)  API_GCCPLUG_RET(props)
/* selectively: with/out return value */
#define GCCPLUG_RETVAL_R(prefix, item) \
    (GCCPLUG_RET_(API_PROPS(prefix, item))) {0}
#define GCCPLUG_RETVAL_N(prefix, item)  /*NOTHING*/

/* to supress "unused parameter" warning */
#define UNUSED(argcnt)  UNUSED_(argcnt)
#define UNUSED_(argcnt) UNUSED_##argcnt
#define UNUSED_0
#define UNUSED_1 UNUSED_0 (void) a1;
#define UNUSED_2 UNUSED_1 (void) a2;
#define UNUSED_3 UNUSED_2 (void) a3;
#define UNUSED_4 UNUSED_3 (void) a4;
#define UNUSED_5 UNUSED_4 (void) a5;
#define UNUSED_6 UNUSED_5 (void) a6;

#define GCCPLUG_STUBS(prefix, item, cnt)                           \
    APPLY(API_GCCPLUG_RET,API_PROPS(prefix, item))                 \
    item                                                           \
    APPLY(API_GCCPLUG_ARGDECL,API_PROPS(prefix, item))             \
    {                                                              \
        UNUSED(APPLY(API_GCCPLUG_ARGCNT,API_PROPS(prefix, item)))  \
        return GCCPLUG_RETVAL(prefix, item);                       \
    }
API_PROCEED(GCCPLUG, GCCPLUG_STUBS)
