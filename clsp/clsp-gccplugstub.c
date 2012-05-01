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

/*
    Expose stub-symbols required (but unused) when facing with GCC plug-in.
 */
    
#include "clsp-apis.h"
#include "clsp-api-gccplug.h"

#define GCCPLUG_RETVAL(prefix, item) \
    GCCPLUG_RETVAL_(APPLY_INNER(API_GCCPLUG_RET,API_PROPS(prefix,item)))
#define GCCPLUG_RETVAL_(ret)  (ret) {0}  /* universal retval */

/* to supress "unused parameter" warning, we pretend usage */
#define UNUSED(argcnt)  UNUSED_(argcnt)  /*unfold args*/
#define UNUSED_(argcnt) UNUSED_##argcnt
#define UNUSED_0
#define UNUSED_1 UNUSED_0 (void) a1;
#define UNUSED_2 UNUSED_1 (void) a2;
#define UNUSED_3 UNUSED_2 (void) a3;
#define UNUSED_4 UNUSED_3 (void) a4;
#define UNUSED_5 UNUSED_4 (void) a5;
#define UNUSED_6 UNUSED_5 (void) a6;

#define GCCPLUG_STUBS(prefix, item, cnt)                                \
    APPLY(API_GCCPLUG_RET,API_PROPS(prefix, item))                      \
    GCCPLUG_STUBS_(APPLY(API_GCCPLUG_KINDONLY,API_PROPS(prefix,item)),prefix,item,cnt)
#define GCCPLUG_STUBS_(...) GCCPLUG_STUBS__(__VA_ARGS__)  /*unfold args*/
#define GCCPLUG_STUBS__(kind, prefix, item, cnt) \
    APPLY(GCCPLUG_STUBS_##kind, prefix, item, cnt)

#define GCCPLUG_STUBS_R(prefix, item, cnt)                              \
    item                                                                \
    APPLY_INNER(API_GCCPLUG_ARGDECL,API_PROPS(prefix, item))            \
    {                                                                   \
        UNUSED(APPLY_INNER(API_GCCPLUG_ARGCNT,API_PROPS(prefix, item))) \
        return GCCPLUG_RETVAL(prefix, item);                            \
    }

#define GCCPLUG_STUBS_N(prefix, item, cnt)                              \
    item                                                                \
    APPLY_INNER(API_GCCPLUG_ARGDECL,API_PROPS(prefix, item))            \
    {                                                                   \
        UNUSED(APPLY_INNER(API_GCCPLUG_ARGCNT,API_PROPS(prefix, item))) \
        return;                                                         \
    }

#define GCCPLUG_STUBS_S(prefix, item, cnt) \
    item;
    

API_PROCEED(GCCPLUG, GCCPLUG_STUBS)
