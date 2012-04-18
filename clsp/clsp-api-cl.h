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
#ifndef CLSP_API_CL_H_GUARD
#define CLSP_API_CL_H_GUARD
/**
    Code Listener API internal mirroring
*/

#include "clsp-apis.h"

#include "code_listener.h"


/*
    global API of Code Listener
 */

#define API_CL_NAME      cl
#define API_CL_FQ(item)  cl_##item
#define API_CL_KIND(type,          \
                    kind,          \
                    argcnt,        \
                    ret,           \
                    argdecl        \
                   )  kind##argcnt
#define API_CL_global_init           A,N,1,void,(struct cl_init_data *)
#define API_CL_global_init_defaults  A,N,2,void,(const char *, int)
#define API_CL_code_listener_create  A,R,1,struct cl_code_listener *,(const char *)
#define API_CL_chain_create          A,R,0,struct cl_code_listener *,(void)
#define API_CL_chain_append          A,N,2,void,(struct cl_code_listener *, struct cl_code_listener *)
#define API_CL_global_cleanup        A,N,0,void,(void)
#define API_CL_LAST                  Z,_,_,_,_

#define API_CL_LIST                  \
    global_init,                     \
    global_init_defaults,            \
    code_listener_create,            \
    chain_create,                    \
    chain_append,                    \
    global_cleanup

/* access other properties */
#define API_CL_RET(type,kind,argcnt,ret,argdecl)      ret
#define API_CL_ARGDECL(type,kind,argcnt,ret,argdecl)  argdecl

#if (API_SHOW > 0)
API_PRAGMA_OVERVIEW(CL)
# if (API_SHOW > 1)
API_PRAGMA_DETAILS(CL)
# endif
#endif


/*
    per-listener part of Code Listener API
 */

#define API_CLOBJ_NAME   cl-obj
#define API_CLOBJ_FQ     IDENTITY
#define API_CLOBJ_KIND(type,                            \
                       kind,                            \
                       argcnt /*incl. "self" argument*/ \
                      )  kind##argcnt
#define API_CLOBJ_file_open          A,N,2
#define API_CLOBJ_file_close         A,N,1
#define API_CLOBJ_fnc_open           A,N,2
#define API_CLOBJ_fnc_arg_decl       A,N,3
#define API_CLOBJ_fnc_close          A,N,1
#define API_CLOBJ_bb_open            A,N,2
#define API_CLOBJ_insn               A,N,2
#define API_CLOBJ_insn_call_open     A,N,4
#define API_CLOBJ_insn_call_arg      A,N,3
#define API_CLOBJ_insn_call_close    A,N,1
#define API_CLOBJ_insn_switch_open   A,N,3
#define API_CLOBJ_insn_switch_case   A,N,5
#define API_CLOBJ_insn_switch_close  A,N,1
#define API_CLOBJ_acknowledge        A,N,1
#define API_CLOBJ_destroy            A,N,1
#define API_CLOBJ_LAST               Z,_,_

#define API_CLOBJ_LIST               \
    file_open,                       \
    file_close,                      \
    fnc_open,                        \
    fnc_arg_decl,                    \
    fnc_close,                       \
    bb_open,                         \
    insn,                            \
    insn_call_open,                  \
    insn_call_arg,                   \
    insn_call_close,                 \
    insn_switch_open,                \
    insn_switch_case,                \
    insn_switch_close,               \
    acknowledge,                     \
    destroy

/* access other properties */
#define API_CLOBJ_KINDONLY(type,kind,argcnt)  kind

#if (API_SHOW > 0)
API_PRAGMA_OVERVIEW(CLOBJ)
# if (API_SHOW > 1)
API_PRAGMA_DETAILS(CLOBJ)
# endif
#endif


#endif
