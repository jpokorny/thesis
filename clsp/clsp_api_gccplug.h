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
#ifndef CLSP_API_GCCPLUG_H_GUARD
#define CLSP_API_GCCPLUG_H_GUARD

/*
    Declare stub-symbols required (but unused) when facing with GCC plug-in.
 */

#define API_GCCPLUG_NAME   gcc-plugin
#define API_GCCPLUG_FQ     IDENTITY

/* actual parameters are very simplified as it does not matter much */
typedef struct {void *foo;} stub_t;
typedef void (*stub_f)(void *a1);

stub_t global_dc, input_location, gimple_ops_offset_, tree_code_type,
       ieee_single_format, gimple_rhs_class_table, current_function_decl,
       gss_for_code_;

#define API_GCCPLUG_error_at                     A,N,2,void  ,(int a1,void *a2)
#define API_GCCPLUG_expand_location              A,R,1,stub_t,(unsigned int a1)
#define API_GCCPLUG_fancy_abort                  A,N,3,void  ,(int a1,int a2,void *a3)
#define API_GCCPLUG_fopen_unlocked               A,R,2,void *,(void *a1,void *a2)
#define API_GCCPLUG_gimple_call_flags            A,R,1,int   ,(void *a1)
#define API_GCCPLUG_htab_create_alloc            A,R,6,void *,(int a1,stub_f a2,stub_f a3,stub_f a4,stub_f a5,stub_f a6)
#define API_GCCPLUG_htab_delete                  A,N,1,void  ,(void *a1)
#define API_GCCPLUG_htab_find                    A,R,2,void *,(void *a1,void *a2)
#define API_GCCPLUG_htab_find_slot               A,R,3,void *,(void *a1,void *a2,int a3)
#define API_GCCPLUG_plugin_default_version_check A,R,2,int   ,(void *a1,void *a2)
#define API_GCCPLUG_real_to_target_fmt           A,R,3,long  ,(void *a1,const void *a2,const void *a3)
#define API_GCCPLUG_register_callback            A,N,4,void  ,(void *a1,int a2,stub_f a3,void *a4)
#define API_GCCPLUG_walk_gimple_seq              A,R,4,void *,(void *a1,stub_f a2,stub_f a3,void *a4)
#define API_GCCPLUG_warning_at                   A,R,3,int   ,(int a1,int a2,void *a3)
#define API_GCCPLUG_xcalloc                      A,R,2,void *,(int a1,int a2)
#define API_GCCPLUG_xrealloc                     A,R,2,void *,(void *a1,int a2)
#define API_GCCPLUG_LAST                         Z,_,_,_,_
#define API_GCCPLUG_LIST           \
    error_at,                      \
    expand_location,               \
    fancy_abort,                   \
    fopen_unlocked,                \
    gimple_call_flags,             \
    htab_create_alloc,             \
    htab_delete,                   \
    htab_find,                     \
    htab_find_slot,                \
    plugin_default_version_check,  \
    real_to_target_fmt,            \
    register_callback,             \
    walk_gimple_seq,               \
    warning_at,                    \
    xcalloc,                       \
    xrealloc

/* access other properties */
#define API_GCCPLUG_ARGCNT( type,kind,argcnt,ret,argdecl)  argcnt
#define API_GCCPLUG_KIND(   type,kind,argcnt,ret,argdecl)  kind
#define API_GCCPLUG_RET(    type,kind,argcnt,ret,argdecl)  ret
#define API_GCCPLUG_ARGDECL(type,kind,argcnt,ret,argdecl)  argdecl


#if (API_SHOW > 0)
API_PRAGMA_OVERVIEW(GCCPLUG)
# if (API_SHOW > 1)
API_PRAGMA_DETAILS(GCCPLUG)
# endif
#endif


#endif
