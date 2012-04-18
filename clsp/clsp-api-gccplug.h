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
/**
    Internal mirroring of GCC API available to its plugins
 */

#include "clsp-apis.h"


/*
    Declare stub-symbols required (but unused) when facing with GCC plug-in.
 */


/* simplified types as it does not matter much */
typedef struct {void *foo;} stub_t;
typedef void (*stub_f)(void *a1);

#define API_GCCPLUG_NAME   gcc-plugin
#define API_GCCPLUG_FQ     IDENTITY
#define API_GCCPLUG_KIND(type,                  \
                         kind,                  \
                         argcnt,                \
                         ret,                   \
                         argdecl)  kind##argcnt

/*tree.h:               GTY(()) tree current_function_decl*/
#define API_GCCPLUG_current_function_decl        A,S,0,stub_t,_
/*gimple.h:             enum gimple_statement_structure_enum const gss_for_code_[]*/
#define API_GCCPLUG_gss_for_code_                A,S,0,stub_t,_
/*gimple.h:             size_t const gimple_ops_offset_[]*/
#define API_GCCPLUG_gimple_ops_offset_           A,S,0,stub_t,_
/*gimple.h:             const unsigned char gimple_rhs_class_table[]*/
#define API_GCCPLUG_gimple_rhs_class_table       A,S,0,stub_t,_
/*diagnostic.h:         diagnostic_context *global_dc*/
#define API_GCCPLUG_global_dc                    A,S,0,stub_t,_
/*real.h:               const struct real_format ieee_single_format*/
#define API_GCCPLUG_ieee_single_format           A,S,0,stub_t,_
/*input.h:              location_t input_location*/
#define API_GCCPLUG_input_location               A,S,0,stub_t,_
/*tree.h:               const enum tree_code_class tree_code_type[]*/
#define API_GCCPLUG_tree_code_type               A,S,0,stub_t,_

/*diagnostic-core.h:    void error_at (location_t, const char *, ...)*/
#define API_GCCPLUG_error_at                     C,N,2,void  ,(int a1,void *a2)
/*input.h:              expanded_location expand_location (source_location)*/
#define API_GCCPLUG_expand_location              C,R,1,stub_t,(unsigned int a1)
/*system.h:             void fancy_abort (const char *, int, const char *)*/
#define API_GCCPLUG_fancy_abort                  C,N,3,void  ,(int a1,int a2,void *a3)
/*system.h:             FILE *fopen_unlocked (const char *, const char *)*/
#define API_GCCPLUG_fopen_unlocked               C,R,2,void *,(void *a1,void *a2)
/*gimple.h:             int gimple_call_flags (const_gimple)*/
#define API_GCCPLUG_gimple_call_flags            C,R,1,int   ,(void *a1)
/*hashtab.h:            htab_t htab_create_alloc (size_t, htab_hash, htab_eq, htab_del, htab_alloc, htab_free)*/
#define API_GCCPLUG_htab_create_alloc            C,R,6,void *,(int a1,stub_f a2,stub_f a3,stub_f a4,stub_f a5,stub_f a6)
/*hashtab.h:            void htab_delete (htab_t)*/
#define API_GCCPLUG_htab_delete                  C,N,1,void  ,(void *a1)
/*hashtab.h:            void *htab_find (htab_t, const void *)*/
#define API_GCCPLUG_htab_find                    C,R,2,void *,(void *a1,void *a2)
/*hashtab.h:            void **htab_find_slot (htab_t, const void *, enum insert_option)*/
#define API_GCCPLUG_htab_find_slot               C,R,3,void *,(void *a1,void *a2,int a3)
/*gcc-plugin.h:         bool plugin_default_version_check (struct plugin_gcc_version *, struct plugin_gcc_version *)*/
#define API_GCCPLUG_plugin_default_version_check C,R,2,int   ,(void *a1,void *a2)
/*real.h:               long real_to_target (long *, const REAL_VALUE_TYPE *, enum machine_mode)*/
#define API_GCCPLUG_real_to_target_fmt           C,R,3,long  ,(void *a1,const void *a2,const void *a3)
/*gcc-plugin.h:         void register_callback (const char *plugin_name, int event, plugin_callback_func callback, void *user_data)*/
#define API_GCCPLUG_register_callback            C,N,4,void  ,(void *a1,int a2,stub_f a3,void *a4)
/*gimple.h:             gimple walk_gimple_seq (gimple_seq, walk_stmt_fn, walk_tree_fn, struct walk_stmt_info *)*/
#define API_GCCPLUG_walk_gimple_seq              C,R,4,void *,(void *a1,stub_f a2,stub_f a3,void *a4)
/*diagnostic-core.h:    bool warning_at (location_t, int, const char *, ...)*/
#define API_GCCPLUG_warning_at                   C,R,3,int   ,(int a1,int a2,void *a3)
/*libiberty.h:          void *xcalloc (size_t, size_t)*/
#define API_GCCPLUG_xcalloc                      C,R,2,void *,(int a1,int a2)
/*libiberty.h:          void *xrealloc (void *, size_t)*/
#define API_GCCPLUG_xrealloc                     C,R,2,void *,(void *a1,int a2)

#define API_GCCPLUG_LAST                         Z,_,_,_,_
#define API_GCCPLUG_LIST           \
    /* globals */                  \
    current_function_decl,         \
    gss_for_code_,                 \
    gimple_ops_offset_,            \
    gimple_rhs_class_table,        \
    global_dc,                     \
    ieee_single_format,            \
    input_location,                \
    tree_code_type,                \
    /* functions */                \
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
#define API_GCCPLUG_ARGCNT(  type,kind,argcnt,ret,argdecl)  argcnt
#define API_GCCPLUG_KINDONLY(type,kind,argcnt,ret,argdecl)  kind
#define API_GCCPLUG_RET(     type,kind,argcnt,ret,argdecl)  ret
#define API_GCCPLUG_ARGDECL( type,kind,argcnt,ret,argdecl)  argdecl


#if (API_SHOW > 0)
API_PRAGMA_OVERVIEW(GCCPLUG)
# if (API_SHOW > 1)
API_PRAGMA_DETAILS(GCCPLUG)
# endif
#endif


#endif
