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

/* TODO: remove APPLY() in some cases? */

/** External API functions are commonly not distinguished from local
    ones.  This is straightforward usage.  But there is no flexibility
    in it.  For instance, change the order of arguments in API call
    requires editing each occurence separately (forgotting coccinelle
    and similar tools for now).

    Thus, seemingly inconvenient, following constructs allow us to
    encapsulate external API dependencies into uniform "internalized" API,
    which buys us great deal of flexibility.  Is there a need to log each
    run-time call through particular API?  Hooking the log facility into
    internalized API is much simpler than manually rewriting each instance.
    Also lookup of calls from particular API in the code is much easier.

    Many macros are being defined here, but mostly they are only private
    dependencies for the one meant for public usage (they help to get
    across some preprocessor language limitations).  Such internal macros
    have trailing '_' in name.

    The public parts of internalized APIs (call it FOO) commonly comprise:

    - set of macros specifying properties of each required API function:

      FOO_bar 1 ==> tells the consuming macro "bar" function takes 1 argument

    - facade (as function-like macro) to perform actual external API call:

      FOO(bar, baz) ==> translates into something like "bar(baz)"
      alt. FOO(bar, r, baz) ==> translates into something like "r = bar(baz)"

    - facade (as function-like macro) to access external variables of API:

      FOO_VAR(spam) ==> translates into something "spam"

    - function-like macro to proceed required API functions with another
      function/macro (e.g., to fill the structure with function pointers)

      FOO_PROCEED(proceed) ==> apply "proceed" to particular parts of FOO

    NOTE: Function-like macros are considered as functions in the explanation
          above.
 */

#ifndef CLSP_APIS_H_GUARD
#define CLSP_APIS_H_GUARD

#include "cl_sparse_general.h"

#define API_SHOW

#if defined(API_SHOW)
# define SPARSE_API_SHOW 1
# define CL_API_SHOW     1
# define CL_OBJ_SHOW     1
PRAGMA_MSGSTR("OVERVIEW OF APIs USED FOLLOWS; A/B=std/macro func,C=global")
#endif


/*  Each internalized API function is defined indirectly through one of
    following macros.  Their form comes partly from effort to avoid
    "ISO C99 requires rest arguments to be used" warning.  Name format:

    API_USE_DEF_<N/R:function has/not a return value><number of arguments>_

    The last, esoteric argument "X" is nothing but a wrapper applied on
    the "fnc".  This is useful in some cases, if not, just use identity:

    #define IDENTITY(what)  (what)
 */


#define API_USE_DEF_N0_(fnc,               X)  X(fnc)(              )
#define API_USE_DEF_N1_(fnc,a1,            X)  X(fnc)(a1            )
#define API_USE_DEF_N2_(fnc,a1,a2,         X)  X(fnc)(a1,a2         )
#define API_USE_DEF_N3_(fnc,a1,a2,a3,      X)  X(fnc)(a1,a2,a3      )
#define API_USE_DEF_N4_(fnc,a1,a2,a3,a4,   X)  X(fnc)(a1,a2,a3,a4   )
#define API_USE_DEF_N5_(fnc,a1,a2,a3,a4,a5,X)  X(fnc)(a1,a2,a3,a4,a5)
#define API_USE_DEF_R0_(fnc,ret,...)  ret = API_USE_DEF_N0_(fnc,__VA_ARGS__)
#define API_USE_DEF_R1_(fnc,ret,...)  ret = API_USE_DEF_N1_(fnc,__VA_ARGS__)
#define API_USE_DEF_R2_(fnc,ret,...)  ret = API_USE_DEF_N2_(fnc,__VA_ARGS__)
#define API_USE_DEF_R3_(fnc,ret,...)  ret = API_USE_DEF_N3_(fnc,__VA_ARGS__)
#define API_USE_DEF_R4_(fnc,ret,...)  ret = API_USE_DEF_N4_(fnc,__VA_ARGS__)
#define API_USE_DEF_R5_(fnc,ret,...)  ret = API_USE_DEF_N5_(fnc,__VA_ARGS__)


#define  API_USE_0(what,...)  what( 1,__VA_ARGS__)
#define  API_USE_1(what,...)  what( 2,__VA_ARGS__)
#define  API_USE_2(what,...)  what( 3,__VA_ARGS__)
#define  API_USE_3(what,...)  what( 4,__VA_ARGS__)
#define  API_USE_4(what,...)  what( 5,__VA_ARGS__)
#define  API_USE_5(what,...)  what( 6,__VA_ARGS__)
#define  API_USE_6(what,...)  what( 7,__VA_ARGS__)
#define  API_USE_7(what,...)  what( 8,__VA_ARGS__)
#define  API_USE_8(what,...)  what( 9,__VA_ARGS__)
#define  API_USE_9(what,...)  what(10,__VA_ARGS__)
#define API_USE_10(what,...)  what(11,__VA_ARGS__)
#define API_USE_11(what,...)  what(12,__VA_ARGS__)
#define API_USE_12(what,...)  what(13,__VA_ARGS__)
#define API_USE_13(what,...)  what(14,__VA_ARGS__)
#define API_USE_14(what,...)  what(15,__VA_ARGS__)
#define API_USE_15(what,...)  what(16,__VA_ARGS__)

#define API_PROCEED_SAFE(prefix,fnc) \
    do {                                          \
        API_PROCEED_(fnc,prefix)     \
    } while (0)
#define API_PROCEED(prefix,fnc)  API_PROCEED2(fnc,prefix,prefix##_LIST,LAST,LAST)
#define API_PROCEED2(...)  API_PROCEED0_(0,__VA_ARGS__)
#define  API_PROCEED0_(c,f,p,h,...) API_USE_##c(API_PROCEED__(p##_##h,c),f,p,h,__VA_ARGS__)
#define  API_PROCEED1_(c,f,p,h,...) API_USE_##c(API_PROCEED__(p##_##h,c),f,p,h,__VA_ARGS__)
#define  API_PROCEED2_(c,f,p,h,...) API_USE_##c(API_PROCEED__(p##_##h,c),f,p,h,__VA_ARGS__)
#define  API_PROCEED3_(c,f,p,h,...) API_USE_##c(API_PROCEED__(p##_##h,c),f,p,h,__VA_ARGS__)
#define  API_PROCEED4_(c,f,p,h,...) API_USE_##c(API_PROCEED__(p##_##h,c),f,p,h,__VA_ARGS__)
#define  API_PROCEED5_(c,f,p,h,...) API_USE_##c(API_PROCEED__(p##_##h,c),f,p,h,__VA_ARGS__)
#define  API_PROCEED6_(c,f,p,h,...) API_USE_##c(API_PROCEED__(p##_##h,c),f,p,h,__VA_ARGS__)
#define  API_PROCEED7_(c,f,p,h,...) API_USE_##c(API_PROCEED__(p##_##h,c),f,p,h,__VA_ARGS__)
#define  API_PROCEED8_(c,f,p,h,...) API_USE_##c(API_PROCEED__(p##_##h,c),f,p,h,__VA_ARGS__)
#define  API_PROCEED9_(c,f,p,h,...) API_USE_##c(API_PROCEED__(p##_##h,c),f,p,h,__VA_ARGS__)
#define API_PROCEED10_(c,f,p,h,...) API_USE_##c(API_PROCEED__(p##_##h,c),f,p,h,__VA_ARGS__)
#define API_PROCEED11_(c,f,p,h,...) API_USE_##c(API_PROCEED__(p##_##h,c),f,p,h,__VA_ARGS__)
#define API_PROCEED12_(c,f,p,h,...) API_USE_##c(API_PROCEED__(p##_##h,c),f,p,h,__VA_ARGS__)
#define API_PROCEED13_(c,f,p,h,...) API_USE_##c(API_PROCEED__(p##_##h,c),f,p,h,__VA_ARGS__)
#define API_PROCEED14_(c,f,p,h,...) API_USE_##c(API_PROCEED__(p##_##h,c),f,p,h,__VA_ARGS__)
#define API_PROCEED15_(c,f,p,h,...) API_USE_##c(API_PROCEED__(p##_##h,c),f,p,h,__VA_ARGS__)

#define API_PROCEED__(props,cnt) API_PROCEED___(API_TYPE(props),cnt)
#define API_PROCEED___(type,cnt)        APPLY(JOIN(API_PROCEED___,type),type,cnt)
#define API_PROCEED___A(type,cnt)       API_PROCEED_##cnt##_
#define API_PROCEED___B(type,cnt)       API_PROCEED_##cnt##_
#define API_PROCEED___C(type,cnt)       API_PROCEED_##cnt##_
#define API_PROCEED___Z(type,cnt)       API_PROCEED_END_

#define API_TYPE(type,...)  type

#define  API_PROCEED_0_(cnt,fnc,prefix,head,...) fnc(head,prefix##_##head,cnt) API_PROCEED##cnt##_(cnt,fnc,prefix,__VA_ARGS__)
#define  API_PROCEED_1_(cnt,fnc,prefix,head,...) fnc(head,prefix##_##head,cnt) API_PROCEED##cnt##_(cnt,fnc,prefix,__VA_ARGS__)
#define  API_PROCEED_2_(cnt,fnc,prefix,head,...) fnc(head,prefix##_##head,cnt) API_PROCEED##cnt##_(cnt,fnc,prefix,__VA_ARGS__)
#define  API_PROCEED_3_(cnt,fnc,prefix,head,...) fnc(head,prefix##_##head,cnt) API_PROCEED##cnt##_(cnt,fnc,prefix,__VA_ARGS__)
#define  API_PROCEED_4_(cnt,fnc,prefix,head,...) fnc(head,prefix##_##head,cnt) API_PROCEED##cnt##_(cnt,fnc,prefix,__VA_ARGS__)
#define  API_PROCEED_5_(cnt,fnc,prefix,head,...) fnc(head,prefix##_##head,cnt) API_PROCEED##cnt##_(cnt,fnc,prefix,__VA_ARGS__)
#define  API_PROCEED_6_(cnt,fnc,prefix,head,...) fnc(head,prefix##_##head,cnt) API_PROCEED##cnt##_(cnt,fnc,prefix,__VA_ARGS__)
#define  API_PROCEED_7_(cnt,fnc,prefix,head,...) fnc(head,prefix##_##head,cnt) API_PROCEED##cnt##_(cnt,fnc,prefix,__VA_ARGS__)
#define  API_PROCEED_8_(cnt,fnc,prefix,head,...) fnc(head,prefix##_##head,cnt) API_PROCEED##cnt##_(cnt,fnc,prefix,__VA_ARGS__)
#define  API_PROCEED_9_(cnt,fnc,prefix,head,...) fnc(head,prefix##_##head,cnt) API_PROCEED##cnt##_(cnt,fnc,prefix,__VA_ARGS__)
#define API_PROCEED_10_(cnt,fnc,prefix,head,...) fnc(head,prefix##_##head,cnt) API_PROCEED##cnt##_(cnt,fnc,prefix,__VA_ARGS__)
#define API_PROCEED_11_(cnt,fnc,prefix,head,...) fnc(head,prefix##_##head,cnt) API_PROCEED##cnt##_(cnt,fnc,prefix,__VA_ARGS__)
#define API_PROCEED_12_(cnt,fnc,prefix,head,...) fnc(head,prefix##_##head,cnt) API_PROCEED##cnt##_(cnt,fnc,prefix,__VA_ARGS__)
#define API_PROCEED_13_(cnt,fnc,prefix,head,...) fnc(head,prefix##_##head,cnt) API_PROCEED##cnt##_(cnt,fnc,prefix,__VA_ARGS__)
#define API_PROCEED_14_(cnt,fnc,prefix,head,...) fnc(head,prefix##_##head,cnt) API_PROCEED##cnt##_(cnt,fnc,prefix,__VA_ARGS__)
#define API_PROCEED_15_(cnt,fnc,prefix,head,...) fnc(head,prefix##_##head,cnt) API_PROCEED##cnt##_(cnt,fnc,prefix,__VA_ARGS__)
#define API_PROCEED_END_(cnt,fnc,prefix,head,last)


/*
 *  sparse API (subset used) via directly available symbols
 */

/*#include "sparse/lib.h"
#include "sparse/expression.h"
#include "sparse/linearize.h"
#include "sparse/scope.h"
#include "sparse/storage.h"*/
/* #include "sparse/flow.h"
   #include "sparse/parse.h"
   #include "sparse/symbol.h"
   #include "sparse/target.h"
   #include "sparse/token.h" */

/*
#undef UNUSED
*/

/* functions (A=std, B=macro) */

#define SPARSE_API(...)            SPARSE_API_(__VA_ARGS__,SPARSE_API_MAP)
#define SPARSE_API_(fnc,...)       APPLY(SPARSE_API_OUT(fnc),SPARSE_API_KIND(fnc),fnc,__VA_ARGS__)
#define SPARSE_API__(kind,fnc,...) API_USE_DEF_##kind##_(fnc,__VA_ARGS__)
#define SPARSE_API_D(kind,fnc,...)           \
    WITH_SWAPPED_STREAM(out, debug)          \
        API_USE_DEF_##kind##_(fnc,__VA_ARGS__)
#define SPARSE_API_E(kind,fnc,...)           \
    WITH_SWAPPED_STREAM(sparse, err)         \
        API_USE_DEF_##kind##_(fnc,__VA_ARGS__)

#define SPARSE_API_NAME(what)  IDENTITY(what)
#define SPARSE_API_KIND(what)  APPLY(SPARSE_API_KIND_,SPARSE_API_##what)
#define SPARSE_API_OUT(what)   APPLY(SPARSE_API_OUT_,SPARSE_API_##what)
#define SPARSE_API_KIND_(type,kind,argcnt,out)  kind##argcnt
#define SPARSE_API_OUT_(type,kind,argcnt,out)   SPARSE_API_##out

#define SPARSE_API_sparse_initialize  A,R,3,E
#define SPARSE_API_sparse             A,R,1,E
#define SPARSE_API_expand_symbol      A,N,1,E
#define SPARSE_API_linearize_symbol   A,R,1,E
#define SPARSE_API_unssa              A,N,1,E
#define SPARSE_API_set_up_storage     A,N,1,E
#define SPARSE_API_free_storage       A,N,0,_
#define SPARSE_API_show_symbol        A,N,1,D
#define SPARSE_API_ptr_list_empty     B,N,1,_

/* global variables (C) */

#define SPARSE_API_input_streams      C,_,_,_

/* altogether */

#define SPARSE_API_LAST               Z,_,_,_
#define SPARSE_API_LIST        \
    /* functions */            \
    sparse_initialize,         \
    sparse,                    \
    expand_symbol,             \
    linearize_symbol,          \
    unssa,                     \
    set_up_storage,            \
    free_storage,              \
    show_symbol,               \
    /* function-like macros */ \
    ptr_list_empty,            \
    /* global variables */     \
    input_streams

#ifdef SPARSE_API_SHOW
# define FNC(what,props,cnt)  PRAGMA_MSG(API:sparse:cnt:API_TYPE(props):what)
PRAGMA_MSGSTR("[[[ sparse ]]]")
API_PROCEED(SPARSE_API,FNC)
# undef FNC
#endif

/*
 *  Code Listener API
 */

//#include "code_listener.h"

/*
#undef UNUSED
*/

/*
 *  global Code Listener API via globals object
 */

/* functions (A=std, B=macro) */

#define CL_API(...)            CL_API_(__VA_ARGS__,CL_API_MAP)
#define CL_API_(fnc, ...)      APPLY(CL_API__,CL_API_KIND(fnc),fnc,__VA_ARGS__)
#define CL_API__(kind,fnc,...) API_USE_DEF_##kind##_(fnc,__VA_ARGS__)

/*TODO: remove and comment*/
#define CL_API_MAP(fnc)    (GLOBALS(cl_api).fnc)

#define CL_API_NAME(what)  cl_##what  /* proper imported symbol name */
#define CL_API_KIND(what)  APPLY(CL_API_KIND_,CL_API_##what)
#define CL_API_KIND_(type,kind,argcnt)  kind##argcnt

#define CL_API_global_init           A,N,1
#define CL_API_global_init_defaults  A,N,2
#define CL_API_code_listener_create  A,R,1
#define CL_API_chain_create          A,R,0
#define CL_API_chain_append          A,N,2
#define CL_API_global_cleanup        A,N,0

/* altogether */

#define CL_API_LAST                  Z,_,_
#define CL_API_LIST       \
    /* functions */       \
    global_init,          \
    global_init_defaults, \
    code_listener_create, \
    chain_create,         \
    chain_append,         \
    global_cleanup

#ifdef CL_API_SHOW
# define FNC(what,props,cnt) \
    PRAGMA_MSG(API:cl-api:cnt:API_TYPE(props):CL_API_NAME(what))
PRAGMA_MSGSTR("[[[ cl-api ]]]")
API_PROCEED(CL_API,FNC)
# undef FNC
#endif

/*
 *  per-listener part of Code Listener API using listener in globals object
 */

/* functions (A=std, B=macro) */

#define CL_OBJ(...)            CL_OBJ_(__VA_ARGS__,CL_OBJ_MAP)
#define CL_OBJ_(fnc,...)       APPLY(CL_OBJ__,CL_OBJ_KIND(fnc),fnc,__VA_ARCL_OBJ)
#define CL_OBJ__(cnt,fnc,...)  API_USE_DEF_N##cnt##_(fnc,CL_OBJ_CL,__VA_ARGS__)

/* #define CL_OBJ_MAP(fnc)        (GLOBALS(cl)->fnc) */
/* CL_OBJ_CL */

#define CL_OBJ_NAME(what)  IDENTITY(what)
#define CL_OBJ_KIND(what)  APPLY(CL_OBJ_KIND_,CL_OBJ_##what)
#define CL_OBJ_KIND_(type,kind,argcnt)  kind##argcnt

/* NOTE: incl. common CL_OBJ_CL argument */
#define CL_OBJ_file_open          A,N,2
#define CL_OBJ_file_close         A,N,1
#define CL_OBJ_fnc_open           A,N,2
#define CL_OBJ_fnc_arg_decl       A,N,3
#define CL_OBJ_fnc_close          A,N,1
#define CL_OBJ_bb_open            A,N,2
#define CL_OBJ_insn               A,N,2
#define CL_OBJ_insn_call_open     A,N,4
#define CL_OBJ_insn_call_arg      A,N,3
#define CL_OBJ_insn_call_close    A,N,1
#define CL_OBJ_insn_switch_open   A,N,3
#define CL_OBJ_insn_switch_case   A,N,5
#define CL_OBJ_insn_switch_close  A,N,1
#define CL_OBJ_acknowledge        A,N,1
#define CL_OBJ_destroy            A,N,1

/* altogether */

#define CL_OBJ_LAST               Z,_,_
#define CL_OBJ_LIST    \
    /* functions */    \
    file_open,         \
    file_close,        \
    fnc_open,          \
    fnc_arg_decl,      \
    fnc_close,         \
    bb_open,           \
    insn,              \
    insn_call_open,    \
    insn_call_arg,     \
    insn_call_close,   \
    insn_switch_open,  \
    insn_switch_case,  \
    insn_switch_close, \
    acknowledge,       \
    destroy

#ifdef CL_OBJ_SHOW
# define FNC(what,props,cnt)  PRAGMA_MSG(API:cl-obj:cnt:API_TYPE(props):what)
PRAGMA_MSGSTR("[[[ cl-obj ]]]")
API_PROCEED(CL_OBJ,FNC)
# undef FNC
#endif


#endif
/* vim:set ts=4 sts=4 sw=4 et: */
