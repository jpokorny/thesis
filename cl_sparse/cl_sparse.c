/*
 * Copyright (C) 2009 Kamil Dudka <kdudka@redhat.com>
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

#define _POSIX_C_SOURCE 200809L // snprintf

//#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <unistd.h>
#include <errno.h>
#include <signal.h>    // kill
#include <sys/wait.h>  // wait
#include <assert.h>
#include <dlfcn.h>     // dlopen, dlsym, dlclose
#include <ctype.h>     // isdigit

#define USE_INT3_AS_BRK
#include "trap.h"
#include "code_listener.h"
#include "type_enumerator.h"

// sparse headers
#include "sparse/lib.h"
#include "sparse/expression.h"
#include "sparse/linearize.h"
#include "sparse/scope.h"
#include "sparse/storage.h"
//#include "sparse/flow.h"
//#include "sparse/parse.h"
//#include "sparse/symbol.h"
//#include "sparse/target.h"
//#include "sparse/token.h"


/* compile options */

// general
#define DO_EXTRA_CHECKS              1
#define USE_EXTENDED_TYPE_CMP        0
#define SHOW_PSEUDO_INSNS            0

// sparse
#define DO_PROCEED_INTERNAL          0
#define DO_EXPAND_SYMBOL             1
#define DO_PER_EP_UNSAA              1
#define DO_PER_EP_SET_UP_STORAGE     1
#define DO_SPARSE_FREE               1
#define FIX_SPARSE_EXTRA_ARG_TO_MEM  1

/* symbolic values */

#define DL_OPEN_FLAGS                  (RTLD_LAZY|RTLD_LOCAL)

#define CLSP_CONFIG_STRING_MAX          512
#define CLSP_NAME                       __FILE__
#define CLSP_SPARSE_INTERNAL_SYMS_FILE  "sparse-internal-symbols"

#define FD_UNDEF     -1
#define FD_DEFERRED  -2

/* common macros */

#ifndef STREQ
# define STREQ(s1, s2)  (!strcmp(s1, s2))
#endif

#define UNFOLD(...)  OPT_##what

#define STRINGIFY(arg)              #arg
#define TOSTRING(arg)               STRINGIFY(arg)

#define PRAGMA(arg)                 _Pragma(arg)
#define COMPILE_TIME_ASSERT(pred)   switch(0){case 0:case pred:;}

// beware of side-effects
#define PARTIALLY_ORDERED(a, b, c)  (a <= b && b <= c)

// yo, Dawg...
#define OR  : case
#define IN(choices)  , choices
#define COND_WHICH(cond, which) \
    switch (cond) {             \
        case which:
#define BEGIN_WHEN(cond_which)  \
        COND_WHICH(cond_which)
#define END_WHEN                \
            ; break;            \
        default:                \
            break;              \
    }


/* allocation */

// NOTE: use "MEM_NEW(foo)" for direct access ("MEM_NEW(foo).bar = 42")
//       and "(MEM_NEW(foo))" as a function parameter ("bar((MEM_NEW(foo)))")

#define NORETWRN(expr)  (void)(expr)

#define MEM_NEW(var)                        \
   (((var) = malloc(sizeof(*(var))))        \
     ? (void) 0  /* NOOP */                 \
     : DIE( ERRNOCODE(EC_MEM, "MEM_NEW") )  \
   ) , (var)

#define MEM_ARR_RESIZE(arr, newcnt)                      \
    (((arr) = realloc((arr), sizeof(*(arr)) * (newcnt))) \
      ? (void) 0 /* NOOP */                              \
      : DIE( ERRNOCODE(EC_MEM, "MEM_ARR_RESIZE") )       \
    ) , (arr)

// NOTE: one-liner for item append: *(MEM_ARR_APPEN(arr,size)) = item
#define MEM_ARR_APPEND(arr, oldcnt)                          \
    (((arr) = realloc((arr), sizeof(*(arr)) * (++(oldcnt)))) \
      ? (void) 0 /* NOOP */                                  \
      : DIE( ERRNOCODE(EC_MEM, "MEM_ARR_APPEND") )           \
    ) , (arr + oldcnt - 1)


//
// globals
//

typedef void (*atexit_fnc)();

enum streams {
    // for both master and worker
    stream_first,
    stream_out = stream_first,
    stream_err,
    // worker only
    stream_worker_first,
    stream_sparse = stream_worker_first,
    stream_cl,
    stream_debug,
    stream_worker_last,
    stream_last = stream_worker_last
};

static struct globals {
    FILE            *stream[stream_last];
    /* buffer for sparse deferred stream */
    struct g_deferred {
        char        *buffer;
        size_t      size;
    } deferred;
    struct cl_code_listener  *cl;
    /* API functions resolved in compile- or run-time set here */
    struct g_cl_api {
        void (*global_init)(struct cl_init_data *);
        void (*global_init_defaults)(const char *, int);
        struct cl_code_listener *(*code_listener_create)(const char *);
        struct cl_code_listener *(*chain_create)();
        void (*chain_append)(struct cl_code_listener *,
                             struct cl_code_listener *);
        void (*global_cleanup)();
    } cl_api;
    struct g_cl_libs {
        size_t      cnt;
        void        **handles;  // !HAS_CL -> first is the main one
    } cl_libs;
    // TODO typedb
    int             debug;
    /* unexposed, but run-modifying options (e.g., for testing) */
    struct {
        bool        register_atexit;
    } unexposed;
} globals;

// this is defined to allow a kind of encapsulation
#define GLOBALS(what)  (globals.what)
#define STREAM(which)  GLOBALS(stream[stream_##which])


//
// outputs
//

/* universal print macro with implicit newline
 *
 * usage: PUT(stream_index, [[fmt], ...])
 * NOTE:  fmt has to be compile-time constant
 */

// NOTE: last argument in order to allow format string as the only
//       argument; compensated appending "%s" later on
#define PUT(which, ...)        PUT_(which, __VA_ARGS__, "")
#define PUT_(which, fmt, ...)  PUT__(which, 0, fmt "%s", __VA_ARGS__)
#define PUT__(which, skip, fmt, ...) \
    fprintf(STREAM(which), &(fmt "\n")[skip], __VA_ARGS__)

/* pre-mortem print macro
 *
 * usage: DIE( WRAPPER(wrapper_args, [fmt, [...]]) )
 *        where WRAPPER is one of macros defined below
 */

#define DLOC \
    "\n" __FILE__ ":" TOSTRING(__LINE__) ": note: from %s [internal location]"

#define ERRNO(...)             DCHR_ERRNO __VA_ARGS__
// code (exit ~) can be [0,10), i.e., containing single digit
#define ECODE_(flag,code,...)  flag STRINGIFY(code) __VA_ARGS__
#define ECODE(...)             ECODE_(DCHR_ECODE, __VA_ARGS__)
#define ERRNOCODE(...)         ECODE_(DCHR_ERRNOCODE, __VA_ARGS__)

// NOTE: format string should not start with any of these (used internally):
#define DCHR_ERRNO      "@"
#define DCHR_ECODE      "$"
#define DCHR_ERRNOCODE  "#"

// see PUT
#define DIE(...)         DIE_(__VA_ARGS__, "")
#define DIE_(fmt, ...)   DIE__(fmt "%s", __VA_ARGS__)
#define DIE__(fmt, ...)                                                       \
    ((*fmt != *DCHR_ERRNO && *fmt != *DCHR_ERRNOCODE)                         \
        ? PUT__(err, *fmt == *DCHR_ECODE ? 2 : 0, fmt DLOC,                   \
             __VA_ARGS__, __func__)                                           \
        : (((fmt)[(*fmt == *DCHR_ERRNOCODE ? 2 : 1)])                         \
            ? PUT__(err, *fmt == *DCHR_ERRNOCODE ? 2 : 1, fmt ": %s" DLOC,    \
                    __VA_ARGS__, strerror(errno), __func__)                   \
            : PUT__(err, *fmt == *DCHR_ERRNOCODE ? 2 : 1, fmt "die: %s" DLOC, \
                    __VA_ARGS__, strerror(errno), __func__))                  \
    , ((*fmt == *DCHR_ERRNOCODE || *fmt == *DCHR_ECODE)                       \
        ? exit((int) (fmt[1] - '0'))                                          \
        : exit(ec_general)))


// common exit codes (1 reserved for sparse)
#define ECVALUE(arg)     arg
#define ECACCESS(index)  index - ec_first
#define ECMSG(suffix)    [ECACCESS(ec_##suffix)] = ec_##suffix()
enum {
    ec_first = 1,
    ec_sparse = ec_first,
#define ec_sparse()   "sparse has not finished successfully"
    ec_general,
#define ec_general()  "something general has failed"
    ec_opt,
#define ec_opt()      "incorrect command-line"
    ec_mem,
#define ec_mem()      "memory handling has failed (probably OOM)"
    ec_tdb,
#define ec_tdb()      "internal type database handling has failed"
    ec_cl,
#define ec_cl()       "Code Listener run has been aborted"
    ec_last
};
static const char *ec_str[ec_last] = {
    ECMSG(sparse),
    ECMSG(general),
    ECMSG(opt),
    ECMSG(mem),
    ECMSG(tdb),
    ECMSG(cl),
};

// output functions
//static void clmsg_ignore(const char *msg) { (void) msg;                     }
static void clmsg_print(const char *msg)  { PUT(cl, "%s", msg);             }
static void clmsg_die(const char *msg)    { DIE( ECODE(ec_cl, "%s", msg) ); }


//
// pointer and array DB, for building pointer* and array hierarchy in order to
// prevent having two semantically same pointers/arrays as two different types
//

struct arr_db_item {
    int             arr_size;
    struct cl_type  *clt;
};

struct ptr_db_item {
    struct cl_type      *clt;
    struct ptr_db_item  *next;
    size_t              arr_cnt;
    struct arr_db_item  **arr;
    bool                free_type;  ///< whether we are responsible for clt
};

struct ptr_db_arr {
    size_t              alloc_size;
    size_t              last;
    struct ptr_db_item  *heads;
};



// shared object for initialization phase data exchange amongst functions
// NOTE: imm -> immediate values; set -> values that were set up later on
struct options {
    /* internal options */
    struct {
        struct {
            bool            fork;
            struct oi_fd {
                int         cl;
                int         sparse;  // FD_DEFERRED for deferred output
                int         debug;
            } fd;
            int             debug;
        } imm;
    } internals;
    /* Code Listener */
    struct {
        struct {
            struct {
                size_t      cnt;
                char        **arr;  // !HAS_CL -> first is the main one
            } listeners;
            bool            default_output;
            struct {
                bool        enable;
                bool        types;
                bool        switch_to_if;
                const char  *file;
            } pprint;
            struct {
                bool        enable;
                const char  *file;
            } gencfg;
            struct {
                bool        enable;
                const char  *file;
            } gentype;
            struct oc_debug {
                bool        location;
                int         level;
            } debug;
        } imm;
    } cl;
    /* sparse */
    struct {
        struct {
            int             argc;
            char            **argv;
        } set;
    } sparse;
    /* globals as a dependency injection */
    struct globals          *globals;
};


/* debugging */

#define DACCESS(index)   index - d_first
#define DVALUE(arg)       (1 << arg)
#define DMSG(suffix)     [DACCESS(d_##suffix)] = d_##suffix()
enum {
    d_first = 0,
    d_instruction = d_first,
#define d_instruction()  "print instruction being processed"
    d_type,
#define d_type()         "print type being processed"
    d_insert_type,
#define d_insert_type()  "print type being inserted into type DB"
    d_file,
#define d_file()         "print current file to be proceeded"
    d_last
};
static const char *d_str[d_last] = {
    DMSG(instruction),
    DMSG(type),
    DMSG(insert_type),
    DMSG(file),
};

// two forms of usage
#define DLOG(level, ...)                \
    if (GLOBALS(debug) & DVALUE(level)) \
        PUT(debug, __VA_ARGS__)

#define WITH_DEBUG_LEVEL(level)                            \
    for (int i=0; 0==i && (GLOBALS(debug) & DVALUE(level)) \
         ? 1                                               \
         : 0                                               \
         ; i++)


const char *GIT_SHA1 = "someversion";

static struct type_ptr_db {
    int                 last_base_type_uid;
    struct typen_data   *type_db;
    struct ptr_db_arr   ptr_db;
} type_ptr_db = {
    .last_base_type_uid = 0,  // to prevent free of non-heap based types
    .type_db            = NULL,
    .ptr_db             = { .alloc_size=0, .last=0, .heads=NULL },
};
typedef struct type_ptr_db *type_ptr_db_t;


#define IDENTITY(what)       (what)
#define APPLY(next,...)      next(__VA_ARGS__)

#define APPLY_DEF_N0_(fnc,               X)  X(fnc)(              )
#define APPLY_DEF_N1_(fnc,a1,            X)  X(fnc)(a1            )
#define APPLY_DEF_N2_(fnc,a1,a2,         X)  X(fnc)(a1,a2         )
#define APPLY_DEF_N3_(fnc,a1,a2,a3,      X)  X(fnc)(a1,a2,a3      )
#define APPLY_DEF_N4_(fnc,a1,a2,a3,a4,   X)  X(fnc)(a1,a2,a3,a4   )
#define APPLY_DEF_N5_(fnc,a1,a2,a3,a4,a5,X)  X(fnc)(a1,a2,a3,a4,a5)
#define APPLY_DEF_R0_(fnc,ret,...)  ret = APPLY_DEF_N0_(fnc,__VA_ARGS__)
#define APPLY_DEF_R1_(fnc,ret,...)  ret = APPLY_DEF_N1_(fnc,__VA_ARGS__)
#define APPLY_DEF_R2_(fnc,ret,...)  ret = APPLY_DEF_N2_(fnc,__VA_ARGS__)
#define APPLY_DEF_R3_(fnc,ret,...)  ret = APPLY_DEF_N3_(fnc,__VA_ARGS__)
#define APPLY_DEF_R4_(fnc,ret,...)  ret = APPLY_DEF_N4_(fnc,__VA_ARGS__)
#define APPLY_DEF_R5_(fnc,ret,...)  ret = APPLY_DEF_N5_(fnc,__VA_ARGS__)

/* facade + reusable enumeration for global CL API (subset used) */

#define CL_API_FNC(fnc)        (GLOBALS(cl_api).fnc)
#define CL_API(...)            CL_API_(__VA_ARGS__,CL_API_FNC)
#define CL_API_(fnc, ...)      APPLY(CL_API__,CL_API_CNT(fnc),fnc,__VA_ARGS__)
#define CL_API__(kind,fnc,...) APPLY_DEF_N##kind##_(fnc,__VA_ARGS__)

#define CL_API_CNT(fnc)              CL_API_##fnc
#define CL_API_global_init           1
#define CL_API_global_init_defaults  2
#define CL_API_code_listener_create  1
#define CL_API_chain_create          0
#define CL_API_chain_append          2
#define CL_API_global_cleanup        0

#define CL_API_PROCEED(proceed)        \
    do {                               \
        proceed(global_init);          \
        proceed(global_init_defaults); \
        proceed(code_listener_create); \
        proceed(chain_create);         \
        proceed(chain_append);         \
        proceed(global_cleanup);       \
    } while (0)

/* facade + reusable enumeration for emitting part of CL API (subset used) */

#define EMIT_FNC(fnc)        (GLOBALS(cl)->fnc)
#define EMIT(...)            EMIT_(__VA_ARGS__,EMIT_FNC)
#define EMIT_(fnc,...)       APPLY(EMIT__,EMIT_KIND(fnc),fnc,__VA_ARGS__)
#define EMIT__(kind,fnc,...) APPLY_DEF_N##kind##_(fnc,GLOBALS(cl),__VA_ARGS__)

// NOTE: +1 for common cl argument
#define EMIT_KIND(fnc)          EMIT_##fnc
#define EMIT_file_open          2
#define EMIT_file_close         1
#define EMIT_fnc_open           2
#define EMIT_fnc_arg_decl       3
#define EMIT_fnc_close          1
#define EMIT_bb_open            2
#define EMIT_insn               2
#define EMIT_insn_call_open     4
#define EMIT_insn_call_arg      3
#define EMIT_insn_call_close    1
#define EMIT_insn_switch_open   3
#define EMIT_insn_switch_case   5
#define EMIT_insn_switch_close  1
#define EMIT_acknowledge        1
#define EMIT_destroy            1

#define EMIT_PROCEED(proceed)       \
    do {                            \
        proceed(file_open);         \
        proceed(file_close);        \
        proceed(fnc_open);          \
        proceed(fnc_arg_decl);      \
        proceed(fnc_close);         \
        proceed(bb_open);           \
        proceed(insn);              \
        proceed(insn_call_open);    \
        proceed(insn_call_arg);     \
        proceed(insn_call_close);   \
        proceed(insn_switch_open);  \
        proceed(insn_switch_case);  \
        proceed(insn_switch_close); \
        proceed(acknowledge);       \
        proceed(destroy);           \
    } while (0)

/* facade + reusable enumeration for sparse API (subset used) */

#define SPARSE_API(...)            SPARSE_API_(__VA_ARGS__,IDENTITY)
#define SPARSE_API_(fnc,...)       APPLY(SPARSE_API_OUT(fnc),SPARSE_API_KIND(fnc),fnc,__VA_ARGS__)
#define SPARSE_API__(kind,fnc,...) APPLY_DEF_##kind##_(fnc,__VA_ARGS__)
#define SPARSE_API_D(kind,fnc,...)           \
    WITH_SWAPPED_STREAM(out, debug)          \
        APPLY_DEF_##kind##_(fnc,__VA_ARGS__)
#define SPARSE_API_E(kind,fnc,...)           \
    WITH_SWAPPED_STREAM(sparse, err)         \
        APPLY_DEF_##kind##_(fnc,__VA_ARGS__)

#define SPARSE_API_KIND(fnc)       APPLY(SPARSE_API_KIND_,SPARSE_API_##fnc)
#define SPARSE_API_KIND_(kind,argcnt,out)  kind##argcnt
#define SPARSE_API_OUT(fnc)        APPLY(SPARSE_API_OUT_,SPARSE_API_##fnc)
#define SPARSE_API_OUT_(kind,argcnt,out)  SPARSE_API_##out

#define SPARSE_API_sparse_initialize  R,3,E
#define SPARSE_API_sparse             R,1,E
#define SPARSE_API_expand_symbol      N,1,E
#define SPARSE_API_linearize_symbol   R,1,E
#define SPARSE_API_unssa              N,1,E
#define SPARSE_API_set_up_storage     N,1,E
#define SPARSE_API_free_storage       N,0,_
#define SPARSE_API_show_symbol        N,1,D

#define SPARSE_API_PROCEED(proceed)  \
    do {                             \
        proceed(sparse_initialize);  \
        proceed(sparse);             \
        proceed(expand_symbol);      \
        proceed(linearize_symbol);   \
        proceed(unssa);              \
        proceed(set_up_storage);     \
        proceed(free_storage);       \
        proceed(show_symbol);        \
    } while (0)

/* Context Managers ala Python */

#define SWAP_STREAM(f1, f2)  swap_stream(STREAM(f1), STREAM(f2))

#define WITH_SWAPPED_STREAM(f1,f2)  \
    for (int i=0; 0==i              \
         ? (SWAP_STREAM(f1, f2), 1) \
         : (SWAP_STREAM(f2, f1), 0) \
         ; i++)

#define WITH_OBJECT(object)               \
    for (int i=0; 0==i                    \
         ? (object##_init(&object), 1)    \
         : (object##_destroy(&object), 0) \
         ; i++)

#define WITH_FILE_TO_EMIT(file)       \
    for (int i=0; i==0                \
         ? (EMIT(file_open, file), 1) \
         : (EMIT(file_close),      0) \
         ; i++)

#define WITH_CALL_TO_EMIT(loc, dst, fnc)            \
    for (int i=0; 0==i                              \
         ? (EMIT(insn_call_open, loc, dst, fnc), 1) \
         : (EMIT(insn_call_close),               0) \
         ; i++)

#define WITH_SWITCH_TO_EMIT(loc, op)            \
    for (int i=0; 0==i                          \
         ? (EMIT(insn_switch_open, loc, op), 1) \
         : (EMIT(insn_switch_close),         0) \
         ; i++)

#define WITH_FNC_TO_EMIT(fnc)       \
    for (int i=0; 0==i              \
         ? (EMIT(fnc_open, fnc), 1) \
         : (EMIT(fnc_close),     0) \
         ; i++)


//
// Initialization/deinitialization functions
//

/* freeing resources connected with type */

static void
free_type(struct cl_type *clt)
{
    // skip base types that are not on heap
    if (clt->uid > type_ptr_db.last_base_type_uid) {

        /* clt->name */
        free((char *) clt->name);

        /* clt->items */
        // selective approach can expose wrong usage through leaked memory
        BEGIN_WHEN(clt->code IN (CL_TYPE_PTR    OR
                                 CL_TYPE_STRUCT OR
                                 CL_TYPE_UNION  OR
                                 CL_TYPE_ARRAY  OR
                                 CL_TYPE_FNC    ))
        {
            int i;
            for (i = 0; i < clt->item_cnt; i++) {
                /* clt->items[i].type (skipped) */

                /* clt->items[i].name */
                free((char *) clt->items[i].name);
            }
            free(clt->items);
        }
        END_WHEN

        /* clt (heap!) */
        free(clt);
    }
}

static void
type_ptr_db_destroy(type_ptr_db_t db)
{
    typen_destroy(db->type_db);

    // destroy pointer hierarchy
    struct ptr_db_arr *ptr_db = &db->ptr_db;
    struct ptr_db_item *item, *item_next;
    int i;
    for (i = 0; i < ptr_db->last; i++) {
        item = &ptr_db->heads[i];

        /* item->clt (skipped, except for those explicitly flagged) */
        if (item->free_type)
            free_type(item->clt);

        /* item->arr */
        int j;
        for (j = 0; j < item->arr_cnt; j++)
            free(item->arr[j]);
        free(item->arr);

        // move onto next items, this one captured by `free(db->ptr_db.heads)'
        item = item->next;

        while (item) {
            item_next = item->next;

            /* item->clt (skipped, except for those explicitly flagged) */
            if (item->free_type)
                free_type(item->clt);

            free(item);
            item = item_next;
        }
    }
    free(ptr_db->heads);
}


//
// worker-initiated setup, mainly regarding Code Listener
//

// NOTE: stdout and stderr skipped
static void
atexit_worker(void)
{
#if 0
    // TODO
    type_ptr_db_destroy();
#endif
    // close the streams used by the worker
    int fileno_cur;
    for (enum streams s=stream_first; s < stream_last; s++) {
        fileno_cur = fileno(GLOBALS(stream[s]));
        if (GLOBALS(stream[s]) && fileno_cur > STDERR_FILENO) {
            // prevent double-close of particular file descriptor
            enum streams sn = s+1;
            for ( ; sn < stream_last; sn++)
                if (fileno_cur == fileno(GLOBALS(stream[sn])))
                    break;
            if (sn == stream_last)
                fclose(GLOBALS(stream[s]));
        }
    }
    // close the access to dynamic libraries
    for (size_t i=0; i < GLOBALS(cl_libs.cnt); i++)
        if (GLOBALS(cl_libs.handles)[i]
          && 0 != dlclose(GLOBALS(cl_libs.handles)[i]))
            PUT(err, "dlclose: %s", dlerror());
}

void
atexit_cl(void) {
    if (GLOBALS(cl)) {
        EMIT(destroy);
        GLOBALS(cl) = NULL;
        CL_API(global_cleanup);
    }
}

// close (->flush to buffer) deferred sparse output and print it
// NOTE: more options here, e.g., extra line prefix
// NOTE: must be called before atexit_worker
void
atexit_sparse(void) {
    // to be sure if deferred stream was opened and used,
    // we fflush the stream and examine size of respective buffer
    errno = 0;
    if (STREAM(sparse)
      && EOF != fflush(STREAM(sparse))
      && 0 < GLOBALS(deferred.size)) {
        if (EOF == fclose(STREAM(sparse)))
            PUT(err, "%s: could not fclose", __func__);
        STREAM(sparse) = NULL;

        PUT(err,"sparse output:");
        PUT(err,"%.*s", (int) GLOBALS(deferred.size), GLOBALS(deferred.buffer));

        free(GLOBALS(deferred.buffer));
        GLOBALS(deferred) = (struct g_deferred) { .buffer=NULL, .size=0 };
    } else if (0 != errno)
        PUT(err, "%s: could not fflush", __func__);
}

static inline void
setup_stream(FILE **stream, int fd)
{
    switch (fd) {
        case STDOUT_FILENO: *stream = stdout; break;
        case STDERR_FILENO: *stream = stderr; break;
        default:
            *stream = fdopen(fd, "a");
            if (!*stream)
                // incl. FD_UNDEF
                DIE( ERRNO("fdopen") );
            break;
    }
}


//
// Empty composite values
//

// TODO: cl_loc_unknown
#define EMPTY_LOC  { .file=NULL, .line=-1, .column=-1, .sysp=false }

static const struct cl_type pristine_cl_type = {
    .uid        = NEW_UID,  ///< in control of type_enumerator
    .code       = CL_TYPE_UNKNOWN,
    .loc        = EMPTY_LOC,
    .scope      = CL_SCOPE_GLOBAL,
    .name       = NULL,
    .size       = 0,
    .item_cnt   = 0,
    .items      = NULL,
    //.array_size = 0,
};


//
// Warnings, failures handling
//

// TODO: pos
#define WARN_UNHANDLED(pos, what) do { \
    /*warn(pos, "warning: '%s' not handled", what);*/ \
    fprintf(stderr, \
            "%s:%d: note: raised from function '%s' [internal location]\n", \
            __FILE__, __LINE__, __FUNCTION__); \
} while (0)

#define WARN_UNHANDLED_SYM(sym) \
    WARN_UNHANDLED((sym)->pos, show_ident((sym)->ident))

#define WARN_VA(pos, fmt, ...) do {\
    /*warn(pos, "warning: " fmt, __VA_ARGS__);*/ \
    fprintf(stderr, \
            "%s:%d: note: raised from function '%s' [internal location]\n", \
            __FILE__, __LINE__, __FUNCTION__); \
} while (0)

#define WARN_CASE_UNHANDLED(pos, what) \
    case what: WARN_UNHANDLED(pos, #what); break;

static inline void
swap_stream(FILE *f1, FILE *f2) {
    int fd1 = fileno(f1), fd2 = fileno(f2);
    if (fd1 == fd2)
        return;

    fflush(f1); fflush(f2);
    int temp = dup(fd1);
    if (-1 == temp
     || -1 == close(fd1)
     || -1 == dup2(fd2, fd1)
     || -1 == dup2(temp, fd2)
     || -1 == close(temp))
        DIE( ERRNO("swap_stream") );
}


//
// Mostly sparse related helper functions
//

// this should accommodate worst-case of pointer hexa reprezentation incl. \0
#define PTR_STRING_MAX  21

// incl. compile-time constraint check to make sure we fit into PTR_STRING_MAX
struct ptr_string {
    char str[sizeof(ptrdiff_t) <= 64 ? PTR_STRING_MAX : -1];
};

#define PTR_STRING(ptr)  (char const*const) ptr_string(ptr).str

// NOTE: returning a short array through stack, but should not hurt anything
static inline struct ptr_string
ptr_string(const void *ptr)
{
    struct ptr_string ret;
    if (0 >= snprintf(ret.str, PTR_STRING_MAX, "%p", ptr))
        DIE("snprintf");
    return ret;
}

static inline int
sizeof_from_bits(int bits)
{/* Alternative:
  * bytes_to_bits (sparse/target.h)
  *     - cons: we need the ceil value (1 bit ~ 1 byte), 0 in "strange" cases
  */
    return (bits > 0) ? (bits + bits_in_char - 1) / bits_in_char : 0;
}

static void
sparse_location(struct cl_loc *cl_loc, struct position pos)
{
    cl_loc->file   = stream_name(pos.stream);
    cl_loc->line   = pos.line;
    cl_loc->column = pos.pos;
    cl_loc->sysp   = /* not used by SPARSE */ false;
}

static void
sparse_scope(enum cl_scope_e *cl_scope, struct scope *scope)
{
    if (!scope || scope == global_scope)
        *cl_scope = CL_SCOPE_GLOBAL;
    else if (scope == file_scope)
        *cl_scope = CL_SCOPE_STATIC;
    else if (scope == function_scope)
        CL_TRAP;
    else if (scope == block_scope)
        CL_TRAP;
    else
        // FIXME
        *cl_scope = CL_SCOPE_FUNCTION;
}

static inline const char *
sparse_string(const struct string *str)
{/* Alternative:
  * show_string (sparse/token.h)
  *     - cons: character escaping, is debug about empty string
  */
    return (str->length) ? strndup(str->data, str->length) : NULL;
}

static inline const char *
sparse_ident(const struct ident *ident)
{/* Alternative:
  * show_ident (sparse/token.h)
  *     - cons: is debug about empty identifier string
  */
    return (ident && ident->len) ? strndup(ident->name, ident->len) : NULL;
}

static struct symbol *
sparse_fn_arg_at(struct symbol *fn, int pos)
{
    struct symbol *sym, *retval = NULL;

    if (pos <= 0)
        return NULL;

    // FIXME: lot of possible but missing checks
    // alternative: use also symbol->arg_count on SYM_FN
    FOR_EACH_PTR(fn->ctype.base_type->arguments, sym) {
        if (!--pos)
            retval = sym;
    } END_FOR_EACH_PTR(sym);
    return retval;
}


//
// Types handling
//

/* sparse - code listener types mapping */

// associated with respective base types in `populate_with_base_types()'
static struct cl_type
    void_clt,
    incomplete_clt,
    bad_clt,
    int_clt,  sint_clt,  uint_clt,     short_clt, sshort_clt, ushort_clt,
    long_clt, slong_clt, ulong_clt,    llong_clt, sllong_clt, ullong_clt,
    // lllong_clt, slllong_clt, ulllong_clt
    char_clt, schar_clt, uchar_clt,
    bool_clt,
    float_clt, double_clt, ldouble_clt;


static const struct {
    struct cl_type  *ref;
    struct symbol   *ctype;
    enum cl_type_e  cl_type;
    const char      *name;
} base_types[] = {
/* Synopsis:
 * sparse/symbol.c (ctype_declaration) + sparse/symbol.h
 *
 * Omitted:
 * - type_ctype
 * - string_ctype..lazy_ptr_type (should not be present at all [?])
 *
 * Note:
 * `lllong' represents non-standard "extra long" at specific platforms [?]
 */
#define TYPE(sym, clt)  { &sym##_clt, &sym##_ctype, CL_TYPE_##clt, #sym }
    /* CL_TYPE_VOID */
    TYPE(void, VOID),

    /* CL_TYPE_UNKNOWN */
    TYPE(incomplete, UNKNOWN),
    TYPE(bad,        UNKNOWN),

    /* CL_TYPE_INT */
    TYPE(int,    INT),   TYPE(sint,    INT),   TYPE(uint,    INT),
    TYPE(short,  INT),   TYPE(sshort,  INT),   TYPE(ushort,  INT),
    TYPE(long,   INT),   TYPE(slong,   INT),   TYPE(ulong,   INT),
    TYPE(llong,  INT),   TYPE(sllong,  INT),   TYPE(ullong,  INT),
    //TYPE(lllong, INT),   TYPE(slllong, INT),   TYPE(ulllong, INT),

    /* CL_TYPE_CHAR */
    TYPE(char,   CHAR),  TYPE(schar,   CHAR),  TYPE(uchar,  CHAR),

    /* CL_TYPE_BOOL */
    TYPE(bool, BOOL),

    /* CL_TYPE_REAL */
    TYPE(float,   REAL),
    TYPE(double,  REAL),
    TYPE(ldouble, REAL),
#undef TYPE
};

/* type "constructor" */

static inline struct cl_type *
empty_type(struct cl_type* clt)
{
    *clt = pristine_cl_type;
    return clt;
}

static inline struct cl_type *
new_type(void)
{
    struct cl_type *retval;
    return empty_type((MEM_NEW(retval)));  // guaranteed not to return NULL
}

static struct cl_type *
type_ptr_db_insert(type_ptr_db_t db, struct cl_type *clt,
                   const struct symbol *type, struct ptr_db_item **ptr);

static void
populate_with_base_types(type_ptr_db_t db)
{
    struct symbol *ctype;
    struct cl_type *clt;
    int i;
    for (i = 0; i < ARRAY_SIZE(base_types); i++) {
        clt = base_types[i].ref;
        empty_type(clt);

        ctype = base_types[i].ctype;

        clt->code  = base_types[i].cl_type;
        clt->scope = CL_SCOPE_GLOBAL;
        clt->name  = base_types[i].name;
        clt->size  = sizeof_from_bits(ctype->bit_size);

        // insert into hash table + pointer hierarchy (at base level)
        type_ptr_db_insert(db, clt, ctype, NULL);
    }

    // set uid of the last type inserted so we can skip the freeing for these
    db->last_base_type_uid = clt->uid;
}


static void
type_ptr_db_init(type_ptr_db_t db)
{
    db->type_db = typen_create(free_type);
    if (!db->type_db)
        DIE( ECODE(ec_tdb, "ht_create() failed") );

    // fill with base types
    populate_with_base_types(db);
}


/* various helpers */

static inline const struct symbol *
type_unwrap(const struct symbol *raw_type)
{/* See also:
  * sparse/symbol.h: get_sym_type()
  */
    if (!raw_type)
        CL_TRAP;

    const struct symbol *retval = raw_type;
    while (retval->type == SYM_NODE || retval->type == SYM_BITFIELD
           /*retval->type == SYM_ENUM */)
        retval = retval->ctype.base_type;

    return retval;
}

static inline bool
type_match(const struct cl_type *t1, const struct cl_type *t2)
{
    if (t1 == t2)
        return true;

#if USE_EXTENDED_TYPE_CMP
    if (t1->code == t2->code && t1->item_cnt == t2->item_cnt
        && t1->item_cnt > 0) {
        int i;
        for (i = 0; i < t1->item_cnt; i++)
            if (!type_match(t1->items[i].type, t2->items[i].type))
                return false;
        return  true;
    }
#else
    return false;
#endif
}

// NOTE: clt->item_cnt can be uninitialized provided that clt->items is NULL
static inline struct cl_type_item *
type_append_item(struct cl_type *clt)
{
    if (!clt->items)
        clt->item_cnt = 0;

    // guaranteed to continue only in case of success
    return MEM_ARR_APPEND(clt->items, clt->item_cnt);
}


/* read composite types */

static struct cl_type *type_from_symbol(const struct symbol *type,
                                          struct ptr_db_item **ptr);

static struct cl_type_item *
read_and_append_subtype(struct cl_type *clt, struct symbol *subtype)
{
    struct cl_type_item *subtype_item = type_append_item(clt);
    subtype_item->type = type_from_symbol(subtype, NULL);
    subtype_item->name = sparse_ident(subtype->ident);

    if (clt->code == CL_TYPE_STRUCT || clt->code == CL_TYPE_UNION)
        subtype_item->offset = subtype->offset;

    return subtype_item;
}

static void
read_and_append_subtypes(struct cl_type *clt, struct symbol_list *subtypes)
{
    struct symbol *subtype;

    FOR_EACH_PTR(subtypes, subtype) {
        read_and_append_subtype(clt, subtype);
    } END_FOR_EACH_PTR(subtype);
}

static inline void
read_type_fnc(struct cl_type *clt, const struct symbol *raw_symbol,
              const struct symbol *type)
{
    read_and_append_subtype(clt, type->ctype.base_type);
    read_and_append_subtypes(clt, type->arguments);
    // XXX: probably convention in cl?
    read_and_append_subtype(clt, &void_ctype);
}

static inline void
read_type_array(struct cl_type *clt, const struct symbol *raw_symbol,
                const struct symbol *type)
{
    int sub_size;

    //CL_TRAP;
    //clt->name = sparse_ident(type->ident);

    if (raw_symbol->type == SYM_NODE)
        // normalize size of the "outer" dimension as well as missing size
        clt->size = sizeof_from_bits(raw_symbol->bit_size);
    sub_size = read_and_append_subtype(clt, type->ctype.base_type)->type->size;
    clt->array_size = clt->size/sub_size;
                      // clt->size/clt->items[0].type->size
}

static inline void
read_type_struct(struct cl_type *clt, const struct symbol *raw_symbol,
                 const struct symbol *type)
{
    clt->name = sparse_ident(type->ident);
    read_and_append_subtypes(clt, type->symbol_list);
}

static inline void
read_type_union(struct cl_type *clt, const struct symbol *raw_symbol,
                const struct symbol *type)
{
    //CL_TRAP;
    clt->name     = sparse_ident(type->ident);
    //TODO:
    read_and_append_subtypes(clt, type->symbol_list);
    //clt->item_cnt = /* TODO */ 0;
    //clt->items    = /* TODO */ NULL;
}

static inline void
read_type_enum(struct cl_type *clt, const struct symbol *raw_symbol,
               const struct symbol *type)
{
    clt->name = sparse_ident(type->ident);
}

static struct cl_type *
read_type(struct cl_type *clt, const struct symbol *raw_symbol,
          const struct symbol *type)
{
    typedef void (*type_converter)(struct cl_type *,
                                   const struct symbol * /*raw_symbol*/,
                                   const struct symbol * /*type*/);
    const struct type_conversion {
        enum cl_type_e      type_code;
        union {
            type_converter  converter;
            const char      *string;
        } prop;
    } type_conversions[] = {
    /* Synopsis:
     * sparse/symbol.h
     *
     * Note:
     * Unhandled types are denoted with CL_TYPE_UNKNOWN.
     */
    #define TYPE_STD(spt, clt, conv) \
        [SYM_##spt]={ .type_code=CL_TYPE_##clt, .prop.converter=conv }
    #define TYPE_IGN(spt, _, __) \
        [SYM_##spt]={ .type_code=CL_TYPE_UNKNOWN, .prop.string="SYM_"#spt }

        // how? | sparse type   | clt    | handler               |
        // -----+---------------+--------+-----------------------|

        /* these should not get there (?) */
        TYPE_IGN( UNINITIALIZED ,        ,                       ),
        TYPE_IGN( PREPROCESSOR  ,        ,                       ),
        TYPE_IGN( BASETYPE      ,        ,                       ),
        TYPE_IGN( NODE          ,        , /*unexpected in type*/),

        /* ready to handle */
        TYPE_STD( PTR           , PTR    , NULL /*set code only*/),
        TYPE_STD( FN            , FNC    , read_type_fnc         ),
        TYPE_STD( ARRAY         , ARRAY  , read_type_array       ),
        TYPE_STD( STRUCT        , STRUCT , read_type_struct      ),
        TYPE_STD( UNION         , UNION  , read_type_union       ),
        TYPE_STD( ENUM          , ENUM   , read_type_enum        ),

        /* what about these? */
        TYPE_IGN( TYPEDEF       ,        ,                       ),
        TYPE_IGN( TYPEOF        ,        ,                       ),
        TYPE_IGN( MEMBER        ,        ,                       ),
        TYPE_IGN( BITFIELD      ,        ,                       ),
        TYPE_IGN( LABEL         ,        ,                       ),
        TYPE_IGN( RESTRICT      ,        ,                       ),
        TYPE_IGN( FOULED        ,        ,                       ),
        TYPE_IGN( KEYWORD       ,        ,                       ),
        TYPE_IGN( BAD           ,        ,                       ),
    };

    const struct type_conversion *conversion;

    WITH_DEBUG_LEVEL(d_type) {
        PUT(debug,"\t%d: type to be processed:", type->pos.line);
        SPARSE_API(show_symbol, (struct symbol *) type);
    }

    //assert(PARTIALLY_ORDERED( SYM_UNINITIALIZED , symbol->type , SYM_BAD ));
    conversion = &type_conversions[type->type];

    // TODO: raw symbol?
    sparse_location(&clt->loc, type->pos);
    sparse_scope(&clt->scope, type->scope);

    clt->code = conversion->type_code;
    // TODO: raw_symbol?
    clt->size = sizeof_from_bits(type->bit_size);

    switch (conversion->type_code) {
        case CL_TYPE_UNKNOWN:
            CL_TRAP;
            WARN_UNHANDLED(type->pos, conversion->prop.string);
            clt->name = strdup(show_typename((struct symbol *)type));
            return clt;
        default:
            break;
    }

    if (conversion->prop.converter)
        conversion->prop.converter(clt, raw_symbol, type);

    return clt;
}


/* pointer and array DB */

static struct ptr_db_item *type_ptr_db_lookup_ptr(struct ptr_db_arr *ptr_db,
                                                  const struct cl_type *clt);

static inline struct ptr_db_item *
empty_ptr_db_item(struct ptr_db_item *item, struct cl_type *clt)
{
    item->clt       = clt;
    item->next      = NULL;
    item->arr_cnt   = 0;
    item->arr       = NULL;
    item->free_type = false;

    return item;
}

static struct ptr_db_item *
new_ptr_db_item(void)
{
    struct ptr_db_item *retval;
    // guaranteed not to return NULL
    return empty_ptr_db_item((MEM_NEW(retval)), NULL);
}

// Note: use `build_referenced_type' when possible
static inline struct cl_type *
referenced_type(const struct cl_type* orig_type)
{/* Problems/exceptions/notes:
  * 1. Reusing location and scope from `orig_type'.
  */

    struct cl_type *retval;
    MEM_NEW(retval);  // guaranteed not to return NULL

    retval->uid   = type_ptr_db.last_base_type_uid+1;
    retval->code  = CL_TYPE_PTR;
    retval->loc   = orig_type->loc;
    retval->scope = orig_type->scope;
    retval->name  = NULL;
    retval->size  = sizeof_from_bits(bits_in_pointer);
    retval->items = NULL;

    struct cl_type_item *item = type_append_item(retval);
    item->type    = orig_type;
    item->name    = NULL;

    // guaranteed not to return NULL
    return retval;
}

static struct cl_type *
build_referenced_type(struct cl_type *orig_clt)
{
    struct ptr_db_arr *ptr_db = &type_ptr_db.ptr_db;

    struct ptr_db_item *prev;
    prev = type_ptr_db_lookup_ptr(ptr_db, orig_clt);

    if (!prev->next) {
        prev->next = new_ptr_db_item();
        prev->next->clt = referenced_type(orig_clt);
        prev->next->free_type = true;
    }

    return prev->next->clt;
}

// for given type "clt", return respective item from pointer hierarchy;
// it is called only when we know such item will be there (already added)
static struct ptr_db_item *
type_ptr_db_lookup_ptr(struct ptr_db_arr *ptr_db, const struct cl_type *clt)
{
    if (clt->code == CL_TYPE_PTR)
        return type_ptr_db_lookup_ptr(ptr_db, clt->items->type)->next;

    size_t i;
    for (i = 0; i < ptr_db->last; i++)
        if (ptr_db->heads[i].clt == clt)
            break;

    if (i < ptr_db->last)
        return &ptr_db->heads[i];

    // not found ... should not happen
    CL_TRAP;
    return NULL;
}

static inline struct cl_type *
type_ptr_db_lookup_item(type_ptr_db_t db, const struct symbol *type,
                        struct ptr_db_item **ptr)
{
    struct cl_type *clt = typen_get_by_key(db->type_db, (void *) type);
    if (clt && ptr)
        *ptr = type_ptr_db_lookup_ptr(&db->ptr_db, clt);

    return clt;
}

static struct cl_type *
type_ptr_db_insert(type_ptr_db_t db, struct cl_type *clt,
                   const struct symbol *type, struct ptr_db_item **ptr)
#define PTRDBARR_SIZE  (128)
{
    WITH_DEBUG_LEVEL(d_insert_type) {
        PUT(debug,"add type (uid = %d, clt = %p): %p", clt->uid, (void *) clt,
            (void *) type);
        SPARSE_API(show_symbol, (struct symbol *) type);
    }

    struct cl_type *retval;
    int uid = clt->uid;

    retval = typen_insert_with_uid(db->type_db, clt, (void *) type);
    if (!retval)
        DIE( ECODE(ec_tdb, "typen_insert_with_uid() failed") );

    if (uid == NEW_UID && type->type != SYM_PTR) {
        // track this really new type also in the pointer hierarchy
        // (at the base level, i.e. no pointer, and respective pointers
        // will be captured in connected singly-linked list)
        struct ptr_db_arr *ptr_db = &db->ptr_db;
        if (!(ptr_db->alloc_size - ptr_db->last)) {
            ptr_db->alloc_size += PTRDBARR_SIZE;
            // guaranteed to continue only in case of success
            MEM_ARR_RESIZE(ptr_db->heads, ptr_db->alloc_size);
        }
        empty_ptr_db_item(&ptr_db->heads[ptr_db->last], clt);

        if (ptr)
            *ptr = &ptr_db->heads[ptr_db->last];

        ptr_db->last++;
    } else if (type->type == SYM_ARRAY /* && uid != NEW_UID */) {
        if (ptr)
            *ptr = type_ptr_db_lookup_ptr(&db->ptr_db, clt);
    }

    // guaranteed to NOT return NULL
    return retval;
}

static inline struct cl_type **
prepare_type_array_ptr(const struct symbol *raw_symbol,
                       struct ptr_db_item **ptr)
{
    struct cl_type **clt_ptr, *ptr_type = NULL;
    struct ptr_db_item *prev = NULL;
    const struct symbol *type = type_unwrap(raw_symbol);

    ptr_type = type_from_symbol(type->ctype.base_type, &prev);

    if (type->type == SYM_PTR) {
        if (!prev->next)
            prev->next = new_ptr_db_item();
        if (ptr)
            *ptr = prev->next;
        clt_ptr = &prev->next->clt;
    } else {
        // SYM_ARRAY
        int size = sizeof_from_bits(raw_symbol->bit_size)/ptr_type->size;
        size_t i;

        for (i = 0; i < prev->arr_cnt; i++)
            if (prev->arr[i]->arr_size == size)
                break;
        if (i == prev->arr_cnt) {
            // not found
            // 2x guaranteed to continue only in case of success
            MEM_ARR_RESIZE(prev->arr, prev->arr_cnt);
            MEM_NEW(prev->arr[i]);
            prev->arr[i]->arr_size = size;
            prev->arr[i]->clt = NULL;
        }
        clt_ptr = &prev->arr[i]->clt;
    }

    if (!*clt_ptr) {
        // new type to be read (no pointer/array alias found)
        *clt_ptr = read_type(new_type(), raw_symbol, type);

        // finalize SYM_PTR (not in `read_type()' as we have needed info here)
        if (type->type == SYM_PTR) {
            // use obtained dereferenced type
            struct cl_type_item *item = type_append_item(*clt_ptr);
            item->type = ptr_type;
            item->name = NULL;
        }
    }

    return clt_ptr;
}

// note: the only function that uses type_ptr_db global variable directly
static struct cl_type *
type_from_symbol(const struct symbol *raw_symbol, struct ptr_db_item **ptr)
{
    struct cl_type *clt, **clt_ptr;
    const struct symbol *type = type_unwrap(raw_symbol);

    // Fastest path, we have the type already in hash table
    clt = type_ptr_db_lookup_item(&type_ptr_db, type, ptr);
    if (clt)
        return clt;

    // Extra handling of pointer/arrays symbols, potentially fast circuit
    // for pointer/array alias (i.e., no allocation)
    if (type->type == SYM_PTR || type->type == SYM_ARRAY)
        clt_ptr = prepare_type_array_ptr(raw_symbol, ptr);
    else
        clt_ptr = &clt;

    bool is_new = (*clt_ptr == NULL);
    if (is_new)
        // any new type except for existing pointer/array alias
        *clt_ptr = new_type();

    clt = type_ptr_db_insert(&type_ptr_db, *clt_ptr, type, ptr);

    if (!is_new)
        return clt;  // existing pointer/array alias

    // Slow path for anything (except for pointers) which is being
    // proceeded for the first time (next time, hashed ctl is used instead)
    //
    // Important: these types are read ex-post in order to prevent recursion
    //            with, e.g., structures
    return read_type(clt, raw_symbol, type);
}

static inline struct cl_type *
type_from_instruction(struct instruction *insn, const pseudo_t pseudo)
{
    //struct pseudo_user *pu;

    // Note: pseudo->def == NULL for copy.32
    if (insn && insn->type) {

#if 0
        // TODO: for casts only?
        // first and most authoritative way of getting the type;
        // if the pseudo is the target pseudo, check whether its immediate
        // user/instruction has `orig_type' and use it if available
        if (insn->target == pseudo) {
            pu = (struct pseudo_user *)
                 PTR_ENTRY((struct ptr_list *) insn->target->users, 0);
            if (pu && pu->insn->orig_type)
                return type_from_symbol(pu->insn->orig_type, NULL);
        }
#endif

        if (PARTIALLY_ORDERED(OP_BINCMP, insn->opcodecase, OP_BINCMP_END))
            return &bool_clt;

        if (insn->opcode == OP_CALL) {
            // NOTE: experimental, mainly for alloc et al.
            // try to find immediatelly following OP_CAST
            // (normally suppressed) and set the type respectively
            if (ptr_list_size((struct ptr_list *) insn->target->users)) {
                struct pseudo_user *u;
                u = (struct pseudo_user *)PTR_ENTRY(insn->target->users,3);
                if (u->insn->opcode == OP_CAST)
                    return type_from_symbol(u->insn->type, NULL);
            }
        }
        return type_from_symbol(insn->type, NULL);
    } else {
        // type fallback
        return &int_clt;
    }
}


//
// operands handling
//

#define CST(op)      (&op->data.cst)
#define CST_INT(op)  (&CST(op)->data.cst_int)
#define CST_STR(op)  (&CST(op)->data.cst_string)
#define CST_FNC(op)  (&CST(op)->data.cst_fnc)
#define CST_REAL(op) (&CST(op)->data.cst_real)

#define VAR(op)      (op->data.var)

/* Sparse operands = pseudos */

static inline bool
pseudo_futile(pseudo_t pseudo)
{
    return !pseudo || pseudo == VOID;
}

static inline bool
pseudo_immediate(pseudo_t pseudo)
{
    return pseudo->type != PSEUDO_SYM && pseudo->type != PSEUDO_ARG;
}


/* operand "constructor" */

static inline struct cl_operand *
new_op(void)
{
    struct cl_operand *retval;
    // guaranteed not to return NULL
    return MEM_NEW(retval);
}

static inline struct cl_operand *
op_shallow_copy(const struct cl_operand *op_src)
{
    struct cl_operand *retval = new_op();
    *retval = *op_src;

    // guaranteed not to return NULL
    return retval;
}


/* freeing resources connected with operand */

static inline void free_op(struct cl_operand *op);

static void
op_free_initializers(struct cl_initializer *initial)
{
    // !!TODO API change
    /* initial->type (skipped) */

    if (!initial->nested_cnt) {
        /* initial->data.value (heap-based!) */
        free_op(initial->data.value);
    } else {
        /* initial->data.nested_initials */
        int i;
        for (i = 0; i < initial->nested_cnt; i++)
            if (initial->data.nested_initials[i])
                op_free_initializers(initial->data.nested_initials[i]);
    }

    /* initial (heap!) */
    free(initial);
}

static void
free_accessor_chain(struct cl_accessor *ac)
{
    struct cl_accessor *ac_next;
    while (ac) {
        ac_next = ac->next;

        /* ac->type (skipped) */
        /* ac->next (in the next round) */

        if (ac->code == CL_ACCESSOR_DEREF_ARRAY)
            /* ac->data.array.index (heap-based!) */
            free_op(ac->data.array.index);

        // free current and go to the next one in the chain
        free(ac);
        ac = ac_next;
    }
}

// Note: for freeing heap-based nested items only (see also `free_op')
static void
op_free_data(struct cl_operand *op)
{
    if (op->code == CL_OPERAND_VOID)
        return;

    /* op->type (skipped) */

    /* op->accessor */
    free_accessor_chain(op->accessor);

    if (op->code == CL_OPERAND_CST) {
        /* op->data.cst... */
        switch (op->data.cst.code) {
            case CL_TYPE_FNC:
                free((char *) CST_FNC(op)->name);
                break;
            case CL_TYPE_STRING:
                free((char *) CST_STR(op)->value);
                break;
            default:
                break;
        }
    } else if (op->code == CL_OPERAND_VAR) {
        /* op->data.var->name */
        free((char *) VAR(op)->name);
        /* op->data.var->initial... */
        if (VAR(op)->initial)
            op_free_initializers(VAR(op)->initial);

        /* op->data.var */
        free(VAR(op));
    }
}

// Note: *op expected to be heap-based (rare!)
static inline void
free_op(struct cl_operand *op)
{
    op_free_data(op);

    /* op (heap!) */
    free(op);
}


/* operator modifiers depending on the usage */

// Note: this is not easily extendable as everything else is uninitialized
static inline struct cl_operand *
op_make_void(struct cl_operand* op)
{
    op->code = CL_OPERAND_VOID;
    return op;
}

// Note: not to be used directly
static inline struct cl_operand *
op_make_cst(struct cl_operand *op)
{
    op->code     = CL_OPERAND_CST;
    op->accessor = NULL;

    return op;
}

static inline struct cl_operand *
op_make_cst_fnc(struct cl_operand *op, const struct symbol *sym)
{
    op_make_cst(op);

    op->type               = type_from_symbol(sym, NULL);
    CST(op)->code          = CL_TYPE_FNC;
    CST_FNC(op)->name      = sparse_ident(sym->ident);
    CST_FNC(op)->is_extern = MOD_EXTERN & sym->ctype.modifiers;
    CST_FNC(op)->uid       = (int)(long) sym;

    return op;
}

static inline struct cl_operand *
op_make_cst_int(struct cl_operand *op, int value)
{
    op_make_cst(op);

    op->type           = &int_clt;
    CST(op)->code      = CL_TYPE_INT;
    CST_INT(op)->value = value;

    return op;
}

static inline struct cl_operand *
op_make_cst_real(struct cl_operand *op, double value)
{
    op_make_cst(op);

    op->type            = &double_clt;
    CST(op)->code       = CL_TYPE_REAL;
    CST_REAL(op)->value = value;

    return op;
}

// TODO: make it accepting const char *
static inline struct cl_operand *
op_make_cst_string(struct cl_operand *op, struct expression *expr)
{
    op_make_cst(op);

    op->type           = type_from_symbol(expr->ctype, NULL); //XXX
    CST(op)->code      = CL_TYPE_STRING;
    CST_STR(op)->value = sparse_string(expr->string);

    return op;
}

// Note: type not (re)set; different semantics from `op_make_cst_*'
static inline struct cl_var *
op_make_var(struct cl_operand *op)
{
    op->code     = CL_OPERAND_VAR;
    op->accessor = NULL;

    MEM_NEW(VAR(op));  // guaranteed to continue only in case of success

    // initialize pointers checked by freeing helper
    VAR(op)->name       = NULL;
    VAR(op)->initial    = NULL;
    VAR(op)->artificial = true;

    // guaranteed not to return NULL
    return VAR(op);
}

static struct cl_operand *
op_use_initializer(struct cl_operand *op, struct expression *expr)
{
    if (!expr) {
        CL_TRAP;
        return op;
    }

    //CL_TRAP;
    switch (expr->type) {
        case EXPR_STRING:
            return op_make_cst_string(op, expr);
        default:
            CL_TRAP;
            return op;
    }
}

static struct cl_operand *
op_from_symbol_base(struct cl_operand *op, struct symbol *sym)
{
    sparse_scope(&op->scope, sym->scope);

    if (sym->bb_target || sym->type != SYM_NODE)
        CL_TRAP;

    // function not treated as a variable
    if (sym->ctype.base_type->type == SYM_FN)
        return op_make_cst_fnc(op, sym);

    // string literal
    if (!sym->ident)
        return op_use_initializer(op, sym->initializer);

    op->type = type_from_symbol(sym, NULL);

    struct cl_var *var = op_make_var(op);
    var->uid        = (int)(long) sym;
    var->name       = sparse_ident(sym->ident);
    var->artificial = false;
#if DO_EXTRA_CHECKS
    assert(var->name);
#endif

    return op;
}

static inline struct cl_operand *
op_from_symbol(struct cl_operand *op, struct symbol *sym)
{
    // !!TODO: simplify/API change
    return op_from_symbol_base(op, sym);
}

static inline struct cl_operand *
op_from_fn_argument(struct cl_operand *op, const pseudo_t pseudo)
{
    struct symbol *arg_sym;

    arg_sym = sparse_fn_arg_at(pseudo->def->bb->ep->name, pseudo->nr);
    if (!arg_sym)
        CL_TRAP;

    // XXX: op->scope       = CL_SCOPE_FUNCTION;
    // !!TODO: simplify/API change
    return op_from_symbol_base(op, arg_sym);
}

static struct cl_operand *
op_from_register(struct cl_operand *op, const struct instruction *insn,
                 const pseudo_t pseudo)
{/* Synopsis:
  * pseudo->def
  *
  */
    op->type = type_from_instruction(pseudo->def, pseudo);

    struct cl_var *var = op_make_var(op);
#if 1
    var->uid  = (int)(long) pseudo;
#else
    var->uid  = pseudo->nr;
#endif

    return op;
}

static inline struct cl_operand *
op_from_value(struct cl_operand *op, const struct instruction *insn, int value)
{
    // !!TODO: simplify/API change
    return op_make_cst_int(op, value);
}

static inline struct cl_operand *
op_from_pseudo(struct cl_operand *op, const struct instruction *insn,
               const pseudo_t pseudo)
{/* Synopsis:
  * sparse/linearize.h
  *
  * Problems/exceptions/notes:
  * 1. PSEUDO_VAL and PSEUDO_REG operands are not holding type information
  * S. Try to use insn->type, pseudo->def->type for PSEUDO_REG, ...
  */
    if (pseudo_futile(pseudo))
        return op_make_void(op);

    switch (pseudo->type) {

        /* real variables/literals (everything important accessible [?]) */

        case PSEUDO_SYM: return op_from_symbol(op, pseudo->sym);
        case PSEUDO_ARG: return op_from_fn_argument(op, pseudo);

        /* immediate values (some information may be hard/impossible to get) */

        case PSEUDO_REG: return op_from_register(op, insn, pseudo);
        case PSEUDO_VAL: return op_from_value(op, insn, /*XXX: from long long */
                                                        (int) pseudo->value);
#if 0
        case PSEUDO_PHI:
            WARN_UNHANDLED(insn->pos, "PSEUDO_PHI");
            break;
#endif
        default:
            // PSEUDO_PHI
            CL_TRAP;
            return op;
    }
}

static inline struct cl_operand *
op_from_expression(struct cl_operand *op, const struct instruction *insn,
                   const struct expression *expr)
{/* Synopsis:
  * sparse/linearize.c: show_instruction: case OP_SETVAL
  * sparse/show-parse.c: show_expression
  *
  * Problems/exceptions/notes:
  * FIXME: currently only EXPR_FVALUE handled
  */
    // !!TODO: simplify/API change
    switch (expr->type) {
        case EXPR_FVALUE:
            return op_make_cst_real(op, /*XXX: from long double */
                                        (double) expr->fvalue);
        default:
            CL_TRAP;
    }
    return op_make_void(op);
}

static struct cl_accessor *
new_cl_accessor()
{
    struct cl_accessor *retval;
    // guaranteed not to return NULL
    MEM_NEW(retval)->next = NULL;
    return retval;
}

static inline void
accessor_array_index(struct cl_accessor *ac, struct cl_loc loc, int index)
{
    ac->code                  = CL_ACCESSOR_DEREF_ARRAY;
    ac->data.array.index      = op_make_cst_int(new_op(), index);
}

static inline struct cl_accessor *
op_append_accessor(struct cl_operand *op, struct cl_accessor *ac)
{
    struct cl_accessor *ac_chain, **retval;

    if (!op->accessor)
        retval = &op->accessor;
    else {
        ac_chain = op->accessor;
        while (ac_chain->next)
            ac_chain = ac_chain->next;
        retval = &ac_chain->next;
    }

    if (ac)
        *retval = ac;
    else
        *retval = new_cl_accessor();

    // guaranteed not to return NULL
    return *retval;
}

static inline struct cl_accessor *
op_prepend_accessor(struct cl_operand *op, struct cl_accessor *ac)
{
    if (!ac)
        ac = new_cl_accessor();

    ac->next = op->accessor;
    op->accessor = ac;

    // guaranteed not to return NULL
    return ac;
}

// Note: returns UINT_MAX when operand could not be dug
static unsigned
op_dig_step(struct cl_operand *op, unsigned insn_offset)
{
    // `insn_offset' is consumed only by CL_TYPE_STRUCT or CL_TYPE_ARRAY;
    // e.g., accessing struct element is different with the first level
    // access (use insn_offset) and with other accesses (always zero offset)
    int retval = insn_offset,
        i = 0;

    struct cl_accessor *ac;

    #define MAP_ACCESSOR(acc, clt, cl_ac) \
        case CL_##clt: acc = new_cl_accessor(); acc->code = CL_##cl_ac;
    switch (op->type->code) {
        MAP_ACCESSOR(ac, TYPE_STRUCT, ACCESSOR_ITEM) {
            for (i = 0; i < op->type->item_cnt-1; i++)
                if (op->type->items[i].offset == insn_offset
                    || op->type->items[i+1].offset > insn_offset)
                    break;

            assert(op->type->items[i].offset <= insn_offset);

            // if item has not been found on exact offset match
            // (then `insn_offset' is expected to be greater than the offset
            // of the last proceeded item in the structure), the next digging
            // (really ought to be possible) will continue through this item

            ac->data.item.id = i;
            retval = insn_offset - op->type->items[i].offset;
            break;
        }
        MAP_ACCESSOR(ac, TYPE_ARRAY, ACCESSOR_DEREF_ARRAY) {
            div_t indexes;
            indexes = div(insn_offset, op->type->size/op->type->array_size);
            // !!TODO API change
            accessor_array_index(ac, op->loc, indexes.quot);
            // the remainder serves for next index-based-dereferencing rounds
            retval = indexes.rem;
            break;
        }
        MAP_ACCESSOR(ac, TYPE_PTR, ACCESSOR_DEREF) {
            if (insn_offset /* && op->type->items->type->size*/) {
                // convert into another accessor then predestined (ptr->arr),
                // but only if resulting index would be 1+
                div_t indexes = div(insn_offset, op->type->items->type->size);
                if (indexes.quot)
                    // !!TODO API change
                    accessor_array_index(ac, op->loc, indexes.quot);
                // the remainder serves for next index-based-deref. rounds
                retval = indexes.rem;
            }
            break;
        }
        default:
            return UINT_MAX;
    }

    op_append_accessor(op, ac);
    // accessor's type is the operand's type (it itself will be peeled off)
    ac->type = (struct cl_type *) op->type;
    // peel off one level of type/access decoration from the operand
    op->type = (struct cl_type *) op->type->items[i].type;

    return retval;
}

// XXX: removal candidate
static inline bool
op_accessible(const struct cl_operand *op)
{/* Problems/exceptions/notes:
  * None.
  */
    switch (op->type->code) {
        case CL_TYPE_STRUCT:
        case CL_TYPE_UNION:
        case CL_TYPE_ARRAY:
        case CL_TYPE_PTR:
            return true;
        default:
            return false;
    }
}

static unsigned
op_dig_for_type_match(struct cl_operand *op,
                      const struct cl_type *expected_type,
                      unsigned initial_offset)
{/* Problems/exceptions/notes:
  * When digging union, we go through its items, apply a DFS-based search
  * in order to get expected type on one, if it ends without success, we try
  * another (on the whole, should not end without success).
  */

    unsigned offset = initial_offset;

    while (!type_match(op->type, expected_type)) {
        if (op->type->code == CL_TYPE_UNION) {
            // unions bring non-determinism as there are more ways how to
            // "dig" -- use DFS with a sort of backtracking (through stack)
            struct cl_operand *op_clone;
            struct cl_accessor *ac;
            int i;
            size_t res;

            // `op_clone' is a special shallow copy with accessor chain reset
            op_clone = op_shallow_copy(op);

            for (i = 0; i < op->type->item_cnt; i++) {
                op_clone->accessor = NULL;
                ac               = op_append_accessor(op_clone, NULL);
                ac->code         = CL_ACCESSOR_ITEM;
                ac->type         = op_clone->type;
                ac->data.item.id = i;
                op_clone->type = (struct cl_type *) ac->type->items[i].type;

                res = op_dig_for_type_match(op_clone, expected_type, offset);

                if (UINT_MAX != res)
                    // successfull case of digging
                    break;

                // restore for the next round
                free_accessor_chain(op_clone->accessor);
                op_clone->type = op->type;
            }

            if (UINT_MAX != res) {
                // reflect the changes collected within successful DFS trace
                // (with `op_clone') back to its preimage `op'
                op->type = op_clone->type;
                assert(op_clone->accessor);
                op_append_accessor(op, op_clone->accessor);
                assert(type_match(op->type, expected_type));
            }

            free(op_clone);
            offset = res;
        } else
            offset = op_dig_step(op, offset);

        if (UINT_MAX == offset)
            break;
    }

    return offset;
}


//
// Instructions handling
//

enum assignment_ops_handling {
    TYPE_LHS_KEEP        = (1 << 0),
    TYPE_RHS_KEEP        = (1 << 1),

    /* LHS */

    // to obtain operand "in a right form", either dig into original one
    // and find the expected inner type item (for "non-immediate" PSEUDO_SYM
    // and PSEUDO_ARG) or keep it and add a dereference accessor
    // (for "immediate" PSEUDO_VAL and PSEUDO_REG);
    //
    // usage: INSN_STORE
    TYPE_LHS_DIG         = (1 << 2),

    /* RHS */

    // to obtain operand "in a right form", dig into original one and find
    // the expected inner type item; when combined with TYPE_RHS_DIG_ALL, this
    // applies for any pseudo type, for "non-immediate" PSEUDO_SYM
    // and PSEUDO_ARG only otherwise
    //
    // usage: all assign instructions except for INSN_COPY
    TYPE_RHS_DIG         = (1 << 3),
    TYPE_RHS_DIG_ANY     = (1 << 4),

    // to obtain operand "in a right form", add a level of pointer indirection
    // (i.e., reference the current one, add a reference accessor);
    // with INSN_STORE (that uses also TYPE_RHS_DIG), this has a special
    // meaning telling that this will be done only after a level
    // of indirection has been successfully removed first (so it
    // is effectively returned back)
    //
    // usage: INSN_STORE, INSN_PTR_CAST
    TYPE_RHS_REFERENCE = (1 << 5),
};

static bool insn_assignment_base(struct cl_insn *cli,
                                 const struct instruction *insn,
                                 pseudo_t lhs, pseudo_t rhs,
                                 enum assignment_ops_handling ops_handling);

/* Helpers for frequently emitted instructions (position filled in advance) */

static inline void
emit_insn_jmp(struct cl_insn *cli, const char *label)
{
    cli->code                = CL_INSN_JMP;
    cli->data.insn_jmp.label = label;

    EMIT(insn, cli);
}

static inline void
emit_insn_cond(struct cl_insn *cli, struct cl_operand *op_cond,
               const char *then_label, const char *else_label)
{
    cli->code                      = CL_INSN_COND;
    cli->data.insn_cond.src        = op_cond;
    cli->data.insn_cond.then_label = then_label;
    cli->data.insn_cond.else_label = else_label;

    EMIT(insn, cli);
}

static inline void
emit_insn_copy(struct cl_insn *cli, const struct instruction *insn,
               pseudo_t lhs, pseudo_t rhs)
{
    cli->code                = CL_INSN_UNOP;
    cli->data.insn_unop.code = CL_UNOP_ASSIGN;
    insn_assignment_base(cli, insn,
        lhs,           /* := */  rhs,
        TYPE_LHS_KEEP      |     TYPE_RHS_KEEP
    );
}

/* Functions dedicated to sparse assignment-like instructions */

static void
insn_assignment_mod_rhs(struct cl_operand *op_rhs, pseudo_t rhs,
                        const struct instruction *insn,
                        enum assignment_ops_handling ops_handling)
{/* Synopsis: see `insn_assignment_base' (the only caller)
  */
    if (ops_handling & TYPE_RHS_KEEP)
        return;

    int offset = insn->offset;
    bool use_rhs_dereference = true;
#if 0
    struct cl_type *type = (insn->opcode == OP_PTRCAST)
                               ? type_from_symbol(insn->orig_type, NULL)
                               : type_from_symbol(insn->type, NULL);
#endif
    struct cl_type *type = (insn->orig_type)
                               ? type_from_symbol(insn->orig_type, NULL)
                               : type_from_symbol(insn->type, NULL);

    // dig rhs (when applicable)
    if (ops_handling & TYPE_RHS_DIG) {
        if (!pseudo_immediate(rhs) || ops_handling & TYPE_RHS_DIG_ANY) {
            const struct cl_type *expected_type = type;

            if (ops_handling & TYPE_RHS_REFERENCE) {
                // remove one level of indirection of both resulting_type
                // and operand type (to be compensated by adding one back
                // in "reference rhs" part)
                offset = 0;
                if (!type_match(op_rhs->type, expected_type)) {
                    expected_type = expected_type->items->type;
                    // XXX: second condition yields better results
                    //      with tests/struct/rs1-03 but makes
                    //      tests/predator/test-0044.c fail
                    if (!type_match(op_rhs->type, expected_type)
                        /*|| op_accessible(op_rhs)*/)
                        op_dig_step(op_rhs, offset);
                } else
                    use_rhs_dereference = false;
            }
            unsigned res = op_dig_for_type_match(op_rhs, expected_type, offset);
            if (res == UINT_MAX) {
                // no success when digging operand for type match,
                // it may be a pointer and we just haven't been told this
                // type information (e.g., due to typeless PSEUDO_VAL)
                if (op_rhs->type->code == CL_TYPE_INT
                    && expected_type->code == CL_TYPE_PTR) {
                    struct cl_accessor *ac;
                    struct cl_type *expected_type_dug;

                    // promote an operand type to a pointer and remove
                    // a level of pointer indirection also from expected type
                    // XXX: should be the base type switched to void?
                    op_rhs->type = build_referenced_type(op_rhs->type);
                    expected_type_dug = (struct cl_type *)
                                        expected_type->items->type;

                    while (expected_type_dug->code == CL_TYPE_PTR) {
                        // now, we do the same but explicitly adding
                        // dereferences, adjusting the level of dereferences
                        // in operand's type
                        ac = op_prepend_accessor(op_rhs, NULL);
                        ac->code = CL_ACCESSOR_DEREF;
                        ac->type = op_rhs->type;

                        op_rhs->type = build_referenced_type(op_rhs->type);
                        expected_type_dug = (struct cl_type *)
                                            expected_type_dug->items->type;
                    }
                } else
                    CL_TRAP;  // should not happen
            }

        } else if (ops_handling & TYPE_RHS_REFERENCE) {
            // OP_STORE with PSEUDO_VAL rhs (e.g., value can be pointer)
            if (rhs->type == PSEUDO_VAL)
                op_rhs->type = type;  // probably no other choice
#if DO_EXTRA_CHECKS
            else if (!type_match(op_rhs->type, type))
                CL_TRAP;  // should be the same type
#endif
            use_rhs_dereference = false;
        }
    }

    // reference rhs (when applicable)
    if (ops_handling & TYPE_RHS_REFERENCE && use_rhs_dereference) {
        // OP_PTRCAST, OP_STORE (for PSEUDO_SYM and PSEUDO_ARG only
        //                       and only when returning level of indirection)
        struct cl_accessor *ac = op_append_accessor(op_rhs, NULL);
        ac->code = CL_ACCESSOR_REF;
        ac->type = op_rhs->type;
        op_rhs->type = build_referenced_type(op_rhs->type);
#if DO_EXTRA_CHECKS
        if (!type_match(op_rhs->type, type))
            CL_TRAP;  // should be the same type
#endif
    }
}

static bool
insn_assignment_base(struct cl_insn *cli, const struct instruction *insn,
                     pseudo_t lhs,    /* := */    pseudo_t rhs,
                     enum assignment_ops_handling ops_handling)
{/* Synopsis (see also the callers):
  * [input] OP_LOAD, OP_STORE, OP_COPY + casts (CAST, SCAST, FPCAST, PTRCAST)
  *     insn->type (not for OP_COPY):       type of final assigned value
  *     insn->orig_type (OP_PTRCAST only):  original type of value to assign
  * [output] CL_INSN_UNOP (set by [transitive] caller, as with location)
  *     data.insn_unop.code (CL_UNOP_ASSIGN, set by [transitive] caller)
  *     data.insn_unop.dst ~ lhs
  *     data.insn_unop.src ~ rhs
  *
  * Problems/exceptions/notes:
  * 1. Problem with a "right form" of both the operands (whether to consider
  *    the whole struct or its first element, etc.); additionally, some
  *    instructions requires (de)referencing of the operands explicitly.
  * S. The way to obtain "right form" of both operands is driven by
  *    `ops_handling' (see `enum assignment_ops_handling').
  *    If appropriate, combine `op_dig_for_type_match' and `insn->type'
  *    (`insn->orig_type') for this adjustment.
  */
    struct cl_operand op_lhs, op_rhs;

    /* prepare LHS */

    cli->data.insn_unop.dst = op_from_pseudo(&op_lhs, insn, lhs);

    // dig lhs (when applicable)
    if (ops_handling & TYPE_LHS_DIG) {
        struct cl_type *type = type_from_symbol(insn->type, NULL);
        if (!op_accessible(&op_lhs)) {
            struct cl_accessor *ac = op_append_accessor(&op_lhs, NULL);
            ac->code = CL_ACCESSOR_DEREF;
            // note: no such clt easily accessible (contrary to previous case)
            ac->type = build_referenced_type(type);
            op_lhs.type = type;
        } else
            op_dig_for_type_match(&op_lhs, type, insn->offset);
    }

    /* prepare RHS (quite complicated compared to LHS) */

    cli->data.insn_unop.src = op_from_pseudo(&op_rhs, insn, rhs);
    insn_assignment_mod_rhs(&op_rhs, rhs, insn, ops_handling);

    /* emit assignment */

    // FIXME (SPARSE?):  sparse generates (due to execution model?) extra
    // instruction, e.g. "store %arg1 -> 0[num]" in case of "num == %arg1"
#if FIX_SPARSE_EXTRA_ARG_TO_MEM
    if (lhs->type != PSEUDO_SYM || rhs->type != PSEUDO_ARG
         || op_lhs.data.var->uid != op_rhs.data.var->uid)
#endif
        EMIT(insn, cli);
#if FIX_SPARSE_EXTRA_ARG_TO_MEM
    else
        WARN_VA(insn->pos, "instruction omitted: %s",
                show_instruction((struct instruction *) insn));
#endif

    op_free_data(&op_lhs);
    op_free_data(&op_rhs);

    return true;
}

static inline bool
handle_insn_store(struct cl_insn *cli, const struct instruction *insn)
{/* Synopsis (see also `insn_assignment_base'):
  * [input] OP_STORE
  *     insn->src:    target memory address (pointer to what is being assigned)
  *     insn->target: source of assignment
  *     insn->type:   type of value to be assigned
  *
  * Problems/exceptions/notes:
  * None.
  */
    //CL_TRAP;
    return insn_assignment_base(cli, insn,
        insn->src,     /* := */  insn->target,
        TYPE_LHS_DIG       |     (TYPE_RHS_DIG | TYPE_RHS_REFERENCE)
    );
}

static inline bool
handle_insn_load(struct cl_insn *cli, const struct instruction *insn)
{/* Synopsis (see also `insn_assignment_base'):
  * [input] OP_LOAD
  *     insn->target ... register (XXX: only?) to be assigned
  *     insn->src    ... mem. address containing source value
  *     insn->type   ... type of value to be assigned
  *
  * Problems/exceptions/notes:
  * None.
  */
    if (insn->target->type != PSEUDO_REG)
        CL_TRAP;

    return insn_assignment_base(cli, insn,
        insn->target,  /* := */  insn->src,
        TYPE_LHS_KEEP      |     (TYPE_RHS_DIG | TYPE_RHS_DIG_ANY)
    );
}

static inline bool
handle_insn_copy(struct cl_insn *cli, const struct instruction *insn)
{/* Synopsis (see also `insn_assignment_base'):
  * [input] OP_COPY
  *     insn->target
  *     insn->src
  *     insn->type
  *
  * Problems/exceptions/notes:
  * FIXME: are cast operations OK?
  */
    return insn_assignment_base(cli, insn,
        insn->target,  /* := */  insn->src,
        TYPE_LHS_KEEP      |     TYPE_RHS_KEEP
    );
}

static inline bool
handle_insn_cast(struct cl_insn *cli, const struct instruction *insn)
{/* Synopsis (see also `insn_assignment_base'):
  * [input] OP_CAST, OP_SCAST
  *     insn->target
  *     insn->src
  *     insn->type
  *     insn->orig_type
  *
  * Problems/exceptions/notes:
  * May end up with with emitting CL_BINOP_BIT_AND when casting "smaller"
  * type to "bigger" type (currently, for bitfields only) to be sure we get
  * rid of unwanted garbage (e.g., data from the next bitfield item).
  */
    if (insn->orig_type->bit_size == insn->type->bit_size)
        handle_insn_copy(cli, insn);
    else if (insn->orig_type->bit_size < insn->type->bit_size
             && insn->orig_type->ctype.base_type->type == SYM_BITFIELD) {
        // we have to apply CL_BINOP_BIT_AND on `insn->src' using mask
        // (currently of int size XXX?) with additional higher bits zeroed

        int mask = ~((~0) << insn->orig_type->bit_size);
        struct cl_operand dst, lhs, op_mask;

        cli->code = CL_INSN_BINOP;
        cli->data.insn_binop.code = CL_BINOP_BIT_AND;
        cli->data.insn_binop.dst  = op_from_pseudo(&dst, insn, insn->target);
        cli->data.insn_binop.src1 = op_from_pseudo(&lhs, insn, insn->src);
        cli->data.insn_binop.src2 = op_from_value(&op_mask, insn, mask);

        EMIT(insn, cli);

        op_free_data(&dst);
        op_free_data(&lhs);
        op_free_data(&op_mask);

        return true;
    }

    CL_TRAP;  // will this ever happen?
    return true;
}

static inline bool
handle_insn_ptrcast(struct cl_insn *cli, const struct instruction *insn)
{/* Synopsis (see also `insn_assignment_base'):
  * [input] OP_PTRCAST
  *     insn->target
  *     insn->src
  *     insn->type
  *     insn->orig_type
  *
  * Problems/exceptions/notes:
  * None.
  */
    return insn_assignment_base(cli, insn,
        insn->target,  /* := */  insn->src,
        TYPE_LHS_KEEP      |     (TYPE_RHS_DIG | TYPE_RHS_REFERENCE)
    );
}

static inline bool
handle_insn_setval(struct cl_insn *cli, const struct instruction *insn)
{/* Synopsis:
  * [input] OP_SETVAL
  *     insn->target: destination
  *     insn->val:    value to be assigned in the form of an expression
  *         EXPR_FVALUE ~ constant of CL_TYPE_REAL type
  *         (other types of expressions not checked yet)
  * [output] CL_INSN_UNOP (set by caller, as with location)
  *     data.insn_unop.dst ~ insn->target
  *     data.insn_unop.src ~ insn->val (see above)
  *
  * Problems/exceptions/notes:
  * See `op_from_expression'.
  */
    struct cl_operand dst, src;

    cli->data.insn_unop.dst = op_from_pseudo(&dst, insn, insn->target);
    cli->data.insn_unop.src = op_from_expression(&src, insn, insn->val);

    EMIT(insn, cli);

    op_free_data(&dst);
    op_free_data(&src);

    return true;
}

/* Functions dedicated to other sparse instructions */

static bool
handle_insn_unop(struct cl_insn *cli, const struct instruction *insn)
{/* Synopsis:
  * [input] OP_NOT, OP_NEG (assignments handled separately)
  *     insn->target: destination
  *     insn->src1:   source
  * [output] CL_INSN_UNOP (set by caller, as with location)
  *     data.insn_unop.code (set by caller, rewrite in case of unary minus)
  *     data.insn_unop.dst ~ insn->target
  *     data.insn_unop.src ~ insn->src1
  *
  * Problems/exceptions/notes:
  * 1. OP_NEG means "unary minus" when applied on int.
  */
    struct cl_operand dst, src;

    cli->data.insn_unop.dst = op_from_pseudo(&dst, insn, insn->target);
    cli->data.insn_unop.src = op_from_pseudo(&src, insn, insn->src1);

    // for "unary minus", rewrite unary operation
    if (src.type->code == CL_TYPE_INT && insn->opcode == OP_NEG)
        cli->data.insn_unop.code = CL_UNOP_MINUS;

    EMIT(insn, cli);

    op_free_data(&dst);
    op_free_data(&src);

    return true;
}

static bool
handle_insn_binop(struct cl_insn *cli, const struct instruction *insn)
{/* Synopsis:
  * [input] (any arithmetic, shift, logical and explicit comparison operation)
  *     insn->target:   destination
  *     insn->src(1|2): operands
  * [output] CL_INSN_BINOP (set by caller, as with location)
  *     data.insn_binop.code (set by caller, rewrite for pointer arithmetics)
  *     data.insn_binop.dst      ~ insn->target
  *     data.insn_binop.src(1|2) ~ insn->src(1|2)
  *
  * Problems/exceptions/notes:
  * 1. Binary arithmetics case has to be detected and imposed explicitly.
  * S. If any of the operand is a pointer or an array, promote CL_BINOP_PLUS
  *    to CL_BINOP_POINTER_PLUS (other operations not expected in this case).
  */
    //CL_TRAP;
    struct cl_operand dst, op1, op2;

    cli->data.insn_binop.dst  = op_from_pseudo(&dst, insn, insn->target);
    cli->data.insn_binop.src1 = op_from_pseudo(&op1, insn, insn->src1);
    cli->data.insn_binop.src2 = op_from_pseudo(&op2, insn, insn->src2);

    // for pointer arithmetics, rewrite binary operation
    if (op1.type->code == CL_TYPE_PTR || op1.type->code == CL_TYPE_ARRAY
        || op2.type->code == CL_TYPE_PTR || op2.type->code == CL_TYPE_ARRAY) {
        switch (cli->data.insn_binop.code) {
            case CL_BINOP_PLUS:
                cli->data.insn_binop.code = CL_BINOP_POINTER_PLUS;
                break;
            default:
                // only addition is supported (XXX: may other ops occur?)
                CL_TRAP;
        }
    }

    EMIT(insn, cli);

    op_free_data(&dst);
    op_free_data(&op1);
    op_free_data(&op2);

    return true;
}

static bool
handle_insn_call(struct cl_insn *cli, const struct instruction *insn)
{/* Synopsis:
  * [input] OP_CALL
  *     insn->target
  *     insn->func
  *     insn->arguments
  * [output] regex: insn_call_open (insn_call_arg)* insn_call_close
  *          (location set by caller)
  *
  * Problems/exceptions/notes:
  * 1. Function can be non-returning.
  * S. Emit CL_INSN_ABORT in such case.
  */
    struct cl_operand dst, fnc, arg_op;
    struct pseudo *arg;
    int cnt = 0;

    WITH_CALL_TO_EMIT(&cli->loc,
                      op_from_pseudo(&dst, insn, insn->target),
                      op_from_pseudo(&fnc, insn, insn->func)) {
        FOR_EACH_PTR(insn->arguments, arg) {
            // XXX: ++cnt repeated side-effect?
            EMIT(insn_call_arg, ++cnt, op_from_pseudo(&arg_op, insn, arg));
            op_free_data(&arg_op);
        } END_FOR_EACH_PTR(arg);
    }

    op_free_data(&dst);
    op_free_data(&fnc);

    // special handling of non-returning function (end of BB)
    if (insn->func->sym->ctype.modifiers & MOD_NORETURN) {
        cli->code = CL_INSN_ABORT;
        EMIT(insn, cli);
        return false;
    }

    return true;
}

static bool
handle_insn_br(struct cl_insn *cli, const struct instruction *insn)
{/* Synopsis:
  * [input] OP_BR
  *     insn->cond
  *     insn->bb_true
  *     insn->bb_false
  * [output] regex: CL_INSN_JMP or CL_INSN_COND (unconditional/cond. jump)
  *          (location set by caller)
  *
  * Problems/exceptions/notes:
  * None.
  */
    // unconditional jump
    if (pseudo_futile(insn->cond)) {
        emit_insn_jmp(cli, PTR_STRING(insn->bb_true));
        return true;
    }

    // conditional jump
    struct cl_operand op;

    op_from_pseudo(&op, insn, insn->cond);
    emit_insn_cond(cli, &op,
                   PTR_STRING(insn->bb_true), PTR_STRING(insn->bb_true));

    op_free_data(&op);
    return true;
}

static bool
handle_insn_sel(struct cl_insn *cli, const struct instruction *insn)
{/* Synopsis:
  * [input] OP_SEL
  *     insn->src1
  *     insn->src2
  *     insn->src3
  *     insn->target
  * [output] regex: CL_INSN_COND (bb_open "assign" CL_INSN_JUMP){2} bb_open
  *          (location set by caller)
  *
  * Problems/exceptions/notes:
  * 1. BB label uniqueness.
  * S. Address of `insn' +0, +1 or +2, provided that insn has size of 4+
  *    and char 1 (compile time constraints?).
  */
    struct cl_operand op_cond;

    // local BB labels
    char const*const bb_label_true  = PTR_STRING(((char *) insn) + 0);
    char const*const bb_label_false = PTR_STRING(((char *) insn) + 1);
    char const*const bb_label_merge = PTR_STRING(((char *) insn) + 2);

    // cond instruction
    op_from_pseudo(&op_cond, insn, insn->src1);
    emit_insn_cond(cli, &op_cond, bb_label_true, bb_label_false);
    op_free_data(&op_cond);

    // first BB ("then" branch) with assignment and jump to merging BB
    EMIT(bb_open, bb_label_true);
    emit_insn_copy(cli, insn, insn->target,  /* := */  insn->src2);
    emit_insn_jmp(cli, bb_label_merge);

    // second BB ("else" branch) with assignment and jump to merging BB
    EMIT(bb_open, bb_label_false);
    emit_insn_copy(cli, insn, insn->target,  /* := */  insn->src3);
    emit_insn_jmp(cli, bb_label_merge);

    // merging BB
    EMIT(bb_open, bb_label_merge);

    return true;
}

static bool
handle_insn_switch(struct cl_insn *cli, const struct instruction *insn)
{/* Synopsis:
  * [input] OP_SWITCH
  *     insn->target:        selection source
  *     insn->multijmp_list: list of branches/cases
  *         jmp->target:               respective basic block
  *         ---
  *         jmp->begin == jmp->end ... single value selection
  *         jmp->begin < jmp->end  ... range selection
  *         jmp->begin > jmp->end  ... default case
  *
  * [output] regex: insn_switch_open (insn_switch_case)* insn_switch_close
  *          (location set by caller)
  *
  * Problems/exceptions/notes:
  * FIXME: not enough accurate location info from SPARSE for switch/case.
  */
    struct cl_operand op, val_lo, val_hi, *val_hi_ptr = &val_lo;
    struct multijmp *jmp;

    WITH_SWITCH_TO_EMIT(&cli->loc, op_from_pseudo(&op, insn, insn->target)) {
        // emit cases
        op_make_void(&val_lo);
        op_make_void(&val_hi);

        FOR_EACH_PTR(insn->multijmp_list, jmp) {
            if (jmp->begin <= jmp->end) {
                // non-default
                op_from_value(&val_lo, insn, jmp->begin)->type = op.type;

                if (jmp->begin != jmp->end) {
                    // range
                    op_from_value(&val_hi, insn, jmp->end)->type = op.type;
                    val_hi_ptr = &val_hi;
                }
            } else
                // default case
                op_make_void(&val_lo);

            EMIT(insn_switch_case, &cli->loc, &val_lo, val_hi_ptr,
                 PTR_STRING(jmp->target));

            // not necessary now, but ...
            op_free_data(&val_lo);
            op_free_data(&val_hi);
        } END_FOR_EACH_PTR(jmp);
    }

    op_free_data(&op);
    return true;
}

static bool
handle_insn_ret(struct cl_insn *cli, const struct instruction *insn)
{/* Synopsis:
  * [input] OP_RET
  *     insn->src:  value to be used as a return value
  *     insn->type: type of return value
  * [output] CL_INSN_RET (set by caller, as with location)
  *     cl_insn.insn_ret.src ~ insn->src
  *
  * Problems:
  * 1. Problem with a "right form" of the operand (whether to consider
  *    the whole struct or its first element, etc.).
  * S. Combine `op_dig_for_type_match' and `insn->type' for adjustment.
  */
    struct cl_operand op;
    const struct cl_type *resulting_type;

    cli->data.insn_ret.src = op_from_pseudo(&op, insn, insn->src);
    // TODO: decide according to the pseudo instead?
    if (op_accessible(&op)) {
        resulting_type = type_from_symbol(insn->type, NULL);
        op_dig_for_type_match(&op, resulting_type, insn->offset);
    }

    EMIT(insn, cli);

    op_free_data(&op);
    return true;
}

static bool
handle_insn(struct instruction *insn)
{
    typedef bool (*insn_handler)(struct cl_insn *, const struct instruction *);
    const struct insn_conversion {
        enum cl_insn_e       insn_code;
        union {
            enum cl_unop_e   unop;
            enum cl_binop_e  binop;
        } code;
        union {
            insn_handler     handler;
            const char       *string;
        } prop;
    } insn_conversions[] = {
    /* Synopsis:
     * sparse/linearize.h
     *
     * Note:
     * Instructions with more complicated rules (more instructions are emitted
     * per the single original one) are denoted with NOP, unhandled with ABORT.
     */
    #define INSN_STD(spi, cli, hnd) \
        [OP_##spi]={ .insn_code=CL_INSN_##cli, .prop.handler=hnd }
    #define INSN_UNI(spi, unop_code, hnd) \
        [OP_##spi]={ .insn_code=CL_INSN_UNOP, \
                     .code.unop=CL_UNOP_##unop_code, .prop.handler=hnd }
    #define INSN_BIN(spi, binop_code, hnd) \
        [OP_##spi]={ .insn_code=CL_INSN_BINOP, \
                     .code.binop=CL_BINOP_##binop_code, .prop.handler=hnd }
    #define INSN_IGN(spi, _, __) \
        [OP_##spi] = { .insn_code=CL_INSN_ABORT, .prop.string="OP_" #spi }

        // how? | sparse insn.    | cl insn. (+uni/bin) | handler            |
        //------+-----------------+---------------------+--------------------|

        INSN_IGN( BADOP           ,                     ,                    ),

        /* Entry */
        INSN_IGN( ENTRY           ,                     ,                    ),

        /* Terminator */
        // OP_TERMINATOR = OP_RET
        INSN_STD( RET             , RET                 , handle_insn_ret    ),
        INSN_STD( BR              , NOP /*JMP or COND*/ , handle_insn_br     ),
        INSN_STD( SWITCH          , NOP /*another way*/ , handle_insn_switch ),
        INSN_IGN( INVOKE          ,                     ,                    ),
        INSN_IGN( COMPUTEDGOTO    ,                     ,                    ),
        INSN_IGN( UNWIND          ,                     ,                    ),
        // OP_TERMINATOR_END = OP_UNWIND

        /* Binary */
        // OP_BINARY = OP_ADD
        INSN_BIN( ADD             , PLUS/*POINTER_PLUS*/, handle_insn_binop  ),
        INSN_BIN( SUB             , MINUS               , handle_insn_binop  ),
        INSN_BIN( MULU            , MULT /*XXX: unsig.*/, handle_insn_binop  ),
        INSN_BIN( MULS            , MULT                , handle_insn_binop  ),
        INSN_BIN( DIVU            , TRUNC_DIV /*unsig.*/, handle_insn_binop  ),
        INSN_BIN( DIVS            , TRUNC_DIV           , handle_insn_binop  ),
        INSN_BIN( MODU            , TRUNC_MOD /*unsig.*/, handle_insn_binop  ),
        INSN_BIN( MODS            , TRUNC_MOD           , handle_insn_binop  ),
        INSN_BIN( SHL             , LSHIFT              , handle_insn_binop  ),
            // OP_ASR (arithmetic shift) is the same as OP_LSR (logical shift)
            // except for that the highest bit is kept the same, not zeroed;
            // - C standard says that right shift perfomed on unsigned type is
            //   of the LSR type, implementation specific (LSR/ASR) otherwise
            //   [C text book by P. Herout, TODO: check real standard]
            // - for sparse, right shift performed on signed operand is
            //   translated into OP_ASR (OP_LSR otherwise as expected XXX:vrfy)
        INSN_BIN( LSR             , RSHIFT              , handle_insn_binop  ),
        INSN_BIN( ASR             , RSHIFT              , handle_insn_binop  ),

        /* Logical */
        INSN_BIN( AND             , BIT_AND             , handle_insn_binop  ),
        INSN_BIN( OR              , BIT_IOR             , handle_insn_binop  ),
        INSN_BIN( XOR             , BIT_XOR             , handle_insn_binop  ),
        INSN_BIN( AND_BOOL        , TRUTH_AND           , handle_insn_binop  ),
        INSN_BIN( OR_BOOL         , TRUTH_OR            , handle_insn_binop  ),
        // OP_BINARY_END = OP_OR_BOOL

        /* Binary comparison */
        // OP_BINCMP = OP_SET_EQ
        INSN_BIN( SET_EQ          , EQ                  , handle_insn_binop  ),
        INSN_BIN( SET_NE          , NE                  , handle_insn_binop  ),
        INSN_BIN( SET_LE          , LE                  , handle_insn_binop  ),
        INSN_BIN( SET_GE          , GE                  , handle_insn_binop  ),
        INSN_BIN( SET_LT          , LT                  , handle_insn_binop  ),
        INSN_BIN( SET_GT          , GT                  , handle_insn_binop  ),
        INSN_BIN( SET_B           , LT /*XXX: unsigned*/, handle_insn_binop  ),
        INSN_BIN( SET_A           , GT /*XXX: unsigned*/, handle_insn_binop  ),
        INSN_BIN( SET_BE          , LE /*XXX: unsigned*/, handle_insn_binop  ),
        INSN_BIN( SET_AE          , GE /*XXX: unsigned*/, handle_insn_binop  ),
        // OP_BINCMP_END = OP_SET_AE,

        /* Uni */
        INSN_UNI( NOT             , BIT_NOT             , handle_insn_unop   ),
        INSN_UNI( NEG             , TRUTH_NOT/*u.minus*/, handle_insn_unop   ),

        /* Select - three input values */
        INSN_STD( SEL             , NOP /*COND*/        , handle_insn_sel    ),

        /* Memory */
        INSN_IGN( MALLOC          ,                     ,                    ),
        INSN_IGN( FREE            ,                     ,                    ),
        INSN_IGN( ALLOCA          ,                     ,                    ),
        INSN_UNI( LOAD            , ASSIGN              , handle_insn_load   ),
        INSN_UNI( STORE           , ASSIGN              , handle_insn_store  ),
        INSN_UNI( SETVAL          , ASSIGN              , handle_insn_setval ),
        INSN_IGN( SYMADDR         ,                     ,                    ),
        INSN_IGN( GET_ELEMENT_PTR ,                     ,                    ),

        /* Other */
            // FIXME: this might be a SPARSE bug if DO_PER_EP_UNSAA is set
            //        and OP_PHI or OP_PHISOURCE occurs (really encountered)
        INSN_IGN( PHI             ,                     ,                    ),
        INSN_IGN( PHISOURCE       ,                     ,                    ),
        INSN_UNI( CAST            , ASSIGN              , handle_insn_cast   ),
        INSN_UNI( SCAST           , ASSIGN              , handle_insn_cast   ),
        INSN_IGN( FPCAST          , ASSIGN /*not sure*/ , handle_insn_copy   ),
        INSN_UNI( PTRCAST         , ASSIGN              , handle_insn_ptrcast),
        INSN_IGN( INLINED_CALL    ,                     ,                    ),
        INSN_STD( CALL            , NOP /*another way*/ , handle_insn_call   ),
        INSN_IGN( VANEXT          ,                     ,                    ),
        INSN_IGN( VAARG           ,                     ,                    ),
        INSN_IGN( SLICE           ,                     ,                    ),
        INSN_IGN( SNOP            ,                     ,                    ),
        INSN_IGN( LNOP            ,                     ,                    ),
        INSN_IGN( NOP             ,                     ,                    ),
        INSN_IGN( DEATHNOTE       ,                     ,                    ),
        INSN_IGN( ASM             ,                     ,                    ),

        /* Sparse tagging (line numbers, context, whatever) */
        INSN_IGN( CONTEXT         ,                     ,                    ),
        INSN_IGN( RANGE           ,                     ,                    ),

        /* Needed to translate SSA back to normal form */
        INSN_UNI( COPY            , ASSIGN              , handle_insn_copy   ),
    };

    struct cl_insn cli;
    const struct insn_conversion *conversion;

    WITH_DEBUG_LEVEL(d_instruction)
        PUT(debug,"\t%d: instruction to be processed: %s",
            insn->pos.line, show_instruction(insn));

    //assert(PARTIALLY_ORDERED( OP_BADOP , insn->opcode , OP_COPY ));
    conversion = &insn_conversions[insn->opcode];

    sparse_location(&cli.loc, insn->pos);
    cli.code = conversion->insn_code;

    switch (conversion->insn_code) {
        case CL_INSN_ABORT:
            WARN_UNHANDLED(insn->pos, conversion->prop.string);
            return true;
        case CL_INSN_UNOP:
            cli.data.insn_unop.code = conversion->code.unop;
            break;
        case CL_INSN_BINOP:
            cli.data.insn_binop.code = conversion->code.binop;
            break;
        default:
            break;
    }

    assert(conversion->prop.handler);
    return conversion->prop.handler(&cli, insn);
}

static bool insn_interesting(struct instruction *insn)
{
    // TODO: investigate
    switch (insn->opcode) {
        case OP_ENTRY:
            return false;

        default:
            return true;
    }
}

static bool handle_bb_insn(struct instruction *insn)
{
    if (!insn)
        return true;

    if (!insn->bb) {
#if SHOW_PSEUDO_INSNS
        WARN_VA(insn->pos, "ignoring pseudo: %s", show_instruction(insn));
#endif
        return true;
    }

    if (!insn_interesting(insn))
        return true;

    return handle_insn(insn);
}


//
// Functions for lower granularity/higher level handling
//

static void handle_bb(struct basic_block *bb)
{/*
  *
  * Problems/exceptions/notes:
  * - avoid being called with !bb
  */
    struct instruction *insn;

    if (!bb)
        return;

    EMIT(bb_open, PTR_STRING(bb));

    FOR_EACH_PTR(bb->insns, insn) {
        if (!handle_bb_insn(insn))
            // subtle: 'break' stmt here does not work as one would expected to
            goto done;
    } END_FOR_EACH_PTR(insn);
done:
    return;
}

static void handle_fnc_ep(struct entrypoint *ep)
{
    struct cl_insn cli;
    struct basic_block *bb;

    /* jump to entry basic block */

    sparse_location(&cli.loc, ep->entry->pos);
    emit_insn_jmp(&cli, PTR_STRING(ep->entry->bb));

    /* go through basic blocks */

    FOR_EACH_PTR(ep->bbs, bb) {
        if (!bb)
            continue;

        if (bb->parents || bb->children || bb->insns
            || /* FIXME: is the following actually useful? */
            2 < GLOBALS(debug)) {
            handle_bb(bb);
        }
    } END_FOR_EACH_PTR(bb);
}

static void handle_fnc_body(struct symbol *sym)
{
    struct entrypoint *ep;
    SPARSE_API(linearize_symbol, /*OUT*/ ep, /*IN*/ sym);
    if (!ep)
        CL_TRAP;

#if DO_PER_EP_UNSAA
    SPARSE_API(unssa, ep);
#endif

#if DO_PER_EP_SET_UP_STORAGE
    SPARSE_API(set_up_storage, ep);
#endif

    handle_fnc_ep(ep);

#if DO_PER_EP_SET_UP_STORAGE
    // no switch, vrfy_storage uses printf anyway
    SPARSE_API(free_storage);
#endif
}

static void handle_fnc_arg_list(struct symbol_list *arg_list)
{
    int argc = 0;
    struct symbol *arg;
    struct cl_operand arg_op;

    FOR_EACH_PTR(arg_list, arg) {
        EMIT(fnc_arg_decl, ++argc, op_from_symbol(&arg_op, arg));
        op_free_data(&arg_op);
    } END_FOR_EACH_PTR(arg);
}

static void handle_fnc_def(struct symbol *sym)
{
    struct cl_operand fnc;

    WITH_FNC_TO_EMIT(op_from_symbol(&fnc, sym)) {
        // dump argument list
        handle_fnc_arg_list(sym->ctype.base_type->arguments);
        // handle fnc body
        handle_fnc_body(sym);
    }

    op_free_data(&fnc);
}

static void handle_sym_fn(struct symbol *sym)
{
    struct symbol *base_type = sym->ctype.base_type;
    struct statement *stmt = base_type->stmt;

    if (stmt) {
        // function definition
        handle_fnc_def(sym);
        return;
    }

    WARN_UNHANDLED_SYM(sym);
}

static void handle_top_level_sym(struct symbol *sym)
{
    struct symbol *base_type;

    if (!sym)
        return;

    base_type = sym->ctype.base_type;
    if (!base_type)
        return;

    switch (base_type->type) {
        WARN_CASE_UNHANDLED(sym->pos, SYM_UNINITIALIZED)
        WARN_CASE_UNHANDLED(sym->pos, SYM_PREPROCESSOR)
        WARN_CASE_UNHANDLED(sym->pos, SYM_BASETYPE)
        WARN_CASE_UNHANDLED(sym->pos, SYM_NODE)
        WARN_CASE_UNHANDLED(sym->pos, SYM_PTR)
        WARN_CASE_UNHANDLED(sym->pos, SYM_ARRAY)
        WARN_CASE_UNHANDLED(sym->pos, SYM_STRUCT)
        WARN_CASE_UNHANDLED(sym->pos, SYM_UNION)
        WARN_CASE_UNHANDLED(sym->pos, SYM_ENUM)
        WARN_CASE_UNHANDLED(sym->pos, SYM_TYPEDEF)
        WARN_CASE_UNHANDLED(sym->pos, SYM_TYPEOF)
        WARN_CASE_UNHANDLED(sym->pos, SYM_MEMBER)
        WARN_CASE_UNHANDLED(sym->pos, SYM_BITFIELD)
        WARN_CASE_UNHANDLED(sym->pos, SYM_LABEL)
        WARN_CASE_UNHANDLED(sym->pos, SYM_RESTRICT)
        WARN_CASE_UNHANDLED(sym->pos, SYM_FOULED)
        WARN_CASE_UNHANDLED(sym->pos, SYM_KEYWORD)
        WARN_CASE_UNHANDLED(sym->pos, SYM_BAD)

        case SYM_FN:
            handle_sym_fn(sym);
            break;
    }

    if (sym->initializer)
        WARN_UNHANDLED(sym->pos, "sym->initializer");
}

static void
proceed_symbols(struct symbol_list *list)
{
    struct symbol *sym;

    FOR_EACH_PTR(list, sym) {

#if DO_EXPAND_SYMBOL
        SPARSE_API(expand_symbol, sym);
#endif

        handle_top_level_sym(sym);
    } END_FOR_EACH_PTR(sym);
}


#undef GLOBALS

//
// from now on, globals are used via `opts' binding (function argument!)
//

#define GLOBALS(what)     (opts->globals->what)
#define INTERNALS(what)   (opts->internals.what)
#define CLOPTS(what)      (opts->cl.what)
#define SPARSEOPTS(what)  (opts->sparse.what)

static void
proceed_sparse(const struct options *opts)
{
    char *file;
    struct string_list *filelist = NULL;
    struct symbol_list *symlist;

    SPARSE_API(sparse_initialize, /*OUT*/ symlist, /*IN*/ SPARSEOPTS(set.argc),
               SPARSEOPTS(set.argv), &filelist);

#if DO_PROCEED_INTERNAL
    // internal symbols
    WITH_FILE_TO_EMIT(SPARSE_INTERNAL_SYMS_FILE)
        proceed_symbols(symlist);
#endif

    // the rest, file by file
    FOR_EACH_PTR_NOTAG(filelist, file) {
        WITH_DEBUG_LEVEL(d_file)
            PUT(debug, "about to proceed '%s'...\n", file);
        WITH_FILE_TO_EMIT(file) {
            SPARSE_API(sparse, /*OUT*/ symlist, /*IN*/ file);
            proceed_symbols(symlist);
        }
    } END_FOR_EACH_PTR_NOTAG(file);

#if DO_SPARSE_FREE
    free(input_streams);
#endif
}

static inline void
setup_worker(struct options *opts)
{
    if (GLOBALS(unexposed.register_atexit)
      && 0 != atexit(atexit_worker))
        DIE("atexit");

    setup_stream(&STREAM(debug), INTERNALS(imm.fd.debug));
    setup_stream(&STREAM(cl),    INTERNALS(imm.fd.cl));

    if (FD_DEFERRED != INTERNALS(imm.fd.sparse))
        setup_stream(&STREAM(sparse), INTERNALS(imm.fd.sparse));
    else {
        // setup deferred output incl. atexit handler for printing it
        if (GLOBALS(unexposed.register_atexit)
          && 0 != atexit(atexit_sparse))
            DIE("atexit");
        STREAM(sparse) = open_memstream(&GLOBALS(deferred.buffer),
                                        &GLOBALS(deferred.size));
        if (!STREAM(sparse))
            DIE( ERRNO("open_memstream") );
    }
}

static bool
clsp_append_via_config(struct cl_code_listener *chain,
                       const char *config_string, const struct options *opts)
{
    struct cl_code_listener *cl = CL_API(code_listener_create, config_string);
    if (!cl) {
        chain->destroy(chain);
        return false;
    }
    CL_API(chain_append, chain, cl);
    return true;
}

static bool
clsp_append_via_config_args(struct cl_code_listener *chain,
                            const char *listener, const char *args,
                            const char *clf, const struct options *opts)
{
    char config_string[CLSP_CONFIG_STRING_MAX];

    if (!clf)
        clf = (/*opts->use_peer*/ true)
            ? "unfold_switch,unify_labels_gl"
            : "unify_labels_fnc";

    if (0 >= snprintf(config_string, CLSP_CONFIG_STRING_MAX,
                      "listener=\"%s\" listener_args=\"%s\" clf=\"%s\"",
                      listener, args, clf))
        DIE("snprintf");

    return clsp_append_via_config(chain, config_string, opts);
}

// NOTE: using the base Code Listener
static struct cl_code_listener*
clsp_chain_init(const struct options *opts)
{
    struct cl_code_listener *chain = CL_API(chain_create);
    if (!chain)
        return NULL;  // error message already emitted

    // location as a first step throughout the run/locator
    if (CLOPTS(imm.debug.location))
        if (!clsp_append_via_config(chain, "listener=\"locator\"", opts))
            return NULL;
    // pretty printing/pp
    if (CLOPTS(imm.pprint.enable)) {
        const char *listener = CLOPTS(imm.pprint.types)
            ? "pp_with_types"
            : "pp";
        const char *file = CLOPTS(imm.pprint.file)
            ? CLOPTS(imm.pprint.file)
            : "";
        const char *clf = CLOPTS(imm.pprint.switch_to_if)
            ? "unify_labels_gl"
            : "unfold_switch,unify_labels_gl";
        if (!clsp_append_via_config_args(chain, listener, file, clf, opts))
            return NULL;
    }
    // control flow graphs generator/dotgen
    if (CLOPTS(imm.gencfg.enable)) {
        const char *file = CLOPTS(imm.gencfg.file)
            ? CLOPTS(imm.gencfg.file)
            : "";
        if (!clsp_append_via_config_args(chain, "dotgen", file, NULL, opts))
            return NULL;
    }
    // type graphs generator/typedot
    if (CLOPTS(imm.gentype.enable))
        if (!clsp_append_via_config_args(chain, "typedot",
                                         CLOPTS(imm.gentype.file), NULL, opts))
            return NULL;
    /*if (OPTS(use_peer)
        && !clsp_append_via_config_args(chain, "easy", OPTS(peer_args), opts))
          return NULL;*/

    // TODO: DLOPEN

    return chain;
}

static inline void
setup_cl(const struct options *opts)
{
    if (GLOBALS(unexposed.register_atexit)
      && 0 != atexit(atexit_cl))
        DIE("atexit");

    GLOBALS(cl_libs.cnt) = CLOPTS(imm.listeners.cnt);
    NORETWRN(MEM_ARR_RESIZE((GLOBALS(cl_libs.handles)), GLOBALS(cl_libs.cnt)));

#ifndef HAS_CL
    // use the symbols from the base Code Listener provided externally
    // TODO: strip args ("lib:args")
    void *handle = dlopen(CLOPTS(imm.listeners.arr)[0], DL_OPEN_FLAGS);
    if (!handle)
        DIE("dlopen: %s", dlerror());
    GLOBALS(cl_libs.handles)[0] = handle;
# define PROCEED(what)                                                        \
    ((*(void **)(&GLOBALS(cl_api.what)) = dlsym(handle, TOSTRING(cl_##what))) \
        ? (void) 0  /* NOOP */                                                \
        : DIE("dlopen: %s", dlerror())                                        \
    )
    CL_API_PROCEED(PROCEED);
# undef PROCEED
#endif

    if (CLOPTS(imm.default_output))
        CL_API(global_init_defaults, CLSP_NAME, CLOPTS(imm.debug.level));
    else {
        struct cl_init_data init = {
            .debug       = clmsg_print,
            .warn        = clmsg_print,
            .error       = clmsg_print,
            .note        = clmsg_print,
            .die         = clmsg_die,
            .debug_level = CLOPTS(imm.debug.level),
        };
        CL_API(global_init, &init);
    }

    GLOBALS(cl) = clsp_chain_init(opts);
    if (!GLOBALS(cl))
        DIE("clsp_chain_init");
}


/** master+worker top-level functions ************************************/


/** Routine of worker (main proceeding).

    @param[in] opts  Target options representation.

    @todo setup_sparse here
 */
int
worker(struct options *opts)
{
    setup_worker(opts);  /* incl. all the streams */
    setup_cl(opts);

    WITH_OBJECT(type_ptr_db)
        proceed_sparse(opts);

    /* rest of cleanup in registered atexit handler(s) */
    EMIT(acknowledge);
    return EXIT_SUCCESS;
}

/** Routine of master.

    @note The only reason to use the "fork" arrangement is probably
          showing the exit code of sparse explicitly.
 */
static int
master(pid_t pid, const struct options *opts)
{
    int status, ret = 0;
    pid_t p;

    while ((pid_t) -1 == (p = waitpid(pid, &status, 0)) && EINTR != errno)
        /* loop */
        ;

    if ((pid_t) -1 == p) {
        ret = errno;
        /* unchecked on purpose */
        kill(pid, SIGKILL);
        errno = ret;
        DIE( ERRNO("wait") );
    }

    if (WIFEXITED(status)) {
        ret = WEXITSTATUS(status);
        PUT(debug, "sparse returned %i", ret);
        return ret;
    }

    DIE("worker ended in an unexpected way");
}


/** options/arguments handling ********************************************/


#define OPTISBIN(what)             APPLY(OPTISBIN_,OPT_##what)
#define OPTPREFIX(what)            APPLY(OPTPREFIX_,OPT_##what)
#define OPTPREFIX_(is_bin,prefix)  prefix
#define OPTISBIN_(is_bin,prefix)   is_bin

#define OPT_SHORT      false,"-"
#define OPT_SHORT_BIN  true ,"-"
#define OPT_LONG       false,"--"
#define OPT_LONG_BIN   true ,"--"
#define OPT_CL         false,"-cl-"
#define OPT_CL_BIN     true ,"-cl-"

/** Version printer.
 */
int
print_version(const struct options *opts)
{
    PUT(out, "%s", GIT_SHA1);
    return 0;
}

/** Help printer.
 */
int
print_help(const char *cmd, const struct options *opts, int ret)
{
#define _(...)       PUT(out, __VA_ARGS__);
#define __           PUT(out, "");
#define ___          ""
#define L(lo, cmt)   PUT(out, "%-32s%s", OPTPREFIX(LONG) lo, cmt);
#define S(so, cmt)   PUT(out, "%-32s%s", OPTPREFIX(SHORT) so, cmt);
#define I(ign, cmt)  PUT(out, "%-32s%s", ign, cmt);
#define B(so, lo, cmt) \
    PUT(out, "%-32s%s", OPTPREFIX(SHORT) so ", " OPTPREFIX(LONG) lo, cmt);
#define C(co, cmt)   PUT(out, "%-32s%s", OPTPREFIX(CL) co, cmt);
#define V(v, cmt)    PUT(out, "%-32d%s", v, cmt);
    _("sparse-based Code Listener frontend, version"                           )
    __                                                                         ;
    _("usage: %s ( cl-global-opts | cl-plugin[:args] | sparse-opts-args )*",cmd)
    __                                                                         ;
#ifndef HAS_CL
    _("As no Code Listener plugin was built-in (-> no one to serve as a base"  )
    _("one at hand), at least one such has to be provided in the form of a"    )
    _("shared library containing necessary symbols of Code Listener interface" )
    _("(Code Listener based plugins for GCC are compatible);"                  )
    _("see `%s' option below.",                          OPTPREFIX(CL) "plugin")
#endif
    __                                                                         ;
    __                                                                         ;
    _("This Code Listener front-end defines a few internal options:"           )
    __                                                                         ;
    B("h", "help"          , "Prints this help text"                           )
    L("version"            , "Prints the version information"                  )
    B("f", "fork"          , "Do fork (only to show sparse exit status {0,1})" )
    _("[specification of file descriptors, use `FD>file' redirection for FD>2]")
    L("cl-fd=FD"           , "Redefine stderr used to display to cl messages"  )
    I(___                  , "(fatal errors are always produced on stderr)"    )
    L("sparse-fd=FD"       , "Redefine stderr used by sparse, `D' for deferred")
    L("debug-fd=FD"        , "Redefine stdout used for debugging messages"     )
    __                                                                         ;
    B("d", "debug[=MASK]"  , "Debug (selectively if MASK specified)"           )
    _("MASK:")             ;              for (int i = d_first; i < d_last; i++)
    V(DVALUE(i)            ,                                  d_str[DACCESS(i)])
    __                                                                         ;
    __                                                                         ;
    _("From the options affecting the run through Code Listener interface,"    )
    _("one particularly important is a way to import other Code Listener(s):"  )
    __                                                                         ;
    C("plugin=FILE[:ARGS]" , "Path to a shared library containg symbols of"    )
    I(___                  , "Code Listener (for instance, GCC plugins can be" )
#ifdef HAS_CL
    I(___                  , "used directly), passing it optional ARGS"        )
#else
    I(___                  , "used directly), passing it optional ARGS;"       )
    I(___                  , "the first one is a base one and must be provided")
#endif
    __                                                                         ;
    __                                                                         ;
#ifndef HAS_CL
    _("and specifically these options are for a base (built-in) Code Listener:")
#else
    _("and specifically these options are for a base (provided) Code Listener:")
#endif
    __                                                                         ;
    C("default-output"     , "Use Code Listener's built-ins to print messages" )
    C("pprint[=FILE]"      , "Dump pretty-printed linearized code"             )
    C("pprint-types"       , "Add type information to pretty-printed code"     )
    C("pprint-switch-to-if", "Unfold `switch' into series of `if' statements " )
    C("gen-cfg[=MAIN_FILE]", "Generate control flow graphs (as per MAIN_FILE)" )
    C("gen-type[=FILE]"    , "Generate type graphs (to FILE if specified)"     )
    C("debug-location"     , "Output location as first step throughout the run")
    C("debug-level[=LEVEL]", "Debug (according to LEVEL if specified)"         )
    __                                                                         ;
    __                                                                         ;
    _("For `sparse-opts-args' (including the specification of the target[s])," )
    _("see sparse documentation;  generally, there is some level of"           )
    _("compatibility with GCC and unrecognized options are ignored anyway."    )
    _("To name a few notable notable ones (referring to current version):"     )
    __                                                                         ;
    S("v"                  , "Report more defects, more likely false positives")
    S("m64"                , "Suppose 64bit architecture (32bit by default)"   )
    S("DNAME[=VALUE]"      , "Define macro NAME (holding value VALUE if spec.)")
    S("W[no[-]]WARNING"    , "Request/not to report WARNING-related issues;"   )
    I(___                  , "`sparse-all' covers all available warnings"      )
    _("%s"                 , "..."                                             )
    __                                                                         ;
    __                                                                         ;
    _("Return values:")    ;            for (int i = ec_first; i < ec_last; i++)
    V(ECVALUE(i)           ,                                ec_str[ECACCESS(i)])
#undef V
#undef C
#undef B
#undef I
#undef S
#undef L
#undef ___
#undef __
#undef _
    return ret;
}

/**  The first/initializing phase of gathering options.
 */
static void
opts_initialize(struct options *opts, const struct globals *globals_obj)
{
    INTERNALS(imm.fork) = false;
    INTERNALS(imm.fd)
      = (struct oi_fd) { .cl=FD_UNDEF, .sparse=FD_UNDEF, .debug=FD_UNDEF };
    INTERNALS(imm.debug) = 0;

    CLOPTS(imm.listeners.cnt)  = 0;
    CLOPTS(imm.listeners.arr)  = NULL;
    CLOPTS(imm.default_output) = false;
    CLOPTS(imm.pprint.enable)  = false;
    CLOPTS(imm.gencfg.enable)  = false;
    CLOPTS(imm.gentype.enable) = false;
    CLOPTS(imm.debug) = (struct oc_debug) { .location=false, .level=0 };

    /* const was meant to denote "no modification to globals" (yet) */
    opts->globals = (struct globals*) globals_obj;
}

/** Positive number converter (from string).

    @todo  More checks.
 */
static inline int
get_positive_num(const char *what, const char *value,
                 const struct options *opts)
{
    if (!isdigit(value[0]))
        DIE( ECODE(ec_opt, "option %s: not a numeric value: %s", what, value) );
    int ret = strtol(value, NULL, 10);
    if (ret < 0)
        DIE( ECODE(ec_opt, "option %s: must be positive number", what) );
    return ret;
}

/** File descriptor specification converter (from string).

    @note Single @c D characters stands for special "deferred" stream
          available as per  @c accept_deferred argument.
 */
static inline int
get_fd(const char *what, const char *value, bool accept_deferred,
       const struct options *opts)
{
    if (accept_deferred && value[0] == 'D' && value[1] == '\0')
        return FD_DEFERRED;
    return get_positive_num(what, value, opts);
}

/** The main phase of gathering options.

    We only handle known options/arguments and leave argv untouched
    behind us (our options should be guaranteed not to collide with
    sparse, though).
 */
static void
proceed_options(int argc, char *argv[], struct options *opts)
{
#define OPTPREFIXEQ(argv, i, type, opt)                                      \
    (strncmp(argv[i],OPTPREFIX(type) opt,strlen(OPTPREFIX(type) opt))        \
        ? NULL                                                               \
        : (((OPTISBIN(type) && argv[i][strlen(OPTPREFIX(type) opt)] != '\0') \
            ? PUT(out, "option %s: binary option with argument (or clash?)", \
                  argv[i])                                                   \
            : 0)                                                             \
            , &argv[i][strlen(OPTPREFIX(type) opt)]))
#define OPTVALUE(argv, i, str)                                               \
    (*str != '\0'                                                            \
        ? (((*str != '=' || *++str != '\0')) ? str : NULL)                   \
        : (argv[i+1][0] != '\0' && argv[i+1][0] != '-')                      \
            ? (str = argv[++i])                                              \
            : NULL)
    if (1 >= argc)
        return print_help(argv[0], opts, 1);

    char *value;
    for (int i=1; i < argc; i++) {

        /* internal options */

        if (OPTPREFIXEQ(argv,i,SHORT_BIN,"h")
          || OPTPREFIXEQ(argv,i,LONG_BIN,"help")) {
            return print_help(argv[0], opts, -1);
        }
        else if ((value = OPTPREFIXEQ(argv,i,LONG_BIN,"version"))) {
            return print_version(opts);
        }
        else if ((value = OPTPREFIXEQ(argv,i,SHORT_BIN,"f"))
          || (value = OPTPREFIXEQ(argv,i,LONG_BIN,"fork"))) {
            INTERNALS(imm.fork) = true;
        }
        else if ((value = OPTPREFIXEQ(argv,i,LONG,"cl-fd"))
          || (value = OPTPREFIXEQ(argv,i,LONG,"sparse-fd"))
          || (value = OPTPREFIXEQ(argv,i,LONG,"debug-fd"))) {
            char *arg = argv[i];  /* preserve across OPTVALUE */
            if (OPTVALUE(argv,i,value)) {
                /* exploiting the difference of initial chars */
                int *to_set;
                switch (arg[strlen(OPTPREFIX(LONG))]) {
                    case 'c': to_set = &INTERNALS(imm.fd.cl);     break;
                    case 's': to_set = &INTERNALS(imm.fd.sparse); break;
                    case 'd': to_set = &INTERNALS(imm.fd.debug);  break;
                    default: DIE("unexpected case");
                }
                *to_set = get_fd(arg, value,
                                 (to_set == &INTERNALS(imm.fd.sparse)), opts);
            } else
                DIE( ECODE(ec_opt, "option %s: omitted value", arg) );
        }
        else if ((value = OPTPREFIXEQ(argv,i,SHORT,"d"))
          || (value = OPTPREFIXEQ(argv,i,LONG,"debug"))) {
            INTERNALS(imm.debug) = OPTVALUE(argv,i,value)
                ? get_positive_num("debug", value, opts)
                : ~0;
        }

        /* Code Listener options */

        else if ((value = OPTPREFIXEQ(argv,i,CL,"plugin"))) {
            *(MEM_ARR_APPEND(CLOPTS(imm.listeners.arr),
                             CLOPTS(imm.listeners.cnt)))
              = OPTVALUE(argv,i,value);
        }
        else if (OPTPREFIXEQ(argv,i,CL_BIN,"default-output")) {
            CLOPTS(imm.default_output) = true;
        }
        else if ((value = OPTPREFIXEQ(argv,i,CL,"pprint"))) {
            CLOPTS(imm.pprint.enable)       = true;
            CLOPTS(imm.pprint.file)         = OPTVALUE(argv,i,value);
            CLOPTS(imm.pprint.types)        = false;
            CLOPTS(imm.pprint.switch_to_if) = false;
        }
        else if (OPTPREFIXEQ(argv,i,CL_BIN,"pprint-types")) {
            if (!CLOPTS(imm.pprint.enable))
                PUT(err, "option %s: cannot be used before %s",
                    OPTPREFIX(CL) "pprint-types", OPTPREFIX(CL) "pprint");
            else
                CLOPTS(imm.pprint.types) = true;
        }else if (OPTPREFIXEQ(argv,i,CL_BIN,"pprint-switch-to-if")) {
            if (!CLOPTS(imm.pprint.enable))
                PUT(err, "option %s: cannot be used before %s",
                    OPTPREFIX(CL) "pprint-switch-to-if",
                    OPTPREFIX(CL) "pprint");
            else
                CLOPTS(imm.pprint.switch_to_if) = true;
        }
        else if ((value = OPTPREFIXEQ(argv,i,CL,"gen-cfg"))) {
            CLOPTS(imm.gencfg.enable) = true;
            CLOPTS(imm.gencfg.file)   = OPTVALUE(argv,i,value);
        }
        else if ((value = OPTPREFIXEQ(argv,i,CL,"gen-type"))) {
            CLOPTS(imm.gentype.enable) = true;
            CLOPTS(imm.gentype.file)   = OPTVALUE(argv,i,value);
        }
        else if (OPTPREFIXEQ(argv,i,CL_BIN,"debug-location")) {
            CLOPTS(imm.debug.location) = true;
        }
        else if ((value = OPTPREFIXEQ(argv,i,CL,"debug-level"))) {
            CLOPTS(imm.debug.level) = OPTVALUE(argv,i,value)
                ? get_positive_num("debug-level", value, opts)
                : ~0;
        }
        /* TODO: remove? */
        /*} else if ((value = OPTPREFIXEQ(argv,i,CL,"cl-args"))) {
            OPTS(peer_args) = OPTVALUE(value)
                ? value
                : "";*/
    }

    return 0;
#undef OPTPREFIXEQ
#undef OPTVALUE
}

/** The last/finalizing phase of gathering options.

    @todo Check code listener if !HAS_CL, ...
 */
static void
opts_initialize(struct options *opts, int argc, char *argv[])
{
#ifndef HAS_CL
    if (0 == CLOPTS(imm.listeners.cnt))
        DIE("no Code Listener specified");
#endif
    if (CLOPTS(imm.default_output) && FD_UNDEF != INTERNALS(imm.fd.cl))
        PUT(err, "option %s: does not make sense with %s",
            OPTPREFIX(CL) "pprint-switch-to-if",
            OPTPREFIX(CL) "pprint");
    SPARSEOPTS(set.argc) = argc;
    SPARSEOPTS(set.argv) = argv;
}

/** Initializes values within object reprezenting globals.

    @param[in,out]  globals_obj  Object representing globals.
    @note Presumes empty object as in case of default static
          initialization and only minimal set of values is treated here.
 */
struct globals *
globals_init(struct globals *globals_obj)
{
    globals_obj->stream[stream_out] = stdout;
    globals_obj->stream[stream_err] = stderr;
    globals_obj->unexposed.register_atexit = true;
#ifdef HAS_CL
    /* set symbols we know are available out-of-the-box */
# define PROCEED(what)  globals_obj->cl_api.what = &cl_##what
    CL_API_PROCEED(PROCEED);
# undef PROCEED
#endif
    return globals_obj;
}

/** Gather options in the structure which is also pre-initialized.

    @param[out] opts         Target options representation.
    @param[in]  globals_obj  Object (read-only in this stage) representing
                             globals.
    @param[in]  argc         argc
    @param[in]  argv         argv
    @return     0 = continue, <0 = exit ok, >0 = exit fail
 */
int
gather_opts(struct options *opts, const struct globals *globals_obj,
     int argc, char *argv[])
{
    int ret;

    opts_initialize(opts, globals_obj);

    if (!(ret = opts_proceed(opts, argc, argv))
        opts_finalize(opts, argc, argv);

    return ret;
}

int
main(int argc, char *argv[])
{
    struct options options, *opts = &options;
    int ret;
    pid_t pid;

    if (!(ret = gather_opts(opts, globals_init(&globals), argc, argv));
        return (ret < 0) ? EXIT_SUCCESS : ret;

    if (!INTERNALS(imm.fork))
        ret = worker(opts);
    else if ((pid_t) -1 == (pid = fork()))
        DIE( ERRNO("fork") );
    else if (pid == 0)
        ret = worker(opts);
    else
        ret = master(pid, opts);

    return ret;
}

/* vim:ts=4:sts=4:sw=4:et: */
