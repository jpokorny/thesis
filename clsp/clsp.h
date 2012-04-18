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
#ifndef CLSP_H_GUARD
#define CLSP_H_GUARD

/*
    Common base to be included to the modules, but always as a first import!
 */

#define _POSIX_C_SOURCE 200809L  /* fileno, open_memstream */

#include <stdio.h>   /* FILE, fprintf, fileno */
#include <stdlib.h>  /* malloc, exit */
#include <string.h>  /* strerror */
#include <errno.h>   /* errno */
#include <unistd.h>  /* dup, close */

#include "clsp_macros.h"
#include "clsp_api_cl.h"
#include "clsp_api_sparse.h"
#include "clsp_enum_color.h"
#include "clsp_enum_ec.h"
#include "type_enumerator.h"

extern const char *GIT_SHA1;


/** object representing globals ******************************************/


enum streams {
    stream_first,
    stream_out = stream_first,
    stream_err,
    stream_debug,
    /* worker only */
    stream_worker_first,
    stream_sparse = stream_worker_first,
    stream_cl,
    stream_worker_last,
    stream_last = stream_worker_last
};

struct outstream {
    FILE        *stream;
    const char  *clr_begin;
    const char  *clr_end;
};

extern struct globals {
    struct outstream stream[stream_last];
    /* buffer for sparse deferred stream */
    struct g_deferred {
        char          *buffer;
        size_t        size;
    } deferred;
    struct cl_code_listener  *cl;
    /* API functions resolved in compile-/run-time set here */
    struct g_cl_api {
#define CL_DECL(prefix, item, cnt)                  \
    APPLY(API_CL_RET, API_PROPS(prefix, item))      \
    (*item)                                         \
    APPLY(API_CL_ARGDECL, API_PROPS(prefix, item));
        API_PROCEED(CL, CL_DECL)
#undef CL_DECL
    } cl_api;
    /* handles to dynamically opened libraried */
    struct g_cl_libs {
        size_t        cnt;
        void          **handles;  // !HAS_CL -> first is the main one
    } cl_libs;
    /* TODO typedb */
    int               debug;
    /* unexposed, but run-modifying options (e.g., for testing) */
    struct {
        bool          register_atexit;
    } unexposed;
} globals;

/* convenient macros for accessing parts of globals */
#define GLOBALS(what)  (globals.what)
#ifndef STREAM
# define STREAMSTRUCT(which)    GLOBALS(stream)[stream_##which]
# define STREAM(which)          GLOBALS(stream)[stream_##which].stream
# define STREAMCLRBEGIN(which)  GLOBALS(stream)[stream_##which].clr_begin
# define STREAMCLREND(which)    GLOBALS(stream)[stream_##which].clr_end
#endif



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


extern struct type_ptr_db {
    int                 last_base_type_uid;
    struct typen_data   *type_db;
    struct ptr_db_arr   ptr_db;
} type_ptr_db;


/** outputs ***************************************************************/


/*
    Universal print macro with implicit newline

    usage: PUT(stream_index, [[fmt], ...])
    NOTE: fmt has to be compile-time constant
    NOTE: last argument in order to allow format string as the only
          argument; compensated appending "%s" later on
 */
#define PUT(which, ...)        PUT_(which, __VA_ARGS__, STREAMCLREND(which))
#define PUT_(which, fmt, ...) \
    PUT__(which, 0, "%s" fmt "%s", STREAMCLRBEGIN(which), __VA_ARGS__)
#define PUT__(which, skip, fmt, ...) \
    fprintf(STREAM(which), &(fmt "\n")[skip], __VA_ARGS__)

/*
    Pre-mortem print macro

    Usage: DIE( WRAPPER(wrapper_args, [fmt, [...]]) )
           where WRAPPER is either nothing or one of macros defined below

    Note:  fmt should not start with any of DCHR_* characters (internal only)
 */

#define DLOC \
    "\n" __FILE__ ":" TOSTRING(__LINE__) ": note: from %s [internal location]"

#define DCHR_ERRNO      "@"
#define DCHR_ECODE      "$"
#define DCHR_ERRNOCODE  "#"

#define ERRNO(...)             DCHR_ERRNO __VA_ARGS__
/* Note: code (exit ~) can be [0,10), i.e., containing single digit */
#define ECODE_(flag,c,...)  flag TOSTRING(ECNUM(c)) __VA_ARGS__
#define ECODE(...)          ECODE_(DCHR_ECODE, __VA_ARGS__)
#define ERRNOCODE(...)      ECODE_(DCHR_ERRNOCODE, __VA_ARGS__)

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


/*
    Debugging
 */

#define DVALUE(arg)    (1 << arg)

#define D_OPTIONS      options,    "dump gathered options"
#define D_PLUGIN       plugin,     "print diagnostics regarding plugins"
#define D_INSTRUCTION  instruction,"print instruction being processed"
#define D_TYPE         type,       "print type being processed"
#define D_INSERT_TYPE  insert_type,"print type being inserted into type DB"
#define D_FILE         file,       "print current file to be proceeded"
#define DLIST(x)            \
    APPLY(x, D_OPTIONS    ) \
    APPLY(x, D_PLUGIN     ) \
    APPLY(x, D_INSTRUCTION) \
    APPLY(x, D_TYPE       ) \
    APPLY(x, D_INSERT_TYPE) \
    APPLY(x, D_FILE       )

enum {
#define X(name,desc)  d_##name,
    DLIST(X)
#undef X
    d_last,
    d_first = 0
};

/* two forms of usage... */
#define DLOG(level, ...)                \
    if (GLOBALS(debug) & DVALUE(level)) \
        PUT(debug, __VA_ARGS__)

#define WITH_DEBUG_LEVEL(level)                              \
    for (int i_=0; 0==i_ && (GLOBALS(debug) & DVALUE(level)) \
         ? 1                                                 \
         : 0                                                 \
         ; i_++)


/*
    Stream mangling
 */

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
     || -1 == close(temp)
    )
        DIE( ERRNO("swap_stream") );
}

/* this (and other similar constructs) mimics context Managers ala Python */
#define WITH_SWAPPED_STREAM(s1, s2)                 \
    for (int i_=0; 0==i_                            \
         ? (swap_stream(STREAM(s1), STREAM(s2)), 1) \
         : (swap_stream(STREAM(s2), STREAM(s1)), 0) \
         ; i_++)

#define WITH_COLORED_STREAM(s1, s2)                   \
    for (int i_=0; 0==i_                              \
         ? (fputs(STREAMCLRBEGIN(s1), STREAM(s2)), 1) \
         : (fputs(STREAMCLREND(s1), STREAM(s2)), 0)   \
         ; i_++)


/** allocations ***********************************************************/


/*  NOTE: use "MEM_NEW(foo)" for direct access ("MEM_NEW(foo).bar = 42")
          and "(MEM_NEW(foo))" as a function parameter ("bar((MEM_NEW(foo)))")
 */
#define MEM_NEW(var)                        \
   (((var) = malloc(sizeof(*(var))))        \
     ? (void) 0  /* NOOP */                 \
     : DIE( ERRNOCODE(MEM, "MEM_NEW") )     \
   ) , (var)

#define MEM_ARR_RESIZE(arr, newcnt)                      \
    (((arr) = realloc((arr), sizeof(*(arr)) * (newcnt))) \
      ? (void) 0 /* NOOP */                              \
      : DIE( ERRNOCODE(MEM, "MEM_ARR_RESIZE") )          \
    ) , (arr)

/* NOTE: one-liner for item append: *(MEM_ARR_APPEN(arr,size)) = item */
#define MEM_ARR_APPEND(arr, oldcnt)                          \
    (((arr) = realloc((arr), sizeof(*(arr)) * (++(oldcnt)))) \
      ? (void) 0 /* NOOP */                                  \
      : DIE( ERRNOCODE(MEM, "MEM_ARR_APPEND") )              \
    ) , (arr + oldcnt - 1)


/** internally mirrored API parts *****************************************/


/*
    sparse API:  API_SPARSE(item, ...)
 */

#define API_SPARSE(...)           API_SPARSE_MAP(__VA_ARGS__, IDENTITY)
#define API_SPARSE_MAP(item,...)  API_SPARSE_HOW(item)(item, __VA_ARGS__)
#define API_SPARSE_(...)          API_USE(SPARSE, __VA_ARGS__)
#define API_SPARSE_HOW(item) \
    API_SPARSE_HOW_(APPLY(API_SPARSE_OUT, API_PROPS(SPARSE, item)))
#define API_SPARSE_HOW_(out)      JOIN(API_SPARSE_USE_, out)

#define API_SPARSE_USE_X(fnc, ...)  API_SPARSE_(fnc, __VA_ARGS__)
#define API_SPARSE_USE_D(fnc, ...)        \
    WITH_SWAPPED_STREAM(out,debug)        \
        WITH_COLORED_STREAM(debug, debug) \
        API_SPARSE_(fnc, __VA_ARGS__)
#define API_SPARSE_USE_E(fnc, ...)        \
    WITH_SWAPPED_STREAM(sparse, err)      \
        WITH_COLORED_STREAM(sparse, err)  \
            API_SPARSE_(fnc, __VA_ARGS__)


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

#define WITH_FILE_TO_EMIT(file)           \
    for (int i_=0; i_==0                  \
         ? (API_EMIT(file_open, file), 1) \
         : (API_EMIT(file_close),      0) \
         ; i_++)

#define WITH_CALL_TO_EMIT(loc, dst, fnc)                \
    for (int i_=0; 0==i_                                \
         ? (API_EMIT(insn_call_open, loc, dst, fnc), 1) \
         : (API_EMIT(insn_call_close),               0) \
         ; i_++)

#define WITH_SWITCH_TO_EMIT(loc, op)                \
    for (int i_=0; 0==i_                            \
         ? (API_EMIT(insn_switch_open, loc, op), 1) \
         : (API_EMIT(insn_switch_close),         0) \
         ; i_++)

#define WITH_FNC_TO_EMIT(fnc)           \
    for (int i_=0; 0==i_                \
         ? (API_EMIT(fnc_open, fnc), 1) \
         : (API_EMIT(fnc_close),     0) \
         ; i_++)


/**
    Proceed sparse linearized code -> Code Listener conversion.

    In case of unrecoverable error (on both local and sparse side),
    dies immediately.

    @param[in] filelist  List of files selected for proceeding.
    @param[in] symlist   Internal symbols obtained by sparse_initialize call
 */
extern void proceed(struct string_list *filelist, struct symbol_list *symlist);


extern struct cl_type
    void_clt,
    incomplete_clt,
    bad_clt,
    int_clt,  sint_clt,  uint_clt,     short_clt, sshort_clt, ushort_clt,
    long_clt, slong_clt, ulong_clt,    llong_clt, sllong_clt, ullong_clt,
    // lllong_clt, slllong_clt, ulllong_clt
    char_clt, schar_clt, uchar_clt,
    bool_clt,
    float_clt, double_clt, ldouble_clt;


/**
    xxx
 */
extern void type_ptr_db_init(struct type_ptr_db *db);

/**
    xxx
 */
extern void type_ptr_db_destroy(struct type_ptr_db *db);


/* type "constructor" */


extern const struct cl_type pristine_cl_type;

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



static inline int
sizeof_from_bits(int bits)
{/* Alternative:
  * bytes_to_bits (sparse/target.h)
  *     - cons: we need the ceil value (1 bit ~ 1 byte), 0 in "strange" cases
  */
    return (bits > 0)
        ? (bits + bits_in_char - 1) / bits_in_char
        : 0;
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


extern struct cl_type *build_referenced_type(struct cl_type *orig_clt);


extern struct cl_type *type_ptr_db_insert(struct type_ptr_db *db,
                                          struct cl_type *clt,
                                          const struct symbol *type,
                                          struct ptr_db_item **ptr);

extern struct cl_type *type_ptr_db_lookup_item(struct type_ptr_db *db,
                                               const struct symbol *type,
                                               struct ptr_db_item **ptr);

extern struct ptr_db_item *new_ptr_db_item(void);


#endif
