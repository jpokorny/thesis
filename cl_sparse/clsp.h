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

#ifndef CL_SPARSE_H_GUARD
#define CL_SPARSE_H_GUARD



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

struct globals {
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
};

extern struct globals globals;

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

/* vim:set ts=4 sts=4 sw=4 et: */
