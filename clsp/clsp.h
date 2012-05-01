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
#ifndef CLSP_H_GUARD
#define CLSP_H_GUARD
/**
    Common base to be the first import in any executive (or globals-using) file

    This is to guarantee _POSIX_C_SOURCE is taken into account properly
    and also aggregates other program-wide headers (see at the bottom).
 */

#define _POSIX_C_SOURCE 200809L  /* fileno */

#define USE_INT3_AS_BRK
#include "trap.h"

#include <stdint.h>  /* uintptr_t */


/*
    Here comes the bootstrap process to solve circular dependency (globals
    struct needs types definitions from other headers [compilation dependency]
    while these depend on macros defined here [preprocessor dependency]),
    a small setback in our cohesion and modularity pursuit:

    - define macros forward-referring to struct globals (struct encapsulating
      our global variables) which is defined later on, after necessary files
      are included

    - include necessary files to complete struct globals

    - include the rest of common headers as this file is meant as a facade
      to include them automatically; for exact reference, see below
 */

/* macros to access globals (declared below), also used by other modules */
#define GLOBALS_PTR    (&globals)
#define GLOBALS(what)  (GLOBALS_PTR->what)
#ifndef STREAM   /* due to test_swapfd.c */
# define STREAMSTRUCT(which)   GLOBALS(outstreams)[outstream_##which]
# define STREAM(which)         GLOBALS(outstreams)[outstream_##which].stream
# define STREAMCLRNORM(which)  GLOBALS(outstreams)[outstream_##which].clr_norm
# define STREAMCLRHIGH(which)  GLOBALS(outstreams)[outstream_##which].clr_high
# define STREAMCLREND(which)   GLOBALS(outstreams)[outstream_##which].clr_end
#endif

/* complete types within struct globals... */
#include "clsp-out-base.h"  /* outstreams (typedef) */
#include "clsp-api-cl.h"    /* cl_code_listener + Code Listener top-level API */
#include "clsp-types.h"     /* type_ptr_db */

/* ...and finally declare it */
extern struct globals {

    outstreams    outstreams;
    long          debug;
    int           indent;

    const char    *basename;
    const char    *basename_free;

    struct cl_code_listener
                  *cl;
    struct type_ptr_db type_ptr_db;

    struct {
        uintptr_t fnc;
        uintptr_t var;
        uintptr_t bb;
    } counters;

    /* API functions resolved in compile-/run-time set here */
    struct g_cl_api {
#define CL_DECL(prefix, item, cnt)                  \
    APPLY(API_CL_RET, API_PROPS(prefix, item))      \
    (*item)                                         \
    APPLY(API_CL_ARGDECL, API_PROPS(prefix, item));
        API_PROCEED(CL, CL_DECL)
#undef CL_DECL
    } cl_api;

    /* handles to dynamically opened libraries */
    struct g_cl_libs {
        size_t    cnt;
        void      **handles;  /**< !HAS_CL -> first is the main one */
    } cl_libs;

    /* unexposed, yet run-modifying options (e.g., for testing) */
    struct {
        bool      register_atexit;
    } unexposed;

} globals;


#define TYPEPTRDB      (&GLOBALS(type_ptr_db))
#define COUNTER(what)  (GLOBALS(counters.what))

/* now, pull in the other "implicit" stuff */
#include "clsp-out-ext.h"  /* WARN, DLOG, DIE, ...*/
#include "clsp-alloc.h"    /* MEM_* */


extern const char *GIT_SHA1;


#endif
