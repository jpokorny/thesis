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
#ifndef CLSP_OPTIONS_H_GUARD
#define CLSP_OPTIONS_H_GUARD

#include <stdbool.h>
#include <limits.h>          /* INT_MIN */
#include "clsp-enum-color.h"
#include "clsp-enum-ec.h"    /* enum retval */


/**
    Special values for file descriptors.
 */
enum fd_extra {
    fd_undef           = -1,       /**< Undefined descriptor. */
    fd_deferred_unspec = INT_MIN,  /**< Deferred descriptor without destination. */
};


/**
    Object representing gathered options.
 */
struct options {
    /* internal options */
    bool                finalized;
    struct {
        struct oi_fd {
            int         warn;
            int         debug;
            int         sp;
            int         cl;
            int         cl_debug;
        } fd;
        struct oi_clr {
            struct palette  warn;
            struct palette  debug;
            struct palette  sp;
            struct palette  cl;
            struct palette  cl_debug;
        } clr;
        int             debug;
        int             emit_props;
    } internals;
    /* Code Listener */
    struct {
        struct {
            size_t      cnt;
            const char  **arr;  /* !HAS_CL -> first is the main one */
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
    } cl;
    /* sparse */
    struct {
        int             argc;
        char            **argv;
        bool            preprocess;
    } sparse;
};

/**
    Gather options in the structure which is also pre-initialized.

    In case of unrecoverable error, dies immediately.

    @param[in,out] opts Target options representation.
    @param[in]     argc Common argc.
    @param[in]     argv Common argv.
    @return        See enum @retval.
 */
extern enum retval options_gather(struct options **opts, int argc,
                                  char *argv[]);

/**
    Dump options.

    @param[in] opts Options to dump.
 */
extern void options_dump(const struct options *opts);

/**
    Free memory acquired while gathering options.

    @param[in,out] opts Options to be disposed.
 */
extern void options_dispose(struct options *opts);

#endif
