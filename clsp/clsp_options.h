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
#include "clsp_enum_color.h"

/**
    Special values for file descriptors.
 */
enum opts_fd_extra {
    opts_fd_undef    = -1,  /**< Undefined descriptor. */
    opts_fd_deferred = -2,  /**< Future deferred descriptor (sparse only). */
};


/**
    Object representing gathered options.

    @note  imm -> immediate values; set -> values that were set up later on
 */
struct options {
    /* internal options */
    bool                finalized;
    struct {
        bool            fork;
        struct oi_fd {
            int         cl;
            int         sparse;  /* options_fd_deferred is an extra option */
            int         debug;
        } fd;
        struct oi_clr {
            enum color cl;
            enum color sparse;
            enum color debug;
        } clr;
        int             debug;
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
    } sparse;
};

/**
    Gather options in the structure which is also pre-initialized.

    In case of unrecoverable error, dies immediately.

    @param[in,out] opts Target options representation.
    @param[in]     argc Common argc.
    @param[in]     argv Common argv.
    @return     0 = continue, <0 = exit ok (help, etc.), >0 = fail w/ this code
 */
extern int options_gather(struct options **opts, int argc, char *argv[]);

/**
    Free memory acquired while gathering options.

    @param[in,out] opts Options to be disposed.
 */
extern void options_dispose(struct options *opts);

/**
    Dump options.

    @param[in] opts Options to dump.
 */
extern void options_dump(const struct options *opts);


#endif
