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

#include "clsp.h"

#include <stdlib.h>       /* strtol */
#include <signal.h>
#include <ctype.h>        /* isspace */
#include <string.h>       /* strcmp */
#include <stdbool.h>
#include <assert.h>

#include "clsp-macros.h"  /* APPLY, FOR_ENUM_RANGE, ... */
#include "clsp-ret.h"     /* enum retval */
#include "clsp-emit.h"    /* emit_file_interactive */


#define PROMPT           "%s> "

#define ICMDLIST(x)                                                        \
    APPLY(x,'h',help,    "show this help"                                ) \
    APPLY(x,'q',quit,    "cancel emitting, quit the program"             ) \
    APPLY(x,'n',next,    "proceed next instruction (same as just enter)" ) \
    APPLY(x,'c',continue,"continuous mode until next file"               ) \
    APPLY(x,'e',env,     "get/set environment parameters (no arg = list)") \
    APPLY(x,'d',debugger,"when debugged, switch to debugger"             )

enum icmds {
#define X(c, which, desc) icmd_##which,
    ICMDLIST(X)
#undef X
    icmd_cnt,
    icmd_last = icmd_cnt-1,
    icmd_first = 0
};

static const char *const icmd_str[icmd_cnt] = {
#define X(c, which, desc)  [icmd_##which] = desc,
    ICMDLIST(X)
#undef X
};

typedef enum retval (cmdhandler)(int *emit_props, char *line);

/* forward declaration */
#define X(c, which, desc) static cmdhandler icmd_handle_##which;
    ICMDLIST(X)
#undef X

static struct icmd {
    const char *fullcmd;
    cmdhandler *cmdhandler;
} icmds[icmd_cnt] = {
#define X(c, which, desc) \
    {.fullcmd = #which, .cmdhandler = &icmd_handle_##which},
    ICMDLIST(X)
#undef X
};

static enum retval
icmd_handle_quit(int *emit_props, char *line)
{
    (void) emit_props; (void) line;
    return ret_negative;
}

static enum retval
icmd_handle_help(int *emit_props, char *line)
{
    (void) emit_props; (void) line;
    PUT(out, "simple interactive mode, available commands:");
    FOR_ENUM_RANGE(i, icmd, first, last)
        PUT(out, "["_1(c)"]"_2(-8s)": "_3(s), icmds[i].fullcmd[0],
                 &icmds[i].fullcmd[1], icmd_str[i]);
    return ret_positive;
}

static enum retval
icmd_handle_next(int *emit_props, char *line)
{
    (void) emit_props; (void) line;
    return ret_escape;
}

/*
    environment parameters intermezzo start
 */

typedef void (*ienv_printer)(const int *emit_props,
                             const struct globals *globals_obj);
typedef enum retval (*ienv_setter)(int *emit_props, struct globals *globals_obj,
                                   const char *value);

/* modified copy from clsp-options.c */
static inline bool
get_nonnegative_num(const char *value, long *to_set)
{
    if (!isdigit(value[0])) {
        PUT(out, "not a numeric value: %s", value);
        return false;
    }
    *to_set = strtol(value, NULL, 10);
    if (0 > *to_set) {
        PUT(out, "must be positive number");
        return false;
    }
    return true;
}

static void
ienv_print_debug(const int *emit_props, const struct globals *globals_obj)
{
    (void) emit_props;
    /* nicer, as with dump_options */
    PUT(out, "debug: "_1(d), globals_obj->debug);
}

static enum retval
ienv_set_debug(int *emit_props, struct globals *globals_obj,
               const char *value)
{
    (void) emit_props;
    long tmp;
    if (!get_nonnegative_num(value, &tmp))
        return ret_escape;

    globals_obj->debug = tmp;
    PUT(out, "debug set to: "_1(d), globals_obj->debug);
    return ret_positive;
}

#define IENV_LIST(x) \
    APPLY(x, debug)

static const struct {
    const char   *key;
    ienv_printer printer;
    ienv_setter  setter;
} ienv_handlers [] = {
#define X(what) \
    { .key = #what, .printer = ienv_print_##what, .setter = ienv_set_##what },
    IENV_LIST(X)
#undef X
};

static enum retval
ienv_print(const int *emit_props, const struct globals *globals_obj,
           const char *what)
{
    if (!what) {
        for (size_t i = 0; i < ARRAY_SIZE(ienv_handlers); i++)
            ienv_handlers[i].printer(emit_props, globals_obj);
        return ret_positive;
    }
    for (size_t i = 0; i < ARRAY_SIZE(ienv_handlers); i++) {
        if (!strcmp(ienv_handlers[i].key, what)) {
            ienv_handlers[i].printer(emit_props, globals_obj);
            return ret_positive;
        }
    }
    return ret_negative;
}

static enum retval
ienv_set(int *emit_props, struct globals *globals_obj,
         const char *what, const char *value)
{
    assert(what);
    for (size_t i = 0; i < ARRAY_SIZE(ienv_handlers); i++) {
        if (!strcmp(ienv_handlers[i].key, what))
            return ienv_handlers[i].setter(emit_props, globals_obj, value);
    }
    return ret_negative;
}

/*
    environment parameters intermezzo end
 */

static enum retval
icmd_handle_env(int *emit_props, char *line)
{
    (void) emit_props;
    char *what_start;
    char *what_end;
    enum retval ret;

    while (!isspace(*line) && '\0' != *line)
        ++line;
    while (isspace(*line))
        ++line;

    if ('\0' == *line || '\n' == *line) {
        /* no parameter -> print all */
        ienv_print(emit_props, GLOBALS_PTR, NULL);
        return ret_positive;
    }

    what_start = line;
    while (!isspace(*line) && '\0' != *line)
        ++line;
    what_end = line;
    while (isspace(*line))
        ++line;
    *what_end = '\0';

    if ('\0' == *line)
        /* 1 parameter -> print specific */
        ret = ienv_print(emit_props, GLOBALS_PTR, what_start);
    else
        /* 2 parameters -> set specific */
        ret = ienv_set(emit_props, GLOBALS_PTR, what_start, line);

    if (ret_negative == ret) {
        PUT(out, "environment parameter not found: "_1(s), what_start);
        ret = ret_positive;
    } else if (ret_escape == ret) {
        /* setter failed due to bad value (already reported) */
        ret = ret_positive;
    }
    return ret;
}

static enum retval
icmd_handle_continue(int *emit_props, char *line)
{
    (void) line;
    *emit_props &= ~emit_file_interactive;
    return ret_escape;
}

static enum retval
icmd_handle_debugger(int *emit_props, char *line)
{
    (void) line;
    *emit_props &= ~emit_file_interactive;

    raise(SIGTRAP);
    return ret_escape;
}

#define BUFSIZE 32
bool
interact(int *emit_props)
{
    char buffer[BUFSIZE];
    char *cmd;
    struct icmd *icmd;
    enum retval ret;

    for (;;) {
        PUT__(out, 0, PROMPT, GLOBALS(basename));  /* omit newline */
        cmd = fgets(buffer, sizeof(buffer), stdin);
        if (!cmd)
            buffer[0] = '\0';
        while (isspace(*cmd))
            ++cmd;

        switch (*buffer) {
#define X(c, which, desc) \
    case c: icmd = &icmds[icmd_##which]; break;
            ICMDLIST(X)
#undef X
            case '\0':
                /* can be EOF -> quit */
                icmd = &icmds[icmd_quit];
                break;
            case '\n':
                /* just enter -> next */
                icmd = &icmds[icmd_next];
                break;
            default:
                PUT(out, "unknown command, press h for help");
                continue;
        }
        ret = icmd->cmdhandler(emit_props, cmd);
        if (ret_positive != ret)
            break;
    }

    return ret_escape == ret;  /* ret_negative otherwise */
}
