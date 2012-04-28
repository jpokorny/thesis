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

#include <signal.h>

#include "clsp-macros.h"  /* APPLY, FOR_ENUM_RANGE, ... */
#include "clsp-ret.h"     /* enum retval */
#include "clsp-emit.h"    /* emit_file_interactive */


#define PROMPT           "clsp> "

#define ICMDLIST(x)                                                        \
    APPLY(x,'h',help,    "show this help"                                ) \
    APPLY(x,'q',quit,    "cancel emitting, quit the program"             ) \
    APPLY(x,'n',next,    "next instruction (same as pressing just enter)") \
    APPLY(x,'c',continue,"continuous mode until next file"               ) \
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

typedef enum retval (cmdhandler)(int *emit_props, const char *line);

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
icmd_handle_quit(int *emit_props, const char *line)
{
    (void) emit_props; (void) line;
    return ret_negative;
}

static enum retval
icmd_handle_help(int *emit_props, const char *line)
{
    (void) emit_props; (void) line;
    PUT(out, "simple interactive mode, available commands:");
    FOR_ENUM_RANGE(i, icmd, first, last)
        PUT(out, "["_1(c)"]"_2(-8s)": "_3(s), icmds[i].fullcmd[0],
                 &icmds[i].fullcmd[1], icmd_str[i]);
    return ret_positive;
}

static enum retval
icmd_handle_next(int *emit_props, const char *line)
{
    (void) emit_props; (void) line;
    return ret_escape;
}

static enum retval
icmd_handle_continue(int *emit_props, const char *line)
{
    (void) line;
    *emit_props &= ~emit_file_interactive;
    return ret_escape;
}

static enum retval
icmd_handle_debugger(int *emit_props, const char *line)
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
        PUT__(out, 0, "%s", PROMPT);
        cmd = fgets(buffer, sizeof(buffer), stdin);
        if (!cmd)
            buffer[0] = '\0';

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
