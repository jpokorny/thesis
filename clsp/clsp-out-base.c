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

#include <stdio.h>
#include <unistd.h>         /* STD*_FILENO */
#include <errno.h>

#include "clsp-out-base.h"
#include "clsp-macros.h"    /* ASSERT_ENUM_RANGE_INTERNAL */

const char *const outstream_str[outstream_cnt] = {
#define X(name)  [outstream_##name] = #name,
    STREAMLIST(X)
#undef X
};


FILE *
stream_setup(outstreams outstreams, enum outstreams which,
             struct outstream_props props)
{
    ASSERT_ENUM_RANGE_INTERNAL(outstream, which);

    struct outstream *outstream = &outstreams[which];
    FILE **f = &outstream->stream;

    if (fd_deferred_restore == props.fd)
        props = OUTSTREAM_PROPS(outstream->deferred_dest, outstream->palette);
    else
        outstream->palette = props.palette;
    outstream->deferred_dest = fd_undef;

    switch (props.fd) {
        case STDOUT_FILENO: *f = stdout;  break;
        case STDERR_FILENO: *f = stderr;  break;
        case fd_deferred_unspec:
            /* unspecified deferred -> deferred, merge into stderr */
            props.fd = -STDERR_FILENO;
            /*FALLTHROUGH*/
        default:
            if (0 == props.fd) {
                *f = fopen("/dev/null", "a");
            } else if (0 < props.fd) {
                *f = fdopen(props.fd, "a");
            } else {
                /* deferred */
                *f = tmpfile();
                props.fd = outstream->deferred_dest = -props.fd;
            }
            if (!*f)
                DIE( ERRNO("fopen/fdopen/tmpfile (%s, fd=%d)",
                           outstream_str[which],props.fd) );
#if 0
            if (0 != setvbuf(*f, NULL, _IOLBF, 0))
                DIE( ERRNO("setvbuf (%s, fd=%d)",outstream_str[which],
                            props.fd) );
#endif
    }
    outstream->isatty = isatty(props.fd);
    if (!outstream->isatty)
        props.palette = PALETTE_NONE;

    /* first two colors are "empty" thus no need to switch anything */
    if ((props.palette.norm|props.palette.high) > clr_last_empty) {
        outstream->clr_norm = clr_codes[props.palette.norm];
        outstream->clr_high = clr_codes[props.palette.high];
        outstream->clr_end  = clr_codes[clr_terminate];
    } else {
        outstream->clr_norm = clr_codes[clr_none];
        outstream->clr_high = clr_codes[clr_none];
        outstream->clr_end  = clr_codes[clr_none];
    }

    /* using uninitialized stream during initial phase is avoided */
    DLOG(d_strm,
         "\t" HIGHLIGHT("stream") ": set up " HIGHLIGHT(_1(s))
         ": fd="_2(d)", isatty="_3(c),
         outstream_str[which], props.fd, GET_YN(outstream->isatty));

    return *f;
}


FILE *
stream_output_deferred(outstreams outstreams, enum outstreams which)
{
    ASSERT_ENUM_RANGE_INTERNAL(outstream, which);

    char *buf = NULL;
    long size = 0;
    struct outstream *outstream = &outstreams[which];
    FILE *file = outstream->stream;

    if (fd_undef == outstream->deferred_dest)
        return file;

    size = ftell(file);
    if (-1 >= size) {

        DIE( ERRNO("ftell") );

    } else if (0 < size) {

        errno = 0;
        rewind(file);
        if (0 != errno)
            DIE( ERRNO("rewind") );

        buf = malloc(sizeof(*buf) * size);
        if (!buf)
            DIE( ERRNO("malloc") );

        clearerr(file);
        size = fread(buf, sizeof(*buf), (size_t) size, file);
        if (ferror(file))
            DIE( ERRNO("fread") );

    }

    fclose(file);

    file = stream_setup(outstream, which,
                        OUTSTREAM_PROPS(fd_deferred_restore, PALETTE_NONE));

    clearerr(file);
    fwrite(buf, sizeof(*buf), (size_t) size, file);
    if (ferror(file))
        DIE( ERRNO("fwrite") );

    free(buf);

    return file;
}
