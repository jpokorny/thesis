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
#ifndef CLSP_CONV_H_GUARD
#define CLSP_CONV_H_GUARD
/**
    Sparse -> CL conversion helpers
 */

#include "clsp-use-sparse.h"
#include "clsp-use-cl.h"

/**
    Sparse position -> CL position (location)
 */
static inline void
conv_position(struct cl_loc *dst, const struct position *src)
{
    dst->file   = SP(stream_name, src->stream);
    dst->line   = src->line;
    dst->column = src->pos;
    /* could we get this by examining input_streams? */
    /* XXXXXXX TODO: according to input_stream_nr XXXXXX */
    dst->sysp   = false;
}

/**
    Sparse scope -> CL scope

    @params[in] src  Symbol scope of which to be converted
 */
enum cl_scope_e conv_scope(const struct symbol *src);

/**
    Sparse string -> common string

    TODO: check there is a real problem with unterminated strings
          as otherwise nothing prevents direct use

    Alternatives:
    - show_string (sparse/token.h)
        - character escaping
        - is verbose about empty string

    NOTE: depends on lifetime, we may no need to strndup at all
 */
static inline const char *
conv_string(const struct string *src)
{
    assert(src->length);  /* there should always be at least the terminator */
    /* return strndup(src->data, src->length); */
    return src->data;
}

/**
    Sparse identifier -> common string

    Alternative:
    - show_ident (sparse/token.h)
        - is verbose about empty identifier string

    TODO: use string allocator
 */
static inline const char *
sparse_ident(const struct ident *src, const char *def)
{
    return (src /*&& ident->len*/)
            ? strndup(src->name, src->len)
            : def ? strdup(def) : def;
}


#endif
