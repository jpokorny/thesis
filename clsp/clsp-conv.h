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


/*
    debug macros: type
 */

#define DEBUG_TYPE_FROM_SYMBOL_SP(sym)                                    \
    WITH_DEBUG_LEVEL(type) {                                              \
        PUT(debug, "\tdebug: " HIGHLIGHT("type") ": sp >>>");             \
        PUTHI(debug, sp, GLOBALS(indent), _1(s), SP(show_typename, sym)); \
    }

#define DEBUG_TYPE_FROM_SYMBOL_CL(clt)                          \
    WITH_DEBUG_LEVEL(type) {                                    \
        PUT(debug, "\tdebug: " HIGHLIGHT("type") ": cl <<<");   \
        debug_cl_type(clt, GLOBALS(indent));                    \
    }


/*
    debug macros: instruction
 */

#define DEBUG_INSN_SP(insn)                                                  \
    WITH_DEBUG_LEVEL(insn) {                                                 \
        PUT(debug,                                                           \
            SPPOSFMT_1 ": debug: " HIGHLIGHT("insn") ": sp >>>",             \
            SPPOS(insn->pos));                                               \
        PUTHI(debug, sp, GLOBALS(indent),_1(s), SP(show_instruction, insn)); \
    }

#define DEBUG_INSN_CL(cli)                                        \
    WITH_DEBUG_LEVEL(insn) {                                      \
        PUT(debug,                                                \
            CLPOSFMT_1 ": debug: " HIGHLIGHT("insn") ": cl <<<",  \
            CLPOS(cli.loc));                                      \
        debug_cl_insn(&cli, GLOBALS(indent), false);              \
    }


/*
    debug macros: operand
 */

#define DEBUG_OP_FROM_PSEUDO_CACHE(pseudo)                                \
    WITH_DEBUG_LEVEL(oper) {                                              \
        PUT(debug, "\tdebug: " HIGHLIGHT("pseudo") ": cache hit");        \
        /* do it safely as it may be "LHS found when initializing it" */  \
        debug_cl_operand(pseudo->priv, GLOBALS(indent), true);            \
    }

#define DEBUG_OP_FROM_SYMBOL_CACHE(sym)                                   \
    WITH_DEBUG_LEVEL(oper) {                                              \
        PUT(debug,                                                        \
            SPPOSFMT_1 ": debug: " HIGHLIGHT("symbol") ": cache hit: "    \
            HIGHLIGHT(_4(s)), SPPOS(sym->pos),                            \
            sym->ident ? sym->ident->name : "<anon-sym>");                \
        /* do it safely as it may be "LHS found when initializing it" */  \
        debug_cl_operand((const struct cl_operand *) sym->pseudo,         \
                         GLOBALS(indent), true);                          \
    }

#define DEBUG_OP_FROM_PSEUDO_SP(pseudo)                                    \
    WITH_DEBUG_LEVEL(oper) {                                               \
        PUT(debug, "\tdebug: " HIGHLIGHT("pseudo") ": sp >>>");            \
        PUTHI(debug, sp, GLOBALS(indent), _1(s), SP(show_pseudo, pseudo)); \
    }

#define DEBUG_OP_FROM_SYMBOL_SP(sym)                                      \
    WITH_DEBUG_LEVEL(oper) {                                              \
        PUT(debug,                                                        \
            SPPOSFMT_1 ": debug: " HIGHLIGHT("symbol") ": sp >>> "        \
            HIGHLIGHT(_4(s)), SPPOS(sym->pos),                            \
            sym->ident ? sym->ident->name : "<anon-sym>");                \
        debug_sparse_symbol(sym, GLOBALS(indent));                        \
    }

#define DEBUG_OP_FROM_PSEUDO_CL(op)                                       \
    WITH_DEBUG_LEVEL(oper) {                                              \
        PUT(debug, "\tdebug: " HIGHLIGHT("pseudo") ": cl <<<");           \
        debug_cl_operand(op, GLOBALS(indent), false);                     \
    }

#define DEBUG_OP_FROM_SYMBOL_CL(sym, op)                                  \
    WITH_DEBUG_LEVEL(oper) {                                              \
        PUT(debug,                                                        \
            SPPOSFMT_1 ": debug: " HIGHLIGHT("symbol") ": cl <<< "        \
            HIGHLIGHT(_4(s)), SPPOS(sym->pos),                            \
            sym->ident ? sym->ident->name : "<anon-sym>");                \
        debug_cl_operand(op, GLOBALS(indent), false);                     \
    }


/*
    debug macros: initializer
 */

#define DEBUG_INITIALIZER_EXPR_START()                               \
    do {                                                             \
        DLOG(init, "\tdebug: " HIGHLIGHT("initializer") ": start");  \
        GLOBALS(indent) += 8/INDENT_MULT ;                           \
    } while (0)

#define DEBUG_INITIALIZER_EXPR_STOP()                                \
     do {                                                            \
        GLOBALS(indent) -= 8/INDENT_MULT ;                           \
        DLOG(init, "\tdebug: " HIGHLIGHT("initializer") ": stop");   \
    } while (0)

#define DEBUG_INITIALIZER_EXPR_SP(insn)                                       \
    WITH_DEBUG_LEVEL(init) {                                                  \
        DLOG(init, "\tdebug: " HIGHLIGHT("initializer") ": sp >>>");          \
        PUTHI(debug, sp, GLOBALS(indent), _1(s), SP(show_instruction, insn)); \
    }

#define DEBUG_INITIALIZER_EXPR_CL(initial)                                   \
    WITH_DEBUG_LEVEL(init) {                                                 \
        DLOG(init, "\tdebug: " HIGHLIGHT("initializer") ": cl <<<");         \
        debug_cl_insn(&(initial)->insn, GLOBALS(indent), true);              \
    }

#define DEBUG_INITIALIZER_SP(expr)                                           \
    WITH_DEBUG_LEVEL(init) {                                                 \
        PUT(debug, "\tdebug: " HIGHLIGHT("initializer") ": sp >>>");         \
        SP(show_expression, expr);                                           \
    }

#define DEBUG_INITIALIZER_CL(initial)                                        \
    WITH_DEBUG_LEVEL(init) {                                                 \
        PUT(debug, "\tdebug: " HIGHLIGHT("initializer") ": cl <<<");         \
        debug_cl_initializer(initial, GLOBALS(indent));                      \
    }


#endif
