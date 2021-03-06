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
#ifndef CLSP_USE_SPARSE_H_GUARD
#define CLSP_USE_SPARSE_H_GUARD
/**
    Customize sparse API (subset of it) to our needs
 */

#include "clsp-api-sparse.h"
#include "clsp-out-base.h"    /* PUT, _1(), ... */
#include "clsp-out-ext.h"     /* WITH_SWAPPED... */
#include "clsp-macros.h"      /* APPLY */


/*
    sparse API:  SP(item, ...)
 */

#define SP(...)           SP_MAP(__VA_ARGS__, IDENTITY)
#define SP_MAP(item,...)  SP_HOW(item)(item, __VA_ARGS__)
#define SP_(...)          API_USE(SPARSE, __VA_ARGS__)
#define SP_HOW(item)  SP_HOW_(APPLY(API_SPARSE_OUT, API_PROPS(SPARSE, item)))
#define SP_HOW_(out)  JOIN(SP_USE_, out)

#define SP_USE_C(...) (0 + SP_(__VA_ARGS__))  /* make an L-value */
#define SP_USE_X      SP_
#define SP_USE_D(fnc, ...)                      \
    WITH_SWAPPED_STREAM_HIGH_AS(debug, out, sp) \
        SP_(fnc, __VA_ARGS__)
#define SP_USE_F(fnc, ...)                      \
    WITH_SWAPPED_STREAM_HIGH_AS(debug, err, sp) \
        SP_(fnc, __VA_ARGS__)
#define SP_USE_E(fnc, ...)            \
    WITH_SWAPPED_STREAM_NORM(sp, err) \
        SP_(fnc, __VA_ARGS__)


/** helpers ***************************************************************/


/*
    output
 */

#define SPPOSFMT    "%s:%d:%d"

#define SPPOSFMT_1  _1(s)":"_2(d)":"_3(d)
#define SPPOSFMT_2  _2(s)":"_3(d)":"_4(d)

#define SPPOS(p)    SP(stream_name, (p).stream), (p).line, (p).pos


/*
    pseudo-related
 */

/**
    Returns if there is anything interesting about pseudo at all
 */
static inline bool
pseudo_futile(pseudo_t pseudo)
{
    return !pseudo || VOID == pseudo;
}

/**
    Returns if the pseudo carries "immediate" value
 */
static inline bool
pseudo_immediate(pseudo_t pseudo)
{
    return PSEUDO_SYM != pseudo->type && PSEUDO_ARG != pseudo->type;
}


/*
    passes (context manager + entry--exit hooks)
 */

#define WITH_PASS(which, ep)               \
    for (int i_=0; 0==i_                   \
         ? (pass_##which##_entry(ep))      \
         : ((pass_##which##_exit(ep)), 0)  \
         ; i_++)

#if DO_PER_EP_SET_UP_STORAGE
/* storage */
static inline bool
pass_storage_entry(struct entrypoint *ep)
{
    SP(set_up_storage, ep);
    return true;
}
static inline void
pass_storage_exit(struct entrypoint *ep)
{
    (void) ep;
    SP(free_storage);
}
#endif

/* unssa */
static inline bool
pass_unssa_entry(struct entrypoint *ep)
{
    SP(unssa, ep);
    return true;
}
static inline void
pass_unssa_exit(struct entrypoint *ep)
{
    (void) ep;
    /* no cleanup */
}


/*
    debug
 */

/**
    Show status of sparse allocators
 */
void sparse_alloc_show(void);


#define SPARSE_NAMESPACE_CODELIST(x) \
    APPLY(x, NONE        )           \
    APPLY(x, MACRO       )           \
    APPLY(x, TYPEDEF     )           \
    APPLY(x, STRUCT      )           \
    APPLY(x, LABEL       )           \
    APPLY(x, SYMBOL      )           \
    APPLY(x, ITERATOR    )           \
    APPLY(x, PREPROCESSOR)           \
    APPLY(x, UNDEF       )           \
    APPLY(x, KEYWORD     )           \

/** Scope to string reprezentation */
static inline const char *
debug_sparse_scope(const struct symbol *sym)
{
    const struct scope *scope = sym->scope;
    const char *scope_str;

    /* the order of comparisons is important */
    if (NULL == scope)
        scope_str = "NULL";
    else if (SP(global_scope) == scope)
        scope_str = "global";
    else if (SP(file_scope) == scope)
        scope_str = "file";
    else if (SP(function_scope) == scope)
        scope_str = "function";
    else if (SP(block_scope) == scope)
        scope_str = "block";
    else if (sym->ctype.modifiers | MOD_TOPLEVEL)
        scope_str = "unknown-toplevel";
    else
        scope_str = "unknown";

    return scope_str;
}

/**
    Show symbol information

    @param[in] sym        Symbol to be exposed
    @param[in] indent     Initial level of indentation
    @param[in] nondeterm  Allow extra, yet nondeterministic info (pointers)
 */
void debug_sparse_symbol(struct symbol *sym, int indent, bool nondeterm);

/**
    As @c debug_sparse_symbol, but with more details
 */
void
debug_sparse_symbol_detailed(struct symbol *sym, int indent);


#endif
