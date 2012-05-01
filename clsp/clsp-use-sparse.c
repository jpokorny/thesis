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
#include "clsp-use-sparse.h"

#include <stdbool.h>

void
debug_sparse_symbol(struct symbol *sym, int indent, bool nondeterm)
{
    const char *scope_str = debug_sparse_scope(sym);
    unsigned long alignment_backup = sym->ctype.alignment;
    struct statement *stmt_backup;

    PUTHI(debug, sp, indent,
          "{kind="_1(s)", initializer="_2(c)", scope="_3(s)_4(s)"}",
          SP(get_type_name, sym->type), GET_YN(sym->initializer),
          scope_str, (SP(is_outer_scope, sym->scope)) ? ":outer" : "");

    if (nondeterm)
        SP(debug_symbol, sym);

    /*
        for function, we follow instructions, no need to see yet another form
        of code generated from statements (pre-linearization phase outcome);
        similarly for initializers -> backup and restore later on
     */

#if HIDE_INITIALIZER
    struct expression *initializer_backup = sym->initializer;
    sym->initializer = NULL;
#endif
    sym->ctype.alignment = 0;

    if (SYM_FN == sym->ctype.base_type->type) {
        stmt_backup = sym->ctype.base_type->stmt;
        sym->ctype.base_type->stmt = NULL;
    }

    SP(show_symbol, sym);

    if (SYM_FN == sym->ctype.base_type->type)
        sym->ctype.base_type->stmt = stmt_backup;
#if HIDE_INITIALIZER
    sym->initializer = initializer_backup;
#endif
    sym->ctype.alignment = alignment_backup;
}


void
debug_sparse_symbol_detailed(struct symbol *sym, int indent)
{
    debug_sparse_symbol(sym, indent, true);

#if 0
    PUT(debug,
        SPPOSFMT_1 ": debug: " HIGHLIGHT("sym") ": begin " HIGHLIGHT(_4(s)),
        SPPOS(sym->pos), sym->ident ? sym->ident->name : "<anon-sym>");

    struct symbol *base_type = sym->ctype.base_type;
    if (!base_type)
        CL_TRAP;
    switch (base_type->type) {
        /*  These could occur:
            SYM_ENUM, SYM_BITFIELD

            These can 100% occur (w/ or w/o initializer, empiric underapprox.):
            SYM_STRUCT    (both)
            SYM_UNION     (w/o)
            SYM_ARRAY     (w/o)
            SYM_BASETYPE  (both)
            SYM_PTR       (both)
            SYM_FN        (w/o + no EP or initial,
                           incl. no ctype.base_type->statement)
         */

        /* Not sure, capture them */
        WARN_CASE_UNHANDLED(sym->pos, SYM_LABEL)
        WARN_CASE_UNHANDLED(sym->pos, SYM_RESTRICT)
        WARN_CASE_UNHANDLED(sym->pos, SYM_FOULED)

        WARN_CASE_UNHANDLED(sym->pos, SYM_UNINITIALIZED)
        WARN_CASE_UNHANDLED(sym->pos, SYM_KEYWORD)
        WARN_CASE_UNHANDLED(sym->pos, SYM_BAD)
        WARN_CASE_UNHANDLED(sym->pos, SYM_NODE)

        /* These should not occur */
        WARN_CASE_UNHANDLED(sym->pos, SYM_PREPROCESSOR)
        WARN_CASE_UNHANDLED(sym->pos, SYM_TYPEDEF)
        WARN_CASE_UNHANDLED(sym->pos, SYM_TYPEOF)

        /*unused*/
        WARN_CASE_UNHANDLED(sym->pos, SYM_MEMBER)
    }

    if (sym->initializer)
        WARN_UNHANDLED(sym->pos, "INITIALIZER missed");

    WITH_DEBUG_LEVEL(symb)
        PUT(debug,
            SPPOSFMT_1 ": debug: " HIGHLIGHT("sym") ": end " HIGHLIGHT(_4(s)),
            SPPOS(sym->endpos), sym->ident ? sym->ident->name : "<anon-sym>");
#endif
}


void
sparse_alloc_show(void)
{
    /* hoisting respective SP() decorator verbatim around all the invocations */
    WITH_SWAPPED_STREAM_HIGH_AS(debug, err, sp) {
        /* common */
        show_ident_alloc();
        /*SP(show_token_alloc();*/
        show_context_alloc();
        show_symbol_alloc();
        show_expression_alloc();
        show_statement_alloc();
        show_string_alloc();
        show_scope_alloc();
        show_bytes_alloc();
        show_basic_block_alloc();
        show_entrypoint_alloc();
        show_instruction_alloc();
        show_multijmp_alloc();
        show_pseudo_alloc();

        /* linearize */
        show_pseudo_user_alloc();
        show_asm_constraint_alloc();
        show_asm_rules_alloc();
    }
}
