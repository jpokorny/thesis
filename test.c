/*
 * Copyright (C) 2009 Kamil Dudka <kdudka@redhat.com>
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


#define _GNU_SOURCE

#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

//#include "config.h"
#include "trap.h"
#include "code_listener.h"
#include "type_enumerator.h"

// sparse headers
#include "sparse/expression.h"
#include "sparse/flow.h"
#include "sparse/linearize.h"
#include "sparse/parse.h"
#include "sparse/scope.h"
#include "sparse/storage.h"
#include "sparse/symbol.h"
#include "sparse/token.h"

#define DO_EXPAND_SYMBOL            1
#define DO_PER_EP_UNSAA             1
#define DO_PER_EP_SET_UP_STORAGE    1
#define SHOW_PSEUDO_INSNS           0

#define WARN_UNHANDLED(pos, what) do { \
    sl_warn(pos, "warning: '%s' not handled", what); \
    fprintf(stderr, \
            "%s:%d: note: raised from function '%s' [internal location]\n", \
            __FILE__, __LINE__, __FUNCTION__); \
} while (0)

#define WARN_UNHANDLED_SYM(sym) \
    WARN_UNHANDLED((sym)->pos, show_ident((sym)->ident))

#define WARN_VA(pos, fmt, ...) do {\
    sl_warn(pos, "warning: " fmt, __VA_ARGS__); \
    fprintf(stderr, \
            "%s:%d: note: raised from function '%s' [internal location]\n", \
            __FILE__, __LINE__, __FUNCTION__); \
} while (0)

#define WARN_CASE_UNHANDLED(pos, what) \
    case what: WARN_UNHANDLED(pos, #what); break;

#define SP_NEW(type) \
    (type *) malloc(sizeof(type))

#define SP_RESIZE(ptr, newnum) \
    realloc(ptr, sizeof(*ptr) * (newnum))

#ifndef STREQ
#   define STREQ(s1, s2) (0 == strcmp(s1, s2))
#endif

typedef struct typen_data *type_db_t;

static struct cl_code_listener *cl = NULL;
static type_db_t type_db = NULL;

static void cb_free_clt(struct cl_type *clt)
{
    enum cl_type_e code = clt->code;
    switch (code) {
        case CL_TYPE_PTR:
            free(clt->items);
            break;

        // TODO
        default:
            break;
    }

    free((char *) clt->name);
    free(clt);
}

static type_db_t type_db_create(void )
{
    type_db_t db = typen_create(cb_free_clt);
    if (!db)
        die("ht_create() failed");

    // guaranteed to NOT return NULL
    return db;
}

static void type_db_destroy(type_db_t db)
{
    typen_destroy(db);
}

static struct cl_type* type_db_insert(type_db_t db, struct cl_type *clt,
                                      void *key)
{
    struct cl_type *rv = typen_insert_as_new(db, clt, key);
    if (!rv)
        die("typen_insert_as_new() failed");

    // guaranteed to NOT return NULL
    return rv;
}

static void sl_warn(struct position pos, const char *fmt, ...)
{
    va_list ap;

    fprintf(stderr, "%s:%d: ", stream_name(pos.stream), pos.line);

    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    va_end(ap);

    fprintf(stderr, "\n");
}

static void read_sparse_location(struct cl_loc *loc, struct position pos)
{
    loc->file   = stream_name(pos.stream);
    loc->line   = pos.line;
    loc->column = pos.pos;
    loc->sysp   = /* not used by SPARSE */ false;
}

static void read_sparse_scope(enum cl_scope_e *p, struct scope *scope)
{
    if (!scope || scope == global_scope)
        *p = CL_SCOPE_GLOBAL;
    else if (scope == file_scope)
        *p = CL_SCOPE_STATIC;
    else if (scope == function_scope)
        TRAP;
    else if (scope == block_scope)
        TRAP;
    else
        // FIXME
        *p = CL_SCOPE_FUNCTION;
}

static struct cl_type* add_type_if_needed(struct symbol *type);

static struct cl_type_item* create_ptr_type_item(struct symbol *type)
{
    struct cl_type_item *item = SP_NEW(struct cl_type_item);
    if (!item)
        die("SP_NEW failed");

    item->type = /* FIXME: unguarded recursion */
                 add_type_if_needed(type->ctype.base_type);
    item->name = NULL;

    // guaranteed to NOT return NULL
    return item;
}

static enum cl_type_e clt_code_from_type(struct symbol *type)
{
    if (type == &void_ctype)
        return CL_TYPE_VOID;

    if (type == &int_ctype ||
        /* FIXME */ type == &sint_ctype || type == &uint_ctype)
        return CL_TYPE_INT;

    if (type == &char_ctype ||
        /* FIXME */ type == &schar_ctype || type == &uchar_ctype)
        return CL_TYPE_CHAR;

    if (type == &bool_ctype)
        return CL_TYPE_BOOL;

    // unknown base type
    return CL_TYPE_UNKNOWN;
}

static void dig_fn_arguments(struct cl_type *clt, struct symbol_list* args)
{
    struct symbol *sym;
    FOR_EACH_PTR(args, sym) {
        clt->items = SP_RESIZE(clt->items, clt->item_cnt+1);
        if (!clt->items)
            die("SP_RESIZE failed");
        struct cl_type_item *item = &clt->items[clt->item_cnt++];
        item->type = /* recursion */ add_type_if_needed(sym->ctype.base_type);
        item->name = NULL;
    } END_FOR_EACH_PTR(sym);
    clt->item_cnt++;
}

static void read_sparse_type(struct cl_type *clt, struct symbol *type)
{
    enum type code = type->type;

    clt->code       = clt_code_from_type(type);
    clt->size       = /* TODO */ 0;
    clt->item_cnt   = 0;
    clt->items      = NULL;
    if (clt->code != CL_TYPE_UNKNOWN)
        return;

    switch (code) {
        case SYM_PTR:
            clt->code       = CL_TYPE_PTR;
            clt->size       = /* TODO */ 0;
            clt->item_cnt   = 1;
            clt->items      = create_ptr_type_item(type);
            break;

        case SYM_STRUCT:
            clt->code       = CL_TYPE_STRUCT;
            clt->name       = strdup(show_ident(type->ident));
            clt->size       = /* TODO */ 0;
            clt->item_cnt   = /* TODO */ 0;
            clt->items      = /* TODO */ NULL;
            break;

        case SYM_UNION:
            clt->code       = CL_TYPE_UNION;
            clt->name       = strdup(show_ident(type->ident));
            clt->size       = /* TODO */ 0;
            clt->item_cnt   = /* TODO */ 0;
            clt->items      = /* TODO */ NULL;
            break;

        case SYM_FN:
            clt->code       = CL_TYPE_FNC;
            clt->item_cnt   = 1;
            clt->items      = create_ptr_type_item(type);
            dig_fn_arguments(clt, type->arguments);
            break;

        case SYM_ENUM:
            clt->code       = CL_TYPE_ENUM;
            clt->name       = strdup(show_ident(type->ident));
            break;

        default:
            // TRAP;
            clt->code       = CL_TYPE_UNKNOWN;
            clt->name       = strdup(show_typename(type));
    }
}

static void skip_sparse_accessors(struct symbol **ptype)
{
    while (*ptype) {
        struct symbol *type = *ptype;
        switch (type->type) {
            case SYM_NODE:
            case SYM_ARRAY:
            case SYM_BITFIELD:
                // skip accessor
                break;

            default:
                return;
        }
        *ptype = type->ctype.base_type;
    }
}

static struct cl_type* add_type_if_needed(struct symbol *type)
{
    struct cl_type *clt;
    
    // FIXME: this approach is completely wrong since we get type info for the
    // operand's base however we need to get type info in regards to the given
    // accessor
    skip_sparse_accessors(&type);

    clt = typen_get_by_key(type_db, type);
    if (clt)
        // type already hashed
        return clt;
    
    // allocate new clt
    clt = SP_NEW(struct cl_type);
    if (!clt)
        die("SP_NEW failed");

    // make sure all members will be initialized
    clt->code       = CL_TYPE_UNKNOWN;
    clt->name       = NULL;
    clt->size       = 0;
    clt->item_cnt   = 0;
    clt->items      = NULL;
    clt->scope      = CL_SCOPE_GLOBAL;
    clt->loc.file   = NULL;
    clt->loc.line   = -1;

    // read type info if available
    if (type) {
        read_sparse_type(clt, type);
        read_sparse_location(&clt->loc, type->pos);
        read_sparse_scope(&clt->scope, type->scope);
        int bit_size = bits_to_bytes(type->bit_size);
        clt->size = bit_size >= 0 ? bit_size : 0;
    }
    
    if (clt->code == CL_TYPE_UNKNOWN && !clt->name)
        clt->name = strdup("<sparse type not available>");

    // hash the just read type for next wheel
    return type_db_insert(type_db, clt, type);
}

static struct cl_type* clt_from_sym(struct symbol *sym)
{
    if (!sym || !sym->ctype.base_type)
        TRAP;

    return add_type_if_needed(sym->ctype.base_type);
}

static bool is_pseudo(pseudo_t pseudo)
{
    return pseudo
        && pseudo != VOID;
}

static void free_cl_cst_data(struct cl_operand *op)
{
    switch (op->data.cst.code) {
        case CL_TYPE_FNC:
            free((char *) op->data.cst.data.cst_fnc.name);
            break;

        case CL_TYPE_STRING:
            free((char *) op->data.cst.data.cst_string.value);
            break;

        // TODO
        default:
            break;
    }
}

static void free_cl_operand_data(struct cl_operand *op)
{
    switch (op->code) {
        case CL_OPERAND_VAR:
            free((char *) op->data.var->name);
            free(op->data.var);
            break;

        case CL_OPERAND_CST:
            free_cl_cst_data(op);
            break;

        // TODO
        default:
            break;
    }

    // TODO
    free(op->accessor);
}

static const char* strdup_sparse_string(const struct string *str)
{
    return (str->length)
        ? strndup(str->data, str->length)
        : NULL;
}

static /* const */ struct cl_type builtin_int_type = {
    .uid            = /* FIXME */ -1,
    .code           = CL_TYPE_INT,
    .loc = {
        .file       = NULL,
        .line       = -1
    },
    .scope          = CL_SCOPE_GLOBAL,
    .name           = "<builtin_int>",
    .size           = /* FIXME */ sizeof(int)
};

static void read_sym_initializer(struct cl_operand *op, struct expression *expr)
{
    if (!expr)
        return;

    switch (expr->type) {
        case EXPR_STRING:
            op->code                    = CL_OPERAND_CST;
            op->type                    = clt_from_sym(expr->ctype);
            op->data.cst.code           = CL_TYPE_STRING;
            op->data.cst.data.cst_string.value   =
                strdup_sparse_string(expr->string);
            return;

        default:
            TRAP;
    }
}

static void read_pseudo_sym(struct cl_operand *op, struct symbol *sym)
{
    struct symbol *base;

    // read symbol location and scope
    read_sparse_location(&op->loc, sym->pos);
    read_sparse_scope(&op->scope, sym->scope);

    if (sym->bb_target) {
        WARN_UNHANDLED(sym->pos, "sym->bb_target");
        op->code = CL_OPERAND_VOID;
        return;
    }

    if (!sym->ident) {
        read_sym_initializer(op, sym->initializer);
        return;
    }

    base = sym->ctype.base_type;
    if (base && base->type == SYM_FN) {
        op->code                            = CL_OPERAND_CST;
        op->type                            = clt_from_sym(sym);
        op->data.cst.code                   = CL_TYPE_FNC;
        op->data.cst.data.cst_fnc.name      = strdup(show_ident(sym->ident));
        op->data.cst.data.cst_fnc.is_extern = MOD_EXTERN & sym->ctype.modifiers;
        op->data.cst.data.cst_fnc.uid       = /* TODO */ (int)(long) sym;
    } else {
        op->code                            = CL_OPERAND_VAR;
        op->type                            = clt_from_sym(sym);
        op->data.var->uid                     = /* TODO */ (int)(long) sym;
        op->data.var->name                   = strdup(show_ident(sym->ident));
    }
}

static struct symbol *get_symbol_at_nr(pseudo_t pseudo)
{
    struct symbol *sym, *retval = NULL;
    int nr = pseudo->nr;

    // FIXME: lot of possible but missing checks
    FOR_EACH_PTR(pseudo->def->bb->ep->name->ctype.base_type->arguments, sym) {
        if (--nr == 0)
            retval = sym;
    } END_FOR_EACH_PTR(sym);
    return retval;
}


static void read_pseudo(struct cl_operand *op, pseudo_t pseudo)
{
    switch(pseudo->type) {
        case PSEUDO_SYM:  /* union -> sym */
            read_pseudo_sym(op, pseudo->sym);
            break;

        case PSEUDO_REG: { /* union -> def */
            op->code                = CL_OPERAND_VAR;
            op->type                = add_type_if_needed(pseudo->def->type);
            op->data.var            = SP_NEW(struct cl_var);
            op->data.var->uid       = /* TODO */ (int)(long) pseudo->def;
            op->data.var->name      = NULL;
            break;
        }

        case PSEUDO_VAL: { /* union -> val */
            long long value = pseudo->value;

            op->code                = CL_OPERAND_CST;
            op->type                = /* TODO */ &builtin_int_type;
            op->data.cst.code       = CL_TYPE_INT;
            op->data.cst.data.cst_int.value  = value;
            return;
        }

        case PSEUDO_ARG: { /* union -> def */
            struct symbol *sym = get_symbol_at_nr(pseudo);
            if (!sym)
                printf("not sym\n");

            op->code                = CL_OPERAND_VAR;
            op->scope               = CL_SCOPE_FUNCTION;
            if (!sym) {
                op->type                = /* TODO */ &builtin_int_type;
                op->data.var            = SP_NEW(struct cl_var);
                op->data.var->uid       = /* TODO */ (int)(long) pseudo->def;
                op->data.var->name      = NULL;
            } else {
                op->type                = clt_from_sym(sym);
                op->data.var            = SP_NEW(struct cl_var);
                op->data.var->uid       = (int)(long) sym;
                op->data.var->name      = strdup(show_ident(sym->ident));
            }
            //op->data.var->artificial = true;
            break;
        }

#if 0
        case PSEUDO_PHI:
            WARN_UNHANDLED(insn->pos, "PSEUDO_PHI");
            break;
#endif

        default:
            TRAP;
    }
}

static void read_insn_op_deref(struct cl_operand *op, struct instruction *insn)
{
    struct cl_accessor *ac;
    if (insn->type
            && insn->type->ident
            && 0 != strcmp("__ptr", show_ident(insn->type->ident)))
    {
        WARN_UNHANDLED(insn->pos, "CL_ACCESSOR_ITEM");
        return;
    }

    // simple deref?
    ac = SP_NEW(struct cl_accessor);
    if (!ac)
        die("SP_NEW failed");

    ac->code = CL_ACCESSOR_DEREF;
    ac->type = /* TODO */ op->type;
    ac->next = NULL;

    op->accessor = ac;
}

static void pseudo_to_cl_operand(struct instruction *insn, pseudo_t pseudo,
                                 struct cl_operand *op, bool deref)
{
    op->code        = CL_OPERAND_VOID;
    op->scope       = CL_SCOPE_GLOBAL;
    op->loc.file    = NULL;
    op->loc.line    = -1;
    op->type        = NULL;
    op->accessor    = NULL;

    if (!is_pseudo(pseudo))
        return;

    read_pseudo(op, pseudo);
    if (deref)
        read_insn_op_deref(op, insn);

    if (!op->type)
        op->type = add_type_if_needed(insn->type);
}

static bool handle_insn_call(struct instruction *insn)
{
    struct cl_operand dst, fnc;
    struct pseudo *arg;
    int cnt = 0;

    struct cl_loc loc;
    read_sparse_location(&loc, insn->pos);

    // open call
    pseudo_to_cl_operand(insn, insn->target , &dst  , false);
    pseudo_to_cl_operand(insn, insn->func   , &fnc  , false);
    cl->insn_call_open(cl, &loc, &dst, &fnc);
    free_cl_operand_data(&dst);
    free_cl_operand_data(&fnc);

    // go through arguments
    FOR_EACH_PTR(insn->arguments, arg) {
        struct cl_operand src;
        pseudo_to_cl_operand(insn, arg, &src, false);

        cl->insn_call_arg(cl, ++cnt, &src);
        free_cl_operand_data(&src);
    } END_FOR_EACH_PTR(arg);

    // close call
    cl->insn_call_close(cl);
    if (insn->func->sym->ctype.modifiers & MOD_NORETURN) {
        // this call never returns --> end of BB!!

        struct cl_insn cli;
        cli.code    = CL_INSN_ABORT;
        cli.loc     = loc;

        cl->insn(cl, &cli);
        return false;
    }

    return true;
}

static void handle_insn_br(struct instruction *insn)
{
    char *bb_name_true = NULL;
    char *bb_name_false = NULL;
    struct cl_operand op;

    if (asprintf(&bb_name_true, "%p", insn->bb_true) < 0)
        die("asprintf failed");

    if (!is_pseudo(insn->cond)) {
        struct cl_insn cli;
        cli.code                    = CL_INSN_JMP;
        cli.data.insn_jmp.label     = bb_name_true;
        read_sparse_location(&cli.loc, insn->pos);
        cl->insn(cl, &cli);
        free(bb_name_true);
        return;
    }

    if (asprintf(&bb_name_false, "%p", insn->bb_false) < 0)
        die("asprintf failed");

    pseudo_to_cl_operand(insn, insn->cond, &op, false);

    // TODO: move to function?
    {
        struct cl_insn cli;
        cli.code                        = CL_INSN_COND;
        cli.data.insn_cond.src          = &op;
        cli.data.insn_cond.then_label   = bb_name_true;
        cli.data.insn_cond.else_label   = bb_name_false;
        read_sparse_location(&cli.loc, insn->pos);
        cl->insn(cl, &cli);
    }

    free_cl_operand_data(&op);
    free(bb_name_true);
    free(bb_name_false);
}

static void handle_insn_switch(struct instruction *insn)
{
    struct cl_operand op;
    struct cl_loc loc;
    struct multijmp *jmp;

    // emit insn_switch_open
    pseudo_to_cl_operand(insn, insn->target, &op, false);
    read_sparse_location(&loc, insn->pos);
    cl->insn_switch_open(cl, &loc, &op);
    free_cl_operand_data(&op);

    // go through cases
    FOR_EACH_PTR(insn->multijmp_list, jmp) {
        struct cl_operand val_lo = { CL_OPERAND_VOID };
        struct cl_operand val_hi = { CL_OPERAND_VOID };
        char *label = NULL;

        // if true, it's case; default otherwise
        if (jmp->begin <= jmp->end) {
            val_lo.code = CL_OPERAND_CST;
            val_hi.code = CL_OPERAND_CST;

            // TODO: read types
            val_lo.type = &builtin_int_type;
            val_hi.type = &builtin_int_type;

            val_lo.data.cst.code = CL_TYPE_INT;
            val_hi.data.cst.code = CL_TYPE_INT;

            val_lo.data.cst.data.cst_int.value = jmp->begin;
            val_hi.data.cst.data.cst_int.value = jmp->end;
        }

        if (asprintf(&label, "%p", jmp->target) < 0)
            die("asprintf failed");

        // emit insn_switch_case
        // FIXME: not enough accurate location info from SPARSE for switch/case
        cl->insn_switch_case(cl, &loc, &val_lo, &val_hi, label);

        free_cl_operand_data(&val_lo);
        free_cl_operand_data(&val_hi);
        free(label);

    } END_FOR_EACH_PTR(jmp);

    // emit insn_switch_close
    cl->insn_switch_close(cl);
}

static void handle_insn_ret(struct instruction *insn)
{
    struct cl_operand op;
    struct cl_insn cli;

    pseudo_to_cl_operand(insn, insn->src, &op, false);
    cli.code                = CL_INSN_RET;
    cli.data.insn_ret.src   = &op;
    read_sparse_location(&cli.loc, insn->pos);
    cl->insn(cl, &cli);
    free_cl_operand_data(&op);
}

static void insn_assignment_base(struct instruction                 *insn,
                                 pseudo_t     lhs,        pseudo_t  rhs,
                                 bool         lhs_deref,  bool      rhs_deref)
{
    struct cl_operand op_lhs;
    struct cl_operand op_rhs;

    pseudo_to_cl_operand(insn, lhs, &op_lhs, lhs_deref);
    pseudo_to_cl_operand(insn, rhs, &op_rhs, rhs_deref);

#if 0
    if (op_lhs.deref && op_lhs.name && op_lhs.offset
            && 0 == strcmp(op_lhs.name, op_lhs.offset))
        TRAP;

    if (op_rhs.deref && op_rhs.name && op_rhs.offset
            && 0 == strcmp(op_rhs.name, op_rhs.offset))
        TRAP;
#endif

    // TODO: move to function?
    {
        struct cl_insn cli;
        cli.code                    = CL_INSN_UNOP;
        cli.data.insn_unop.code     = CL_UNOP_ASSIGN;
        cli.data.insn_unop.dst      = &op_lhs;
        cli.data.insn_unop.src      = &op_rhs;
        read_sparse_location(&cli.loc, insn->pos);
        cl->insn(cl, &cli);
    }

    free_cl_operand_data(&op_lhs);
    free_cl_operand_data(&op_rhs);
}

static void handle_insn_store(struct instruction *insn)

{
    insn_assignment_base(insn,
            insn->symbol, insn->target,
            true        , false);
}
static void handle_insn_load(struct instruction *insn)
{
    insn_assignment_base(insn,
            insn->target, insn->symbol,
            false       , true);
}
static void handle_insn_copy(struct instruction *insn)
{
    insn_assignment_base(insn,
            insn->target, insn->src,
            false       , false);
}

static void handle_insn_binop(struct instruction *insn, enum cl_binop_e code)
{
    struct cl_operand dst, src1, src2;

    pseudo_to_cl_operand(insn, insn->target , &dst  , false);
    pseudo_to_cl_operand(insn, insn->src1   , &src1 , false);
    pseudo_to_cl_operand(insn, insn->src2   , &src2 , false);

    // TODO: move to function?
    {
        struct cl_insn cli;
        cli.code = CL_INSN_BINOP;
        cli.data.insn_binop.code    = code;
        cli.data.insn_binop.dst     = &dst;
        cli.data.insn_binop.src1    = &src1;
        cli.data.insn_binop.src2    = &src2;
        read_sparse_location(&cli.loc, insn->pos);
        cl->insn(cl, &cli);
    }

    free_cl_operand_data(&dst);
    free_cl_operand_data(&src1);
    free_cl_operand_data(&src2);
}

static bool handle_insn(struct instruction *insn)
{
    switch (insn->opcode) {
        WARN_CASE_UNHANDLED(insn->pos, OP_BADOP)

        /* Entry */
        case OP_ENTRY:
            // ignore for now
            break;

        /* Terminator */
        case OP_RET /*= OP_TERMINATOR*/:
            handle_insn_ret(insn);
            break;

        case OP_BR:
            handle_insn_br(insn);
            break;

        case OP_SWITCH:
            handle_insn_switch(insn);
            break;

        WARN_CASE_UNHANDLED(insn->pos, OP_INVOKE)
        WARN_CASE_UNHANDLED(insn->pos, OP_COMPUTEDGOTO)
        WARN_CASE_UNHANDLED(insn->pos, OP_TERMINATOR_END /*= OP_UNWIND*/)

        /* Binary */
        case OP_ADD /*= OP_BINARY*/:
            handle_insn_binop(insn, CL_BINOP_PLUS);
            break;

        WARN_CASE_UNHANDLED(insn->pos, OP_SUB)
        WARN_CASE_UNHANDLED(insn->pos, OP_MULU)
        WARN_CASE_UNHANDLED(insn->pos, OP_MULS)
        WARN_CASE_UNHANDLED(insn->pos, OP_DIVU)
        WARN_CASE_UNHANDLED(insn->pos, OP_DIVS)
        WARN_CASE_UNHANDLED(insn->pos, OP_MODU)
        WARN_CASE_UNHANDLED(insn->pos, OP_MODS)
        WARN_CASE_UNHANDLED(insn->pos, OP_SHL)
        WARN_CASE_UNHANDLED(insn->pos, OP_LSR)
        WARN_CASE_UNHANDLED(insn->pos, OP_ASR)

        /* Logical */
        WARN_CASE_UNHANDLED(insn->pos, OP_AND)
        WARN_CASE_UNHANDLED(insn->pos, OP_OR)
        WARN_CASE_UNHANDLED(insn->pos, OP_XOR)
        WARN_CASE_UNHANDLED(insn->pos, OP_AND_BOOL)
        WARN_CASE_UNHANDLED(insn->pos, OP_BINARY_END /*= OP_OR_BOOL*/)

        /* Binary comparison */
        case OP_SET_EQ /*= OP_BINCMP*/:
            handle_insn_binop(insn, CL_BINOP_EQ);
            break;

        case OP_SET_NE:
            handle_insn_binop(insn, CL_BINOP_NE);
            break;

        case OP_SET_LE:
            handle_insn_binop(insn, CL_BINOP_LE);
            break;

        case OP_SET_GE:
            handle_insn_binop(insn, CL_BINOP_GE);
            break;

        case OP_SET_LT:
            handle_insn_binop(insn, CL_BINOP_LT);
            break;

        case OP_SET_GT:
            handle_insn_binop(insn, CL_BINOP_GT);
            break;

        WARN_CASE_UNHANDLED(insn->pos, OP_SET_B)
        WARN_CASE_UNHANDLED(insn->pos, OP_SET_A)
        WARN_CASE_UNHANDLED(insn->pos, OP_SET_BE)
        WARN_CASE_UNHANDLED(insn->pos, OP_BINCMP_END /*= OP_SET_AE*/)

        /* Uni */
        WARN_CASE_UNHANDLED(insn->pos, OP_NOT)
        WARN_CASE_UNHANDLED(insn->pos, OP_NEG)

        /* Select - three input values */
        WARN_CASE_UNHANDLED(insn->pos, OP_SEL)

        /* Memory */
        WARN_CASE_UNHANDLED(insn->pos, OP_MALLOC)
        WARN_CASE_UNHANDLED(insn->pos, OP_FREE)
        WARN_CASE_UNHANDLED(insn->pos, OP_ALLOCA)
        case OP_LOAD:
            handle_insn_load(insn);
            break;

        case OP_STORE:
            handle_insn_store(insn);
            break;

        WARN_CASE_UNHANDLED(insn->pos, OP_SETVAL)
        WARN_CASE_UNHANDLED(insn->pos, OP_SYMADDR)
        WARN_CASE_UNHANDLED(insn->pos, OP_GET_ELEMENT_PTR)

        /* Other */
        case OP_PHI:
        case OP_PHISOURCE:
            // FIXME: this might be a SPARSE bug if DO_PER_EP_UNSAA is set
            WARN_UNHANDLED(insn->pos, show_instruction(insn));
            break;

        case OP_CAST:
        case OP_SCAST:
        case OP_FPCAST:
        case OP_PTRCAST:
            // TODO: separate handler?
            handle_insn_copy(insn);
            break;

        WARN_CASE_UNHANDLED(insn->pos, OP_INLINED_CALL)
        case OP_CALL:
            return handle_insn_call(insn);

        WARN_CASE_UNHANDLED(insn->pos, OP_VANEXT)
        WARN_CASE_UNHANDLED(insn->pos, OP_VAARG)
        WARN_CASE_UNHANDLED(insn->pos, OP_SLICE)
        case OP_SNOP:
            //handle_insn_store(insn);
            WARN_UNHANDLED(insn->pos, show_instruction(insn));
            break;

        case OP_LNOP:
            //handle_insn_load(insn);
            WARN_UNHANDLED(insn->pos, show_instruction(insn));
            break;

        WARN_CASE_UNHANDLED(insn->pos, OP_NOP)
        WARN_CASE_UNHANDLED(insn->pos, OP_DEATHNOTE)
        WARN_CASE_UNHANDLED(insn->pos, OP_ASM)

        /* Sparse tagging (line numbers, context, whatever) */
        WARN_CASE_UNHANDLED(insn->pos, OP_CONTEXT)
        WARN_CASE_UNHANDLED(insn->pos, OP_RANGE)

        /* Needed to translate SSA back to normal form */
        case OP_COPY:
            handle_insn_copy(insn);
            break;
    }
    return true;
}

static bool is_insn_interesting(struct instruction *insn)
{
#if 0
    unsigned size = insn->size;
    if (size && KNOWN_PTR_SIZE != size) {
        WARN_VA(insn->pos, "ignored instruction with operand of size %d",
                insn->size);
        return false;
    }
#endif

    switch (insn->opcode) {
        case OP_ENTRY:
            return false;

        default:
            return true;
    }
}

static bool handle_bb_insn(struct instruction *insn)
{
    if (!insn)
        return true;

    if (!insn->bb) {
#if SHOW_PSEUDO_INSNS
        WARN_VA(insn->pos, "ignoring pseudo: %s", show_instruction(insn));
#endif
        return true;
    }

    if (!is_insn_interesting(insn))
        return true;

    return handle_insn(insn);
}

static void handle_bb(struct basic_block *bb)
{
    struct instruction *insn;
    char *bb_name;

    if (!bb)
        return;

    if (asprintf(&bb_name, "%p", bb) < 0)
        die("asprintf failed");

    cl->bb_open(cl, bb_name);
    free(bb_name);

    FOR_EACH_PTR(bb->insns, insn) {
        if (!handle_bb_insn(insn))
            // subtle: 'break' stmt here does not work as one would expected to
            goto done;
    } END_FOR_EACH_PTR(insn);
done:
    return;
}

static void handle_fnc_ep(struct entrypoint *ep)
{
    struct instruction *entry = ep->entry;
    struct basic_block *bb;
    char *entry_name;

    // jump to entry basic block
    if (asprintf(&entry_name, "%p", entry->bb) < 0)
        die("asprintf failed");

    // TODO: move to function?
    {
        struct cl_insn cli;
        cli.code                    = CL_INSN_JMP;
        cli.data.insn_jmp.label     = entry_name;
        read_sparse_location(&cli.loc, entry->pos);
        cl->insn(cl, &cli);
    }
    free(entry_name);

    // go through basic blocks
    FOR_EACH_PTR(ep->bbs, bb) {
        if (!bb)
            continue;

        if (bb->parents || bb->children || bb->insns
                || /* FIXME: is the following actually useful? */ 2 < verbose)
        {
            handle_bb(bb);
        }
    } END_FOR_EACH_PTR(bb);
}

static void handle_fnc_body(struct symbol *sym)
{
    struct entrypoint *ep = linearize_symbol(sym);
    if (!ep)
        TRAP;

#if DO_PER_EP_UNSAA
    unssa(ep);
#endif

#if DO_PER_EP_SET_UP_STORAGE
    set_up_storage(ep);
#endif

    handle_fnc_ep(ep);

#if DO_PER_EP_SET_UP_STORAGE
    free_storage();
#endif
}

static void handle_fnc_arg_list(struct symbol_list *arg_list)
{
    struct symbol *arg;
    int argc = 0;
    FOR_EACH_PTR(arg_list, arg) {
        struct cl_operand op;
        op.code                     = CL_OPERAND_VAR;
        op.scope                    = CL_SCOPE_FUNCTION;
        op.type                     = clt_from_sym(arg);
        op.accessor                 = NULL;
        // TODO: JP
        op.data.var = SP_NEW(struct cl_var);
#if 1
        op.data.var->uid             = /* TODO */ (int)(long) arg;
        op.data.var->name            = strdup(show_ident(arg->ident));
#endif        

        read_sparse_location(&op.loc, arg->pos);
        cl->fnc_arg_decl(cl, ++argc, &op);

        free_cl_operand_data(&op);
    } END_FOR_EACH_PTR(arg);
}

static void handle_fnc_def(struct symbol *sym)
{
    struct cl_operand fnc;
    read_sparse_location(&fnc.loc, sym->pos);
    read_sparse_scope(&fnc.scope, sym->scope);

    fnc.code                            = CL_OPERAND_CST;
    fnc.type                            = clt_from_sym(sym);
    fnc.accessor                        = NULL;
    fnc.data.cst.code                   = CL_TYPE_FNC;
    fnc.data.cst.data.cst_fnc.name      = show_ident(sym->ident);
    fnc.data.cst.data.cst_fnc.is_extern = false;

    cl->fnc_open(cl, &fnc);
    /* no need to call free_cl_operand_data() */

    // dump argument list
    handle_fnc_arg_list(sym->ctype.base_type->arguments);

    // handle fnc body
    handle_fnc_body(sym);
    cl->fnc_close(cl);
}

static void handle_sym_fn(struct symbol *sym)
{
    struct symbol *base_type = sym->ctype.base_type;
    struct statement *stmt = base_type->stmt;

    if (stmt) {
        // function definition
        handle_fnc_def(sym);
        return;
    }

    WARN_UNHANDLED_SYM(sym);
}

static void handle_top_level_sym(struct symbol *sym)
{
    struct symbol *base_type;

    if (!sym)
        return;

    base_type = sym->ctype.base_type;
    if (!base_type)
        return;

    switch (base_type->type) {
        WARN_CASE_UNHANDLED(sym->pos, SYM_UNINITIALIZED)
        WARN_CASE_UNHANDLED(sym->pos, SYM_PREPROCESSOR)
        WARN_CASE_UNHANDLED(sym->pos, SYM_BASETYPE)
        WARN_CASE_UNHANDLED(sym->pos, SYM_NODE)
        WARN_CASE_UNHANDLED(sym->pos, SYM_PTR)
        WARN_CASE_UNHANDLED(sym->pos, SYM_ARRAY)
        WARN_CASE_UNHANDLED(sym->pos, SYM_STRUCT)
        WARN_CASE_UNHANDLED(sym->pos, SYM_UNION)
        WARN_CASE_UNHANDLED(sym->pos, SYM_ENUM)
        WARN_CASE_UNHANDLED(sym->pos, SYM_TYPEDEF)
        WARN_CASE_UNHANDLED(sym->pos, SYM_TYPEOF)
        WARN_CASE_UNHANDLED(sym->pos, SYM_MEMBER)
        WARN_CASE_UNHANDLED(sym->pos, SYM_BITFIELD)
        WARN_CASE_UNHANDLED(sym->pos, SYM_LABEL)
        WARN_CASE_UNHANDLED(sym->pos, SYM_RESTRICT)
        WARN_CASE_UNHANDLED(sym->pos, SYM_FOULED)
        WARN_CASE_UNHANDLED(sym->pos, SYM_KEYWORD)
        WARN_CASE_UNHANDLED(sym->pos, SYM_BAD)

        case SYM_FN:
            handle_sym_fn(sym);
            break;
    }

    if (sym->initializer)
        WARN_UNHANDLED(sym->pos, "sym->initializer");
}

static void clean_up_symbols(struct symbol_list *list,
                             struct cl_code_listener *cl)
{
    struct symbol *sym;

    FOR_EACH_PTR(list, sym) {
#if DO_EXPAND_SYMBOL
        expand_symbol(sym);
#endif
        handle_top_level_sym(sym);
    } END_FOR_EACH_PTR(sym);
}

static struct cl_code_listener* create_cl_chain(void)
{
    struct cl_code_listener *listener;
    struct cl_code_listener *chain = cl_chain_create();
    if (!chain)
        // error message already emitted
        return NULL;

    if (1 < verbose) {
        listener = cl_code_listener_create("listener=\"locator\"");
        if (!listener) {
            chain->destroy(chain);
            return NULL;
        }
        cl_chain_append(chain, listener);
    }

    listener = cl_code_listener_create("listener=\"pp_with_types\" "
//            "clf=\"unify_labels_fnc\""
              "clf=\"unify_labels_gl\""
            );
    if (!listener) {
        chain->destroy(chain);
        return NULL;
    }
    cl_chain_append(chain, listener);

#if 0
    listener = cl_code_listener_create("listener=\"dotgen\" "
            "clf=\"arg_subst,unify_labels_fnc,unify_regs,unify_vars\"");
    if (!listener) {
        chain->destroy(chain);
        return NULL;
    }
    cl_chain_append(chain, listener);
#endif

    return chain;
}

int main(int argc, char **argv)
{
    char *file;
    struct string_list *filelist = NULL;
    struct symbol_list *symlist;

#if 1
    setbuf(stdout, NULL);
    setbuf(stderr, NULL);
#endif

    symlist = sparse_initialize(argc, argv, &filelist);

    // initialize code listener
    cl_global_init_defaults(NULL, verbose);
    cl = create_cl_chain();
    if (!cl)
        // error message already emitted
        return EXIT_FAILURE;

    // initialize type database
    type_db = type_db_create();

    cl->file_open(cl, "sparse-internal-symbols");
    clean_up_symbols(symlist, cl);
    cl->file_close(cl);

    FOR_EACH_PTR_NOTAG(filelist, file) {
        if (0 < verbose)
            fprintf(stderr, "%s: about to process '%s'...\n", argv[0], file);

        cl->file_open(cl, file);
        clean_up_symbols(sparse(file), cl);
        cl->file_close(cl);
    } END_FOR_EACH_PTR_NOTAG(file);

    type_db_destroy(type_db);
    cl->destroy(cl);
    cl_global_cleanup();

    return 0;
}
