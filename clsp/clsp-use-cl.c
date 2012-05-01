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

#include "clsp.h"
#include "clsp-use-cl.h"


const struct cl_operand no_operand = { .code = CL_OPERAND_VOID };


/** debug primitives ******************************************************/


const char *const cl_scope_codelist_str[CL_SCOPE_RESERVED] = {
#define X(what)  [CL_SCOPE_##what] = #what,
    CL_SCOPE_CODELIST(X)
#undef X
};

const char *const cl_type_codelist_str[CL_TYPE_RESERVED] = {
#define X(what)  [CL_TYPE_##what] = #what,
    CL_TYPE_CODELIST(X)
#undef X
};

const char *const cl_accessor_codelist_str[CL_ACCESSOR_RESERVED] = {
#define X(what)  [CL_ACCESSOR_##what] = #what,
    CL_ACCESSOR_CODELIST(X)
#undef X
};

const char *const cl_operand_codelist_str[CL_OPERAND_RESERVED] = {
#define X(what)  [CL_OPERAND_##what] = #what,
    CL_OPERAND_CODELIST(X)
#undef X
};


const char *const cl_insn_codelist_str[CL_INSN_TOTAL] = {
#define X(what) \
    [CL_INSN_##what]                   = #what,

#define Y_(what) \
    [CL_UNOP_##what + CL_UNOP_START]   = #what " (UNOP)",
#define Y(what, sublist)                                                    \
    [CL_UNOP_START - 1]                = (const char *) CL_INSN_MARK_UNOP,  \
    [CL_INSN_##what]                   = (const char *)                     \
        ((const char *const *)(&cl_insn_codelist_str) + CL_UNOP_START - 1), \
    sublist(Y_)

#define Z_(what) \
    [CL_BINOP_##what + CL_BINOP_START] = #what " (BINOP)",
#define Z(what, sublist)                                                     \
    [CL_BINOP_START - 1]               = (const char *) CL_INSN_MARK_BINOP,  \
    [CL_INSN_##what]                   = (const char *)                      \
        ((const char *const *)(&cl_insn_codelist_str) + CL_BINOP_START - 1), \
    sublist(Z_)

    CL_INSN_CODELIST(X, Y, Z)

#undef Z
#undef Z_
#undef Y
#undef Y_
#undef X
};


/** debug printers *******************************************************/


#define ENT(x)  "<" #x ">"

/**
    Show constant-specific operand information
 */
static void
debug_cl_cst(const struct cl_cst *cst, int indent)
{
    switch (cst->code) {
        case CL_TYPE_FNC:
            PUTHI(debug, cl_debug, indent,
                  ENT(constant-fnc) " {"
                      "name="   _1(s) ", "
                      "uid="    _2(d) ", "
                      "extern=" _3(c)
                  "}: " CLPOSFMT_4,
                  cst->data.cst_fnc.name,
                  cst->data.cst_fnc.uid,
                  GET_YN(cst->data.cst_fnc.is_extern),
                  CLPOS(cst->data.cst_fnc.loc));
            break;
        case CL_TYPE_INT:
            PUTHI(debug, cl_debug, indent,
                  ENT(constant-int) " ["
                      _1(d)
                  "]",
                  cst->data.cst_int.value);
            break;
        case CL_TYPE_STRING:
            PUTHI(debug, cl_debug, indent,
                  ENT(constant-string) " ["
                      _1(s)
                  "]",
                  cst->data.cst_string.value);
            break;
        case CL_TYPE_REAL:
            PUTHI(debug, cl_debug, indent,
                  ENT(constant-real) " ["
                      _1(lf)
                  "]",
                  cst->data.cst_real.value);
            break;
        case CL_TYPE_VOID:
        case CL_TYPE_UNKNOWN:
        case CL_TYPE_PTR:
        case CL_TYPE_STRUCT:
        case CL_TYPE_UNION:
        case CL_TYPE_ARRAY:
        case CL_TYPE_CHAR:
        case CL_TYPE_BOOL:
        case CL_TYPE_ENUM:
            PUTHI(debug, cl_debug, indent,
                  ENT(constant-error) " {"
                      "code="_1(d)
                  "}",
                  cst->code);
        default:
            PUTHI(debug, cl_debug, indent,
                  ENT(constant-uknown) " {"
                      "code="_1(d)
                  "}",
                  cst->code);
    }
}

void
debug_cl_initializer(const struct cl_initializer *initial, int indent,
                     bool safely)
{
    if (initial) {
        PUTHI(debug, cl_debug, indent++, ENT(initializer) _1(s), "");
        do {
            debug_cl_insn(&initial->insn, indent, safely);
            initial = (const struct cl_initializer *) initial->next;
        } while (initial);
    }
}

/**
    Show variable-specific operand information
 */
static inline void
debug_cl_var(const struct cl_var *var, int indent, bool safely)
{
    PUTHI(debug, cl_debug, indent,
          ENT(operand-var) " {"
              "uid="         _1(d) ", "
              "name="        _2(s) ", "
              "artificial="  _3(c) ", "
              "initialized=" _4(c)
          "}: " CLPOSFMT_5,
          var->uid,
          var->name ? var->name : "(none)",
          GET_YN(var->artificial),
          GET_YN(var->initialized),
          CLPOS(var->loc));
    debug_cl_initializer(var->initial, indent+1, safely);
}

void
debug_cl_type(const struct cl_type *clt, int indent, int recursion_limit)
{
    PUTHI(debug, cl_debug, indent++,
          ENT(type) " {"
              "kind="        _1(s) ", "
              "uid="         _2(d) ", "
              "name="        _3(s) ", "
              "sizeof="      _4(d) ", "
              "item_cnt="    _5(d) ", "
              "is_unsigned=" _6(c)
          "}: " CLPOSFMT_7,
          debug_cl_type_code(clt->code),
          clt->uid,
          clt->name ? clt->name : "(none)",
          clt->size,
          clt->item_cnt,
          GET_YN(clt->is_unsigned),
          CLPOS(clt->loc));

    if (indent > recursion_limit && 0 < clt->item_cnt) {
        PUTHI(debug, cl_debug, indent,
              _1(s), "(forcibly skipped: guard for recursive data types)");
        return;
    }

    for (int i = 0; i < clt->item_cnt; i++) {
        if (!clt->items[i].type) {
            PUTHI(debug, cl_debug, indent,
                  "(stopping at item id="_1(d)" due to unevaluated"
                  " recursive type at higher level)", i);
            break;
        }
        switch (clt->code) {
            case CL_TYPE_UNKNOWN:
            case CL_TYPE_STRUCT:
            case CL_TYPE_UNION:
                PUTHI(debug, cl_debug, indent,
                      ENT(type-item) " {"
                          "name="   _1(s) ", "
                          "offset=" _2(d)
                      "}",
                      clt->items[i].name ? clt->items[i].name : "(anonymous)",
                      clt->items[i].offset);
                break;

            case CL_TYPE_PTR:
            case CL_TYPE_ARRAY:
            case CL_TYPE_FNC:
                PUTHI(debug, cl_debug, indent,
                      ENT(type-item) " {"
                          "name=" _1(s)
                      "}",
                      clt->items[i].name ? clt->items[i].name : "(anonymous)");
                break;
            case CL_TYPE_VOID:
            case CL_TYPE_INT:
            case CL_TYPE_CHAR:
            case CL_TYPE_BOOL:
            case CL_TYPE_ENUM:
            case CL_TYPE_REAL:
            case CL_TYPE_STRING:
                PUTHI(debug, cl_debug, indent,
                      ENT(type-error) " {"
                          "item-cnt=" _1(d)
                      "}",
                      clt->item_cnt);
                break;
        }
        /* recurse for type item */
        debug_cl_type(clt->items[i].type, indent+1, recursion_limit);
    }
}

/**
    Show accessor in detail
 */
static void
debug_cl_accessor(const struct cl_accessor *accessor, int indent)
{
    while (accessor) {
        PUTHI(debug, cl_debug, indent++,
              ENT(accessor) " {"
                "kind=" _1(s)
              "}",
              debug_cl_accessor_code(accessor->code));

        debug_cl_type(accessor->type, indent, indent+2 /*details elsewhere*/);

        switch (accessor->code) {
            case CL_ACCESSOR_REF:
            case CL_ACCESSOR_DEREF:
                /* nothing to show */
                break;
            case CL_ACCESSOR_DEREF_ARRAY:
                PUTHI(debug, cl_debug, indent, ENT(array-index) _1(s), "");
                debug_cl_operand(accessor->data.array.index, indent+1, false);
                break;
            case CL_ACCESSOR_ITEM:
                PUTHI(debug, cl_debug, indent, "{"
                          "id=" _1(d)
                      "}",
                      accessor->data.item.id);
                break;
            /* otherwise, error already indicated as kind */
        }
        accessor = accessor->next;
    }
}

void
debug_cl_operand(const struct cl_operand *op, int indent, bool safely)
{
    PUTHI(debug, cl_debug, indent++,
          ENT(operand) " {"
              "kind="   _1(s) ", "
              "scope="  _2(s)
          "}",
          debug_cl_operand_code(op->code),
          CL_OPERAND_VOID == op->code ? "N/A" : debug_cl_scope_code(op->scope));

    if (CL_OPERAND_VOID == op->code)
        return;

    debug_cl_type(op->type, indent, indent+10 /*made up*/);
    debug_cl_accessor(op->accessor, indent);

    if (CL_OPERAND_CST == op->code)
        debug_cl_cst(&op->data.cst, indent);
    else if (!safely || !op->data.var->initial)
        debug_cl_var(op->data.var, indent, true);
    else
        PUTHI(debug, cl_debug, indent,
              _1(s), "(skipped for possible initialization infloop)");
}

void
debug_cl_insn(const struct cl_insn *insn, int indent, bool safely)
{
    PUTHI(debug, cl_debug, indent++,
          ENT(instruction) " ["
              _1(s)
          "]: " CLPOSFMT_2,
          debug_cl_insn_code(insn),
          CLPOS(insn->loc));

    switch (insn->code) {
        case CL_INSN_NOP:
            break;
        case CL_INSN_JMP:
            PUTHI(debug, cl_debug, indent,
                  ENT(jmp-label) " ["
                      _1(s)
                  "]",
                  insn->data.insn_jmp.label);
            break;
        case CL_INSN_COND:
            PUTHI(debug, cl_debug, indent, ENT(cond-src) _1(s), "");
            debug_cl_operand(insn->data.insn_cond.src, indent+1, false);
            PUTHI(debug, cl_debug, indent,
                  ENT(cond-then-label) " ["
                      _1(s)
                  "]",
                  insn->data.insn_cond.then_label);
            PUTHI(debug, cl_debug, indent,
                  ENT(cond-else-label) " ["
                      _1(s)
                  "]",
                  insn->data.insn_cond.else_label);
            break;
        case CL_INSN_RET:
            PUTHI(debug, cl_debug, indent, ENT(ret-src) _1(s), "");
            debug_cl_operand(insn->data.insn_ret.src, indent+1, false);
            break;
        case CL_INSN_ABORT:
            break;
        case CL_INSN_UNOP:
            PUTHI(debug, cl_debug, indent, ENT(unop-dst) _1(s), "");
            debug_cl_operand(insn->data.insn_unop.dst, indent+1, safely);
            PUTHI(debug, cl_debug, indent, ENT(unop-src) _1(s), "");
            debug_cl_operand(insn->data.insn_unop.src, indent+1, false);
            break;
        case CL_INSN_BINOP:
            PUTHI(debug, cl_debug, indent, ENT(binop-dst) _1(s), "");
            debug_cl_operand(insn->data.insn_binop.dst, indent+1, false);
            PUTHI(debug, cl_debug, indent, ENT(binop-src1) _1(s), "");
            debug_cl_operand(insn->data.insn_binop.src1, indent+1, false);
            PUTHI(debug, cl_debug, indent, ENT(binop-src2) _1(s), "");
            debug_cl_operand(insn->data.insn_binop.src2, indent+1, false);
            break;
        case CL_INSN_CALL:
            break;
        case CL_INSN_SWITCH:
            break;
        case CL_INSN_LABEL:
            PUTHI(debug, cl_debug, indent,
                  ENT(label-name) " ["
                      _1(s)
                  "]",
                  insn->data.insn_label.name);
            break;
        /* otherwise, error already indicated as instruction name */
    }
}

void
debug_cl_switch_case(const struct cl_operand *val_lo,
                     const struct cl_operand *val_hi,
                     int indent)
{
    if (val_lo != val_hi)
        PUTHI(debug, cl_debug, indent,
              ENT(switch-case) " ["
                _1(d) " ... " _2(d)
              "]",
              CST_INT(val_lo)->value, CST_INT(val_hi)->value);
    else if (val_lo != NO_OPERAND_USE)
        PUTHI(debug, cl_debug, indent,
              ENT(switch-case) " ["
                _1(d)
              "]",
              CST_INT(val_lo)->value);
    else
        PUTHI(debug, cl_debug, indent,
              ENT(switch-case) " ["
                _1(s)
              "]", "default");
}
