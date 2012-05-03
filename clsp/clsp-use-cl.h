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
#ifndef CLSP_USE_CL_H_GUARD
#define CLSP_USE_CL_H_GUARD
/**
    Customize Code Listener API (subset of it) to our needs

    Ties closely with GLOBALS function-like macro as per @c clsp.h
    (master header file for this one).
 */

#include "clsp-api-cl.h"
#include "clsp-out-base.h"  /* PUT, _1(), ... */
#include "clsp-macros.h"    /* APPLY */

/*
    Code Listener API
    NOTE: relying on availability of GLOBALS(cl/cl_api), see above
 */

/* global API of Code Listener:  API_CL(item, ...) */
#define API_CL(...)           API_USE(CL,__VA_ARGS__,API_CL_GLOBALS)
#define API_CL_GLOBALS(item)  GLOBALS(cl_api).item

/*
    per-listener part of Code Listener API:  EMIT(item, ...)
    NOTE: injecting "self" as a first argument skipped by outer call
    NOTE: works only until there is non-function or returning function
 */
#define API_EMIT(...)           API_EMIT_(__VA_ARGS__, API_EMIT_GLOBALS)
#define API_EMIT_(item, ...)    API_USE(CLOBJ, item, GLOBALS(cl), __VA_ARGS__)
#define API_EMIT_GLOBALS(item)  GLOBALS(cl)->item


/** helpers ***************************************************************/


/**
    Unified void operand to allow direct pointer comparison

    We have to be careful not to modify it.
 */
extern const struct cl_operand no_operand;
#define NO_OPERAND      (&no_operand)
#define NO_OPERAND_USE  ((struct cl_operand *) &no_operand)

/* self-explanatory "pretty" accessors */
#define CST(op)      (&op->data.cst)
#define CST_INT(op)  (&CST(op)->data.cst_int)
#define CST_STR(op)  (&CST(op)->data.cst_string)
#define CST_FNC(op)  (&CST(op)->data.cst_fnc)
#define CST_REAL(op) (&CST(op)->data.cst_real)

#define VAR(op)      (op->data.var)

#define UNOP(insn)   (&(insn)->data.insn_unop)
#define BINOP(insn)  (&(insn)->data.insn_binop)
#define RET(insn)    (&(insn)->data.insn_ret)
#define COND(insn)   (&(insn)->data.insn_cond)
#define JMP(insn)    (&(insn)->data.insn_jmp)


/*
    output
 */

#define CLPOSFMT    "%s:%d:%d"
#define CLPOSFMT_1  _1(s)":"_2(d)":"_3(d)
#define CLPOSFMT_2  _2(s)":"_3(d)":"_4(d)
#define CLPOSFMT_3  _3(s)":"_4(d)":"_5(d)
#define CLPOSFMT_4  _4(s)":"_5(d)":"_6(d)
#define CLPOSFMT_5  _5(s)":"_6(d)":"_7(d)
#define CLPOSFMT_6  _6(s)":"_7(d)":"_8(d)
#define CLPOSFMT_7  _7(s)":"_8(d)":"_9(d)
#define CLPOSFMT_8  _8(s)":"_9(d)":"_10(d)
#define CLPOS(p)    (p).file, (p).line, (p).column

/* config strings for built-in listeners */
#define CL_BUILTIN_LOCATOR_STR  "listener=\"locator\""


/** debug primitives ******************************************************/


/*
    scope
 */

#define CL_SCOPE_CODELIST(x) \
    APPLY(x, GLOBAL)         \
    APPLY(x, STATIC)         \
    APPLY(x, FUNCTION)       \
    APPLY(x, BB)
#define CL_SCOPE_RESERVED  8

extern const char *const cl_scope_codelist_str[CL_SCOPE_RESERVED];

/**
    Scope to string reprezentation
 */
static inline const char *
debug_cl_scope_code(enum cl_scope_e scope)
{
    const char *ret = cl_scope_codelist_str[scope];
    return ret ? ret : "(error)";
}


/*
    type
 */

#define CL_TYPE_CODELIST(x) \
    APPLY(x, VOID   )       \
    APPLY(x, UNKNOWN)       \
    APPLY(x, PTR    )       \
    APPLY(x, STRUCT )       \
    APPLY(x, UNION  )       \
    APPLY(x, ARRAY  )       \
    APPLY(x, FNC    )       \
    APPLY(x, INT    )       \
    APPLY(x, CHAR   )       \
    APPLY(x, BOOL   )       \
    APPLY(x, ENUM   )       \
    APPLY(x, REAL   )       \
    APPLY(x, STRING )
#define CL_TYPE_RESERVED  16

extern const char *const cl_type_codelist_str[CL_TYPE_RESERVED];

static inline bool
cl_type_ptrlike(enum cl_type_e typecode)
{
    return CL_TYPE_ARRAY == typecode || CL_TYPE_PTR == typecode;
}

/**
    Basic type to string reprezentation
 */
static inline const char *
debug_cl_type_code(enum cl_operand_e code)
{
    const char *ret = cl_type_codelist_str[code];
    return ret ? ret : "(error)";
}


/*
    accessor
 */

#define CL_ACCESSOR_CODELIST(x) \
    APPLY(x, REF        )       \
    APPLY(x, DEREF      )       \
    APPLY(x, DEREF_ARRAY)       \
    APPLY(x, ITEM       )
#define CL_ACCESSOR_RESERVED  8

extern const char *const cl_accessor_codelist_str[CL_ACCESSOR_RESERVED];

/**
    Kind of accessor to string reprezentation
 */
static inline const char *
debug_cl_accessor_code(enum cl_accessor_e code)
{
    const char *ret = cl_accessor_codelist_str[code];
    return ret ? ret : "(error)";
}


/*
    operand
 */

#define CL_OPERAND_CODELIST(x) \
    APPLY(x, VOID)             \
    APPLY(x, CST)              \
    APPLY(x, VAR)
#define CL_OPERAND_RESERVED  8

extern const char *const cl_operand_codelist_str[CL_OPERAND_RESERVED];

/**
    Kind of operand to string reprezentation
 */
static inline const char *
debug_cl_operand_code(enum cl_operand_e code)
{
    const char *ret = cl_operand_codelist_str[code];
    return ret ? ret : "(error)";
}


#define CL_CST_CODELIST(x) \
    APPLY(x, FNC)          \
    APPLY(x, INT)          \
    APPLY(x, REAL)         \
    APPLY(x, STRING)
#define CL_CST_RESERVED  8


/*
    instruction
 */

#define CL_UNOP_CODELIST(x)   \
    APPLY_INNER(x, ASSIGN   ) \
    APPLY_INNER(x, TRUTH_NOT) \
    APPLY_INNER(x, BIT_NOT  ) \
    APPLY_INNER(x, MINUS    ) \
    APPLY_INNER(x, ABS      ) \
    APPLY_INNER(x, FLOAT    )
#define CL_UNOP_RESERVED  16

#define CL_BINOP_CODELIST(x)     \
    APPLY_INNER(x, EQ          ) \
    APPLY_INNER(x, NE          ) \
    APPLY_INNER(x, LT          ) \
    APPLY_INNER(x, GT          ) \
    APPLY_INNER(x, LE          ) \
    APPLY_INNER(x, GE          ) \
    APPLY_INNER(x, TRUTH_AND   ) \
    APPLY_INNER(x, TRUTH_OR    ) \
    APPLY_INNER(x, TRUTH_XOR   ) \
    APPLY_INNER(x, PLUS        ) \
    APPLY_INNER(x, MINUS       ) \
    APPLY_INNER(x, MULT        ) \
    APPLY_INNER(x, EXACT_DIV   ) \
    APPLY_INNER(x, TRUNC_DIV   ) \
    APPLY_INNER(x, TRUNC_MOD   ) \
    APPLY_INNER(x, RDIV        ) \
    APPLY_INNER(x, MIN         ) \
    APPLY_INNER(x, MAX         ) \
    APPLY_INNER(x, POINTER_PLUS) \
    APPLY_INNER(x, BIT_AND     ) \
    APPLY_INNER(x, BIT_IOR     ) \
    APPLY_INNER(x, BIT_XOR     ) \
    APPLY_INNER(x, LSHIFT      ) \
    APPLY_INNER(x, RSHIFT      ) \
    APPLY_INNER(x, LROTATE     ) \
    APPLY_INNER(x, RROTATE     )
#define CL_BINOP_RESERVED  32

#define CL_INSN_CODELIST(x, y, z)      \
    APPLY(x, NOP                     ) \
    APPLY(x, JMP                     ) \
    APPLY(x, COND                    ) \
    APPLY(x, RET                     ) \
    APPLY(x, ABORT                   ) \
    APPLY(y, UNOP,  CL_UNOP_CODELIST ) \
    APPLY(z, BINOP, CL_BINOP_CODELIST) \
    APPLY(x, CALL                    ) \
    APPLY(x, SWITCH                  ) \
    APPLY(x, LABEL                   )
#define CL_INSN_RESERVED  16

#define CL_INSN_START     0
#define CL_UNOP_START     CL_INSN_START + CL_INSN_RESERVED + 1
#define CL_BINOP_START    CL_UNOP_START + CL_UNOP_RESERVED + 1
#define CL_INSN_TOTAL     CL_BINOP_START + CL_BINOP_RESERVED + 1

/* special pointers values in character range not colliding with common strings */
#define CL_INSN_MARK_UNOP      0x1
#define CL_INSN_MARK_BINOP     0x2

extern const char *const cl_insn_codelist_str[CL_INSN_TOTAL];

/**
    Instruction to string reprezentation
 */
static inline const char *
debug_cl_insn_code(const struct cl_insn *insn) {
    const char *ret = cl_insn_codelist_str[insn->code];
    if ((const char) CL_INSN_MARK_UNOP == *ret)
        return ((const char *const *) ret)[insn->data.insn_unop.code + 1];
    else if ((const char) CL_INSN_MARK_BINOP == *ret)
        return ((const char *const *) ret)[insn->data.insn_binop.code + 1];
    else
        return ret ? ret : "(error)";
}


/** debug printers *******************************************************/


/**
    Show initializer in detail

    @param[in] op      Initializer to be exposed
    @param[in] indent  Initial level of indentation
    @param[in] safely  Should be safe to keep true, used mainly internally
 */
void debug_cl_initializer(const struct cl_initializer *initial, int indent,
                          bool safely);

/**
    Show operand in detail (even bits missing in pretty printed code)

    @param[in] op      Operand to be exposed
    @param[in] indent  Initial level of indentation
    @param[in] safely  Should be safe to keep true, used mainly internally
 */
void debug_cl_operand(const struct cl_operand *op, int indent, bool safely);

/**
    Show instruction in detail

    @param[in] op      Instruction to be exposed
    @param[in] indent  Initial level of indentation
    @param[in] safely  Should be safe to keep true (internal recursion guard)
 */
void debug_cl_insn(const struct cl_insn *insn, int indent, bool safely);

/**
    Show type in detail

    @param[in] op               Type to be exposed
    @param[in] indent           Initial level of indentation
    @param[in] recursion_limit  Guard for recursive data types
 */
void debug_cl_type(const struct cl_type *clt, int indent, int recursion_limit);

/**
    Show switch case value/range

    @param[in] val_lo           Range beginning operand for case.
    @param[in] val_lo           Range end operand for case.
    @param[in] indent           Initial level of indentation
 */
void debug_cl_switch_case(const struct cl_operand *val_lo,
                          const struct cl_operand *val_hi,
                          int indent);


#endif
