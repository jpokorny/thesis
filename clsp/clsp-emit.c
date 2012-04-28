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

#include "clsp.h"

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <stdint.h>    /* uintptr_t */
#include <inttypes.h>  /* PRIxPTR */
#include <assert.h>

#include "clsp-emit.h"
#include "clsp-conv.h"
#include "clsp-interact.h"

#define  GLOBALS(what)  (globals.what)


/* compile options */

// general
#define DO_EXTRA_CHECKS              0
#define USE_EXTENDED_TYPE_CMP        0
#define SHOW_PSEUDO_INSNS            0

// sparse
#define FIX_SPARSE_EXTRA_ARG_TO_MEM  1

/* assertions */
#define ASSERT_SYMBOL_BELONGS_TO_CURRENT_FILE   1
#define ASSERT_CST_HAVE_VALID_CODE              1


/* if (!emit_props & emit_skip_initial), use this name for "initial" symbols */
static const char *const initial_file = "<initial-metafile>";


#define SYM_FNC_ARGS(sym)  (sym->ctype.base_type->arguments)

#define CST(op)      (&op->data.cst)
#define CST_INT(op)  (&CST(op)->data.cst_int)
#define CST_STR(op)  (&CST(op)->data.cst_string)
#define CST_FNC(op)  (&CST(op)->data.cst_fnc)
#define CST_REAL(op) (&CST(op)->data.cst_real)

#define VAR(op)      (op->data.var)

#define OP_INITIALIZED_VAR(op) \
    (op->code == CL_OPERAND_VAR && op->data.var->initialized)

#define WITH_FILE_TO_EMIT(file, symref, private)                           \
    for (int i_=0; i_==0                                                   \
        ? (                                                                \
          (initial_file == file                                            \
            ? DLOG(file, _1(s)": debug: " HIGHLIGHT("file") ": begin"      \
                         " (aggregation of various sources)", file)        \
            : DLOG(file, SPPOSFMT_1 ": debug: " HIGHLIGHT("file")          \
                         ": begin (first symbol)",                         \
                          SPPOS((symref)->pos))),                          \
          private ? (void) 0 : API_EMIT(file_open, file),                  \
          1                                                                \
        ) : (                                                              \
          private ? (void) 0 : API_EMIT(file_close),                       \
          (initial_file == file                                            \
            ? DLOG(file, _1(s)": debug: " HIGHLIGHT("file") ": end", file) \
            : DLOG(file, SPPOSFMT_1 ": debug: " HIGHLIGHT("file")          \
                         ": end (last symbol)",                            \
                         SPPOS((symref)->endpos))),                        \
          0                                                                \
        ) ; i_++)

#define WITH_CALL_TO_EMIT(loc, dst, fnc)                                   \
    for (int i_=0; 0==i_                                                   \
         ? (API_EMIT(insn_call_open, loc, dst, fnc), 1)                    \
         : (API_EMIT(insn_call_close),               0)                    \
         ; i_++)

#define WITH_SWITCH_TO_EMIT(loc, op)                                       \
    for (int i_=0; 0==i_                                                   \
         ? (API_EMIT(insn_switch_open, loc, op), 1)                        \
         : (API_EMIT(insn_switch_close),         0)                        \
         ; i_++)

#define WITH_FUNCTION_TO_EMIT(fnc, endpos)                                 \
    for (int i_=0; 0==i_                                                   \
        ? (                                                                \
          DLOG(func,                                                       \
               CLPOSFMT_1 ": debug: " HIGHLIGHT("function") ": begin: "    \
               HIGHLIGHT(_4(s)),                                           \
               CLPOS(CST_FNC(fnc)->loc), CST_FNC(fnc)->name),              \
          API_EMIT(fnc_open, fnc),                                         \
          1                                                                \
        ) : (                                                              \
          API_EMIT(fnc_close),                                             \
          DLOG(func,                                                       \
               SPPOSFMT_1 ": debug: " HIGHLIGHT("function") ": end: "      \
               HIGHLIGHT(_4(s)), SPPOS(endpos), CST_FNC(fnc)->name),       \
          0                                                                \
        ) ; i_++)


/*
    Warnings, failures handling
 */

#define WARN_UNHANDLED(pos, what)                                      \
    WARN("unhandled " HIGHLIGHT(_1(s)) "\n" SPPOSFMT_2 ": note: here", \
         what, SPPOS(pos))

#define WARN_UNHANDLED_SYM(sym) \
    WARN_UNHANDLED((sym)->pos, show_ident((sym)->ident))

#define WARN_CASE_UNHANDLED(pos, what) \
    case what: WARN_UNHANDLED(pos, #what); break;


/*
    Allocators
 */

#define ALLOCATORS_LIST(x)                   \
    APPLY(x, cl_operand    , "operands"    ) \
    APPLY(x, cl_accessor   , "accessors"   ) \
    APPLY(x, cl_var        , "variables"   ) \
    APPLY(x, cl_initializer, "initializers")

#define X(which, name)                              \
    DECLARE_ALLOCATOR(which)                        \
    ALLOCATOR(which, name)                          \
    static inline struct which *alloc_##which(void) \
    {                                               \
        return __alloc_##which(0);                  \
    }
ALLOCATORS_LIST(X)
#undef X

static inline struct cl_initializer *
alloc_cl_initializer_safe(void)
{
    struct cl_initializer *ret = alloc_cl_initializer();
    ret->next = NULL;
    return ret;
}

#if 0
static inline struct cl_accessor *
alloc_cl_accessor_safe(void)
{
    struct cl_accessor *ret = alloc_cl_accessor();
    ret->next = NULL;
    return ret;
}
#endif


/**
    Show status of local allocators
 */
static inline void
local_alloc_show(void)
{
    WITH_SWAPPED_STREAM_HIGH_AS(debug, err, sp) {
#define X(which, name) \
    show_##which##_alloc();
    ALLOCATORS_LIST(X)
#undef X
    }
}


//
// Mostly sparse related helper functions
//

// this should accommodate worst-case of pointer hexa representation incl. \0
// check in Python: [(x,len(x)) for x in hex(2**64-1).strip('L').partition('x')]
#define PTR_STRING_MAX  17

// incl. compile-time constraint check to make sure we fit into PTR_STRING_MAX
struct ptr_string {
    char str[sizeof(uintptr_t) <= 8 ? PTR_STRING_MAX : -1];
};

#define PTR_STRING(ptr)  (const char*) ptr_string(ptr).str

// NOTE: returning a short array through stack, but should not hurt anything
static inline struct ptr_string
ptr_string(const void *ptr)
{
    struct ptr_string ret;
    if (0 >= snprintf(ret.str, sizeof(ret.str), "%" PRIxPTR, (uintptr_t) ptr))
        DIE("snprintf");
    return ret;
}


static struct symbol *
sparse_fn_arg_at(struct symbol *fn, int pos)
{
    struct symbol *sym, *retval = NULL;

    if (pos <= 0)
        return NULL;

    // FIXME: lot of possible but missing checks
    // alternative: use also symbol->arg_count on SYM_FN
    FOR_EACH_PTR(SYM_FNC_ARGS(fn), sym) {
        if (!--pos)
            retval = sym;
    } END_FOR_EACH_PTR(sym);
    return retval;
}


//
// Types handling
//


/* various helpers */

static inline struct symbol *
type_unwrap(struct symbol *raw_type)
{/* See also:
  * sparse/symbol.h: get_sym_type()
  */
    if (!raw_type)
        CL_TRAP;

    struct symbol *retval = raw_type;
    while (retval->type == SYM_NODE || retval->type == SYM_BITFIELD
           /*retval->type == SYM_ENUM */)
        retval = retval->ctype.base_type;

    /* important, otherwise some info may be missing */
    SP(examine_symbol_type, retval);

    return retval;
}

static inline bool
type_match(const struct cl_type *t1, const struct cl_type *t2)
{
    if (t1 == t2)
        return true;

#if USE_EXTENDED_TYPE_CMP
    if (t1->code == t2->code && t1->item_cnt == t2->item_cnt
        && t1->item_cnt > 0) {
        int i;
        for (i = 0; i < t1->item_cnt; i++)
            if (!type_match(t1->items[i].type, t2->items[i].type))
                return false;
        return  true;
    }
#else
    return false;
#endif
}



/* read composite types */

static struct cl_type *type_from_symbol(struct symbol *type,
                                        struct ptr_db_item **ptr);

static struct cl_type_item *
read_and_append_subtype(struct cl_type *clt, struct symbol *subtype)
{
    struct cl_type_item *subtype_item = type_append_item(clt);
    subtype_item->type = type_from_symbol(subtype, NULL);
    subtype_item->name = sparse_ident(subtype->ident, NULL);

    if (clt->code == CL_TYPE_STRUCT || clt->code == CL_TYPE_UNION)
        subtype_item->offset = subtype->offset;

    return subtype_item;
}

static void
read_and_append_subtypes(struct cl_type *clt, struct symbol_list *subtypes)
{
    struct symbol *subtype;

    FOR_EACH_PTR(subtypes, subtype) {
        read_and_append_subtype(clt, subtype);
    } END_FOR_EACH_PTR(subtype);
}

static inline void
read_type_fnc(struct cl_type *clt, const struct symbol *raw_symbol,
              const struct symbol *type)
{
    /* return value */
    read_and_append_subtype(clt, type->ctype.base_type);
    /* arguments */
    read_and_append_subtypes(clt, type->arguments);

    // XXX: probably convention in cl?
    //read_and_append_subtype(clt, &void_ctype);
}

static inline void
read_type_array(struct cl_type *clt, const struct symbol *raw_symbol,
                const struct symbol *type)
{
    int sub_size;

    //CL_TRAP;
    //clt->name = sparse_ident(type->ident, NULL);

    if (raw_symbol->type == SYM_NODE)
        // normalize size of the "outer" dimension as well as missing size
        clt->size = sizeof_from_bits(raw_symbol->bit_size);
    sub_size = read_and_append_subtype(clt, type->ctype.base_type)->type->size;
    clt->array_size = clt->size/sub_size;
                      // clt->size/clt->items[0].type->size
}

static inline void
read_type_struct(struct cl_type *clt, const struct symbol *raw_symbol,
                 const struct symbol *type)
{
    clt->name = sparse_ident(type->ident, NULL);
    read_and_append_subtypes(clt, type->symbol_list);
}

static inline void
read_type_union(struct cl_type *clt, const struct symbol *raw_symbol,
                const struct symbol *type)
{
    //CL_TRAP;
    clt->name     = sparse_ident(type->ident, NULL);
    //TODO:
    read_and_append_subtypes(clt, type->symbol_list);
    //clt->item_cnt = /* TODO */ 0;
    //clt->items    = /* TODO */ NULL;
}

static inline void
read_type_enum(struct cl_type *clt, const struct symbol *raw_symbol,
               const struct symbol *type)
{
    clt->name = sparse_ident(type->ident, NULL);
}

static struct cl_type *
read_type(struct cl_type *clt, const struct symbol *raw_symbol,
          const struct symbol *type)
{
    typedef void (*type_converter)(struct cl_type *,
                                   const struct symbol * /*raw_symbol*/,
                                   const struct symbol * /*type*/);
    const struct type_conversion {
        enum cl_type_e      type_code;
        union {
            type_converter  converter;
            const char      *string;
        } prop;
    } type_conversions[] = {
    /* Synopsis:
     * sparse/symbol.h
     *
     * Note:
     * Unhandled types are denoted with CL_TYPE_UNKNOWN.
     */
    #define TYPE_STD(spt, clt, conv)                                          \
        [SYM_##spt] = { .type_code=CL_TYPE_##clt, .prop.converter=conv }
    #define TYPE_IGN(spt, _, __)                                              \
        [SYM_##spt] = { .type_code=CL_TYPE_UNKNOWN, .prop.string="SYM_"#spt }
        // how? | sparse type   | clt    | handler               |
        // -----+---------------+--------+-----------------------|

        /* these should not get there (?) */
        TYPE_IGN( UNINITIALIZED ,        ,                       ),
        TYPE_IGN( PREPROCESSOR  ,        ,                       ),
        TYPE_IGN( BASETYPE      ,        ,                       ),
        TYPE_IGN( NODE          ,        , /*unexpected in type*/),

        /* ready to handle */
        TYPE_STD( PTR           , PTR    , NULL /*set code only*/),
        TYPE_STD( FN            , FNC    , read_type_fnc         ),
        TYPE_STD( ARRAY         , ARRAY  , read_type_array       ),
        TYPE_STD( STRUCT        , STRUCT , read_type_struct      ),
        TYPE_STD( UNION         , UNION  , read_type_union       ),
        TYPE_STD( ENUM          , ENUM   , read_type_enum        ),

        /* what about these? */
        /* TODO restrict: fouled_bitwise.c */
        TYPE_IGN( TYPEDEF       ,        ,                       ),
        TYPE_IGN( TYPEOF        ,        ,                       ),
        TYPE_IGN( MEMBER        ,        ,                       ),
        TYPE_IGN( BITFIELD      ,        ,                       ),
        TYPE_IGN( LABEL         ,        ,                       ),
        TYPE_IGN( RESTRICT      ,        ,                       ),
        TYPE_IGN( FOULED        ,        ,                       ),
        TYPE_IGN( KEYWORD       ,        ,                       ),
        TYPE_IGN( BAD           ,        ,                       ),
    #undef TYPE_IGN
    #undef TYPE_STD
    };

    const struct type_conversion *conversion;

    DLOG(type,
         SPPOSFMT_1 ": " HIGHLIGHT("type") ": handle " HIGHLIGHT(_4(s)),
         SPPOS(type->pos), SP(show_typename, (struct symbol *) type));

    //assert(ORDERED( SYM_UNINITIALIZED , symbol->type , SYM_BAD ));
    conversion = &type_conversions[type->type];

    // TODO: raw symbol?
    conv_position(&clt->loc, &type->pos);
    clt->scope = conv_scope(type);

    clt->code = conversion->type_code;
    // TODO: raw_symbol?
    clt->size = sizeof_from_bits(type->bit_size);

    switch (conversion->type_code) {
        case CL_TYPE_UNKNOWN:
            CL_TRAP;
            WARN_UNHANDLED(type->pos, conversion->prop.string);
            clt->name = strdup(SP(show_typename, (struct symbol *)type));
            return clt;
        default:
            break;
    }

    if (conversion->prop.converter)
        conversion->prop.converter(clt, raw_symbol, type);

    return clt;
}


/* pointer and array DB */


static struct cl_type **
prepare_type_array_ptr(struct symbol *raw_symbol,
                       struct ptr_db_item **ptr)
{
    struct cl_type **clt_ptr, *ptr_type = NULL;
    struct ptr_db_item *prev = NULL;
    const struct symbol *type = type_unwrap(raw_symbol);

    ptr_type = type_from_symbol(type->ctype.base_type, &prev);

    if (type->type == SYM_PTR) {
        if (!prev->next)
            prev->next = new_ptr_db_item();
        if (ptr)
            *ptr = prev->next;
        clt_ptr = &prev->next->clt;
    } else {
        // SYM_ARRAY
        int size = sizeof_from_bits(raw_symbol->bit_size)/ptr_type->size;
        size_t i;

        for (i = 0; i < prev->arr_cnt; i++)
            if (prev->arr[i]->arr_size == size)
                break;
        if (i == prev->arr_cnt) {
            // not found
            // guaranteed to continue only in case of success
            NORETWRN(MEM_ARR_APPEND_NEW(prev->arr, prev->arr_cnt));
            prev->arr[i]->arr_size = size;
            prev->arr[i]->clt = NULL;
        }
        clt_ptr = &prev->arr[i]->clt;
    }

    if (!*clt_ptr) {
        // new type to be read (no pointer/array alias found)
        *clt_ptr = read_type(new_type(), raw_symbol, type);

        // finalize SYM_PTR (not in `read_type()' as we have needed info here)
        if (type->type == SYM_PTR) {
            // use obtained dereferenced type
            struct cl_type_item *item = type_append_item(*clt_ptr);
            item->type = ptr_type;
            item->name = NULL;
        }
    }

    return clt_ptr;
}




// note: the only function that uses type_ptr_db global variable directly
static struct cl_type *
type_from_symbol(struct symbol *raw_symbol, struct ptr_db_item **ptr)
{
    struct cl_type *clt, **clt_ptr;
    const struct symbol *type = type_unwrap(raw_symbol);

    DEBUG_TYPE_FROM_SYMBOL_SP(type);

    // Fastest path, we have the type already in hash table
    clt = type_ptr_db_lookup_item(TYPEPTRDB, type, ptr);
    if (!clt) {

        // Extra handling of pointer/arrays symbols, potentially fast circuit
        // for pointer/array alias (i.e., no allocation)
        if (type->type == SYM_PTR || type->type == SYM_ARRAY)
            clt_ptr = prepare_type_array_ptr(raw_symbol, ptr);
        else
            clt_ptr = &clt;

        bool is_new = (*clt_ptr == NULL);
        if (is_new)
            // any new type except for existing pointer/array alias
            *clt_ptr = new_type();

        clt = type_ptr_db_insert(TYPEPTRDB, *clt_ptr, type, ptr);

        if (is_new)
            // Slow path for anything (except for pointers) which is being
            // proceeded for the first time (next time, hashed ctl is used instead)
            //
            // Not an existing pointer/array alias
            //
            // Important: these types are read ex-post in order to prevent recursion
            //            with, e.g., structures
            clt = read_type(clt, raw_symbol, type);
    }

    DEBUG_TYPE_FROM_SYMBOL_CL(clt);

    return clt;
}

static inline struct cl_type *
type_from_instruction(struct instruction *insn, const pseudo_t pseudo)
{
    //struct pseudo_user *pu;

    // Note: pseudo->def == NULL for copy.32
    if (insn && insn->type) {

#if 0
        // TODO: for casts only?
        // first and most authoritative way of getting the type;
        // if the pseudo is the target pseudo, check whether its immediate
        // user/instruction has `orig_type' and use it if available
        if (insn->target == pseudo) {
            pu = (struct pseudo_user *)
                 PTR_ENTRY((struct ptr_list *) insn->target->users, 0);
            if (pu && pu->insn->orig_type)
                return type_from_symbol(pu->insn->orig_type, NULL);
        }
#endif

        if (ORDERED(OP_BINCMP, insn->opcode, OP_BINCMP_END))
            return &bool_clt;

        if (insn->opcode == OP_CALL) {
            // NOTE: experimental, mainly for alloc et al.
            // try to find immediatelly following OP_CAST
            // (normally suppressed) and set the type respectively
            if (ptr_list_size((struct ptr_list *) insn->target->users)) {
                struct pseudo_user *u;
                u = (struct pseudo_user *)PTR_ENTRY(insn->target->users,3);
                if (u->insn->opcode == OP_CAST)
                    return type_from_symbol(u->insn->type, NULL);
            }
        }
        return type_from_symbol(insn->type, NULL);
    } else {
        // type fallback
        return &int_clt;
    }
}


/** operands handling *****************************************************/


static const struct cl_operand no_operand = { .code = CL_OPERAND_VOID };
#define NO_OPERAND      (&no_operand)
#define NO_OPERAND_USE  ((struct cl_operand *) &no_operand)


enum copy_depth {
    copy_shallow,
    copy_shallow_ac_deep,
    copy_shallow_ac_null,
};


static struct cl_accessor *
accessor_copy(const struct cl_accessor *orig)
{
    struct cl_accessor *ret = alloc_cl_accessor();
    *ret = *orig;
    return ret;
}

static inline struct cl_operand *
op_copy(const struct cl_operand *op_src, enum copy_depth copy_depth)
{
    struct cl_operand *ret = alloc_cl_operand();
    struct cl_accessor **ac_chain;
    *ret = *op_src;

    if (copy_shallow_ac_null == copy_depth) {
        ret->accessor = NULL;
    } else if (copy_shallow_ac_deep == copy_depth) {
        ac_chain = &ret->accessor;
        while (*ac_chain) {
            *ac_chain = accessor_copy(*ac_chain);
            ac_chain = &(*ac_chain)->next;
        }
    }
    return ret;
}


#ifdef  LEGACY
/* freeing resources connected with operand */

static void
free_accessor_chain(struct cl_accessor *ac)
{
    struct cl_accessor *ac_next;
    while (ac) {
        ac_next = ac->next;

        /* ac->type (skipped) */
        /* ac->next (in the next round) */

        if (ac->code == CL_ACCESSOR_DEREF_ARRAY)
            /* ac->data.array.index (heap-based!) */
            free_op(ac->data.array.index);

        // free current and go to the next one in the chain
        free(ac);
        ac = ac_next;
    }
}

#endif


/*
    constant operand (primitive literal + string)
 */

/**
    Generic constant operand (primitive literal) constructor (half-way)

    This needs to be defined:
        op:       scope, type
 */
static inline struct cl_operand *
op_make_cst(enum cl_type_e code)
{
#if ASSERT_CST_HAVE_VALID_CODE
    if (true
# define X(type)  && !CL_TYPE_##type == code
        CL_CST_CODELIST(X)
# undef X
    )
        CL_TRAP;  /* only items listed in CL_CST_TYPE_LIST are valid */
#endif

    struct cl_operand *op = alloc_cl_operand();

    op->code = CL_OPERAND_CST;

    CST(op)->code = code;

#if NONZERO_NULL
    op->accessor = NULL;
#endif

    return op;
}

/**
    Function-as-operand constructor (half-way)

    This needs to be defined:
        op:       scope, type 
 */
static struct cl_operand *
op_make_cst_fnc(const struct symbol *sym)
{
    struct cl_operand *op = op_make_cst(CL_TYPE_FNC);

    CST_FNC(op)->uid = ++COUNTER(fnc);  /* starting with 1 looks better */
    CST_FNC(op)->name = sparse_ident(sym->ident, NULL);
    CST_FNC(op)->is_extern = MOD_EXTERN & sym->ctype.modifiers;
    conv_position(&CST_FNC(op)->loc, &sym->pos);

    return op;
}

/**
    Integral-value-as-operand (literal) constructor (may be complete)
 */
static inline struct cl_operand *
op_make_cst_int(long long value)
{
    struct cl_operand *op = op_make_cst(CL_TYPE_INT);

    op->scope = CL_SCOPE_GLOBAL;
    op->type = &int_clt;

    /* TODO: check downcasting safety */
    CST_INT(op)->value = (int) value;

    return op;
}

/**
    Floating-point-value-as-operand (literal) constructor (may be complete)
 */
static inline struct cl_operand *
op_make_cst_real(long double value)
{
    struct cl_operand *op = op_make_cst(CL_TYPE_REAL);

    op->scope = CL_SCOPE_GLOBAL;
    op->type = &double_clt;

    /* TODO: check downcasting safety */
    CST_REAL(op)->value = (double) value;

    return op;
}

/**
    String-as-operand (literal) constructor (may be complete)

    TODO: make it accepting const char * or another func
 */
static struct cl_operand *
op_make_cst_string(const struct expression *expr)
{
    struct cl_operand *op = op_make_cst(CL_TYPE_STRING);

    op->scope = CL_SCOPE_GLOBAL;
    op->type = type_from_symbol(expr->ctype, NULL); /* XXX */

    CST_STR(op)->value = conv_string(expr->string);

    return op;
}


/*
    variable operand
 */

/**
    Generic variable operand constructor (half-way)

    Implicit defaults:
        artificial  = false
        initialized = false

    This needs to be defined:
        op:       scope, type 
        VAR(op):  loc
 */
static inline struct cl_operand *
op_make_var(void)
{
    struct cl_operand *op = alloc_cl_operand();
    op->code = CL_OPERAND_VAR;

    VAR(op) = alloc_cl_var();
    VAR(op)->uid = ++COUNTER(var);  /* starting with 1 looks better */

#if NONZERO_NULL
    op->accessor     = NULL;
    VAR(op)->name    = NULL;
    VAR(op)->initial = NULL;
#endif

    return op;
}


/*
    real operand deal
 */

/**
    Try to get primitive literal from expression
 */
static inline struct cl_operand *
op_from_primitive_literal_maybe(const struct expression *expr)
{
    if (!expr)
        return NO_OPERAND_USE;
    else if (EXPR_VALUE == expr->type)
        return op_make_cst_int(expr->value);
    else if (EXPR_FVALUE == expr->type)
        return op_make_cst_real(expr->fvalue);
    else
        return NO_OPERAND_USE;
}

/**
    Strictly require primitive literal from expression
 */
static inline struct cl_operand *
op_from_primitive_literal(const struct expression *expr)
{
    struct cl_operand *op = op_from_primitive_literal_maybe(expr);
    if (NO_OPERAND == op)
        CL_TRAP;
    return op;
}

static inline struct cl_operand *op_from_symbol(struct symbol *sym);

static inline struct cl_insn *
insn_setops_store(struct cl_insn *cli, const struct instruction *insn);


/**
    Initialize var operand from sparse symbol initializer

    @return Value to be set to VAR(op)->initialized
 */
static bool
op_initialize_var_from_initializer(struct cl_operand *op,
                                   struct symbol *sym)
{
    /*
        we have to mock environment for sparse before calling
        linearize_expression yielding the required instructions
     */
    struct instruction *insn;
    unsigned long orig_modifiers = sym->ctype.modifiers;
    struct expression *expr = sym->initializer;
    struct cl_initializer **initial = &VAR(op)->initial;

    /* XXX block to consider for upstreaming start */
    struct entrypoint *ep = __alloc_entrypoint(0);

    ep->active = __alloc_basic_block(0);
    ep->active->ep = ep;

    expr = alloc_expression(expr->pos, EXPR_SYMBOL);
    expr->symbol = sym;

    /* trick sparse to accept this symbol for initialization */
    sym->ctype.modifiers &= ~(MOD_STATIC | MOD_TOPLEVEL);

    SP(linearize_expression, ep, expr);
    assert(!ptr_list_empty(ep->active->insns));

    /*
        restore (sym->pseudo is set by linearize_expression, but
        no longer needed, so we set the operand being created right
        now here to avoid recursive infloop for converting
        the operand as initializer contains self-references)
     */
    sym->ctype.modifiers = orig_modifiers;
    sym->pseudo = (void *) op;

    /* XXX block to consider for upstreaming end */

    DEBUG_INITIALIZER_EXPR_START();

    FOR_EACH_PTR(ep->active->insns, insn) {
        DEBUG_INITIALIZER_EXPR_SP(insn);
        switch (insn->opcode) {
            case OP_STORE:
                *initial = alloc_cl_initializer_safe();
                conv_position(&(*initial)->insn.loc, &insn->pos);
                (*initial)->insn.code = CL_INSN_UNOP;
                (*initial)->insn.data.insn_unop.code = CL_UNOP_ASSIGN;
                insn_setops_store(&(*initial)->insn, insn);
                break;
            case OP_SYMADDR:
                /* this marks end of initializer (?) */
                PUTHI(debug, cl_debug, GLOBALS(indent), _1(s),
                      "(no cl instruction)");
                continue;
            default:
                WARN_UNHANDLED(insn->pos, "initializer instruction");
                continue;
        }
        DEBUG_INITIALIZER_EXPR_CL(*initial);
        initial = &(*initial)->next;
    } END_FOR_EACH_PTR(insn);

    DEBUG_INITIALIZER_EXPR_STOP();

    return true;
}

/**
    Try to initialize operand using symbol's initializer

    @return Value to be set to VAR(op)->initialized
 */
static bool
op_initialize_var_maybe(struct cl_operand *op,
                        struct symbol *sym)
{
    assert(CL_OPERAND_VAR == op->code);

    struct expression *expr = sym->initializer;
    struct cl_initializer **initial;
    struct cl_operand *from;

    if (!expr)
        /* XXX static globals are implicitly initialized */
        return sym->ctype.modifiers & MOD_STATIC;

    DEBUG_INITIALIZER_SP(expr);

    switch (expr->type) {
        case EXPR_STRING:
            /*
                proceeding through EXPR_INITIALIZER case is possible, but
                unnecessarily complicated as the initializer looks like this:

                    set.80      %r31 <- "var_block"
                    store.80    %r31 -> 0["var_block"]
             */
            VAR(op)->initial = alloc_cl_initializer_safe();
            VAR(op)->initial->insn.code = CL_INSN_UNOP;
            VAR(op)->initial->insn.data.insn_unop.code = CL_UNOP_ASSIGN;
            conv_position(&VAR(op)->initial->insn.loc, &expr->pos);

            VAR(op)->initial->insn.data.insn_unop.dst = op;
            VAR(op)->initial->insn.data.insn_unop.src = op_make_cst_string(expr);
            break;
        case EXPR_SYMBOL:
            /*
                get operand for that symbol (good as we cache the resolution
                anyway), then "steal" its initializer;
                hopefully no self-recursion...
             */
            from = op_from_symbol(expr->symbol);
            assert(OP_INITIALIZED_VAR(from));
            VAR(op)->initial = from->data.var->initial;
            break;
        case EXPR_INITIALIZER:
            /* no need to "debug" the same over again */
            return op_initialize_var_from_initializer(op, sym);
        default:
            WARN("unhandled initializer expression type");
            return false;
    }

    DEBUG_INITIALIZER_CL(VAR(op)->initial);

    return true;
}

/**
    Operand from symbol

    There is a simple caching mechanism storing the resolved operand to
    sym->pseudo and reusing it when needed (this item is only used
    for linearization and cleared after this pass).
 */
static inline struct cl_operand *
op_from_symbol(struct symbol *sym)
{
    struct cl_operand *op = NO_OPERAND_USE;

    if (sym->pseudo) {
        DEBUG_OP_FROM_SYMBOL_CACHE(sym);
        return (struct cl_operand *) sym->pseudo;
    }

    DEBUG_OP_FROM_SYMBOL_SP(sym);

    /*
        no identifier -> may be a primitive literal;
        function has to have it (unless anonymous? XXX)
     */
    if (!sym->ident) {
        assert(SYM_FN == sym->ctype.base_type->type);
        op = op_from_primitive_literal_maybe(sym->initializer);
    } else if (SYM_FN == sym->ctype.base_type->type) {
        op = op_make_cst_fnc(sym);
    }

    if (NO_OPERAND == op)
        /* so it will be var, we return to it in a moment */
        op = op_make_var();

    /* yet uninitialized or get it right for integral type (signedness), etc. */
    op->scope = conv_scope(sym);
    op->type = type_from_symbol(sym, NULL);

    if (CL_OPERAND_VAR == op->code) {
        VAR(op)->name = sparse_ident(sym->ident, NULL);
        VAR(op)->artificial = !VAR(op)->name;
        conv_position(&VAR(op)->loc, &sym->pos);

        DLOG(init, "\tsymbol initialized? "_1(c), GET_YN(sym->initialized));
        VAR(op)->initialized = op_initialize_var_maybe(op, sym);
    }

    DEBUG_OP_FROM_SYMBOL_CL(sym, op);

    return (struct cl_operand *) (sym->pseudo = (void *) op);
}

/**
    Operand from function argument
 */
static inline struct cl_operand *
op_from_fn_argument(const pseudo_t pseudo)
{
    struct symbol *arg_sym;
    struct cl_operand *op;

    arg_sym = sparse_fn_arg_at(pseudo->def->bb->ep->name, pseudo->nr);
    if (!arg_sym)
        CL_TRAP;

    op = op_from_symbol(arg_sym);
    assert(CL_SCOPE_FUNCTION == op->scope);
    return op;
}

/**
    Operand from register

    The type is available in the defining instruction (pseudo->def).
 */
static struct cl_operand *
op_from_register(const struct instruction *insn, const pseudo_t pseudo)
{
    struct cl_operand *op = op_make_var();

    op->scope = CL_SCOPE_BB;
    op->type  = type_from_instruction(pseudo->def, pseudo);

    VAR(op)->artificial = true;
    conv_position(&VAR(op)->loc, &insn->pos);

    return op;
}

/** Operand from integral value */
static inline struct cl_operand *
op_from_intval(int value)
{
    return op_make_cst_int(value);
}

/**
    Operand from general pseudo

    There is a simple caching mechanism storing the resolved operand to
    pseudo->priv and reusing it when needed.

    Problems/exceptions/notes:
    1. PSEUDO_VAL and PSEUDO_REG operands are not holding type information
       (it is reachable from insn however)
 */
static inline struct cl_operand *
op_from_pseudo(const struct instruction *insn, const pseudo_t pseudo)
{
    assert(pseudo);

    struct cl_operand *op;

    if (pseudo->priv) {
        DEBUG_OP_FROM_PSEUDO_CACHE(pseudo);
        return (struct cl_operand *) pseudo->priv;
    }

    switch (pseudo->type) {
        case PSEUDO_VOID: op = NO_OPERAND_USE; break;
        /* symbol (argument boils down to it too) is "debugged" in-place */
        case PSEUDO_SYM:  return pseudo->priv = op_from_symbol(pseudo->sym);
        case PSEUDO_ARG:  return pseudo->priv = op_from_fn_argument(pseudo);
        case PSEUDO_REG:  op = op_from_register(insn, pseudo); break;
        case PSEUDO_VAL:  op = op_from_intval((int) pseudo->value); break;
        case PSEUDO_PHI:
        default:
            WARN("unhandled pseudo: from instruction " HIGHLIGHT(_1(s)),
                 show_instruction((struct instruction *) insn));
            return pseudo->priv = NO_OPERAND_USE;
    }

    DEBUG_OP_FROM_PSEUDO_SP(pseudo);
    DEBUG_OP_FROM_PSEUDO_CL(op);

    return pseudo->priv = op;
}


static inline void
accessor_array_index(struct cl_accessor *ac, int index)
{
    ac->code                  = CL_ACCESSOR_DEREF_ARRAY;
    ac->data.array.index      = op_make_cst_int(index);
}

static inline struct cl_accessor *
op_append_accessor(struct cl_operand *op, struct cl_accessor *ac)
{
    struct cl_accessor *ac_chain, **retval;

    if (!op->accessor)
        retval = &op->accessor;
    else {
        ac_chain = op->accessor;
        while (ac_chain->next)
            ac_chain = ac_chain->next;
        retval = &ac_chain->next;
    }

    if (ac)
        *retval = ac;
    else
        *retval = alloc_cl_accessor();

    // guaranteed not to return NULL
    return *retval;
}

static inline struct cl_accessor *
op_prepend_accessor(struct cl_operand *op, struct cl_accessor *ac)
{
    struct cl_accessor *ac_cur = ac;

    if (!ac_cur)
        ac_cur = alloc_cl_accessor();
    else
        while (ac_cur->next)
            ac_cur = ac_cur->next;

    ac_cur->next = op->accessor;
    op->accessor = ac;

    return ac;
}

// Note: returns UINT_MAX when operand could not be dug
static unsigned
op_dig_step(const struct cl_operand **op_composite, unsigned insn_offset)
{
    // `insn_offset' is consumed only by CL_TYPE_STRUCT or CL_TYPE_ARRAY;
    // e.g., accessing struct element is different with the first level
    // access (use insn_offset) and with other accesses (always zero offset)
    int retval = insn_offset,
        i = 0;

    struct cl_accessor *ac;
    const struct cl_operand *op = *op_composite;

    #define MAP_ACCESSOR(acc, clt, cl_ac) \
        case CL_##clt: acc = alloc_cl_accessor(); acc->code = CL_##cl_ac;
    switch (op->type->code) {
        MAP_ACCESSOR(ac, TYPE_STRUCT, ACCESSOR_ITEM) {
            for (i = 0; i < op->type->item_cnt-1; i++)
                if (op->type->items[i].offset == insn_offset
                    || op->type->items[i+1].offset > insn_offset)
                    break;

            assert(op->type->items[i].offset <= insn_offset);

            // if item has not been found on exact offset match
            // (then `insn_offset' is expected to be greater than the offset
            // of the last proceeded item in the structure), the next digging
            // (really ought to be possible) will continue through this item

            ac->data.item.id = i;
            retval = insn_offset - op->type->items[i].offset;
            break;
        }
        MAP_ACCESSOR(ac, TYPE_ARRAY, ACCESSOR_DEREF_ARRAY) {
            div_t indexes;
            indexes = div(insn_offset, op->type->size/op->type->array_size);
            // !!TODO API change
            accessor_array_index(ac, indexes.quot);
            // the remainder serves for next index-based-dereferencing rounds
            retval = indexes.rem;
            break;
        }
        MAP_ACCESSOR(ac, TYPE_PTR, ACCESSOR_DEREF) {
            if (insn_offset /* && op->type->items->type->size*/) {
                // convert into another accessor then predestined (ptr->arr),
                // but only if resulting index would be 1+
                div_t indexes = div(insn_offset, op->type->items->type->size);
                if (indexes.quot)
                    // !!TODO API change
                    accessor_array_index(ac, indexes.quot);
                // the remainder serves for next index-based-deref. rounds
                retval = indexes.rem;
            }
            break;
        }
        default:
            return UINT_MAX;
    }

    struct cl_operand *clone = op_copy(op, copy_shallow_ac_deep);
    op_prepend_accessor(clone, ac);
    // accessor's type is the operand's type (it itself will be peeled off)
    ac->type = (struct cl_type *) clone->type;
    // peel off one level of type/access decoration from the operand
    clone->type = (struct cl_type *) clone->type->items[i].type;

    *op_composite = clone;

    return retval;
}

// XXX: removal candidate
static inline bool
op_accessible(const struct cl_operand *op)
{
    if (op->code == CL_OPERAND_VOID)
        return false;

    switch (op->type->code) {
        /* all these imply CL_VAR */
        case CL_TYPE_STRUCT:
        case CL_TYPE_UNION:
        case CL_TYPE_ARRAY:
        case CL_TYPE_PTR:
            return true;
        default:
            return false;
    }
}

static unsigned
op_dig_for_type_match(const struct cl_operand **op_composite,
                      const struct cl_type *expected_type,
                      unsigned initial_offset)
{/* Problems/exceptions/notes:
  * When digging union, we go through its items, apply a DFS-based search
  * in order to get expected type on one, if it ends without success, we try
  * another (on the whole, should not end without success).
  */

    unsigned offset = initial_offset;

    const struct cl_operand *op;

    while (!type_match((op = *op_composite)->type, expected_type)) {

        if (op->type->code == CL_TYPE_UNION) {
            // unions bring non-determinism as there are more ways how to
            // "dig" -- use DFS with a sort of backtracking (through stack)
            struct cl_operand *op_clone;
            struct cl_accessor *ac;
            int i;
            size_t res;

            op_clone = op_copy(op, copy_shallow_ac_null);

            for (i = 0; i < op->type->item_cnt; i++) {
                op_clone->accessor = NULL;
                ac               = op_append_accessor(op_clone, NULL);
                ac->code         = CL_ACCESSOR_ITEM;
                ac->type         = op_clone->type;
                ac->data.item.id = i;
                op_clone->type = (struct cl_type *) ac->type->items[i].type;

                res = op_dig_for_type_match((const struct cl_operand **)
                                            &op_clone, expected_type, offset);

                if (UINT_MAX != res)
                    // successfull case of digging
                    break;

                // restore for the next round
                /*free_accessor_chain(op_clone->accessor); */
                op_clone->accessor = NULL;
                op_clone->type = op->type;
            }

            if (UINT_MAX != res) {
                // reflect the changes collected within successful DFS trace
                // (with `op_clone') back to its preimage `op'
                assert(op_clone->accessor);
                op_prepend_accessor(op_clone, op->accessor);
                assert(type_match(op_clone->type, expected_type));
                *op_composite = op_clone;
            }

            /*free(op_clone);*/
            offset = res;
        } else
            offset = op_dig_step(op_composite, offset);

        if (UINT_MAX == offset)
            break;
    }

    return offset;
}


//
// Instructions handling
//

enum assignment_ops_handling {
    TYPE_LHS_KEEP        = (1 << 0),
    TYPE_RHS_KEEP        = (1 << 1),

    /* LHS */

    // to obtain operand "in a right form", either dig into original one
    // and find the expected inner type item (for "non-immediate" PSEUDO_SYM
    // and PSEUDO_ARG) or keep it and add a dereference accessor
    // (for "immediate" PSEUDO_VAL and PSEUDO_REG);
    //
    // usage: INSN_STORE
    TYPE_LHS_DIG         = (1 << 2),

    /* RHS */

    // to obtain operand "in a right form", dig into original one and find
    // the expected inner type item; when combined with TYPE_RHS_DIG_ALL, this
    // applies for any pseudo type, for "non-immediate" PSEUDO_SYM
    // and PSEUDO_ARG only otherwise
    //
    // usage: all assign instructions except for INSN_COPY
    TYPE_RHS_DIG         = (1 << 3),
    TYPE_RHS_DIG_ANY     = (1 << 4),

    // to obtain operand "in a right form", add a level of pointer indirection
    // (i.e., reference the current one, add a reference accessor);
    // with INSN_STORE (that uses also TYPE_RHS_DIG), this has a special
    // meaning telling that this will be done only after a level
    // of indirection has been successfully removed first (so it
    // is effectively returned back)
    //
    // usage: INSN_STORE, INSN_PTR_CAST
    TYPE_RHS_REFERENCE = (1 << 5),
};

/**
    Modify RHS operand as per expected type
 */
static void
insn_assignment_mod_rhs(struct cl_operand **op_rhs, pseudo_t rhs,
                        const struct instruction *insn,
                        enum assignment_ops_handling ops_handling)
{/* Synopsis: see `insn_assignment_base' (the only caller)
  */
    if (ops_handling & TYPE_RHS_KEEP)
        return;

    int offset = insn->offset;
    bool use_rhs_dereference = true;
#if 0
    struct cl_type *type = (insn->opcode == OP_PTRCAST)
                               ? type_from_symbol(insn->orig_type, NULL)
                               : type_from_symbol(insn->type, NULL);
#endif
    struct cl_type *type = (insn->orig_type)
                               ? type_from_symbol(insn->orig_type, NULL)
                               : type_from_symbol(insn->type, NULL);

    // dig rhs (when applicable)
    if (ops_handling & TYPE_RHS_DIG) {
        if (!pseudo_immediate(rhs) || ops_handling & TYPE_RHS_DIG_ANY) {
            const struct cl_type *expected_type = type;

            if (ops_handling & TYPE_RHS_REFERENCE) {
                // remove one level of indirection of both resulting_type
                // and operand type (to be compensated by adding one back
                // in "reference rhs" part)
                offset = 0;
                if (!type_match((*op_rhs)->type, expected_type)) {
                    expected_type = expected_type->items->type;
                    // XXX: second condition yields better results
                    //      with tests/struct/rs1-03 but makes
                    //      tests/predator/test-0044.c fail
                    if (!type_match((*op_rhs)->type, expected_type)
                        /*|| op_accessible(op_rhs)*/)
                        op_dig_step((const struct cl_operand **) op_rhs,
                                    offset);
                } else
                    use_rhs_dereference = false;
            }

            unsigned res = op_dig_for_type_match((const struct cl_operand **)
                                                 op_rhs, expected_type, offset);
            if (res == UINT_MAX) {
                // no success when digging operand for type match,
                // it may be a pointer and we just haven't been told this
                // type information (e.g., due to typeless PSEUDO_VAL)
                if ((*op_rhs)->type->code == CL_TYPE_INT
                    && expected_type->code == CL_TYPE_PTR) {
                    struct cl_accessor *ac;
                    struct cl_type *expected_type_dug;

                    // promote an operand type to a pointer and remove
                    // a level of pointer indirection also from expected type
                    // XXX: should be the base type switched to void?
                    (*op_rhs)->type = build_referenced_type((*op_rhs)->type);
                    expected_type_dug = (struct cl_type *)
                                        expected_type->items->type;

                    while (expected_type_dug->code == CL_TYPE_PTR) {
                        // now, we do the same but explicitly adding
                        // dereferences, adjusting the level of dereferences
                        // in operand's type
                        ac = op_prepend_accessor(*op_rhs, NULL);
                        ac->code = CL_ACCESSOR_DEREF;
                        ac->type = (*op_rhs)->type;

                        (*op_rhs)->type = build_referenced_type((*op_rhs)->type);
                        expected_type_dug = (struct cl_type *)
                                            expected_type_dug->items->type;
                    }
                } else
                    CL_TRAP;  // should not happen
            }

        } else if (ops_handling & TYPE_RHS_REFERENCE) {
            // OP_STORE with PSEUDO_VAL rhs (e.g., value can be pointer)
            if (rhs->type == PSEUDO_VAL)
                (*op_rhs)->type = type;  // probably no other choice
#if DO_EXTRA_CHECKS
            else if (!type_match((*op_rhs)->type, type))
                CL_TRAP;  // should be the same type
#endif
            use_rhs_dereference = false;
        }
    }

    // reference rhs (when applicable)
    if (ops_handling & TYPE_RHS_REFERENCE && use_rhs_dereference) {
        // OP_PTRCAST, OP_STORE (for PSEUDO_SYM and PSEUDO_ARG only
        //                       and only when returning level of indirection)
        struct cl_accessor *ac = op_append_accessor(*op_rhs, NULL);
        ac->code = CL_ACCESSOR_REF;
        ac->type = (*op_rhs)->type;
        (*op_rhs)->type = build_referenced_type((*op_rhs)->type);
#if DO_EXTRA_CHECKS
        if (!type_match((*op_rhs)->type, type))
            CL_TRAP;  // should be the same type
#endif
    }
}


/**
    Set operands for assigment as per provided hints for operands treating

    [input] OP_LOAD, OP_STORE, OP_COPY + casts (CAST, SCAST, FPCAST, PTRCAST)
        insn->type (not for OP_COPY):       type of final assigned value
        insn->orig_type (OP_PTRCAST only):  original type of value to assign
    [output] CL_INSN_UNOP (set by [transitive] caller, as with location)
        data.insn_unop.code (CL_UNOP_ASSIGN, set by [transitive] caller)
        data.insn_unop.dst ~ lhs
        data.insn_unop.src ~ rhs

    Problems/exceptions/notes:
    1. Problem with a "right form" of both the operands (whether to consider
       the whole struct or its first element, etc.); additionally, some
       instructions requires (de)referencing of the operands explicitly.
    S. The way to obtain "right form" of both operands is driven by
       `ops_handling' (see `enum assignment_ops_handling').
       If appropriate, combine `op_dig_for_type_match' and `insn->type'
       (`insn->orig_type') for this adjustment.
 */
static struct cl_insn *
insn_assignment_base(struct cl_insn *cli, const struct instruction *insn,
                     pseudo_t lhs,    /* := */    pseudo_t rhs,
                     enum assignment_ops_handling ops_handling)
{
    struct cl_operand
        **op_lhs = (struct cl_operand **) &cli->data.insn_unop.dst,
        **op_rhs = (struct cl_operand **) &cli->data.insn_unop.src;

    /* prepare LHS */

    *op_lhs = op_from_pseudo(insn, lhs);

    if (ops_handling & TYPE_LHS_DIG) {
        struct cl_type *type = type_from_symbol(insn->type, NULL);
        if (!op_accessible(*op_lhs)) {
            struct cl_accessor *ac = op_append_accessor(*op_lhs, NULL);
            ac->code = CL_ACCESSOR_DEREF;
            // note: no such clt easily accessible (contrary to previous case)
            ac->type = build_referenced_type(type);
            (*op_lhs)->type = type;
        } else {
            op_dig_for_type_match((const struct cl_operand **) op_lhs,
                                  type, insn->offset);
        }
    }

    /* prepare RHS (quite complicated compared to LHS) */

    *op_rhs = op_from_pseudo(insn, rhs);
    insn_assignment_mod_rhs(op_rhs, rhs, insn, ops_handling);

    /*
        FIXME (SPARSE?):  sparse generates (due to execution model?) extra
        instruction, e.g. "store %arg1 -> 0[num]" in case of "num == %arg1"
     */
#if FIX_SPARSE_EXTRA_ARG_TO_MEM
    if (lhs->type != PSEUDO_SYM || rhs->type != PSEUDO_ARG
      || (*op_lhs)->data.var->uid != (*op_rhs)->data.var->uid)
#endif
        return cli;
#if FIX_SPARSE_EXTRA_ARG_TO_MEM
    WARN("instruction omitted: " HIGHLIGHT(_1(s)),
         show_instruction((struct instruction *) insn));
    return NULL;
#endif
}

/**
    Set operands for "store indirectly whatever to whatever" assignment

    [input] OP_STORE
        insn->src:    target memory address (pointer to what is being assigned)
        insn->target: source of assignment
        insn->type:   type of value to be assigned
 */
static inline struct cl_insn *
insn_setops_store(struct cl_insn *cli, const struct instruction *insn)
{
    return insn_assignment_base(cli, insn,
        insn->src,     /* := */  insn->target,
        TYPE_LHS_DIG       |     (TYPE_RHS_DIG | TYPE_RHS_REFERENCE)
    );
}

/**
    Set operands for "load from memory to the new register" assignment

    [input] OP_LOAD
        insn->target ... register to be assigned
        insn->src    ... mem. address containing source value
        insn->type   ... type of value to be assigned

 */
static inline struct cl_insn *
insn_setops_load(struct cl_insn *cli, const struct instruction *insn)
{
    assert(PSEUDO_REG == insn->target->type);

    return insn_assignment_base(cli, insn,
        insn->target,  /* := */  insn->src,
        TYPE_LHS_KEEP      |     (TYPE_RHS_DIG | TYPE_RHS_DIG_ANY)
    );
}

/**
    Set operands for simple alias-making assignment

    XXX merge with _setval?

    [input] OP_COPY
        insn->target
        insn->src
        insn->type

    Problems/exceptions/notes:
    FIXME: are cast operations OK?
 */
static inline struct cl_insn *
insn_setops_copy(struct cl_insn *cli, const struct instruction *insn)
{
    assert(PSEUDO_REG == insn->target->type);

    return insn_assignment_base(cli, insn,
        insn->target,  /* := */  insn->src,
        TYPE_LHS_KEEP      |     TYPE_RHS_KEEP
    );
}

/**
    Set operands for assignment with type casting

    [input] OP_CAST, OP_SCAST
        insn->target
        insn->src
        insn->type
        insn->orig_type
        insn->size (?)

    Problems/exceptions/notes:
    May end up with with emitting CL_BINOP_BIT_AND when casting "smaller"
    type to "bigger" type (currently, for bitfields only) to be sure we get
    rid of unwanted garbage (e.g., data from the next bitfield item).
 */
static inline struct cl_insn *
insn_setops_cast(struct cl_insn *cli, const struct instruction *insn)
{
  if (insn->orig_type->bit_size < insn->type->bit_size
             && insn->orig_type->ctype.base_type->type == SYM_BITFIELD) {
        // we have to apply CL_BINOP_BIT_AND on `insn->src' using mask
        // (currently of int size XXX?) with additional higher bits zeroed

        int mask = ~((~0) << insn->orig_type->bit_size);
        struct cl_operand dst, lhs, op_mask;

        cli->code = CL_INSN_BINOP;
        cli->data.insn_binop.code = CL_BINOP_BIT_AND;
        cli->data.insn_binop.dst  = op_from_pseudo(insn, insn->target);
        cli->data.insn_binop.src1 = op_from_pseudo(insn, insn->src);
        cli->data.insn_binop.src2 = op_from_intval(mask);

        return cli;
    }

    /*
        if orig_type->bit_size > type->bit_size, something may be lost, but:
        (1) sparse would (should?) warn about this if it is not intentional
        (2) sparse does char arithmetics by upcasting to integers first, then
            downcasting the result back to char, so this may be the case
    */
    return insn_setops_copy(cli, insn);
}

/**
    Set operands for assignment with pointer casting

    [input] OP_PTRCAST
        insn->target
        insn->src
        insn->type
        insn->orig_type

 */
static inline struct cl_insn *
insn_setops_ptrcast(struct cl_insn *cli, const struct instruction *insn)
{
    return insn_assignment_base(cli, insn,
        insn->target,  /* := */  insn->src,
        TYPE_LHS_KEEP      |     (TYPE_RHS_DIG | TYPE_RHS_REFERENCE)
    );
}

/**
    Set operands for assignment of "primitive literal"

    [input] OP_SETVAL
        insn->target: destination
        insn->val:    value to be assigned in the form of an expression
            EXPR_FVALUE ~ constant of CL_TYPE_REAL type
            (other types of expressions not checked yet)
    [output] CL_INSN_UNOP
        data.insn_unop.dst ~ insn->target
        data.insn_unop.src ~ insn->val (see above)

    Problems/exceptions/notes:
    XXX See `op_from_expression'.
 */
static inline struct cl_insn *
insn_setops_setval(struct cl_insn *cli, const struct instruction *insn)
{
    cli->data.insn_unop.dst = op_from_pseudo(insn, insn->target);
    cli->data.insn_unop.src = op_from_primitive_literal(insn->val);
    return cli;
}

/**
    Set operands for unary operations (except for assignments)

    [input] OP_NOT, OP_NEG
        insn->target: destination
        insn->src1:   source
    [output] CL_INSN_UNOP
        data.insn_unop.code (set by caller, rewrite in case of unary minus)
        data.insn_unop.dst ~ insn->target
        data.insn_unop.src ~ insn->src1

    Problems/exceptions/notes:
    1. OP_NEG means "unary minus" when applied on int.
 */
static inline struct cl_insn *
insn_setops_unop(struct cl_insn *cli, const struct instruction *insn)
{
    cli->data.insn_unop.dst = op_from_pseudo(insn, insn->target);
    cli->data.insn_unop.src = op_from_pseudo(insn, insn->src1);

    /* for "unary minus", rewrite unary operation */
    if (CL_TYPE_INT == cli->data.insn_unop.src->type->code
      && OP_NEG == insn->opcode)
        cli->data.insn_unop.code = CL_UNOP_MINUS;

    return cli;
}

/**
    Set operands for binary operations

    [input] (any arithmetic, shift, logical and explicit comparison operation)
        insn->target:   destination
        insn->src(1|2): operands
    [output] CL_INSN_BINOP
        data.insn_binop.code (set by caller, rewrite for pointer arithmetics)
        data.insn_binop.dst      ~ insn->target
        data.insn_binop.src(1|2) ~ insn->src(1|2)

    Problems/exceptions/notes:
    1. Binary arithmetics case has to be detected and imposed explicitly.
    S. If any of the operand is a pointer or an array, promote CL_BINOP_PLUS
       to CL_BINOP_POINTER_PLUS (other operations not expected in this case).
 */
static struct cl_insn *
insn_setops_binop(struct cl_insn *cli, const struct instruction *insn)
{
    struct cl_operand *op1, *op2;
    cli->data.insn_binop.dst  = op_from_pseudo(insn, insn->target);
    cli->data.insn_binop.src1 = op1 = op_from_pseudo(insn, insn->src1);
    cli->data.insn_binop.src2 = op2 = op_from_pseudo(insn, insn->src2);

    /* for pointer arithmetics, rewrite binary operation */
    if (op1->type->code == CL_TYPE_PTR || op1->type->code == CL_TYPE_ARRAY
        || op2->type->code == CL_TYPE_PTR || op2->type->code == CL_TYPE_ARRAY) {
        if (CL_BINOP_PLUS == cli->data.insn_binop.code)
            cli->data.insn_binop.code = CL_BINOP_POINTER_PLUS;
        else
            // only addition is supported (XXX: may other ops occur?)
            /* TODO: pointer minus = -pointer plus */
            CL_TRAP;
    }

    return cli;
}

/**
    Set operand for return statement instruction

    [input] OP_RET
        insn->src:  value to be used as a return value (extra case: NULL)
        insn->type: type of return value
    [output] CL_INSN_RET
        cl_insn.insn_ret.src ~ insn->src

    Problems:
    1. Problem with a "right form" of the operand (whether to consider
       the whole struct or its first element, etc.).
    S. Combine `op_dig_for_type_match' and `insn->type' for adjustment.
 */
static inline struct cl_insn *
insn_setops_ret(struct cl_insn *cli, const struct instruction *insn)
{
    const struct cl_type *resulting_type;

    cli->data.insn_ret.src = insn->src ? op_from_pseudo(insn, insn->src)
                                       : NO_OPERAND;

    /* TODO: decide according to the pseudo instead? */
    if (op_accessible(cli->data.insn_ret.src)) {
        resulting_type = type_from_symbol(insn->type, NULL);
        op_dig_for_type_match(&cli->data.insn_ret.src, resulting_type, insn->offset);
    }

    return cli;
}


/*
    helpers for emitting "weak" instructions (without 1:1 mapping with sparse)
 */

static inline void
insn_emit_jmp(struct cl_insn *cli, const char *label)
{
    cli->code                = CL_INSN_JMP;
    cli->data.insn_jmp.label = label;
    API_EMIT(insn, cli);
}

static inline void
insn_emit_cond(struct cl_insn *cli, struct cl_operand *op_cond,
               const char *then_label, const char *else_label)
{
    cli->code                      = CL_INSN_COND;
    cli->data.insn_cond.src        = op_cond;
    cli->data.insn_cond.then_label = then_label;
    cli->data.insn_cond.else_label = else_label;
    API_EMIT(insn, cli);
}

static inline void
insn_emit_copy(struct cl_insn *cli, const struct instruction *insn,
               pseudo_t lhs, pseudo_t rhs)
{
    cli->code                = CL_INSN_UNOP;
    cli->data.insn_unop.code = CL_UNOP_ASSIGN;
    if (insn_assignment_base(cli, insn,
        lhs,           /* := */  rhs,
        TYPE_LHS_KEEP      |     TYPE_RHS_KEEP
    ))
        API_EMIT(insn, cli);
}


/*
    emitting direct instruction mapping (resulting in one or more instructions)
 */

/** Function call, aborting further intra-BB run if marked as non-returning */
static void
insn_emit_call(struct cl_insn *cli, const struct instruction *insn)
{
    struct pseudo *arg;
    int cnt = 0;

    WITH_CALL_TO_EMIT(&cli->loc, op_from_pseudo(insn, insn->target),
                      /* = */  op_from_pseudo(insn, insn->func)  /* (...) */)
        FOR_EACH_PTR(insn->arguments, arg)
            /* XXX: ++cnt repeated side-effect? */
            API_EMIT(insn_call_arg, ++cnt, op_from_pseudo(insn, arg));
        END_FOR_EACH_PTR(arg);


    /*
        special handling of non-returning function (forced end of BB);
        if the programmer is lying, she is triggering UB anyway
        (as per C11 draft; actually challenge for sparse)
     */
    if (insn->func->sym->ctype.modifiers & MOD_NORETURN) {
        cli->code = CL_INSN_ABORT;
        API_EMIT(insn, cli);
    }
}

/** Un/conditional jump */
static void
insn_emit_br(struct cl_insn *cli, const struct instruction *insn)
{
    if (pseudo_futile(insn->cond))
        /* unconditional jump */
        insn_emit_jmp(cli, PTR_STRING(insn->bb_true));
    else
        /* conditional jump */
        insn_emit_cond(cli, op_from_pseudo(insn, insn->cond),
                       PTR_STRING(insn->bb_true), PTR_STRING(insn->bb_false));
}

/**
    Conditional operator

    [input] OP_SEL
        insn->src1
        insn->src2
        insn->src3
        insn->target

    [output] regex: CL_INSN_COND (bb_open "assign" CL_INSN_JUMP){2} bb_open
             (location set by caller)

    Problems/exceptions/notes:
    1. BB label uniqueness.
    S. Address of `insn' +0, +1 or +2, provided that insn has size of 4+
       and char 1 (compile time constraints?).
 */
static void
insn_emit_sel(struct cl_insn *cli, const struct instruction *insn)
{
    /* local BB labels */
    const char *const bb_true  = PTR_STRING(((char *) insn) + 0);
    const char *const bb_false = PTR_STRING(((char *) insn) + 1);
    const char *const bb_merge = PTR_STRING(((char *) insn) + 2);

    /* cond instruction */
    op_from_pseudo(insn, insn->src1);
    insn_emit_cond(cli, op_from_pseudo(insn, insn->src1), bb_true, bb_false);

    /* first BB ("then" branch):  assign + jump to merging BB */
    API_EMIT(bb_open, bb_true);
    insn_emit_copy(cli, insn, insn->target,  /* := */  insn->src2);
    insn_emit_jmp(cli, bb_merge);

    /* second BB ("else" branch):  assign + jump to merging BB */
    API_EMIT(bb_open, bb_false);
    insn_emit_copy(cli, insn, insn->target,  /* := */  insn->src3);
    insn_emit_jmp(cli, bb_merge);

    /* merging BB */
    API_EMIT(bb_open, bb_merge);
}

/**
    Switch (incl. GNU C range extension as supported by sparse)

    [input] OP_SWITCH
        insn->target:        selection source
        insn->multijmp_list: list of branches/cases
            jmp->target:               respective basic block
            ---
            jmp->begin == jmp->end ... single value selection
            jmp->begin < jmp->end  ... range selection
            jmp->begin > jmp->end  ... default case

    [output] regex: insn_switch_open (insn_switch_case)* insn_switch_close
             (location set by caller)

    Problems/exceptions/notes:
    - not enough accurate location info from SPARSE for switch/case
 */
static void
insn_emit_switch(struct cl_insn *cli, const struct instruction *insn)
{
    struct cl_operand *op, *val_lo, *val_hi, **val_hi_ptr = &val_lo;
    struct multijmp *jmp;

    op = op_from_pseudo(insn, insn->target);
    WITH_SWITCH_TO_EMIT(&cli->loc, op) {

        FOR_EACH_PTR(insn->multijmp_list, jmp) {
            if (jmp->begin <= jmp->end) {
                /* non-default */
                val_lo = op_from_intval(jmp->begin);
                val_lo->type = op->type;
                val_hi = NO_OPERAND_USE;

                if (jmp->begin != jmp->end) {
                    /* actually a range */
                    val_hi = op_from_intval(jmp->end);
                    val_hi->type = op->type;
                }
            } else {
                /* default case */
                val_lo = val_hi = NO_OPERAND_USE;
            }

            API_EMIT(insn_switch_case, &cli->loc, val_lo, val_hi,
                     PTR_STRING(jmp->target));

        } END_FOR_EACH_PTR(jmp);

    }
}


enum conv_type {
    conv_setops,
    conv_emit,
    conv_ignore,
    conv_warn
};

/**
    Consider instruction for emitting

    @returns ret_negative=not emitted, ret_positive=emitted, ret_escape=abort bb
 */
static enum retval
consider_instruction(struct instruction *insn)
{
    typedef struct cl_insn *(*insn_setops)(
        struct cl_insn *,
        const struct instruction *);
    typedef void (*insn_emit)(struct cl_insn *, const struct instruction *);

    static const struct insn_conversion {
        enum cl_insn_e       insn_code;
        union {
            enum cl_unop_e   unop;
            enum cl_binop_e  binop;
            enum conv_type   conv;
        } code;
        union {
            insn_setops      setops;
            insn_emit        emit;
            const char       *string;
        } prop;
    } insn_conversions[] = {
    #define INSN_SET(spi, cli, hnd)                               \
        [OP_##spi] = { .insn_code=CL_INSN_##cli,                  \
                       .code.conv=conv_setops, .prop.setops=hnd }
    #define INSN_EMT(spi, _, hnd) \
        [OP_##spi] = { .code.conv=conv_emit, .prop.emit=hnd }
    #define INSN_UNI(spi, unop_code, hnd)                                 \
        [OP_##spi] = { .insn_code=CL_INSN_UNOP,                           \
                       .code.unop=CL_UNOP_##unop_code, .prop.setops=hnd }
    #define INSN_BIN(spi, binop_code, hnd)                                   \
        [OP_##spi] = { .insn_code=CL_INSN_BINOP,                             \
                       .code.binop=CL_BINOP_##binop_code, .prop.setops=hnd }
    #define INSN_WRN(spi, _, __) \
        [OP_##spi] = { .code.conv = conv_warn, .prop.string="OP_" #spi }
    #define INSN_IGN(spi, _, __) [OP_##spi] = { .code.conv = conv_ignore }
        /*
           how? | sparse insn.    | cl insn. (+uni/bin) | handler             |
          ------+-----------------+---------------------+---------------------|
         */
        INSN_WRN( BADOP           ,                     ,                     ),
        /* Entry */
        INSN_IGN( ENTRY           ,                     ,                     ),
        /* Terminator */
        INSN_SET( RET             , RET                 , insn_setops_ret     ),
        INSN_EMT( BR              , /*JMP or COND*/     , insn_emit_br        ),
        INSN_EMT( SWITCH          ,                     , insn_emit_switch    ),
        INSN_WRN( INVOKE          ,                     ,                     ),
        INSN_WRN( COMPUTEDGOTO    ,                     ,                     ),
        INSN_WRN( UNWIND          ,                     ,                     ),
        /* Binary */
        INSN_BIN( ADD             , PLUS/*POINTER_PLUS*/, insn_setops_binop   ),
        INSN_BIN( SUB             , MINUS               , insn_setops_binop   ),
        INSN_BIN( MULU            , MULT /*XXX: unsig.*/, insn_setops_binop   ),
        INSN_BIN( MULS            , MULT                , insn_setops_binop   ),
        INSN_BIN( DIVU            , TRUNC_DIV /*unsig.*/, insn_setops_binop   ),
        INSN_BIN( DIVS            , TRUNC_DIV           , insn_setops_binop   ),
        INSN_BIN( MODU            , TRUNC_MOD /*unsig.*/, insn_setops_binop   ),
        INSN_BIN( MODS            , TRUNC_MOD           , insn_setops_binop   ),
        INSN_BIN( SHL             , LSHIFT              , insn_setops_binop   ),
            // OP_ASR (arithmetic shift) is the same as OP_LSR (logical shift)
            // except for that the highest bit is kept the same, not zeroed;
            // - C standard says that right shift perfomed on unsigned type is
            //   of the LSR type, implementation specific (LSR/ASR) otherwise
            //   [C text book by P. Herout, TODO: check real standard]
            // - for sparse, right shift performed on signed operand is
            //   translated into OP_ASR (OP_LSR otherwise as expected XXX:vrfy)
        INSN_BIN( LSR             , RSHIFT              , insn_setops_binop   ),
        INSN_BIN( ASR             , RSHIFT              , insn_setops_binop   ),
        /* Logical */
        INSN_BIN( AND             , BIT_AND             , insn_setops_binop   ),
        INSN_BIN( OR              , BIT_IOR             , insn_setops_binop   ),
        INSN_BIN( XOR             , BIT_XOR             , insn_setops_binop   ),
        INSN_BIN( AND_BOOL        , TRUTH_AND           , insn_setops_binop   ),
        INSN_BIN( OR_BOOL         , TRUTH_OR            , insn_setops_binop   ),
        /* Binary comparison */
        INSN_BIN( SET_EQ          , EQ                  , insn_setops_binop   ),
        INSN_BIN( SET_NE          , NE                  , insn_setops_binop   ),
        INSN_BIN( SET_LE          , LE                  , insn_setops_binop   ),
        INSN_BIN( SET_GE          , GE                  , insn_setops_binop   ),
        INSN_BIN( SET_LT          , LT                  , insn_setops_binop   ),
        INSN_BIN( SET_GT          , GT                  , insn_setops_binop   ),
        INSN_BIN( SET_B           , LT /*XXX: unsigned*/, insn_setops_binop   ),
        INSN_BIN( SET_A           , GT /*XXX: unsigned*/, insn_setops_binop   ),
        INSN_BIN( SET_BE          , LE /*XXX: unsigned*/, insn_setops_binop   ),
        INSN_BIN( SET_AE          , GE /*XXX: unsigned*/, insn_setops_binop   ),
        /* Uni */
        INSN_UNI( NOT             , BIT_NOT             , insn_setops_unop    ),
        INSN_UNI( NEG             , TRUTH_NOT/*u.minus*/, insn_setops_unop    ),
        /* Select - three input values */
        INSN_EMT( SEL             , /*COND*/            , insn_emit_sel       ),
        /* Memory */
        INSN_WRN( MALLOC          ,                     ,                     ),
        INSN_WRN( FREE            ,                     ,                     ),
        INSN_WRN( ALLOCA          ,                     ,                     ),
        INSN_UNI( LOAD            , ASSIGN              , insn_setops_load    ),
        INSN_UNI( STORE           , ASSIGN              , insn_setops_store   ),
        INSN_UNI( SETVAL          , ASSIGN              , insn_setops_setval  ),
        INSN_WRN( SYMADDR         ,                     ,                     ),
        INSN_WRN( GET_ELEMENT_PTR ,                     ,                     ),
        /* Other */
            // FIXME: this might be a SPARSE bug if DO_PER_EP_UNSAA is set
            //        and OP_PHI or OP_PHISOURCE occurs (really encountered)
        INSN_WRN( PHI             ,                     ,                     ),
        INSN_WRN( PHISOURCE       ,                     ,                     ),
        INSN_UNI( CAST            , ASSIGN              , insn_setops_cast    ),
        INSN_UNI( SCAST           , ASSIGN              , insn_setops_cast    ),
        INSN_IGN( FPCAST          , ASSIGN /*not sure*/ , insn_setops_copy    ),
        INSN_UNI( PTRCAST         , ASSIGN              , insn_setops_ptrcast ),
        INSN_EMT( INLINED_CALL    ,                     , insn_emit_call      ),
        INSN_EMT( CALL            ,                     , insn_emit_call      ),
        INSN_WRN( VANEXT          ,                     ,                     ),
        INSN_WRN( VAARG           ,                     ,                     ),
        INSN_WRN( SLICE           ,                     ,                     ),
        INSN_WRN( SNOP            ,                     ,                     ),
        INSN_WRN( LNOP            ,                     ,                     ),
        INSN_WRN( NOP             ,                     ,                     ),
        INSN_IGN( DEATHNOTE       ,                     ,                     ),
        INSN_WRN( ASM             ,                     ,                     ),
        /* Sparse tagging (line numbers, context, whatever) */
        INSN_IGN( CONTEXT         ,                     ,                     ),
        INSN_IGN( RANGE           ,                     ,                     ),
        /* Needed to translate SSA back to normal form */
        INSN_UNI( COPY            , ASSIGN              , insn_setops_copy    ),
    #undef INSN_IGN
    #undef INSN_BIN
    #undef INSN_UNI
    #undef INSN_EMT
    #undef INSN_SET
    };
    assert(ORDERED( OP_BADOP , insn->opcode , OP_COPY ));

    struct cl_insn cli;
    const struct insn_conversion *conversion;

    if (!insn->bb)
        return ret_negative;  /* zombie instruction */

    DEBUG_INSN_SP(insn);

    conversion = &insn_conversions[insn->opcode];
    cli.code = conversion->insn_code;
    conv_position(&cli.loc, &insn->pos);

    enum conv_type conv = conv_setops;
    if (CL_INSN_UNOP == conversion->insn_code)
        cli.data.insn_unop.code = conversion->code.unop;
    else if (CL_INSN_BINOP == conversion->insn_code)
        cli.data.insn_binop.code = conversion->code.binop;
    else
        conv = conversion->code.conv;

    switch (conv) {
        case conv_ignore:
            return ret_negative;
        case conv_setops:
            if (conversion->prop.setops(&cli, insn)) {
                DEBUG_INSN_CL(cli);
                API_EMIT(insn, &cli);
            }
            return ret_positive;
        case conv_emit:
            conversion->prop.emit(&cli, insn);
            return CL_INSN_ABORT == cli.code ? ret_escape : ret_positive;
        case conv_warn:
        default:
            WARN_UNHANDLED(insn->pos, conversion->prop.string);
            return ret_negative;
    }
}

/**
    Take care of function payload (arguments and definition -- linearized code)

    @returns Fail to force exit (interactive quit), true otherwise
 */
static bool
emit_function_payload(struct entrypoint *ep, int *emit_props)
{
    int argc = 0;
    struct symbol *arg;
    struct cl_insn cli;
    struct basic_block *bb;
    struct instruction *insn;

    /* function arguments */
    FOR_EACH_PTR(SYM_FNC_ARGS(ep->name), arg)
        API_EMIT(fnc_arg_decl, ++argc, op_from_symbol(arg));
    END_FOR_EACH_PTR(arg);

    /* jump to entry BB */
    conv_position(&cli.loc, &ep->entry->pos);
    insn_emit_jmp(&cli, PTR_STRING(ep->entry->bb));

    /*
        function definition (with "ep -> bb -> instruction" progress)
     */

#if DO_PER_EP_SET_UP_STORAGE
    /* storage pass: we don't need storage analysis (seems incomplete anyway) */
    WITH_PASS(storage, ep)
#endif
        /* unSSA pass: we currently rely on it */
        WITH_PASS(unssa, ep) {

            FOR_EACH_PTR(ep->bbs, bb) {
                assert(bb /*&& bb->parents && bb->children*/);

                if (ptr_list_empty(bb->insns)) {
                    assert(ep->entry->bb != bb);
                    continue;
                }

                API_EMIT(bb_open, PTR_STRING(bb));

                FOR_EACH_PTR(bb->insns, insn)
                    switch (consider_instruction(insn)) {
                        case ret_positive:
                            if (*emit_props & emit_file_interactive
                              && !interact(emit_props))
                                return false;
                            break;
                        case ret_escape:
                            goto terminated;  /* cancel BB (noreturn call) */
                        case ret_negative:
                            break;
                    }
                END_FOR_EACH_PTR(insn);
terminated:
                (void) 0;  /* fix "label at end of compound statement" error */

            } END_FOR_EACH_PTR(bb);

        }

    return true;
}

/**
    Consider emitting the file and its symbols

    @param[in] file        Respective file (NULL for symbols expansion only)
    @param[in] symlist     Symbols to be considered
    @param[in] emit_props  OR'ed @c emit_props enumerations values
    @param[in] stream_nr   Current stream number (for symbols filtering)
    @return    Zero indicates OK, +-1 non/trapping error
    @note  The caller should clear @c die_if_error flag before subsequently
           calling @c sparse function (which may set it back again, or not)
 */
static int
consider_file(const char *file, struct symbol_list *symlist, int emit_props,
              int stream_nr)
{
    bool is_private;
    struct symbol *sym;
    struct entrypoint *ep;

    /* only if no error detected when parsing the file (or we try harder)
       and provided that there are some symbols to proceed */
    if (!ptr_list_empty(symlist)
      && (!SP(die_if_error) || emit_props & emit_file_try_hard)) {

        if (SP(die_if_error))
            PUT(err,
                _1(s)": sparse-roundtrip: allegedly defective, but trying...",
                file);

        is_private = emit_props & emit_file_private;
        sym = SP(first_ptr_list, (struct ptr_list *) symlist);  /* look ahead */

        WITH_FILE_TO_EMIT(file, sym, is_private)
            FOR_EACH_PTR(symlist, sym) {
#if ASSERT_SYMBOL_BELONGS_TO_CURRENT_FILE
                assert(sym->pos.stream == stream_nr || 0 > stream_nr);
#endif
                SP(expand_symbol, sym);  /* expand constant expressions */
                /* XXX: even if is_private as it can be referenced later (?) */
                SP(linearize_symbol, /*out*/ep, /*in*/sym);

                WITH_DEBUG_LEVEL(symb)
                    debug_sparse_symbol_detailed(sym, 8/INDENT_MULT);

                if (!ep || is_private)
                    continue;
                WITH_FUNCTION_TO_EMIT(op_from_symbol(sym), sym->endpos)
                    if (!emit_function_payload(ep, &emit_props))
                        return -1;  /* interactive quit looks like error (OK) */
            } END_FOR_EACH_PTR(sym);
    }

    /* something may have gone wrong even if it was OK at the beginning */
    if (SP(die_if_error)) {
        if (!(emit_props & emit_files_keep_going)) {
            PUT(err,
                _1(s)": sparse-roundtrip: allegedly defective, time to stop",
                file);
            return -1;
        }
        PUT(err,
            _1(s)": sparse-roundtrip: allegedly defective, continuing though",
            file);
        return 1;
    }

    return 0;
}

enum retval
emit(struct string_list *filelist, struct symbol_list *symlist, int emit_props)
{
    int errors = 0, files = 1 /* initial */, file_props, stream_nr;
    char *file;

    /*
        just for a (strange) case the initial includes defined functions;
        depending on emit_props emit them as well
     */
    file_props = (emit_props & emit_skip_initial)
                    ? emit_file_private
                    : emit_vanilla;
    errors += consider_file(initial_file, symlist, emit_props | file_props, -1);
    if (0 > errors)
        return ret_negative;

    FOR_EACH_PTR_NOTAG(filelist, file) {
        SP(die_if_error) = 0;  /* reset error flag */

        DLOG(misc,
             HIGHLIGHT("current input_stream_nr") ": "_1(d), input_stream_nr);

        stream_nr = SP(input_stream_nr);
        SP(sparse, /*out*/symlist, /*in*/file);

        errors += consider_file(file, symlist, emit_props, stream_nr);
        if (0 > errors)
            return ret_negative;
        ++files;

        /* XXX drop some allocations */

    } END_FOR_EACH_PTR_NOTAG(file);

    if (1 < errors)
        PUT(err,
            _1(s)": sparse-roundtrip: last file proceeded; in aggregation "
            _2(d)"/"_3(d)" files allegedly defective", file, errors, files);

    WITH_DEBUG_LEVEL(allo) {
        PUT(debug, "\t" HIGHLIGHT("sparse allocators:"));
        sparse_alloc_show();
        PUT(debug, "\t" HIGHLIGHT("local allocators:"));
        local_alloc_show();
    }

    return errors ? ret_escape : ret_positive;
}
