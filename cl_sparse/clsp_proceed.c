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


#include "clsp.h"      /* bootstrap all other dependencies */

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <unistd.h>
#include <errno.h>
#include <stdint.h>    /* uintptr_t */
#include <inttypes.h>  /* PRIxPTR */
#include <assert.h>

#define USE_INT3_AS_BRK
#include "trap.h"

#define  GLOBALS(what)  (globals.what)


/* compile options */

// general
#define DO_EXTRA_CHECKS              1
#define USE_EXTENDED_TYPE_CMP        0
#define SHOW_PSEUDO_INSNS            0

// sparse
#define DO_PROCEED_INTERNAL          0
#define DO_EXPAND_SYMBOL             1
#define DO_PER_EP_UNSAA              1
#define DO_PER_EP_SET_UP_STORAGE     1
#define DO_SPARSE_FREE               1
#define FIX_SPARSE_EXTRA_ARG_TO_MEM  1

/* symbolic values */

#define CLSP_SPARSE_INTERNAL_SYMS_FILE  "sparse-internal-symbols"



//
// Empty composite values
//



//
// Warnings, failures handling
//

// TODO: pos
#define WARN_UNHANDLED(pos, what) do { \
    /*warn(pos, "warning: '%s' not handled", what);*/ \
    fprintf(stderr, \
            "%s:%d: note: raised from function '%s' [internal location]\n", \
            __FILE__, __LINE__, __FUNCTION__); \
} while (0)

#define WARN_UNHANDLED_SYM(sym) \
    WARN_UNHANDLED((sym)->pos, show_ident((sym)->ident))

#define WARN_VA(pos, fmt, ...) do {\
    /*warn(pos, "warning: " fmt, __VA_ARGS__);*/ \
    fprintf(stderr, \
            "%s:%d: note: raised from function '%s' [internal location]\n", \
            __FILE__, __LINE__, __FUNCTION__); \
} while (0)

#define WARN_CASE_UNHANDLED(pos, what) \
    case what: WARN_UNHANDLED(pos, #what); break;



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


static void
sparse_location(struct cl_loc *cl_loc, struct position pos)
{
    API_SPARSE(stream_name, /*out*/ cl_loc->file, /*int*/ pos.stream);
    cl_loc->line   = pos.line;
    cl_loc->column = pos.pos;
    cl_loc->sysp   = /* not used by SPARSE */ false;
}

static void
sparse_scope(enum cl_scope_e *cl_scope, struct scope *scope)
{
    if (!scope || scope == global_scope)
        *cl_scope = CL_SCOPE_GLOBAL;
    else if (scope == file_scope)
        *cl_scope = CL_SCOPE_STATIC;
    else if (scope == function_scope)
        CL_TRAP;
    else if (scope == block_scope)
        CL_TRAP;
    else
        // FIXME
        *cl_scope = CL_SCOPE_FUNCTION;
}

static inline const char *
sparse_string(const struct string *str)
{/* Alternative:
  * show_string (sparse/token.h)
  *     - cons: character escaping, is debug about empty string
  */
    return (str->length) ? strndup(str->data, str->length) : NULL;
}

static inline const char *
sparse_ident(const struct ident *ident)
{/* Alternative:
  * show_ident (sparse/token.h)
  *     - cons: is debug about empty identifier string
  */
    return (ident && ident->len) ? strndup(ident->name, ident->len) : NULL;
}

static struct symbol *
sparse_fn_arg_at(struct symbol *fn, int pos)
{
    struct symbol *sym, *retval = NULL;

    if (pos <= 0)
        return NULL;

    // FIXME: lot of possible but missing checks
    // alternative: use also symbol->arg_count on SYM_FN
    FOR_EACH_PTR(fn->ctype.base_type->arguments, sym) {
        if (!--pos)
            retval = sym;
    } END_FOR_EACH_PTR(sym);
    return retval;
}


//
// Types handling
//




/* various helpers */

static inline const struct symbol *
type_unwrap(const struct symbol *raw_type)
{/* See also:
  * sparse/symbol.h: get_sym_type()
  */
    if (!raw_type)
        CL_TRAP;

    const struct symbol *retval = raw_type;
    while (retval->type == SYM_NODE || retval->type == SYM_BITFIELD
           /*retval->type == SYM_ENUM */)
        retval = retval->ctype.base_type;

    /* important, otherwise some info may be missing */
    examine_symbol_type(retval);

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

static struct cl_type *type_from_symbol(const struct symbol *type,
                                          struct ptr_db_item **ptr);

static struct cl_type_item *
read_and_append_subtype(struct cl_type *clt, struct symbol *subtype)
{
    struct cl_type_item *subtype_item = type_append_item(clt);
    subtype_item->type = type_from_symbol(subtype, NULL);
    subtype_item->name = sparse_ident(subtype->ident);

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
    read_and_append_subtype(clt, type->ctype.base_type);
    read_and_append_subtypes(clt, type->arguments);
    // XXX: probably convention in cl?
    read_and_append_subtype(clt, &void_ctype);
}

static inline void
read_type_array(struct cl_type *clt, const struct symbol *raw_symbol,
                const struct symbol *type)
{
    int sub_size;

    //CL_TRAP;
    //clt->name = sparse_ident(type->ident);

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
    clt->name = sparse_ident(type->ident);
    read_and_append_subtypes(clt, type->symbol_list);
}

static inline void
read_type_union(struct cl_type *clt, const struct symbol *raw_symbol,
                const struct symbol *type)
{
    //CL_TRAP;
    clt->name     = sparse_ident(type->ident);
    //TODO:
    read_and_append_subtypes(clt, type->symbol_list);
    //clt->item_cnt = /* TODO */ 0;
    //clt->items    = /* TODO */ NULL;
}

static inline void
read_type_enum(struct cl_type *clt, const struct symbol *raw_symbol,
               const struct symbol *type)
{
    clt->name = sparse_ident(type->ident);
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

    WITH_DEBUG_LEVEL(d_type) {
        PUT(debug,"\t%d: type to be processed:", type->pos.line);
        API_SPARSE(show_symbol, (struct symbol *) type);
    }

    //assert(PARTIALLY_ORDERED( SYM_UNINITIALIZED , symbol->type , SYM_BAD ));
    conversion = &type_conversions[type->type];

    // TODO: raw symbol?
    sparse_location(&clt->loc, type->pos);
    sparse_scope(&clt->scope, type->scope);

    clt->code = conversion->type_code;
    // TODO: raw_symbol?
    clt->size = sizeof_from_bits(type->bit_size);

    switch (conversion->type_code) {
        case CL_TYPE_UNKNOWN:
            CL_TRAP;
            WARN_UNHANDLED(type->pos, conversion->prop.string);
            clt->name = strdup(API_SPARSE(show_typename, (struct symbol *)type));
            return clt;
        default:
            break;
    }

    if (conversion->prop.converter)
        conversion->prop.converter(clt, raw_symbol, type);

    return clt;
}


/* pointer and array DB */



struct cl_type **
prepare_type_array_ptr(const struct symbol *raw_symbol,
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
            // 2x guaranteed to continue only in case of success
            MEM_ARR_APPEND(prev->arr, prev->arr_cnt);
            MEM_NEW(prev->arr[i]);
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
type_from_symbol(const struct symbol *raw_symbol, struct ptr_db_item **ptr)
{
    struct cl_type *clt, **clt_ptr;
    const struct symbol *type = type_unwrap(raw_symbol);

    // Fastest path, we have the type already in hash table
    clt = type_ptr_db_lookup_item(&type_ptr_db, type, ptr);
    if (clt)
        return clt;

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

    clt = type_ptr_db_insert(&type_ptr_db, *clt_ptr, type, ptr);

    if (!is_new)
        return clt;  // existing pointer/array alias

    // Slow path for anything (except for pointers) which is being
    // proceeded for the first time (next time, hashed ctl is used instead)
    //
    // Important: these types are read ex-post in order to prevent recursion
    //            with, e.g., structures
    return read_type(clt, raw_symbol, type);
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

        if (PARTIALLY_ORDERED(OP_BINCMP, insn->opcode, OP_BINCMP_END))
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


//
// operands handling
//

#define CST(op)      (&op->data.cst)
#define CST_INT(op)  (&CST(op)->data.cst_int)
#define CST_STR(op)  (&CST(op)->data.cst_string)
#define CST_FNC(op)  (&CST(op)->data.cst_fnc)
#define CST_REAL(op) (&CST(op)->data.cst_real)

#define VAR(op)      (op->data.var)

/* Sparse operands = pseudos */

static inline bool
pseudo_futile(pseudo_t pseudo)
{
    return !pseudo || pseudo == VOID;
}

static inline bool
pseudo_immediate(pseudo_t pseudo)
{
    return pseudo->type != PSEUDO_SYM && pseudo->type != PSEUDO_ARG;
}


/* operand "constructor" */

static inline struct cl_operand *
new_op(void)
{
    struct cl_operand *retval;
    // guaranteed not to return NULL
    return MEM_NEW(retval);
}

static inline struct cl_operand *
op_shallow_copy(const struct cl_operand *op_src)
{
    struct cl_operand *retval = new_op();
    *retval = *op_src;

    // guaranteed not to return NULL
    return retval;
}


/* freeing resources connected with operand */

static inline void free_op(struct cl_operand *op);

static void
op_free_initializers(struct cl_initializer *initial)
{
    /* initial (heap!) */
    free(initial);

    /* XXX: plain iteration */
    if (initial->next)
        op_free_initializers(initial->next);
}

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

// Note: for freeing heap-based nested items only (see also `free_op')
static void
op_free_data(struct cl_operand *op)
{
    if (op->code == CL_OPERAND_VOID)
        return;

    /* op->type (skipped) */

    /* op->accessor */
    free_accessor_chain(op->accessor);

    if (op->code == CL_OPERAND_CST) {
        /* op->data.cst... */
        switch (op->data.cst.code) {
            case CL_TYPE_FNC:
                free((char *) CST_FNC(op)->name);
                break;
            case CL_TYPE_STRING:
                free((char *) CST_STR(op)->value);
                break;
            default:
                break;
        }
    } else if (op->code == CL_OPERAND_VAR) {
        /* op->data.var->name */
        free((char *) VAR(op)->name);
        /* op->data.var->initial... */
        if (VAR(op)->initial)
            op_free_initializers(VAR(op)->initial);

        /* op->data.var */
        free(VAR(op));
    }
}

// Note: *op expected to be heap-based (rare!)
static inline void
free_op(struct cl_operand *op)
{
    op_free_data(op);

    /* op (heap!) */
    free(op);
}


/* operator modifiers depending on the usage */

// Note: this is not easily extendable as everything else is uninitialized
static inline struct cl_operand *
op_make_void(struct cl_operand* op)
{
    op->code = CL_OPERAND_VOID;
    return op;
}

// Note: not to be used directly
static inline struct cl_operand *
op_make_cst(struct cl_operand *op)
{
    op->code     = CL_OPERAND_CST;
    op->accessor = NULL;

    return op;
}

static inline struct cl_operand *
op_make_cst_fnc(struct cl_operand *op, const struct symbol *sym)
{
    op_make_cst(op);

    op->type               = type_from_symbol(sym, NULL);
    CST(op)->code          = CL_TYPE_FNC;
    CST_FNC(op)->name      = sparse_ident(sym->ident);
    CST_FNC(op)->is_extern = MOD_EXTERN & sym->ctype.modifiers;
    CST_FNC(op)->uid       = (int)(long) sym;

    return op;
}

static inline struct cl_operand *
op_make_cst_int(struct cl_operand *op, int value)
{
    op_make_cst(op);

    op->type           = &int_clt;
    CST(op)->code      = CL_TYPE_INT;
    CST_INT(op)->value = value;

    return op;
}

static inline struct cl_operand *
op_make_cst_real(struct cl_operand *op, double value)
{
    op_make_cst(op);

    op->type            = &double_clt;
    CST(op)->code       = CL_TYPE_REAL;
    CST_REAL(op)->value = value;

    return op;
}

// TODO: make it accepting const char *
static inline struct cl_operand *
op_make_cst_string(struct cl_operand *op, struct expression *expr)
{
    op_make_cst(op);

    op->type           = type_from_symbol(expr->ctype, NULL); //XXX
    CST(op)->code      = CL_TYPE_STRING;
    CST_STR(op)->value = sparse_string(expr->string);

    return op;
}

// Note: type not (re)set; different semantics from `op_make_cst_*'
static inline struct cl_var *
op_make_var(struct cl_operand *op)
{
    op->code     = CL_OPERAND_VAR;
    op->accessor = NULL;

    MEM_NEW(VAR(op));  // guaranteed to continue only in case of success

    // initialize pointers checked by freeing helper
    VAR(op)->name       = NULL;
    VAR(op)->initial    = NULL;
    VAR(op)->artificial = true;

    // guaranteed not to return NULL
    return VAR(op);
}

static struct cl_operand *
op_use_initializer(struct cl_operand *op, struct expression *expr)
{
    if (!expr) {
        CL_TRAP;
        return op;
    }

    //CL_TRAP;
    switch (expr->type) {
        case EXPR_STRING:
            return op_make_cst_string(op, expr);
        default:
            CL_TRAP;
            return op;
    }
}

static struct cl_operand *
op_from_symbol_base(struct cl_operand *op, struct symbol *sym)
{
    sparse_scope(&op->scope, sym->scope);

    if (sym->bb_target || sym->type != SYM_NODE)
        CL_TRAP;

    // function not treated as a variable
    if (sym->ctype.base_type->type == SYM_FN)
        return op_make_cst_fnc(op, sym);

    // string literal
    if (!sym->ident)
        return op_use_initializer(op, sym->initializer);

    op->type = type_from_symbol(sym, NULL);

    struct cl_var *var = op_make_var(op);
    var->uid        = (int)(long) sym;
    var->name       = sparse_ident(sym->ident);
    var->artificial = false;
#if DO_EXTRA_CHECKS
    assert(var->name);
#endif

    return op;
}

static inline struct cl_operand *
op_from_symbol(struct cl_operand *op, struct symbol *sym)
{
    // !!TODO: simplify/API change
    return op_from_symbol_base(op, sym);
}

static inline struct cl_operand *
op_from_fn_argument(struct cl_operand *op, const pseudo_t pseudo)
{
    struct symbol *arg_sym;

    arg_sym = sparse_fn_arg_at(pseudo->def->bb->ep->name, pseudo->nr);
    if (!arg_sym)
        CL_TRAP;

    // XXX: op->scope       = CL_SCOPE_FUNCTION;
    // !!TODO: simplify/API change
    return op_from_symbol_base(op, arg_sym);
}

static struct cl_operand *
op_from_register(struct cl_operand *op, const struct instruction *insn,
                 const pseudo_t pseudo)
{/* Synopsis:
  * pseudo->def
  *
  */
    op->type = type_from_instruction(pseudo->def, pseudo);

    struct cl_var *var = op_make_var(op);
#if 1
    var->uid  = (int)(long) pseudo;
#else
    var->uid  = pseudo->nr;
#endif

    return op;
}

static inline struct cl_operand *
op_from_value(struct cl_operand *op, const struct instruction *insn, int value)
{
    // !!TODO: simplify/API change
    return op_make_cst_int(op, value);
}

static inline struct cl_operand *
op_from_pseudo(struct cl_operand *op, const struct instruction *insn,
               const pseudo_t pseudo)
{/* Synopsis:
  * sparse/linearize.h
  *
  * Problems/exceptions/notes:
  * 1. PSEUDO_VAL and PSEUDO_REG operands are not holding type information
  * S. Try to use insn->type, pseudo->def->type for PSEUDO_REG, ...
  */
    if (pseudo_futile(pseudo))
        return op_make_void(op);

    switch (pseudo->type) {

        /* real variables/literals (everything important accessible [?]) */

        case PSEUDO_SYM: return op_from_symbol(op, pseudo->sym);
        case PSEUDO_ARG: return op_from_fn_argument(op, pseudo);

        /* immediate values (some information may be hard/impossible to get) */

        case PSEUDO_REG: return op_from_register(op, insn, pseudo);
        case PSEUDO_VAL: return op_from_value(op, insn, /*XXX: from long long */
                                                        (int) pseudo->value);
#if 0
        case PSEUDO_PHI:
            WARN_UNHANDLED(insn->pos, "PSEUDO_PHI");
            break;
#endif
        default:
            // PSEUDO_PHI
            CL_TRAP;
            return op;
    }
}

static inline struct cl_operand *
op_from_expression(struct cl_operand *op, const struct instruction *insn,
                   const struct expression *expr)
{/* Synopsis:
  * sparse/linearize.c: show_instruction: case OP_SETVAL
  * sparse/show-parse.c: show_expression
  *
  * Problems/exceptions/notes:
  * FIXME: currently only EXPR_FVALUE handled
  */
    // !!TODO: simplify/API change
    switch (expr->type) {
        case EXPR_FVALUE:
            return op_make_cst_real(op, /*XXX: from long double */
                                        (double) expr->fvalue);
        default:
            CL_TRAP;
    }
    return op_make_void(op);
}

static struct cl_accessor *
new_cl_accessor()
{
    struct cl_accessor *retval;
    // guaranteed not to return NULL
    MEM_NEW(retval)->next = NULL;
    return retval;
}

static inline void
accessor_array_index(struct cl_accessor *ac, int index)
{
    ac->code                  = CL_ACCESSOR_DEREF_ARRAY;
    ac->data.array.index      = op_make_cst_int(new_op(), index);
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
        *retval = new_cl_accessor();

    // guaranteed not to return NULL
    return *retval;
}

static inline struct cl_accessor *
op_prepend_accessor(struct cl_operand *op, struct cl_accessor *ac)
{
    if (!ac)
        ac = new_cl_accessor();

    ac->next = op->accessor;
    op->accessor = ac;

    // guaranteed not to return NULL
    return ac;
}

// Note: returns UINT_MAX when operand could not be dug
static unsigned
op_dig_step(struct cl_operand *op, unsigned insn_offset)
{
    // `insn_offset' is consumed only by CL_TYPE_STRUCT or CL_TYPE_ARRAY;
    // e.g., accessing struct element is different with the first level
    // access (use insn_offset) and with other accesses (always zero offset)
    int retval = insn_offset,
        i = 0;

    struct cl_accessor *ac;

    #define MAP_ACCESSOR(acc, clt, cl_ac) \
        case CL_##clt: acc = new_cl_accessor(); acc->code = CL_##cl_ac;
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

    op_append_accessor(op, ac);
    // accessor's type is the operand's type (it itself will be peeled off)
    ac->type = (struct cl_type *) op->type;
    // peel off one level of type/access decoration from the operand
    op->type = (struct cl_type *) op->type->items[i].type;

    return retval;
}

// XXX: removal candidate
static inline bool
op_accessible(const struct cl_operand *op)
{/* Problems/exceptions/notes:
  * None.
  */
    switch (op->type->code) {
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
op_dig_for_type_match(struct cl_operand *op,
                      const struct cl_type *expected_type,
                      unsigned initial_offset)
{/* Problems/exceptions/notes:
  * When digging union, we go through its items, apply a DFS-based search
  * in order to get expected type on one, if it ends without success, we try
  * another (on the whole, should not end without success).
  */

    unsigned offset = initial_offset;

    while (!type_match(op->type, expected_type)) {
        if (op->type->code == CL_TYPE_UNION) {
            // unions bring non-determinism as there are more ways how to
            // "dig" -- use DFS with a sort of backtracking (through stack)
            struct cl_operand *op_clone;
            struct cl_accessor *ac;
            int i;
            size_t res;

            // `op_clone' is a special shallow copy with accessor chain reset
            op_clone = op_shallow_copy(op);

            for (i = 0; i < op->type->item_cnt; i++) {
                op_clone->accessor = NULL;
                ac               = op_append_accessor(op_clone, NULL);
                ac->code         = CL_ACCESSOR_ITEM;
                ac->type         = op_clone->type;
                ac->data.item.id = i;
                op_clone->type = (struct cl_type *) ac->type->items[i].type;

                res = op_dig_for_type_match(op_clone, expected_type, offset);

                if (UINT_MAX != res)
                    // successfull case of digging
                    break;

                // restore for the next round
                free_accessor_chain(op_clone->accessor);
                op_clone->type = op->type;
            }

            if (UINT_MAX != res) {
                // reflect the changes collected within successful DFS trace
                // (with `op_clone') back to its preimage `op'
                op->type = op_clone->type;
                assert(op_clone->accessor);
                op_append_accessor(op, op_clone->accessor);
                assert(type_match(op->type, expected_type));
            }

            free(op_clone);
            offset = res;
        } else
            offset = op_dig_step(op, offset);

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

static bool insn_assignment_base(struct cl_insn *cli,
                                 const struct instruction *insn,
                                 pseudo_t lhs, pseudo_t rhs,
                                 enum assignment_ops_handling ops_handling);

/* Helpers for frequently emitted instructions (position filled in advance) */

static inline void
emit_insn_jmp(struct cl_insn *cli, const char *label)
{
    cli->code                = CL_INSN_JMP;
    cli->data.insn_jmp.label = label;

    API_EMIT(insn, cli);
}

static inline void
emit_insn_cond(struct cl_insn *cli, struct cl_operand *op_cond,
               const char *then_label, const char *else_label)
{
    cli->code                      = CL_INSN_COND;
    cli->data.insn_cond.src        = op_cond;
    cli->data.insn_cond.then_label = then_label;
    cli->data.insn_cond.else_label = else_label;

    API_EMIT(insn, cli);
}

static inline void
emit_insn_copy(struct cl_insn *cli, const struct instruction *insn,
               pseudo_t lhs, pseudo_t rhs)
{
    cli->code                = CL_INSN_UNOP;
    cli->data.insn_unop.code = CL_UNOP_ASSIGN;
    insn_assignment_base(cli, insn,
        lhs,           /* := */  rhs,
        TYPE_LHS_KEEP      |     TYPE_RHS_KEEP
    );
}

/* Functions dedicated to sparse assignment-like instructions */

static void
insn_assignment_mod_rhs(struct cl_operand *op_rhs, pseudo_t rhs,
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
                if (!type_match(op_rhs->type, expected_type)) {
                    expected_type = expected_type->items->type;
                    // XXX: second condition yields better results
                    //      with tests/struct/rs1-03 but makes
                    //      tests/predator/test-0044.c fail
                    if (!type_match(op_rhs->type, expected_type)
                        /*|| op_accessible(op_rhs)*/)
                        op_dig_step(op_rhs, offset);
                } else
                    use_rhs_dereference = false;
            }
            unsigned res = op_dig_for_type_match(op_rhs, expected_type, offset);
            if (res == UINT_MAX) {
                // no success when digging operand for type match,
                // it may be a pointer and we just haven't been told this
                // type information (e.g., due to typeless PSEUDO_VAL)
                if (op_rhs->type->code == CL_TYPE_INT
                    && expected_type->code == CL_TYPE_PTR) {
                    struct cl_accessor *ac;
                    struct cl_type *expected_type_dug;

                    // promote an operand type to a pointer and remove
                    // a level of pointer indirection also from expected type
                    // XXX: should be the base type switched to void?
                    op_rhs->type = build_referenced_type(op_rhs->type);
                    expected_type_dug = (struct cl_type *)
                                        expected_type->items->type;

                    while (expected_type_dug->code == CL_TYPE_PTR) {
                        // now, we do the same but explicitly adding
                        // dereferences, adjusting the level of dereferences
                        // in operand's type
                        ac = op_prepend_accessor(op_rhs, NULL);
                        ac->code = CL_ACCESSOR_DEREF;
                        ac->type = op_rhs->type;

                        op_rhs->type = build_referenced_type(op_rhs->type);
                        expected_type_dug = (struct cl_type *)
                                            expected_type_dug->items->type;
                    }
                } else
                    CL_TRAP;  // should not happen
            }

        } else if (ops_handling & TYPE_RHS_REFERENCE) {
            // OP_STORE with PSEUDO_VAL rhs (e.g., value can be pointer)
            if (rhs->type == PSEUDO_VAL)
                op_rhs->type = type;  // probably no other choice
#if DO_EXTRA_CHECKS
            else if (!type_match(op_rhs->type, type))
                CL_TRAP;  // should be the same type
#endif
            use_rhs_dereference = false;
        }
    }

    // reference rhs (when applicable)
    if (ops_handling & TYPE_RHS_REFERENCE && use_rhs_dereference) {
        // OP_PTRCAST, OP_STORE (for PSEUDO_SYM and PSEUDO_ARG only
        //                       and only when returning level of indirection)
        struct cl_accessor *ac = op_append_accessor(op_rhs, NULL);
        ac->code = CL_ACCESSOR_REF;
        ac->type = op_rhs->type;
        op_rhs->type = build_referenced_type(op_rhs->type);
#if DO_EXTRA_CHECKS
        if (!type_match(op_rhs->type, type))
            CL_TRAP;  // should be the same type
#endif
    }
}

static bool
insn_assignment_base(struct cl_insn *cli, const struct instruction *insn,
                     pseudo_t lhs,    /* := */    pseudo_t rhs,
                     enum assignment_ops_handling ops_handling)
{/* Synopsis (see also the callers):
  * [input] OP_LOAD, OP_STORE, OP_COPY + casts (CAST, SCAST, FPCAST, PTRCAST)
  *     insn->type (not for OP_COPY):       type of final assigned value
  *     insn->orig_type (OP_PTRCAST only):  original type of value to assign
  * [output] CL_INSN_UNOP (set by [transitive] caller, as with location)
  *     data.insn_unop.code (CL_UNOP_ASSIGN, set by [transitive] caller)
  *     data.insn_unop.dst ~ lhs
  *     data.insn_unop.src ~ rhs
  *
  * Problems/exceptions/notes:
  * 1. Problem with a "right form" of both the operands (whether to consider
  *    the whole struct or its first element, etc.); additionally, some
  *    instructions requires (de)referencing of the operands explicitly.
  * S. The way to obtain "right form" of both operands is driven by
  *    `ops_handling' (see `enum assignment_ops_handling').
  *    If appropriate, combine `op_dig_for_type_match' and `insn->type'
  *    (`insn->orig_type') for this adjustment.
  */
    struct cl_operand op_lhs, op_rhs;

    /* prepare LHS */

    cli->data.insn_unop.dst = op_from_pseudo(&op_lhs, insn, lhs);

    // dig lhs (when applicable)
    if (ops_handling & TYPE_LHS_DIG) {
        struct cl_type *type = type_from_symbol(insn->type, NULL);
        if (!op_accessible(&op_lhs)) {
            struct cl_accessor *ac = op_append_accessor(&op_lhs, NULL);
            ac->code = CL_ACCESSOR_DEREF;
            // note: no such clt easily accessible (contrary to previous case)
            ac->type = build_referenced_type(type);
            op_lhs.type = type;
        } else
            op_dig_for_type_match(&op_lhs, type, insn->offset);
    }

    /* prepare RHS (quite complicated compared to LHS) */

    cli->data.insn_unop.src = op_from_pseudo(&op_rhs, insn, rhs);
    insn_assignment_mod_rhs(&op_rhs, rhs, insn, ops_handling);

    /* emit assignment */

    // FIXME (SPARSE?):  sparse generates (due to execution model?) extra
    // instruction, e.g. "store %arg1 -> 0[num]" in case of "num == %arg1"
#if FIX_SPARSE_EXTRA_ARG_TO_MEM
    if (lhs->type != PSEUDO_SYM || rhs->type != PSEUDO_ARG
         || op_lhs.data.var->uid != op_rhs.data.var->uid)
#endif
        API_EMIT(insn, cli);
#if FIX_SPARSE_EXTRA_ARG_TO_MEM
    else
        WARN_VA(insn->pos, "instruction omitted: %s",
                show_instruction((struct instruction *) insn));
#endif

    op_free_data(&op_lhs);
    op_free_data(&op_rhs);

    return true;
}

static inline bool
handle_insn_store(struct cl_insn *cli, const struct instruction *insn)
{/* Synopsis (see also `insn_assignment_base'):
  * [input] OP_STORE
  *     insn->src:    target memory address (pointer to what is being assigned)
  *     insn->target: source of assignment
  *     insn->type:   type of value to be assigned
  *
  * Problems/exceptions/notes:
  * None.
  */
    //CL_TRAP;
    return insn_assignment_base(cli, insn,
        insn->src,     /* := */  insn->target,
        TYPE_LHS_DIG       |     (TYPE_RHS_DIG | TYPE_RHS_REFERENCE)
    );
}

static inline bool
handle_insn_load(struct cl_insn *cli, const struct instruction *insn)
{/* Synopsis (see also `insn_assignment_base'):
  * [input] OP_LOAD
  *     insn->target ... register (XXX: only?) to be assigned
  *     insn->src    ... mem. address containing source value
  *     insn->type   ... type of value to be assigned
  *
  * Problems/exceptions/notes:
  * None.
  */
    if (insn->target->type != PSEUDO_REG)
        CL_TRAP;

    return insn_assignment_base(cli, insn,
        insn->target,  /* := */  insn->src,
        TYPE_LHS_KEEP      |     (TYPE_RHS_DIG | TYPE_RHS_DIG_ANY)
    );
}

static inline bool
handle_insn_copy(struct cl_insn *cli, const struct instruction *insn)
{/* Synopsis (see also `insn_assignment_base'):
  * [input] OP_COPY
  *     insn->target
  *     insn->src
  *     insn->type
  *
  * Problems/exceptions/notes:
  * FIXME: are cast operations OK?
  */
    return insn_assignment_base(cli, insn,
        insn->target,  /* := */  insn->src,
        TYPE_LHS_KEEP      |     TYPE_RHS_KEEP
    );
}

static inline bool
handle_insn_cast(struct cl_insn *cli, const struct instruction *insn)
{/* Synopsis (see also `insn_assignment_base'):
  * [input] OP_CAST, OP_SCAST
  *     insn->target
  *     insn->src
  *     insn->type
  *     insn->orig_type
  *
  * Problems/exceptions/notes:
  * May end up with with emitting CL_BINOP_BIT_AND when casting "smaller"
  * type to "bigger" type (currently, for bitfields only) to be sure we get
  * rid of unwanted garbage (e.g., data from the next bitfield item).
  */
    if (insn->orig_type->bit_size == insn->type->bit_size)
        handle_insn_copy(cli, insn);
    else if (insn->orig_type->bit_size < insn->type->bit_size
             && insn->orig_type->ctype.base_type->type == SYM_BITFIELD) {
        // we have to apply CL_BINOP_BIT_AND on `insn->src' using mask
        // (currently of int size XXX?) with additional higher bits zeroed

        int mask = ~((~0) << insn->orig_type->bit_size);
        struct cl_operand dst, lhs, op_mask;

        cli->code = CL_INSN_BINOP;
        cli->data.insn_binop.code = CL_BINOP_BIT_AND;
        cli->data.insn_binop.dst  = op_from_pseudo(&dst, insn, insn->target);
        cli->data.insn_binop.src1 = op_from_pseudo(&lhs, insn, insn->src);
        cli->data.insn_binop.src2 = op_from_value(&op_mask, insn, mask);

        API_EMIT(insn, cli);

        op_free_data(&dst);
        op_free_data(&lhs);
        op_free_data(&op_mask);

        return true;
    }

    CL_TRAP;  // will this ever happen?
    return true;
}

static inline bool
handle_insn_ptrcast(struct cl_insn *cli, const struct instruction *insn)
{/* Synopsis (see also `insn_assignment_base'):
  * [input] OP_PTRCAST
  *     insn->target
  *     insn->src
  *     insn->type
  *     insn->orig_type
  *
  * Problems/exceptions/notes:
  * None.
  */
    return insn_assignment_base(cli, insn,
        insn->target,  /* := */  insn->src,
        TYPE_LHS_KEEP      |     (TYPE_RHS_DIG | TYPE_RHS_REFERENCE)
    );
}

static inline bool
handle_insn_setval(struct cl_insn *cli, const struct instruction *insn)
{/* Synopsis:
  * [input] OP_SETVAL
  *     insn->target: destination
  *     insn->val:    value to be assigned in the form of an expression
  *         EXPR_FVALUE ~ constant of CL_TYPE_REAL type
  *         (other types of expressions not checked yet)
  * [output] CL_INSN_UNOP (set by caller, as with location)
  *     data.insn_unop.dst ~ insn->target
  *     data.insn_unop.src ~ insn->val (see above)
  *
  * Problems/exceptions/notes:
  * See `op_from_expression'.
  */
    struct cl_operand dst, src;

    cli->data.insn_unop.dst = op_from_pseudo(&dst, insn, insn->target);
    cli->data.insn_unop.src = op_from_expression(&src, insn, insn->val);

    API_EMIT(insn, cli);

    op_free_data(&dst);
    op_free_data(&src);

    return true;
}

/* Functions dedicated to other sparse instructions */

static bool
handle_insn_unop(struct cl_insn *cli, const struct instruction *insn)
{/* Synopsis:
  * [input] OP_NOT, OP_NEG (assignments handled separately)
  *     insn->target: destination
  *     insn->src1:   source
  * [output] CL_INSN_UNOP (set by caller, as with location)
  *     data.insn_unop.code (set by caller, rewrite in case of unary minus)
  *     data.insn_unop.dst ~ insn->target
  *     data.insn_unop.src ~ insn->src1
  *
  * Problems/exceptions/notes:
  * 1. OP_NEG means "unary minus" when applied on int.
  */
    struct cl_operand dst, src;

    cli->data.insn_unop.dst = op_from_pseudo(&dst, insn, insn->target);
    cli->data.insn_unop.src = op_from_pseudo(&src, insn, insn->src1);

    // for "unary minus", rewrite unary operation
    if (src.type->code == CL_TYPE_INT && insn->opcode == OP_NEG)
        cli->data.insn_unop.code = CL_UNOP_MINUS;

    API_EMIT(insn, cli);

    op_free_data(&dst);
    op_free_data(&src);

    return true;
}

static bool
handle_insn_binop(struct cl_insn *cli, const struct instruction *insn)
{/* Synopsis:
  * [input] (any arithmetic, shift, logical and explicit comparison operation)
  *     insn->target:   destination
  *     insn->src(1|2): operands
  * [output] CL_INSN_BINOP (set by caller, as with location)
  *     data.insn_binop.code (set by caller, rewrite for pointer arithmetics)
  *     data.insn_binop.dst      ~ insn->target
  *     data.insn_binop.src(1|2) ~ insn->src(1|2)
  *
  * Problems/exceptions/notes:
  * 1. Binary arithmetics case has to be detected and imposed explicitly.
  * S. If any of the operand is a pointer or an array, promote CL_BINOP_PLUS
  *    to CL_BINOP_POINTER_PLUS (other operations not expected in this case).
  */
    //CL_TRAP;
    struct cl_operand dst, op1, op2;

    cli->data.insn_binop.dst  = op_from_pseudo(&dst, insn, insn->target);
    cli->data.insn_binop.src1 = op_from_pseudo(&op1, insn, insn->src1);
    cli->data.insn_binop.src2 = op_from_pseudo(&op2, insn, insn->src2);

    // for pointer arithmetics, rewrite binary operation
    if (op1.type->code == CL_TYPE_PTR || op1.type->code == CL_TYPE_ARRAY
        || op2.type->code == CL_TYPE_PTR || op2.type->code == CL_TYPE_ARRAY) {
        switch (cli->data.insn_binop.code) {
            case CL_BINOP_PLUS:
                cli->data.insn_binop.code = CL_BINOP_POINTER_PLUS;
                break;
            default:
                // only addition is supported (XXX: may other ops occur?)
                CL_TRAP;
        }
    }

    API_EMIT(insn, cli);

    op_free_data(&dst);
    op_free_data(&op1);
    op_free_data(&op2);

    return true;
}

static bool
handle_insn_call(struct cl_insn *cli, const struct instruction *insn)
{/* Synopsis:
  * [input] OP_CALL
  *     insn->target
  *     insn->func
  *     insn->arguments
  * [output] regex: insn_call_open (insn_call_arg)* insn_call_close
  *          (location set by caller)
  *
  * Problems/exceptions/notes:
  * 1. Function can be non-returning.
  * S. Emit CL_INSN_ABORT in such case.
  */
    struct cl_operand dst, fnc, arg_op;
    struct pseudo *arg;
    int cnt = 0;

    WITH_CALL_TO_EMIT(&cli->loc,
                      op_from_pseudo(&dst, insn, insn->target),
                      op_from_pseudo(&fnc, insn, insn->func)) {
        FOR_EACH_PTR(insn->arguments, arg) {
            // XXX: ++cnt repeated side-effect?
            API_EMIT(insn_call_arg, ++cnt, op_from_pseudo(&arg_op, insn, arg));
            op_free_data(&arg_op);
        } END_FOR_EACH_PTR(arg);
    }

    op_free_data(&dst);
    op_free_data(&fnc);

    // special handling of non-returning function (end of BB)
    if (insn->func->sym->ctype.modifiers & MOD_NORETURN) {
        cli->code = CL_INSN_ABORT;
        API_EMIT(insn, cli);
        return false;
    }

    return true;
}

static bool
handle_insn_br(struct cl_insn *cli, const struct instruction *insn)
{/* Synopsis:
  * [input] OP_BR
  *     insn->cond
  *     insn->bb_true
  *     insn->bb_false
  * [output] regex: CL_INSN_JMP or CL_INSN_COND (unconditional/cond. jump)
  *          (location set by caller)
  *
  * Problems/exceptions/notes:
  * None.
  */
    // unconditional jump
    if (pseudo_futile(insn->cond)) {
        emit_insn_jmp(cli, PTR_STRING(insn->bb_true));
        return true;
    }

    // conditional jump
    struct cl_operand op;

    op_from_pseudo(&op, insn, insn->cond);
    emit_insn_cond(cli, &op,
                   PTR_STRING(insn->bb_true), PTR_STRING(insn->bb_true));

    op_free_data(&op);
    return true;
}

static bool
handle_insn_sel(struct cl_insn *cli, const struct instruction *insn)
{/* Synopsis:
  * [input] OP_SEL
  *     insn->src1
  *     insn->src2
  *     insn->src3
  *     insn->target
  * [output] regex: CL_INSN_COND (bb_open "assign" CL_INSN_JUMP){2} bb_open
  *          (location set by caller)
  *
  * Problems/exceptions/notes:
  * 1. BB label uniqueness.
  * S. Address of `insn' +0, +1 or +2, provided that insn has size of 4+
  *    and char 1 (compile time constraints?).
  */
    struct cl_operand op_cond;

    // local BB labels
    char const*const bb_label_true  = PTR_STRING(((char *) insn) + 0);
    char const*const bb_label_false = PTR_STRING(((char *) insn) + 1);
    char const*const bb_label_merge = PTR_STRING(((char *) insn) + 2);

    // cond instruction
    op_from_pseudo(&op_cond, insn, insn->src1);
    emit_insn_cond(cli, &op_cond, bb_label_true, bb_label_false);
    op_free_data(&op_cond);

    // first BB ("then" branch) with assignment and jump to merging BB
    API_EMIT(bb_open, bb_label_true);
    emit_insn_copy(cli, insn, insn->target,  /* := */  insn->src2);
    emit_insn_jmp(cli, bb_label_merge);

    // second BB ("else" branch) with assignment and jump to merging BB
    API_EMIT(bb_open, bb_label_false);
    emit_insn_copy(cli, insn, insn->target,  /* := */  insn->src3);
    emit_insn_jmp(cli, bb_label_merge);

    // merging BB
    API_EMIT(bb_open, bb_label_merge);

    return true;
}

static bool
handle_insn_switch(struct cl_insn *cli, const struct instruction *insn)
{/* Synopsis:
  * [input] OP_SWITCH
  *     insn->target:        selection source
  *     insn->multijmp_list: list of branches/cases
  *         jmp->target:               respective basic block
  *         ---
  *         jmp->begin == jmp->end ... single value selection
  *         jmp->begin < jmp->end  ... range selection
  *         jmp->begin > jmp->end  ... default case
  *
  * [output] regex: insn_switch_open (insn_switch_case)* insn_switch_close
  *          (location set by caller)
  *
  * Problems/exceptions/notes:
  * FIXME: not enough accurate location info from SPARSE for switch/case.
  */
    struct cl_operand op, val_lo, val_hi, *val_hi_ptr = &val_lo;
    struct multijmp *jmp;

    WITH_SWITCH_TO_EMIT(&cli->loc, op_from_pseudo(&op, insn, insn->target)) {
        // emit cases
        op_make_void(&val_lo);
        op_make_void(&val_hi);

        FOR_EACH_PTR(insn->multijmp_list, jmp) {
            if (jmp->begin <= jmp->end) {
                // non-default
                op_from_value(&val_lo, insn, jmp->begin)->type = op.type;

                if (jmp->begin != jmp->end) {
                    // range
                    op_from_value(&val_hi, insn, jmp->end)->type = op.type;
                    val_hi_ptr = &val_hi;
                }
            } else
                // default case
                op_make_void(&val_lo);

            API_EMIT(insn_switch_case, &cli->loc, &val_lo, val_hi_ptr,
                     PTR_STRING(jmp->target));

            // not necessary now, but ...
            op_free_data(&val_lo);
            op_free_data(&val_hi);
        } END_FOR_EACH_PTR(jmp);
    }

    op_free_data(&op);
    return true;
}

static bool
handle_insn_ret(struct cl_insn *cli, const struct instruction *insn)
{/* Synopsis:
  * [input] OP_RET
  *     insn->src:  value to be used as a return value
  *     insn->type: type of return value
  * [output] CL_INSN_RET (set by caller, as with location)
  *     cl_insn.insn_ret.src ~ insn->src
  *
  * Problems:
  * 1. Problem with a "right form" of the operand (whether to consider
  *    the whole struct or its first element, etc.).
  * S. Combine `op_dig_for_type_match' and `insn->type' for adjustment.
  */
    struct cl_operand op;
    const struct cl_type *resulting_type;

    cli->data.insn_ret.src = op_from_pseudo(&op, insn, insn->src);
    // TODO: decide according to the pseudo instead?
    if (op_accessible(&op)) {
        resulting_type = type_from_symbol(insn->type, NULL);
        op_dig_for_type_match(&op, resulting_type, insn->offset);
    }

    API_EMIT(insn, cli);

    op_free_data(&op);
    return true;
}

static bool
handle_insn(struct instruction *insn)
{
    typedef bool (*insn_handler)(struct cl_insn *, const struct instruction *);
    const struct insn_conversion {
        enum cl_insn_e       insn_code;
        union {
            enum cl_unop_e   unop;
            enum cl_binop_e  binop;
        } code;
        union {
            insn_handler     handler;
            const char       *string;
        } prop;
    } insn_conversions[] = {
    /* Synopsis:
     * sparse/linearize.h
     *
     * Note:
     * Instructions with more complicated rules (more instructions are emitted
     * per the single original one) are denoted with NOP, unhandled with ABORT.
     */
    #define INSN_STD(spi, cli, hnd) \
        [OP_##spi]={ .insn_code=CL_INSN_##cli, .prop.handler=hnd }
    #define INSN_UNI(spi, unop_code, hnd) \
        [OP_##spi]={ .insn_code=CL_INSN_UNOP, \
                     .code.unop=CL_UNOP_##unop_code, .prop.handler=hnd }
    #define INSN_BIN(spi, binop_code, hnd) \
        [OP_##spi]={ .insn_code=CL_INSN_BINOP, \
                     .code.binop=CL_BINOP_##binop_code, .prop.handler=hnd }
    #define INSN_IGN(spi, _, __) \
        [OP_##spi] = { .insn_code=CL_INSN_ABORT, .prop.string="OP_" #spi }

        // how? | sparse insn.    | cl insn. (+uni/bin) | handler            |
        //------+-----------------+---------------------+--------------------|

        INSN_IGN( BADOP           ,                     ,                    ),

        /* Entry */
        INSN_IGN( ENTRY           ,                     ,                    ),

        /* Terminator */
        // OP_TERMINATOR = OP_RET
        INSN_STD( RET             , RET                 , handle_insn_ret    ),
        INSN_STD( BR              , NOP /*JMP or COND*/ , handle_insn_br     ),
        INSN_STD( SWITCH          , NOP /*another way*/ , handle_insn_switch ),
        INSN_IGN( INVOKE          ,                     ,                    ),
        INSN_IGN( COMPUTEDGOTO    ,                     ,                    ),
        INSN_IGN( UNWIND          ,                     ,                    ),
        // OP_TERMINATOR_END = OP_UNWIND

        /* Binary */
        // OP_BINARY = OP_ADD
        INSN_BIN( ADD             , PLUS/*POINTER_PLUS*/, handle_insn_binop  ),
        INSN_BIN( SUB             , MINUS               , handle_insn_binop  ),
        INSN_BIN( MULU            , MULT /*XXX: unsig.*/, handle_insn_binop  ),
        INSN_BIN( MULS            , MULT                , handle_insn_binop  ),
        INSN_BIN( DIVU            , TRUNC_DIV /*unsig.*/, handle_insn_binop  ),
        INSN_BIN( DIVS            , TRUNC_DIV           , handle_insn_binop  ),
        INSN_BIN( MODU            , TRUNC_MOD /*unsig.*/, handle_insn_binop  ),
        INSN_BIN( MODS            , TRUNC_MOD           , handle_insn_binop  ),
        INSN_BIN( SHL             , LSHIFT              , handle_insn_binop  ),
            // OP_ASR (arithmetic shift) is the same as OP_LSR (logical shift)
            // except for that the highest bit is kept the same, not zeroed;
            // - C standard says that right shift perfomed on unsigned type is
            //   of the LSR type, implementation specific (LSR/ASR) otherwise
            //   [C text book by P. Herout, TODO: check real standard]
            // - for sparse, right shift performed on signed operand is
            //   translated into OP_ASR (OP_LSR otherwise as expected XXX:vrfy)
        INSN_BIN( LSR             , RSHIFT              , handle_insn_binop  ),
        INSN_BIN( ASR             , RSHIFT              , handle_insn_binop  ),

        /* Logical */
        INSN_BIN( AND             , BIT_AND             , handle_insn_binop  ),
        INSN_BIN( OR              , BIT_IOR             , handle_insn_binop  ),
        INSN_BIN( XOR             , BIT_XOR             , handle_insn_binop  ),
        INSN_BIN( AND_BOOL        , TRUTH_AND           , handle_insn_binop  ),
        INSN_BIN( OR_BOOL         , TRUTH_OR            , handle_insn_binop  ),
        // OP_BINARY_END = OP_OR_BOOL

        /* Binary comparison */
        // OP_BINCMP = OP_SET_EQ
        INSN_BIN( SET_EQ          , EQ                  , handle_insn_binop  ),
        INSN_BIN( SET_NE          , NE                  , handle_insn_binop  ),
        INSN_BIN( SET_LE          , LE                  , handle_insn_binop  ),
        INSN_BIN( SET_GE          , GE                  , handle_insn_binop  ),
        INSN_BIN( SET_LT          , LT                  , handle_insn_binop  ),
        INSN_BIN( SET_GT          , GT                  , handle_insn_binop  ),
        INSN_BIN( SET_B           , LT /*XXX: unsigned*/, handle_insn_binop  ),
        INSN_BIN( SET_A           , GT /*XXX: unsigned*/, handle_insn_binop  ),
        INSN_BIN( SET_BE          , LE /*XXX: unsigned*/, handle_insn_binop  ),
        INSN_BIN( SET_AE          , GE /*XXX: unsigned*/, handle_insn_binop  ),
        // OP_BINCMP_END = OP_SET_AE,

        /* Uni */
        INSN_UNI( NOT             , BIT_NOT             , handle_insn_unop   ),
        INSN_UNI( NEG             , TRUTH_NOT/*u.minus*/, handle_insn_unop   ),

        /* Select - three input values */
        INSN_STD( SEL             , NOP /*COND*/        , handle_insn_sel    ),

        /* Memory */
        INSN_IGN( MALLOC          ,                     ,                    ),
        INSN_IGN( FREE            ,                     ,                    ),
        INSN_IGN( ALLOCA          ,                     ,                    ),
        INSN_UNI( LOAD            , ASSIGN              , handle_insn_load   ),
        INSN_UNI( STORE           , ASSIGN              , handle_insn_store  ),
        INSN_UNI( SETVAL          , ASSIGN              , handle_insn_setval ),
        INSN_IGN( SYMADDR         ,                     ,                    ),
        INSN_IGN( GET_ELEMENT_PTR ,                     ,                    ),

        /* Other */
            // FIXME: this might be a SPARSE bug if DO_PER_EP_UNSAA is set
            //        and OP_PHI or OP_PHISOURCE occurs (really encountered)
        INSN_IGN( PHI             ,                     ,                    ),
        INSN_IGN( PHISOURCE       ,                     ,                    ),
        INSN_UNI( CAST            , ASSIGN              , handle_insn_cast   ),
        INSN_UNI( SCAST           , ASSIGN              , handle_insn_cast   ),
        INSN_IGN( FPCAST          , ASSIGN /*not sure*/ , handle_insn_copy   ),
        INSN_UNI( PTRCAST         , ASSIGN              , handle_insn_ptrcast),
        INSN_IGN( INLINED_CALL    ,                     ,                    ),
        INSN_STD( CALL            , NOP /*another way*/ , handle_insn_call   ),
        INSN_IGN( VANEXT          ,                     ,                    ),
        INSN_IGN( VAARG           ,                     ,                    ),
        INSN_IGN( SLICE           ,                     ,                    ),
        INSN_IGN( SNOP            ,                     ,                    ),
        INSN_IGN( LNOP            ,                     ,                    ),
        INSN_IGN( NOP             ,                     ,                    ),
        INSN_IGN( DEATHNOTE       ,                     ,                    ),
        INSN_IGN( ASM             ,                     ,                    ),

        /* Sparse tagging (line numbers, context, whatever) */
        INSN_IGN( CONTEXT         ,                     ,                    ),
        INSN_IGN( RANGE           ,                     ,                    ),

        /* Needed to translate SSA back to normal form */
        INSN_UNI( COPY            , ASSIGN              , handle_insn_copy   ),
    #undef INSN_IGN
    #undef INSN_BIN
    #undef INSN_UNI
    #undef INSN_STD
    };

    struct cl_insn cli;
    const struct insn_conversion *conversion;

    WITH_DEBUG_LEVEL(d_instruction)
        PUT(debug,"\t%d: instruction to be processed: %s",
            insn->pos.line, API_SPARSE(show_instruction, insn));

    //assert(PARTIALLY_ORDERED( OP_BADOP , insn->opcode , OP_COPY ));
    conversion = &insn_conversions[insn->opcode];

    sparse_location(&cli.loc, insn->pos);
    cli.code = conversion->insn_code;

    switch (conversion->insn_code) {
        case CL_INSN_ABORT:
            WARN_UNHANDLED(insn->pos, conversion->prop.string);
            return true;
        case CL_INSN_UNOP:
            cli.data.insn_unop.code = conversion->code.unop;
            break;
        case CL_INSN_BINOP:
            cli.data.insn_binop.code = conversion->code.binop;
            break;
        default:
            break;
    }

    assert(conversion->prop.handler);
    return conversion->prop.handler(&cli, insn);
}

static bool insn_interesting(struct instruction *insn)
{
    // TODO: investigate
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

    if (!insn_interesting(insn))
        return true;

    return handle_insn(insn);
}


//
// Functions for lower granularity/higher level handling
//

static void handle_bb(struct basic_block *bb)
{/*
  *
  * Problems/exceptions/notes:
  * - avoid being called with !bb
  */
    struct instruction *insn;

    if (!bb)
        return;

    API_EMIT(bb_open, PTR_STRING(bb));

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
    struct cl_insn cli;
    struct basic_block *bb;

    /* jump to entry basic block */

    sparse_location(&cli.loc, ep->entry->pos);
    emit_insn_jmp(&cli, PTR_STRING(ep->entry->bb));

    /* go through basic blocks */

    FOR_EACH_PTR(ep->bbs, bb) {
        if (!bb)
            continue;

        if (bb->parents || bb->children || bb->insns
            || /* FIXME: is the following actually useful? */
            2 < GLOBALS(debug)) {
            handle_bb(bb);
        }
    } END_FOR_EACH_PTR(bb);
}

static void handle_fnc_body(struct symbol *sym)
{
    struct entrypoint *ep;
    API_SPARSE(linearize_symbol, /*out*/ep, /*in*/sym);
    if (!ep)
        CL_TRAP;

#if DO_PER_EP_UNSAA
    API_SPARSE(unssa, ep);
#endif

#if DO_PER_EP_SET_UP_STORAGE
    API_SPARSE(set_up_storage, ep);
#endif

    handle_fnc_ep(ep);

#if DO_PER_EP_SET_UP_STORAGE
    // no switch, vrfy_storage uses printf anyway
    API_SPARSE(free_storage);
#endif
}

static void handle_fnc_arg_list(struct symbol_list *arg_list)
{
    int argc = 0;
    struct symbol *arg;
    struct cl_operand arg_op;

    FOR_EACH_PTR(arg_list, arg) {
        API_EMIT(fnc_arg_decl, ++argc, op_from_symbol(&arg_op, arg));
        op_free_data(&arg_op);
    } END_FOR_EACH_PTR(arg);
}

static void handle_fnc_def(struct symbol *sym)
{
    struct cl_operand fnc;

    WITH_FNC_TO_EMIT(op_from_symbol(&fnc, sym)) {
        // dump argument list
        handle_fnc_arg_list(sym->ctype.base_type->arguments);
        // handle fnc body
        handle_fnc_body(sym);
    }

    op_free_data(&fnc);
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

static void
proceed_symbols(struct symbol_list *list)
{
    struct symbol *sym;

    FOR_EACH_PTR(list, sym) {

#if DO_EXPAND_SYMBOL
        API_SPARSE(expand_symbol, sym);
#endif

        handle_top_level_sym(sym);
    } END_FOR_EACH_PTR(sym);
}


/* see clsp.h */
void
proceed(struct string_list *filelist, struct symbol_list *symlist)
{
    char *file;

#if DO_PROCEED_INTERNAL
    /* internal symbols */
    WITH_FILE_TO_EMIT(SPARSE_INTERNAL_SYMS_FILE)
        proceed_symbols(symlist);
#endif

    /* the rest, file by file */
    FOR_EACH_PTR_NOTAG(filelist, file) {
        WITH_DEBUG_LEVEL(d_file)
            PUT(debug, "about to proceed '%s'...\n", file);
        WITH_FILE_TO_EMIT(file) {
            API_SPARSE(sparse, /*out*/symlist, /*in*/file);
            proceed_symbols(symlist);
        }
    } END_FOR_EACH_PTR_NOTAG(file);

#if DO_SPARSE_FREE
    free(input_streams);
#endif
}
