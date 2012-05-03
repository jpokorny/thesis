/*
 * Copyright 2009 Kamil Dudka <kdudka@redhat.com>
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


/*
    compile options
 */

/* general */
#define DO_EXTRA_CHECKS              0
#define USE_EXTENDED_TYPE_CMP        0
#define SHOW_PSEUDO_INSNS            0

/* sparse */
#define FIX_SPARSE_EXTRA_ARG_TO_MEM  0

/* assertions */
#define ASSERT_SYMBOL_BELONGS_TO_CURRENT_FILE   1
#define ASSERT_CST_HAVE_VALID_CODE              1


/* if (!emit_props & emit_skip_initial), use this name for "initial" symbols */
static const char *const initial_file = "<initial-metafile>";



#define OP_INITIALIZED_VAR(op) \
    (op->code == CL_OPERAND_VAR && op->data.var->initialized)


#define SYM_FNC_ARGS(sym)  (sym->ctype.base_type->arguments)

#define DEBUG_BB_HEADER(label)                          \
    DLOG(d_bblk,                                        \
         "\tdebug: " HIGHLIGHT("basic-block") ": open " \
         HIGHLIGHT("["_1(s)"]"), label)

#define DEBUG_FNC_ARG_HEADER(arg_id)                                      \
    DLOG(d_func,                                                          \
         "\tdebug: " HIGHLIGHT("function") ": argument "HIGHLIGHT(_1(d)), \
         arg_id)                                                          \

#define DEBUG_SWITCH_CASE_HEADER(loc, val_lo, val_hi, label)     \
    WITH_DEBUG_LEVEL(d_insn) {                                   \
        PUT(debug,                                               \
            CLPOSFMT_1 ": debug: " HIGHLIGHT("instruction")      \
            ": switch-case: target " HIGHLIGHT("["_4(s)"]"),     \
            CLPOS(*loc), label);                                 \
        debug_cl_switch_case(val_lo, val_hi, GLOBALS(indent)+1); \
    }

#define WITH_FILE_TO_EMIT(file, symref, private)                             \
    for (int i_=0; i_==0                                                     \
        ? (                                                                  \
          (initial_file == file                                              \
            ? DLOG(d_file, _1(s)": debug: " HIGHLIGHT("file") ": begin"      \
                           " (aggregation of various sources)", file)        \
            : DLOG(d_file, SPPOSFMT_1 ": debug: " HIGHLIGHT("file")          \
                           ": begin (first symbol)",                         \
                           SPPOS((symref)->pos))),                           \
          private ? (void) 0 : API_EMIT(file_open, file),                    \
          1                                                                  \
        ) : (                                                                \
          private ? (void) 0 : API_EMIT(file_close),                         \
          (initial_file == file                                              \
            ? DLOG(d_file, _1(s)": debug: " HIGHLIGHT("file") ": end", file) \
            : DLOG(d_file, SPPOSFMT_1 ": debug: " HIGHLIGHT("file")          \
                           ": end (last symbol)",                            \
                           SPPOS((symref)->endpos))),                        \
          0                                                                  \
        ) ; i_++)

#define WITH_CALL_TO_EMIT(loc, dst, fnc)                                     \
    for (int i_=0; 0==i_                                                     \
         ? (                                                                 \
           API_EMIT(insn_call_open, loc, dst, fnc),                          \
           1                                                                 \
         ) : (                                                               \
           API_EMIT(insn_call_close),                                        \
           0                                                                 \
         ) ; i_++)

#define WITH_SWITCH_TO_EMIT(loc, op)                                         \
    for (int i_=0; 0==i_                                                     \
         ? (                                                                 \
           DLOG(d_insn, CLPOSFMT_1 ": debug: " HIGHLIGHT("instruction")      \
                ": cl <<< switch begin", CLPOS(*loc)),                       \
           API_EMIT(insn_switch_open, loc, op),                              \
           1                                                                 \
         ) : (                                                               \
           API_EMIT(insn_switch_close),                                      \
           DLOG(d_insn,                                                      \
                "\tdebug: " HIGHLIGHT("instruction") ": cl <<< switch end"), \
           0                                                                 \
         ) ; i_++)


#define WITH_FUNCTION_TO_EMIT(fnc, endpos)                                   \
    for (int i_=0; 0==i_                                                     \
        ? (                                                                  \
          DLOG(d_func,                                                       \
               CLPOSFMT_1 ": debug: " HIGHLIGHT("function") ": begin "       \
               HIGHLIGHT(_4(s)),                                             \
               CLPOS(CST_FNC(fnc)->loc), CST_FNC(fnc)->name),                \
          API_EMIT(fnc_open, fnc),                                           \
          1                                                                  \
        ) : (                                                                \
          API_EMIT(fnc_close),                                               \
          DLOG(d_func,                                                       \
               SPPOSFMT_1 ": debug: " HIGHLIGHT("function") ": end "         \
               HIGHLIGHT(_4(s)), SPPOS(endpos), CST_FNC(fnc)->name),         \
          0                                                                  \
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
#if NONZERO_NULL
    ret->next = NULL;
#endif
    return ret;
}

#if 0
static inline struct cl_accessor *
alloc_cl_accessor_safe(void)
{
    struct cl_accessor *ret = alloc_cl_accessor();
#if NONZERO_NULL
    ret->next = NULL;
#endif
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


/*
    mostly sparse related helper functions
 */

/*
    this should accommodate worst-case of pointer hexa representation incl. \0
    Python check: [(x,len(x)) for x in hex(2**64-1).strip('L').partition('x')]
 */
#define BB_LABEL_MAX  17

/* incl. compile-time constraint check to make sure we fit into BB_LABEL_MAX */
struct bb_label {
    char str[sizeof(uintptr_t) <= 8 ? BB_LABEL_MAX : -1];
};

#define BB_LABEL(bb)  (const char *) bb_label((uintptr_t) bb->priv).str

/* NOTE: returning a short array through stack, but should not hurt */
static inline struct bb_label
bb_label(uintptr_t cnt)
{
    struct bb_label ret;
    assert(0 != cnt);  /* 0 means unexpected BB */
    if (0 >= snprintf(ret.str, sizeof(ret.str), "%" PRIxPTR, cnt))
        DIE("snprintf");
    return ret;
}



/*
    types handling
 */


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

/**
    Compare two types: current and expected one (in this order!)

    For ordered pair T1, T2:

    T1 eq T2 -> T1 == T2             # reflective (eq is a pointer match)
    T1 == T2 -> ARRAY(T1) == PTR(T2) # promotion array -> pointer (OK)
    T1 == T2 -> PTR(T1) == ARRAY(T2) # promotion pointer -> array (unexpected?)

    T1 != T2                         # otherwise
 */
static inline bool
type_match(const struct cl_type *t1, const struct cl_type *t2)
{
    if (t1 == t2)
        return true;

    assert((t1->code|t2->code) < sizeof(int) * 8);  /* shift overflow check*/
    if (!( (1<<t1->code | 1<<t2->code) & ~(1<<CL_TYPE_ARRAY | 1<<CL_TYPE_PTR) )
      && type_match(t1->items->type, t2->items->type))
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

    /* fastest path, we have the type already in hash table */
    clt = type_ptr_db_lookup_item(TYPEPTRDB, type, ptr);
    if (clt) {
        DEBUG_TYPE_FROM_SYMBOL_CACHE(type, clt);
        return clt;
    }

    DEBUG_TYPE_FROM_SYMBOL_SP(type);

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

    DEBUG_TYPE_FROM_SYMBOL_CL(clt);

    return clt;
}

static struct cl_type *
type_from_register(const pseudo_t pseudo)
{
    struct pseudo_user *pu;
    struct symbol *type = NULL;

    /*
        most accurate way of getting the output type (type of target
        pseudo) when not properly exposed by some instructions
        (OP_CALL, binary comparisons) or instruction is not directly
        available from pseudo->def

        check whether immediate user (instruction) of target pseudo
        has "authority to decide the type" (casts, OP_RET, OP_COPY)

        it is desired to have the pseudo type correct from the beginning,
        but also as we cache the whole pseudo, it is an imperative to do
        this forward analysis (currently of depth 1)

        note: the target of CALL may not be used at all (no users), still
              it (probably) cannot be removed by sparse optimizations
     */
    if (!ptr_list_empty(pseudo->users)) {
        pu = SP(first_ptr_list, (struct ptr_list *) pseudo->users);
        switch (pu->insn->opcode) {
            case OP_CAST:
            case OP_SCAST:
            case OP_FPCAST:
            case OP_PTRCAST:
                type = pu->insn->orig_type;
                break;
            case OP_RET:
            case OP_COPY:
                /* OP_COPY needs this patch applied:
                   http://comments.gmane.org/gmane.comp.parsers.sparse/2802 */
                type = pu->insn->type;
                break;
            default:
                /* other instructions may have an incorrect type as well (?) */
                assert(pseudo->def);
                break;
        }
    }

    /* second-level type authority */
    if (!type && pseudo->def)
        type = pseudo->def->type;

    if (type)
        return type_from_symbol(type, NULL);

    /*
        sadly, last resort
        XXX: look at insn->size
     */
    CL_TRAP;
    return &int_clt;
}

/**
    Get scope from register (either CL_SCOPE_FUNCTION or CL_SCOPE_BB)

    CL_SCOPE_BB is considered a special case of CL_SCOPE_FUNCTION
    where all the register users are present in the same BB.
    Otherwise, the register is used accross more BBs (within the same
    function) and thus its scope has to be CL_SCOPE_FUNCTION
 */
static enum cl_scope_e
scope_from_register(const pseudo_t pseudo, const struct basic_block *bb)
{
    assert(PSEUDO_REG == pseudo->type);

    struct pseudo_user *pu;

    FOR_EACH_PTR(pseudo->users, pu)
        if (pu->insn->bb != bb)
            return CL_SCOPE_FUNCTION;
    END_FOR_EACH_PTR(pu);

    return CL_SCOPE_BB;
}


/** operands handling *****************************************************/


enum copy_depth {
    copy_shallow,
    copy_shallow_ac_deep,
    copy_shallow_ac_null,
    copy_shallow_var_deep,
    copy_shallow_var_deep_uid,
};


static struct cl_accessor *
accessor_copy(const struct cl_accessor *orig)
{
    struct cl_accessor *ret = alloc_cl_accessor();
    *ret = *orig;
    return ret;
}

/**
    Copy operand

    params[in] new_var  Whether to associate new UID with the operand.
 */
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

    if (copy_shallow_var_deep == copy_depth && CL_OPERAND_VAR == op_src->code) {
        ret->data.var = alloc_cl_var();
        *ret->data.var = *op_src->data.var;
    }

    if (copy_shallow_var_deep == copy_depth
      || copy_shallow_var_deep_uid == copy_depth
      && CL_OPERAND_VAR == op_src->code) {
        ret->data.var = alloc_cl_var();
        *ret->data.var = *op_src->data.var;
        if (copy_shallow_var_deep_uid == copy_depth)
            VAR(ret)->uid = ++COUNTER(var);
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
        op:       scope, type (initially NULL)
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
    op->type     = NULL;
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
        op:       scope, type (initially NULL)
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
    op->type         = NULL;
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
insn_setops_store(struct cl_insn *cli, const struct instruction **insn);


/**
    Initialize var operand from sparse symbol initializer

    This is only for static/extern globals which apparently could not
    be linearized in per-entrypoint pass because they are not local
    to particular entry point.  We make a fake entry point and force
    sparse to linearize this initializator and proceed resulting
    instruction to build real CL initializator.

    [ Per function initialized variables have the initializator
    spread directly in instruction stream (STORE instructions) and thus
    CL will receive it in such a form as well ]

    This should never get into recursion as it is prevented early
    in @c op_from_symbol, looking at sym->pseudo we set here.


    @param[in,out] initial  Address where to start initializators chain
    @param[in]     sym      Initializator of this symbol is examined
    @return  Value to be set to VAR(op)->initialized
 */
static bool
op_initialize_var_from_initializer(struct cl_operand *op,
                                   struct symbol *sym)
{
    assert(sym->initializer);
    assert(sym->ctype.modifiers & (MOD_STATIC | MOD_TOPLEVEL));

    struct expression *expr;
    struct instruction *insn;
    struct cl_initializer **initial = &VAR(op)->initial;

    /*
        we have to mock environment for sparse before calling
        linearize_expression yielding the required instructions
     */

    /* XXX block to consider for upstreaming start XXX */

    unsigned long backup_modifiers = sym->ctype.modifiers;
    struct entrypoint *ep = __alloc_entrypoint(0);

    ep->active = __alloc_basic_block(0);
    ep->active->ep = ep;

    expr = alloc_expression(sym->initializer->pos, EXPR_SYMBOL);
    expr->symbol = sym;

    /* trick sparse to accept this symbol for initialization */
    assert(!sym->pseudo);
    sym->ctype.modifiers &= ~(MOD_STATIC | MOD_TOPLEVEL);

    SP(linearize_expression, ep, expr);
    assert(!ptr_list_empty(ep->active->insns));

    sym->ctype.modifiers = backup_modifiers;

    /* XXX block to consider for upstreaming end XXX */

    /*
        sym->pseudo is set by linearize_expression, but no longer needed,
        so we set the operand being created right now here to avoid recursive
        infloop/duplicated variables (which is bad) when converting
        initializer operands as it contains self-references
     */
    sym->pseudo = (void *) op;
    DEBUG_INITIALIZER_EXPR_START();

    FOR_EACH_PTR(ep->active->insns, insn) {

        DEBUG_INITIALIZER_EXPR_SP(insn);
        switch (insn->opcode) {
            case OP_STORE:
                /* XXX recursion (orig. pseudo can be passed and precached) */
                *initial = alloc_cl_initializer_safe();
                conv_position(&(*initial)->insn.loc, &insn->pos);
                (*initial)->insn.code = CL_INSN_UNOP;
                (*initial)->insn.data.insn_unop.code = CL_UNOP_ASSIGN;
                insn_setops_store(&(*initial)->insn, &insn);
                assert(!insn); /* not a multi-phased instruction */
                break;
            case OP_SYMADDR:
                /* this marks end of initializer (?) */
                DEBUG_INITIALIZER_EXPR_CL_SPECIAL(_1(s), "(ignored)");
                continue;
            default:
                WARN_UNHANDLED(insn->pos, "initializer instruction");
                continue;
        }
        DEBUG_INITIALIZER_EXPR_CL(*initial);

        initial = &(*initial)->next;  /* move onto the next initializer item */

    } END_FOR_EACH_PTR(insn);

    DEBUG_INITIALIZER_EXPR_STOP();
    sym->pseudo = NULL;  /* in-depth initialization done, no longer needed */

    return false;  /* initialization implied by the scope */
}

/**
    Try to initialize operand using symbol's initializer

    @return  Value to be set to VAR(op)->initialized
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
        /*
            static/extern globals are implicitly initialized to {0}
            but this is implied by the scope
         */
        return false;

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
                anyway), then steal its initializer;
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
 */
static inline struct cl_operand *
op_from_symbol(struct symbol *sym)
{
    struct cl_operand *op = NO_OPERAND_USE;
    if (sym->pseudo)
        op = (struct cl_operand *) sym->pseudo;

    DEBUG_OP_FROM_SYMBOL_SP(sym);

    if (NO_OPERAND_USE != op)
        return op;

    /*
        no identifier -> may be a primitive literal;
        function has to have it (unless anonymous? XXX)
     */
    if (!sym->ident) {
        assert(SYM_FN != sym->ctype.base_type->type);
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

        VAR(op)->initialized = op_initialize_var_maybe(op, sym);
    }

    DEBUG_OP_FROM_SYMBOL_CL(sym, op);

    return op;
}

/**
    Operand from function argument

    This does not work: find the right PSEUDO_SYM in pseudo->users
                        and use its ->sym.
 */
static inline struct cl_operand *
op_from_fnc_argument(const pseudo_t pseudo)
{
    assert(0 <= pseudo->nr);

    struct cl_operand *op;
    struct symbol *sym;
    struct symbol_list * arguments;
    int pos = pseudo->nr;

    arguments = pseudo->def->bb->ep->name->ctype.base_type->arguments;

    FOR_EACH_PTR(arguments, sym)
        if (!--pos)
            goto right_position;
    END_FOR_EACH_PTR(sym);

right_position:
    assert(!pos && sym);  /* internal corruption otherwise */

    op = op_from_symbol(sym);
    assert(CL_SCOPE_FUNCTION == op->scope);
    return op;
}

/**
    Operand from register

    The type is available at the defining instruction (pseudo->def),
    (approximate) position dtto.
 */
static struct cl_operand *
op_from_register(const struct instruction *insn, const pseudo_t pseudo)
{
    struct cl_operand *op = op_make_var();

    op->scope = scope_from_register(pseudo, insn->bb);
    op->type  = type_from_register(pseudo);

    VAR(op)->name = sparse_ident(pseudo->ident, NULL);
    VAR(op)->artificial = !VAR(op)->name;
    conv_position(&VAR(op)->loc, &insn->pos);

    return op;
}

/**
    Operand from integral value
 */
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
op_from_pseudo_maybe(const struct instruction *insn, const pseudo_t pseudo)
{
    struct cl_operand *op;

    if (!pseudo)
        return NO_OPERAND_USE;

    if (pseudo->priv) {
        DEBUG_OP_FROM_PSEUDO_CACHE(pseudo);
        return (struct cl_operand *) pseudo->priv;
    }

    switch (pseudo->type) {
        case PSEUDO_VOID: op = NO_OPERAND_USE; break;
        /* symbol (argument boils down to it too) is "debugged" in-place */
        case PSEUDO_SYM:  return pseudo->priv = op_from_symbol(pseudo->sym);
        case PSEUDO_ARG:  return pseudo->priv = op_from_fnc_argument(pseudo);
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


/**
    Wrapper for @c op_from_pseudo_maybe when void is not expected
 */
static inline struct cl_operand *
op_from_pseudo(const struct instruction *insn, const pseudo_t pseudo)
{
    /*assert(pseudo);*/
    struct cl_operand *ret = op_from_pseudo_maybe(insn, pseudo);
    assert(NO_OPERAND_USE != ret);
    return ret;
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
    struct cl_accessor *ac_end = ac;

    if (!ac)
        ac = ac_end = alloc_cl_accessor();
    else
        while (ac_end->next)
            ac_end = ac_end->next;

    ac_end->next = op->accessor;
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
            } else {
                /* {deref, ref} = {} */
                if (op->accessor && CL_ACCESSOR_REF == op->accessor->code) {
                    struct cl_operand *clone = op_copy(op, copy_shallow_ac_null);
                    op_prepend_accessor(clone, op->accessor->next);
                    clone->type = op->accessor->type;
                    *op_composite = clone;
                    return retval;
                }
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

    if (op->accessor)
        return true;

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


/** instructions + high-level dealing with operands ***********************/


static inline void
emit_bb_open(const char *label)
{
    DEBUG_BB_HEADER(label);
    API_EMIT(bb_open, label);
}

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
insn_assignment_base(struct cl_insn *cli, const struct instruction **insn,
                     pseudo_t lhs,    /* := */    pseudo_t rhs,
                     enum assignment_ops_handling ops_handling)
{
    struct instruction *assign = *insn;
    struct cl_operand
        **op_lhs = (struct cl_operand **) &UNOP(cli)->dst,
        **op_rhs = (struct cl_operand **) &UNOP(cli)->src;

    *insn = NULL;

    /* prepare LHS */

    *op_lhs = op_from_pseudo(assign, lhs);

    if (ops_handling & TYPE_LHS_DIG) {
        struct cl_type *type = type_from_symbol(assign->type, NULL);
        if (!type_match((*op_lhs)->type, type)) {
            if (!op_accessible(*op_lhs)) {
                struct cl_accessor *ac = op_append_accessor(*op_lhs, NULL);
                ac->code = CL_ACCESSOR_DEREF;
                // note: no such clt easily accessible (contrary to previous case)
                ac->type = build_referenced_type(type);
                (*op_lhs)->type = type;
            } else {
                op_dig_for_type_match((const struct cl_operand **) op_lhs,
                                      type, assign->offset);
            }
        }
    }

    /* prepare RHS (quite complicated compared to LHS) */

    *op_rhs = op_from_pseudo(assign, rhs);
    insn_assignment_mod_rhs(op_rhs, rhs, assign, ops_handling);

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
         show_instruction((struct instruction *) assign));
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
insn_setops_store(struct cl_insn *cli, const struct instruction **insn)
{
    return insn_assignment_base(cli, insn,
        (*insn)->src,     /* := */  (*insn)->target,
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
insn_setops_load(struct cl_insn *cli, const struct instruction **insn)
{
    assert(PSEUDO_REG == (*insn)->target->type);

    return insn_assignment_base(cli, insn,
        (*insn)->target,  /* := */  (*insn)->src,
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
insn_setops_copy(struct cl_insn *cli, const struct instruction **insn)
{
    assert(PSEUDO_REG == (*insn)->target->type);

    return insn_assignment_base(cli, insn,
        (*insn)->target,  /* := */  (*insn)->src,
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
insn_setops_cast(struct cl_insn *cli, const struct instruction **insn)
{
  if ((*insn)->orig_type->bit_size < (*insn)->type->bit_size
    && (*insn)->orig_type->ctype.base_type->type == SYM_BITFIELD) {
        // we have to apply CL_BINOP_BIT_AND on `insn->src' using mask
        // (currently of int size XXX?) with additional higher bits zeroed

        int mask = ~((~0) << (*insn)->orig_type->bit_size);
        struct cl_operand dst, lhs, op_mask;

        cli->code = CL_INSN_BINOP;
        BINOP(cli)->code = CL_BINOP_BIT_AND;
        BINOP(cli)->dst  = op_from_pseudo(*insn, (*insn)->target);
        BINOP(cli)->src1 = op_from_pseudo(*insn, (*insn)->src);
        BINOP(cli)->src2 = op_from_intval(mask);

        *insn = NULL;
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
insn_setops_ptrcast(struct cl_insn *cli, const struct instruction **insn)
{
    return insn_assignment_base(cli, insn,
        (*insn)->target,  /* := */  (*insn)->src,
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

    Problems/exceptions/notes:
    XXX See `op_from_expression'.
 */
static inline struct cl_insn *
insn_setops_setval(struct cl_insn *cli, const struct instruction **insn)
{
    UNOP(cli)->dst = op_from_pseudo(*insn, (*insn)->target);
    UNOP(cli)->src = op_from_primitive_literal((*insn)->val);

    *insn = NULL;
    return cli;
}

/**
    Set operands for unary operations (except for assignments)
 */
static inline struct cl_insn *
insn_setops_unop(struct cl_insn *cli, const struct instruction **insn)
{
    UNOP(cli)->dst = op_from_pseudo(*insn, (*insn)->target);
    UNOP(cli)->src = op_from_pseudo(*insn, (*insn)->src1);

    *insn = NULL;
    return cli;
}

/**
    Set operands for binary operations, taking care of pointer arithmetics
 */
static struct cl_insn *
insn_setops_binop(struct cl_insn *cli, const struct instruction **insn)
{
    const struct cl_type *t1, *t2;
    struct cl_operand *src1, *src2, *dst;
    struct cl_instruction *next;

    BINOP(cli)->dst  = dst  = op_from_pseudo(*insn, (*insn)->target);
    BINOP(cli)->src1 = src1 = op_from_pseudo(*insn, (*insn)->src1);
    BINOP(cli)->src2 = src2 = op_from_pseudo(*insn, (*insn)->src2);

    t1 = src1->type;
    t2 = src2->type;

    /*
        for pointer vs. non-pointer, there are some special cases
     */
    if (cl_type_ptrlike(t1->code) != cl_type_ptrlike(t2->code)) {
        if (CL_TYPE_INT == t1->code && CL_TYPE_ARRAY == t2->code) {
            /*
                let's start with the most-painful-to-grasp one:
                "int A + array(T)" denotes late phase of accessing array,
                where the int A is computed offset from the start of
                the array;
                we just replace the target operand with type-enhanced
                version so the upcoming LOAD can use it appropriately:

                    type: ptr(T)  (this top-level is implicit)
                      - accessor: ref on type T
                        - accessor: deref-array on type array(T) with index(*)

                (*) index operand is available by backtracing the origin
                    of A as it is defined as A <- B, sizeof(T)

                note that this deep type information is the same as without
                any intervention when viewed only on the surface, but carries
                the necessary information for upcoming LOAD instruction to
                be emitted as correct array access (unlike the akward, yet
                proper code as without this intervention)

                the downside is that A and B will most probably be dead
                variables, but (1) not 100% due to contant propagation and
                (2) CL infrastructure is capable of dealing with it
             */
            assert(!src2->accessor);

            //VAR(src2)->uid = VAR(dst)->uid;
            t1 = dst->type;

            dst = op_copy(src2, copy_shallow_var_deep);

            /* build the accessor chain from the back */

            struct cl_accessor *ac = op_prepend_accessor(dst, NULL);
            ac->code = CL_ACCESSOR_DEREF_ARRAY;
            ac->type = t2;

            struct instruction *def_offset = (*insn)->src1->def;
            assert(OP_MULS == def_offset->opcode && def_offset->src1->priv);

            ac->data.array.index = def_offset->src1->priv;

            ac = op_prepend_accessor(dst, NULL);
            ac->code = CL_ACCESSOR_REF;
            ac->type = dst->type->items->type;

            dst->type = t1;

            /* set adjusted operand for load to see; do not emitting anything */
            (*insn)->target->priv = dst;

            *insn = NULL;
            return NULL;

        } else if (CL_TYPE_INT == t2->code) {
            /*
                the only other valid possibility is when second operand is
                integral which means we are heading towards
                CL_BINOP_POINTER_PLUS (asymmetrical pointer +- int operation)

             */
            if (CL_BINOP_PLUS == BINOP(cli)->code) {

                BINOP(cli)->code = CL_BINOP_POINTER_PLUS;

            } else if (CL_BINOP_MINUS == BINOP(cli)->code
              && CL_OPERAND_CST == src2->code) {

                BINOP(cli)->src2 = op_make_cst_int(0 - CST_INT(src2)->value);
                BINOP(cli)->code = CL_BINOP_POINTER_PLUS;

            } else if (CL_BINOP_MINUS == BINOP(cli)->code) {

                /*
                    a bit more work with non-constant; emit UNOP_MINUS first,
                    postponing the actual instruction to the next round (rare)
                    -- in fact we make a copy of both instruction and second
                    operand pseudo as we need to "precache" its CL counterpart
                    we evaluated here already and this change has to be local
                    to this instruction only
                 */
                cli->code = CL_INSN_UNOP;
                UNOP(cli)->code = CL_UNOP_MINUS;
                UNOP(cli)->dst  = op_copy(src2, copy_shallow_var_deep_uid);
                UNOP(cli)->src  = src2;

                struct instruction *next = __alloc_instruction(0);
                pseudo_t modified_src2 = __alloc_pseudo(0);

                *next = **insn;                  /* instruction shallow copy */
                next->opcode = OP_ADD;

                *modified_src2 = *(*insn)->src2;  /* int pseudo shallow copy */
                modified_src2->priv = UNOP(cli)->dst;

                next->src2 = modified_src2;
                *insn = next; /* proceed the modified copy in next the round */

                return cli;

            } else {

                /* something suspicious */
                WARN("unexpect binary operation pointer vs. int\n"
                     CLPOSFMT_1 ": note: here", CLPOS(cli->loc));

            }
        } else {
            /* something suspicious */
            WARN("unexpect binary operation pointer vs. non-pointer\n"
                 CLPOSFMT_1 ": note: here", CLPOS(cli->loc));
        }
    }

    *insn = NULL;
    return cli;
}

/**
    Set operand for return statement instruction

    [input] OP_RET
        insn->src:  value to be used as a return value (extra case: NULL)
        insn->type: type of return value

    Problems:
    1. Problem with a "right form" of the operand (whether to consider
       the whole struct or its first element, etc.).
    S. Combine `op_dig_for_type_match' and `insn->type' for adjustment.
 */
static inline struct cl_insn *
insn_setops_ret(struct cl_insn *cli, const struct instruction **insn)
{
    const struct cl_type *resulting_type;

    RET(cli)->src = op_from_pseudo_maybe(*insn, (*insn)->src);

    /* TODO: decide according to the pseudo instead? */
    if (op_accessible(RET(cli)->src)) {
        resulting_type = type_from_symbol((*insn)->type, NULL);
        op_dig_for_type_match(&RET(cli)->src, resulting_type, (*insn)->offset);
    }

    *insn = NULL;
    return cli;
}


/*
    helpers for emitting "weak" instructions (without 1:1 mapping with sparse)
 */

static inline void
insn_emit_jmp(struct cl_insn *cli, const char *label)
{
    cli->code = CL_INSN_JMP;
    JMP(cli)->label = label;
    DEBUG_INSN_CL(cli);
    API_EMIT(insn, cli);
}

static inline void
insn_emit_cond(struct cl_insn *cli, struct cl_operand *op_cond,
               const char *then_label, const char *else_label)
{
    cli->code = CL_INSN_COND;
    COND(cli)->src        = op_cond;
    COND(cli)->then_label = then_label;
    COND(cli)->else_label = else_label;

    DEBUG_INSN_CL(cli);
    API_EMIT(insn, cli);
}

static inline void
insn_emit_copy(struct cl_insn *cli, const struct instruction *insn,
               pseudo_t lhs, pseudo_t rhs)
{
    cli->code = CL_INSN_UNOP;
    UNOP(cli)->code = CL_UNOP_ASSIGN;

    if (insn_assignment_base(cli, &insn,
        lhs,           /* := */  rhs,
        TYPE_LHS_KEEP      |     TYPE_RHS_KEEP
    )) {
        DEBUG_INSN_CL(cli);
        API_EMIT(insn, cli);
    }
}


/*
    emitting direct instruction mapping (resulting in one or more instructions)
 */

/**
    Function call, aborting further intra-BB run if marked as non-returning
 */
static void
insn_emit_call(struct cl_insn *cli, const struct instruction *insn)
{
    struct pseudo *arg;
    int cnt = 0;

    struct cl_operand *target = op_from_pseudo(insn, insn->target),
                      *fnc    = op_from_pseudo(insn, insn->func);

    DEBUG_INSN_CL_SPECIAL("(call to "_1(s)" )", CST_FNC(target)->name);

    WITH_CALL_TO_EMIT(&cli->loc, target, fnc)
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
        DEBUG_INSN_CL(cli);
        API_EMIT(insn, cli);
    }
}

/**
    Un/conditional jump
 */
static void
insn_emit_br(struct cl_insn *cli, const struct instruction *insn)
{
    if (pseudo_futile(insn->cond))
        /* unconditional jump */
        insn_emit_jmp(cli, BB_LABEL(insn->bb_true));
    else
        /* conditional jump */
        insn_emit_cond(cli, op_from_pseudo(insn, insn->cond),
                       BB_LABEL(insn->bb_true), BB_LABEL(insn->bb_false));
}

/**
    Conditional operator
 */
static void
insn_emit_sel(struct cl_insn *cli, const struct instruction *insn)
{
    /* local BB labels */
    const char *const bb_true  = bb_label(++COUNTER(bb)).str;
    const char *const bb_false = bb_label(++COUNTER(bb)).str;
    const char *const bb_merge = bb_label(++COUNTER(bb)).str;

    /* cond instruction */
    op_from_pseudo(insn, insn->src1);
    insn_emit_cond(cli, op_from_pseudo(insn, insn->src1), bb_true, bb_false);

    /* first BB ("then" branch):  assign + jump to merging BB */
    emit_bb_open(bb_true);
    insn_emit_copy(cli, insn, insn->target, insn->src2);
    insn_emit_jmp(cli, bb_merge);

    /* second BB ("else" branch):  assign + jump to merging BB */
    emit_bb_open(bb_false);
    insn_emit_copy(cli, insn, insn->target, insn->src3);
    insn_emit_jmp(cli, bb_merge);

    /* merging BB */
    emit_bb_open(bb_merge);
}

/**
    Switch (incl. GNU C range extension as supported by sparse)
 */
static void
insn_emit_switch(struct cl_insn *cli, const struct instruction *insn)
{
    struct cl_operand *op, *val_lo, *val_hi;
    struct multijmp *jmp;
    const char *label;
    struct cl_loc bb_loc, *loc = &cli->loc;

    op = op_from_pseudo(insn, insn->target);

    WITH_SWITCH_TO_EMIT(loc, op) {

        FOR_EACH_PTR(insn->multijmp_list, jmp) {

            if (jmp->begin <= jmp->end) {

                /* non-default */
                val_lo = op_from_intval(jmp->begin);
                val_lo->type = op->type;
                val_hi = val_lo;

                if (jmp->begin != jmp->end) {
                    /* actually a range */
                    val_hi = op_from_intval(jmp->end);
                    val_hi->type = op->type;
                }

            } else {
                /* default case */
                val_lo = val_hi = NO_OPERAND_USE;
            }

            /*
                if the case is not "break only", it has a dedicated BB
                (currently, even if the statements for different cases
                are equal [maybe due to "falltrough" potentially breaking
                symmetry]) and we use its position as the position of the
                case (still quite accurate information)

                "break only" cases boil down to jump to the BB merging
                all the cases (having position even before the position
                of switch instruction) and we just use the position of the
                switch instruction for these (such empty cases are not
                interesting anyway)

                that is the best approximation available if we want to avoid
                rerunning the parse tree for current function, searching
                for switch statements and trying to figure out the right one
                -- the only reliable key would probably be bb_target field
                of case symbol, if at all; complexity at least O(n^2)
             */
            if (1 >= SP(ptr_list_size,
                        (struct ptr_list *) jmp->target->parents)) {
                conv_position(&bb_loc, &jmp->target->pos);
                loc = &bb_loc;
            } else {
                loc = &cli->loc;
            }

            label = BB_LABEL(jmp->target);

            DEBUG_SWITCH_CASE_HEADER(loc, val_lo, val_hi, label);
            API_EMIT(insn_switch_case, loc, val_lo, val_hi, label);

        } END_FOR_EACH_PTR(jmp);

    }
}


/** Enumeration of how to deal with sparse instruction */
enum conv_type {
    conv_setops,  /**< set operands and use common emit point */
    conv_emit,    /**< dedicated emitting (usually more CL instructions) */
    conv_ignore,  /**< instruction with special meaning within sparse */
    conv_warn     /**< instruction we currently cannot handle */
};

/**
    Consider instruction for emitting

    @return ret_negative=not emitted, ret_positive=emitted, ret_escape=abort bb
 */
static enum retval
consider_instruction(struct instruction *insn)
{
    typedef struct cl_insn *(*insn_setops)(
        struct cl_insn *,
        const struct instruction **);
    typedef void (*insn_emit)(struct cl_insn *, const struct instruction *);

    static const struct insn_conversion {
        enum cl_insn_e       insn_code; /**< target insn (conv_setops only) */
        union {
            enum cl_unop_e   unop;  /**< unary op. (conv_setops implied) */
            enum cl_binop_e  binop; /**< binary op. (conv_setops implied) */
            enum conv_type   conv;  /**< kind of dealing (if not implied) */
        } code;
        union {
            insn_setops      setops;  /**< setting operands (conv_setops) */
            insn_emit        emit;    /**< dedicated emitting (conv_emit) */
            const char       *string; /**< instruction as string (conv_warn) */
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
        INSN_BIN( MULU            , MULT                , insn_setops_binop   ),
        INSN_BIN( MULS            , MULT                , insn_setops_binop   ),
        INSN_BIN( DIVU            , TRUNC_DIV           , insn_setops_binop   ),
        INSN_BIN( DIVS            , TRUNC_DIV           , insn_setops_binop   ),
        INSN_BIN( MODU            , TRUNC_MOD           , insn_setops_binop   ),
        INSN_BIN( MODS            , TRUNC_MOD           , insn_setops_binop   ),
        INSN_BIN( SHL             , LSHIFT              , insn_setops_binop   ),
        INSN_BIN( LSR /*unsigned*/, RSHIFT              , insn_setops_binop   ),
        INSN_BIN( ASR /*signed  */, RSHIFT              , insn_setops_binop   ),
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
        INSN_BIN( SET_B           , LT                  , insn_setops_binop   ),
        INSN_BIN( SET_A           , GT                  , insn_setops_binop   ),
        INSN_BIN( SET_BE          , LE                  , insn_setops_binop   ),
        INSN_BIN( SET_AE          , GE                  , insn_setops_binop   ),
        /* Uni */
        INSN_UNI( NOT             , BIT_NOT             , insn_setops_unop    ),
        INSN_UNI( NEG             , MINUS               , insn_setops_unop    ),
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
    ASSERT_ENUM_RANGE(OP_BADOP, insn->opcode, OP_COPY);

    struct cl_insn cli;
    const struct insn_conversion *conversion;

    if (!insn->bb)
        return ret_negative;  /* zombie instruction */

    DEBUG_INSN_SP(insn);

    do {

        conversion = &insn_conversions[insn->opcode];
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
                DEBUG_INSN_CL_SPECIAL(_1(s), "(ignored)");
                return ret_negative;
            case conv_setops:
                cli.code = conversion->insn_code;
                if (conversion->prop.setops(&cli, &insn)) {
                    DEBUG_INSN_CL(&cli);
                    API_EMIT(insn, &cli);
                }
                /*
                    this case may loop repeatedly, depending on whether
                    the conversion is 1:1 or 1:M
                    (e.g., x = ptr - i  ::=  i' = -i;  x = ptr POINTER_PLUS i')
                 */
                break;
            case conv_emit:
                /* (initial) instruction position is set */
                conversion->prop.emit(&cli, insn);
                return CL_INSN_ABORT == cli.code ? ret_escape : ret_positive;
            case conv_warn:
            default:
                WARN_UNHANDLED(insn->pos, conversion->prop.string);
                return ret_negative;
        }

    } while (insn);

    return ret_positive;
}


/** top-level of emitting control *****************************************/


/**
    Take care of function payload (arguments and definition -- linearized code)

    @return False to force exit (interactive quit), true otherwise
 */
static bool
emit_function_payload(struct entrypoint *ep, int *emit_props)
{
    pseudo_t arg;
    struct basic_block *bb;
    struct instruction *insn;
    struct cl_insn cli;
    struct cl_operand *op_arg;

#if DO_PER_EP_SET_UP_STORAGE
    /* storage pass: we don't need storage analysis (seems incomplete anyway) */
    WITH_PASS(storage, ep)
#endif
        /* unSSA pass: we currently rely on it */
        WITH_PASS(unssa, ep) {

            /* function arguments */
            FOR_EACH_PTR(ep->entry->arg_list, arg) {
                DEBUG_FNC_ARG_HEADER(arg->nr);
                op_arg = op_from_pseudo(ep->entry, arg);
                API_EMIT(fnc_arg_decl, arg->nr, op_arg);
            } END_FOR_EACH_PTR(arg);

            /* tag BBs with IDs (used for labels later on) */
            COUNTER(bb) = 0;  /* zero (unused) denotes unexpected BB later on */
            FOR_EACH_PTR(ep->bbs, bb) {
                assert(bb->priv == (uintptr_t) 0);
                *(uintptr_t *)(&bb->priv) = ++COUNTER(bb);
            } END_FOR_EACH_PTR(bb);

            /* function definition (with "ep -> bb -> instruction" progress) */

            /* jump to entry BB */
            conv_position(&cli.loc, &ep->entry->pos);
            insn_emit_jmp(&cli, BB_LABEL(ep->entry->bb));

            FOR_EACH_PTR(ep->bbs, bb) {
                assert(bb /*&& bb->parents && bb->children*/);

                if (ptr_list_empty(bb->insns))
                    continue;

                emit_bb_open(BB_LABEL(bb));

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
                            /* instruction ignored */
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
    struct cl_operand *fnc_op;

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

                WITH_DEBUG_LEVEL(d_symb)
                    debug_sparse_symbol_detailed(sym, 8/INDENT_MULT);

                if (!ep || is_private)
                    continue;

                fnc_op = op_from_symbol(sym);
                WITH_FUNCTION_TO_EMIT(fnc_op, sym->endpos)
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

        DLOG(d_misc,
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

    WITH_DEBUG_LEVEL(d_allo) {
        PUT(debug, "\t" HIGHLIGHT("sparse allocators:"));
        sparse_alloc_show();
        PUT(debug, "\t" HIGHLIGHT("local allocators:"));
        local_alloc_show();
    }

    return errors ? ret_escape : ret_positive;
}
