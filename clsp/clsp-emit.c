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
#include "clsp-types.h"


/*
    compile options
 */

/* general */
#define DO_EXTRA_CHECKS              0
#define USE_EXTENDED_TYPE_CMP        0
#define SHOW_PSEUDO_INSNS            0

/* sparse */
#define FIX_SPARSE_EXTRA_ARG_TO_MEM  0
#define USE_STRINGS_DIRECTLY         1
#define FNC_RESET_BB_CNT             1

/* assertions */
#define ASSERT_SYMBOL_BELONGS_TO_CURRENT_FILE   0
#define ASSERT_CST_HAVE_VALID_CODE              1


/* if (!emit_props & emit_skip_initial), use this name for "initial" symbols */
static const char *const initial_file = "<initial-metafile>";
static struct instruction *last_insn;


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

#define DEBUG_CALL_ARG_HEADER(arg_id, op)                                     \
    WITH_DEBUG_LEVEL(d_insn) {                                                \
        DLOG(d_insn,                                                          \
             "\tdebug: " HIGHLIGHT("instruction") ": function call argument " \
             HIGHLIGHT(_1(d)),                                                \
             arg_id);                                                         \
        debug_cl_operand((op), GLOBALS(indent), false);                       \
    }

#define DEBUG_SWITCH_CASE_HEADER(loc, val_lo, val_hi, label)     \
    WITH_DEBUG_LEVEL(d_insn) {                                   \
        PUT(debug,                                               \
            CLPOSFMT_1 ": debug: " HIGHLIGHT("instruction")      \
            ": switch-case: target " HIGHLIGHT("["_4(s)"]"),     \
            CLPOS(*loc), label);                                 \
        debug_cl_switch_case(val_lo, val_hi, GLOBALS(indent)+1); \
    }

#define WITH_FILE_TO_EMIT(file, symref, private)                               \
    for (int i_=0; i_==0                                                       \
        ? (                                                                    \
          (initial_file == file                                                \
            ? DLOG(d_file, "\n"_1(s)": debug: " HIGHLIGHT("file") ": begin"    \
                           " (aggregation of various sources)", file)          \
            : DLOG(d_file, "\n" SPPOSFMT_1 ": debug: " HIGHLIGHT("file")       \
                           ": begin (first symbol)",                           \
                           SPPOS((symref)->pos))),                             \
          private ? (void) 0 : API_EMIT(file_open, file),                      \
          1                                                                    \
        ) : (                                                                  \
          private ? (void) 0 : API_EMIT(file_close),                           \
          (initial_file == file                                                \
            ? DLOG(d_file, _1(s)": debug: " HIGHLIGHT("file") ": end\n", file) \
            : DLOG(d_file, SPPOSFMT_1 ": debug: " HIGHLIGHT("file")            \
                           ": end (last symbol)\n",                            \
                           SPPOS((symref)->endpos))),                          \
          0                                                                    \
        ) ; i_++)

#define WITH_CALL_TO_EMIT(loc, dst, fnc)                                       \
    for (int i_=0; 0==i_                                                       \
         ? (                                                                   \
           DLOG(d_insn, CLPOSFMT_1 ": debug: " HIGHLIGHT("instruction")        \
                ": cl <<< call begin", CLPOS(*loc)),                           \
           API_EMIT(insn_call_open, loc, dst, fnc),                            \
           1                                                                   \
         ) : (                                                                 \
           API_EMIT(insn_call_close),                                          \
           DLOG(d_insn, CLPOSFMT_1 ": debug: " HIGHLIGHT("instruction")        \
                ": cl <<< call end", CLPOS(*loc)),                             \
           0                                                                   \
         ) ; i_++)

#define WITH_SWITCH_TO_EMIT(loc, op)                                           \
    for (int i_=0; 0==i_                                                       \
         ? (                                                                   \
           DLOG(d_insn, CLPOSFMT_1 ": debug: " HIGHLIGHT("instruction")        \
                ": cl <<< switch begin", CLPOS(*loc)),                         \
           API_EMIT(insn_switch_open, loc, op),                                \
           1                                                                   \
         ) : (                                                                 \
           API_EMIT(insn_switch_close),                                        \
           DLOG(d_insn,                                                        \
                "\tdebug: " HIGHLIGHT("instruction") ": cl <<< switch end"),   \
           0                                                                   \
         ) ; i_++)


#define WITH_FUNCTION_TO_EMIT(fnc, endpos)                                     \
    for (int i_=0; 0==i_                                                       \
        ? (                                                                    \
          DLOG(d_func,                                                         \
               "\n" CLPOSFMT_1 ": debug: " HIGHLIGHT("function") ": begin "    \
               HIGHLIGHT(_4(s)),                                               \
               CLPOS(CST_FNC(fnc)->loc), CST_FNC(fnc)->name),                  \
          API_EMIT(fnc_open, fnc),                                             \
          1                                                                    \
        ) : (                                                                  \
          API_EMIT(fnc_close),                                                 \
          DLOG(d_func,                                                         \
               SPPOSFMT_1 ": debug: " HIGHLIGHT("function") ": end "           \
               HIGHLIGHT(_4(s)) "\n", SPPOS(endpos), CST_FNC(fnc)->name),      \
          0                                                                    \
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

/**
    Get under SYM_NODE, fill some type info if still missing
 */
static inline struct symbol *
type_unwrap(struct symbol *raw_type)
{
    assert(raw_type);

    struct symbol *retval;

    /* very important, otherwise some info may be missing */
    SP(examine_symbol_type, /*out*/ retval, /*in*/ raw_type);

    while (retval->type == SYM_NODE
           || retval->type == SYM_BITFIELD
           /*||retval->type == SYM_ENUM */)
        retval = retval->ctype.base_type;

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

    if (CL_TYPE_STRING == t1->code && CL_TYPE_PTR == t2->code
      && CL_TYPE_CHAR == t2->items->type->code)
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

static bool
read_and_append_subtypes(struct cl_type *clt, struct symbol_list *subtypes)
{
    struct symbol *subtype = NULL;

    FOR_EACH_PTR(subtypes, subtype)
        read_and_append_subtype(clt, subtype);
    END_FOR_EACH_PTR(subtype);

    return !!subtype;
}

/**
    Read type of a function

    XXX OP_CALL: sym->fntype
 */
static inline void
read_type_fnc(struct cl_type *clt, const struct symbol *raw_symbol,
              const struct symbol *type)
{
    /* return value */
    read_and_append_subtype(clt, type->ctype.base_type);
    /* arguments */
    if (!read_and_append_subtypes(clt, type->arguments))
        /* add artificial void as the only argument (as gccplug) */
        read_and_append_subtype(clt, &void_ctype);
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

#if 0
    if (type->ctype.modifiers & MOD_USERTYPE)
        CL_TRAP;
#endif

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

/**
    Get type from register, return NULL if cannot be (reliably) found
 */
static struct cl_type *
type_from_register(const pseudo_t pseudo)
{
    assert(PSEUDO_REG == pseudo->type);

    struct cl_type *clt;
    struct pseudo_user *pu;
    struct symbol *type = NULL;

    enum opcode opcode = OP_NOP;
    if (pseudo->def)
        opcode = pseudo->def->opcode;

    switch (opcode) {
        case OP_SYMADDR:
            clt = type_from_symbol(pseudo->def->symbol->sym, NULL);
            return build_referenced_type(clt);

        default:
            if (pseudo->def->type)
                return type_from_symbol(pseudo->def->type, NULL);
            type = &int_ctype;
            /* FALLTHROUGH */

        case OP_SET_EQ:
        case OP_SET_NE:
        case OP_SET_LE:
        case OP_SET_GE:
        case OP_SET_LT:
        case OP_SET_GT:
        case OP_SET_B:
        case OP_SET_A:
        case OP_SET_BE:
        case OP_SET_AE:
            if (!type)
                type = &bool_ctype;
            /* FALLTHROUGH */

        case OP_NOP: {
            /*
                pretty accurate way of getting the output type (type of target
                pseudo) when not properly available by some instructions
                (binary comparisons) or instruction is not directly
                available from pseudo->def (OP_COPY)

                check whether immediate user (instruction) of target pseudo
                has "authority to decide the type" (casts, OP_RET, OP_COPY)

                it is desired to have the pseudo type correct from the beginning,
                but also as we cache the first guess of pseudo conversion, it is
                an imperative to do this forward analysis (currently of depth 1)

                note: the target of CALL may not be used at all (no users), still
                      it (probably) cannot be removed by sparse optimizations

                XXX OP_CALL: sym->fntype
             */
            FOR_EACH_PTR(pseudo->users, pu) {
                if (!pu->insn->bb)  /* "zombie" instruction */
                    continue;
                switch (pu->insn->opcode) {
                    case OP_CAST:
                    case OP_SCAST:
                    case OP_FPCAST:
                    case OP_PTRCAST:
                        type = pu->insn->orig_type;
                        goto done;
                    case OP_RET:  /* the only operand, the type has to be right */
                        type = pu->insn->type;
                        goto done;
                    case OP_COPY:
                        /* OP_COPY needs upstream patch:
                           http://git.kernel.org/?p=devel/sparse/chrisl/
                           sparse.git;a=commitdiff;h=db72a46 */
                        /*
                            evaluate transitively as per target, but not if the user
                            is another OP_COPY (was causing infloop recursion)
                         */
                        if (!pu->insn->target->def
                          || OP_COPY != pu->insn->target->def->opcode)
                            return type_from_register(pu->insn->target);
                        /* see if there is something better (?) */
                        type = pu->insn->type;
                        break;
                    default:
                        /* other instructions may have an incorrect type as well (?) */
                        break;
                }
            } END_FOR_EACH_PTR(pu);
done:
            if (type)
                return type_from_symbol(type, NULL);
            CL_TRAP;
        }
    }
    CL_TRAP;
    return NULL;
}


/** operands handling *****************************************************/


static inline bool
op_accessible(const struct cl_operand *op)
{
    if (op->code == CL_OPERAND_VOID)
        return false;

    if (op->accessor)
        return true;

    switch (op->type->code) {
        case CL_TYPE_STRUCT:
        case CL_TYPE_UNION:
        case CL_TYPE_ARRAY:
        case CL_TYPE_PTR:
        case CL_TYPE_FNC:
            return true;
        default:
            return false;
    }
}

static inline bool
type_composite(enum cl_type_e type)
{
    switch (type) {
        case CL_TYPE_STRUCT:
        case CL_TYPE_UNION:
        case CL_TYPE_ARRAY:
        /* CL_TYPE_PTR + CL_TYPE_FNC omitted */
        case CL_TYPE_STRING:
            return true;
        default:
            return false;
    }
}

static inline bool
type_primitive(enum cl_type_e type)
{
    switch (type) {
        case CL_TYPE_INT:
        case CL_TYPE_BOOL:
            return true;
        default:
            return false;
    }
}

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

    if ((copy_shallow_var_deep == copy_depth
      || copy_shallow_var_deep_uid == copy_depth)
      && CL_OPERAND_VAR == op_src->code) {

        assert(!ret->accessor);

        ret->data.var = alloc_cl_var();
        *ret->data.var = *op_src->data.var;
        if (copy_shallow_var_deep_uid == copy_depth) {
            VAR(ret)->uid = ++COUNTER(var);
            /* drop also identifier if present */
            VAR(ret)->name = NULL;
        }

    }

    return ret;
}


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
    CST_FNC(op)->is_extern = !sym->definition;  /* XXX multiple files */
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
    op->type = &string_clt;
    /*
        this generates array of char:
        op->type = type_from_symbol(expr->ctype, NULL);
     */

    CST_STR(op)->value = conv_string(expr->string);

    return op;
}


/*
    variable operand
 */

/**
    Generic variable operand constructor (half-way)

    Implicit defaults:
        initialized = false

    This needs to be defined:
        op:       scope, type (initially NULL)
        VAR(op):  loc
 */
static inline struct cl_operand *
op_make_var(const char *ident)
{
    struct cl_operand *op = alloc_cl_operand();
    op->code = CL_OPERAND_VAR;

    VAR(op) = alloc_cl_var();
    VAR(op)->uid = ++COUNTER(var);  /* starting with 1 looks better */
    VAR(op)->name = ident;
    VAR(op)->artificial = !ident;

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


static inline struct cl_operand *op_ref(struct cl_operand* op);

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
#if USE_STRINGS_DIRECTLY
    else if (EXPR_STRING == expr->type)
        return op_make_cst_string(expr);
#endif
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

static struct instruction *
insn_setops_binop(struct cl_insn *cli, const struct instruction *insn);

static unsigned
op_dig_step(const struct cl_operand **op_composite, unsigned insn_offset);

static inline struct instruction *
insn_setops_store(struct cl_insn *cli, const struct instruction *insn);


/**
    Make sparse linearize ad-hoc symbol initializer

    We have to mock environment for sparse before calling
    linearize_expression yielding the required instructions.

    XXX this or similar function should really go to upstream
 */
static struct instruction_list *
linearize_symbol_initializer(struct symbol *sym)
{
    struct expression *expr;
    unsigned long modifiers_backup = sym->ctype.modifiers;
    struct entrypoint *ep = __alloc_entrypoint(0);

    ep->active = __alloc_basic_block(0);
    ep->active->ep = ep;

    expr = alloc_expression(sym->initializer->pos, EXPR_SYMBOL);
    expr->symbol = sym;

    /* trick sparse to accept this symbol for initialization */
    assert(!sym->pseudo);
    sym->ctype.modifiers &= ~(MOD_STATIC | MOD_TOPLEVEL);

    SP(linearize_expression, ep, expr);

    sym->ctype.modifiers = modifiers_backup;
    return ep->active->insns;
}

/**
    Initialize var operand from sparse symbol initializer

    This is primarily for static/extern globals which apparently could
    not be linearized in per-entrypoint pass because they are not local
    to particular entry point, or for structures that (for some reason)
    were not linearized despite being direct part of the function.

    We make a fake entry point and force sparse to linearize this
    initializator and proceed resulting instruction to build real
    CL initializator.

    [ Per function initialized variables have the initializator
    spread directly in instruction stream (STORE instructions) and thus
    CL will receive it in such a form as well ]

    This should never get into recursion as it is prevented early
    in @c op_from_symbol, looking at sym->pseudo we set here.


    @param[in,out] op    Operand to initialize
    @param[in]     sym   Initializator of this symbol to be examined
    @return  Value to be set to VAR(op)->initialized
 */
static bool
op_initialize_var_from_initializer(struct cl_operand *op,
                                   struct symbol *sym)
{
    assert(sym->initializer);
    assert(sym->ctype.modifiers & (MOD_STATIC | MOD_TOPLEVEL)
           /*|| SYM_STRUCT == sym->ctype.base_type->type*/);

    struct instruction *insn;
    struct cl_initializer **initial = &VAR(op)->initial;
    struct instruction_list *insns = linearize_symbol_initializer(sym);

    unsigned offset = 0;
    struct cl_operand *from = NULL;

    /*
        sym->pseudo is set by our call to linearize_expression and kept around,
        so we use its priv to set the operand being created right now
        here to avoid recursive infloop/duplicated variables (which is bad)
        when converting initializer operands as it contains self-references
     */
    assert(sym->pseudo && !sym->pseudo->priv && PSEUDO_VAL != sym->pseudo->type);
    sym->pseudo->priv = (void *) op;

    DEBUG_INITIALIZER_EXPR_START();

    FOR_EACH_PTR(insns, insn) {

        DEBUG_INITIALIZER_EXPR_SP(insn);
        switch (insn->opcode) {
            case OP_STORE:
                /* XXX recursion (orig. pseudo can be passed and precached) */
                assert(PSEUDO_SYM == insn->src->type
                       && insn->src->sym == sym);
                *initial = alloc_cl_initializer_safe();
                (*initial)->insn.code = CL_INSN_UNOP;
                UNOP(&(*initial)->insn)->code = CL_UNOP_ASSIGN;
                conv_position(&(*initial)->insn.loc, &insn->pos);
                if (insn_setops_store(&(*initial)->insn, insn))
                    assert(0);  /* multi-phased instruction? */
                assert(CL_INSN_NOP != (*initial)->insn.code);

                /*
                    use direct reference to operand we built along
                    OP_ADD instructions incl. corresponding accessors
                    (instead of temporary registers used for the same
                    phased-access reasons)
                 */
                if (from) {
                    assert(op_accessible(from));
                    UNOP(&(*initial)->insn)->src = op_ref(from);
                }

                offset = 0;
                from = NULL;
                break;
            case OP_SYMADDR:
                /*
                    take a reference of a symbol and prepare such referenced
                    operand in cache
                 */
                from = op_from_symbol(insn->symbol->sym);
                DEBUG_INITIALIZER_EXPR_CL_SPECIAL(_1(s),
                                                  "(only base operand taken)");
                continue;  /* no instruction to add to initializer */
            case OP_ADD:
                assert(PSEUDO_VAL == insn->src2->type);
                offset += insn->src2->value;
                offset = op_dig_step(&from, offset);
                assert(UINT_MAX != offset);
                DEBUG_INITIALIZER_EXPR_CL_SPECIAL(_1(s),"(offset incremented)");
                continue;  /* no instruction to add to initializer */
            case OP_PTRCAST: /* test 92 */
                DEBUG_INITIALIZER_EXPR_CL_SPECIAL(_1(s), "(ignored)");
                continue;  /* no instruction to add to initializer */
            default:
                CL_TRAP;
                WARN_UNHANDLED(insn->pos, "initializer instruction");
                continue;
        }
        DEBUG_INITIALIZER_EXPR_CL(*initial);

        initial = &(*initial)->next;  /* move onto the next initializer item */

    } END_FOR_EACH_PTR(insn);

    DEBUG_INITIALIZER_EXPR_STOP();

    return false;  /* initialization implied by the scope */
}

static inline struct cl_accessor *
op_append_accessor(struct cl_operand *op, struct cl_accessor *ac);

/**
    Try to initialize operand using symbol's initializer

    @return  Value to be set to VAR(op)->initialized
 */
static bool
op_initialize_var_maybe(struct cl_operand *op, struct symbol *sym)
{
    assert(CL_OPERAND_VAR == op->code);

    struct expression *expr = sym->initializer;
    struct cl_initializer **initial;
    struct cl_operand *from;

    struct cl_accessor *ac;

    if (!expr || sym->initialized)
        /*
            initialization of static/extern globals implied by the scope
            anyway, for local ones, they are initialized along the
            instruction stream
         */
        return false;

    DEBUG_INITIALIZER_SP(expr);

    switch (expr->type) {
        case EXPR_VALUE:
        case EXPR_STRING:
            /*
                EXPR_INITIALIZER case cannot handle this correctly as it
                initializes char array by char array not ever boiling down
                to real string literal
             */
            VAR(op)->initial = alloc_cl_initializer_safe();
            VAR(op)->initial->insn.code = CL_INSN_UNOP;
            conv_position(&VAR(op)->initial->insn.loc, &expr->pos);

            UNOP(&VAR(op)->initial->insn)->code = CL_UNOP_ASSIGN;
            UNOP(&VAR(op)->initial->insn)->dst = op;
            UNOP(&VAR(op)->initial->insn)->src =
                op_from_primitive_literal(expr);
            break;

        case EXPR_PREOP: /*212*/
            assert('*' == expr->op && EXPR_SYMBOL == expr->unop->type);
            expr= expr->unop;
        case EXPR_SYMBOL:
            /*
                get operand for that symbol (good as we cache the resolution
                anyway), then use this symbol itself as source of
                initialization;  hopefully no recursion drama ahead
             */
            from = op_from_symbol(expr->symbol);

            if (CL_TYPE_STRING != from->type->code) {
                assert(!from->accessor);
                from = op_copy(from, copy_shallow_ac_deep);
                ac = op_append_accessor(from, NULL);
                ac->code = CL_ACCESSOR_REF;
                ac->type = from->type;
                from->type = build_referenced_type(from->type);
            }

            VAR(op)->initial = alloc_cl_initializer_safe();
            VAR(op)->initial->insn.code = CL_INSN_UNOP;
            conv_position(&VAR(op)->initial->insn.loc, &expr->pos);

            UNOP(&VAR(op)->initial->insn)->code = CL_UNOP_ASSIGN;
            UNOP(&VAR(op)->initial->insn)->dst  = op;
            UNOP(&VAR(op)->initial->insn)->src  = from;

            break;

        case EXPR_INITIALIZER:
            /* no need to "debug" the same over again */
            return op_initialize_var_from_initializer(op, sym);

        default:
            CL_TRAP;
            WARN("unhandled initializer expression type");
            return false;
    }

    DEBUG_INITIALIZER_CL(VAR(op)->initial);

    /* XXX empty initializer chain for local variables */
    return sym->ctype.modifiers & (MOD_STATIC | MOD_TOPLEVEL);
}

/**
    Operand from symbol
 */
static inline struct cl_operand *
op_from_symbol(struct symbol *sym)
{
    struct cl_operand *op = NO_OPERAND_USE;

    /* try to lookup in cache */
    if (sym->pseudo && sym->pseudo->priv)
        /*
            local symbol with initializers;
            thanks to cache being held by pseudo, it can be look up also
            by this pseudo directly
         */
        op = (struct cl_operand *) sym->pseudo->priv;
    else if (sym->aux)
        /* already linearized function (incl. current one) or static symbol */
        op = (struct cl_operand *) sym->aux;


    DEBUG_OP_FROM_SYMBOL_SP(sym);

    if (NO_OPERAND_USE != op) {
        DEBUG_OP_FROM_SYMBOL_CL(sym, op);
        return op;
    }

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
        op = op_make_var(sparse_ident(sym->ident, NULL));

    /* yet uninitialized or get it right for integral type (signedness), etc. */
    op->scope = conv_scope(sym);
    if (!op->type || CL_TYPE_STRING != op->type->code)
        op->type = type_from_symbol(sym, NULL);

    if (CL_OPERAND_VAR == op->code) {
        conv_position(&VAR(op)->loc, &sym->pos);
        VAR(op)->initialized = op_initialize_var_maybe(op, sym);
    }

    DEBUG_OP_FROM_SYMBOL_CL(sym, op);

    if (sym->ctype.modifiers & (MOD_STATIC | MOD_TOPLEVEL))
        sym->aux = (void *) op;

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
    assert(CL_OPERAND_VAR == op->code);
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
    struct cl_operand *op = op_make_var(sparse_ident(pseudo->ident, NULL));

    op->scope = CL_SCOPE_FUNCTION;
    op->type = type_from_register(pseudo);

    VAR(op)->artificial = !VAR(op)->name;
    conv_position(&VAR(op)->loc, &insn->pos);

    return op;
}

/**
    Operand from integral value
 */
static inline struct cl_operand *
op_from_intval(const struct instruction *insn, int value)
{
    struct cl_operand *op = op_make_cst_int(value);

    if (insn && insn->type) {

        /* better type guess from instruction context */
        struct cl_type *type = type_from_symbol(insn->type, NULL);
        if (type_primitive(type->code))
            op->type = type;
    }

    return op;
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

    if (!pseudo) {
        op = NO_OPERAND_USE;
    } else {
        if (pseudo->priv) {
            /* cached */
            DEBUG_OP_FROM_PSEUDO_CACHE(pseudo);
            return (struct cl_operand *) pseudo->priv;
        }

        switch (pseudo->type) {
            case PSEUDO_VOID: op = NO_OPERAND_USE; break;
            case PSEUDO_SYM: op = op_from_symbol(pseudo->sym);     break;
            case PSEUDO_ARG: op = op_from_fnc_argument(pseudo);    break;
            case PSEUDO_REG: op = op_from_register(insn, pseudo);  break;
            case PSEUDO_VAL:
                op = op_from_intval(insn, (int) pseudo->value);
                break;
            case PSEUDO_PHI:
            default:
                WARN("unhandled pseudo: from instruction " HIGHLIGHT(_1(s)),
                     show_instruction((struct instruction *) insn));
                return pseudo->priv = NO_OPERAND_USE;
        }
    }

    if (NO_OPERAND_USE != op) {
        if (PSEUDO_VAL != pseudo->type)
            pseudo->priv = op;  /* cache it */
        if (PSEUDO_SYM == pseudo->type || PSEUDO_ARG == pseudo->type)
            return op;  /* skip debug printout, already done */
    }

    DEBUG_OP_FROM_PSEUDO_SP(pseudo);
    DEBUG_OP_FROM_PSEUDO_CL(op);

    return op;
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


/**
    Wrapper for @c op_from_pseudo when we want operand "directly" (copy, casts)
 */
static inline struct cl_operand *
op_from_pseudo_ref(const struct instruction *insn, const pseudo_t pseudo)
{
    struct cl_operand *ret = op_from_pseudo(insn, pseudo);

    if (PSEUDO_SYM == pseudo->type && op_accessible(ret))
        ret = op_ref(ret);

    return ret;
}

/**
    Make operand a reference if suitable

    100 % sane usage is for PSEUDO_SYM + op_accessible(op), but the use is
    wider (e.g., STORE used always referenced operands).
 */
static inline struct cl_operand *
op_ref(struct cl_operand* op)
{
    /* assert(!op->accessor);  <-- not for initializer  */
    op = op_copy(op, copy_shallow_ac_deep);
    struct cl_accessor *ac = op_append_accessor(op, NULL);

    ac->code = CL_ACCESSOR_REF;
    ac->type = op->type;
    op->type = build_referenced_type(op->type);

    return op;
}

/**
    Cancel the effect of selected accessor if it is the last one
 */
static inline struct cl_operand *
op_cancel_ac(struct cl_operand *op, enum cl_accessor_e ac_code)
{
    if (!op->accessor)
        return op;

    struct cl_operand *ret = op_copy(op, copy_shallow_ac_deep);
    struct cl_accessor **seek = &ret->accessor;

    while ((*seek)->next)
        seek = &(*seek)->next;

    if (*seek && ac_code == (*seek)->code) {
        assert(ret->type != (*seek)->type);
        ret->type = (*seek)->type;
        *seek = NULL;
    } else {
        assert(ret->type != (*seek)->type);
    }

    return ret;  /* one can compare ret->type and op->type */
}


static inline void
accessor_array_index(struct cl_accessor *ac, int index)
{
    ac->code             = CL_ACCESSOR_DEREF_ARRAY;
    ac->data.array.index = op_make_cst_int(index);
}


/**
    Perform one step of digging into type

    @return @c UINT_MAX when operand could not be dug, or remaining offset
 */
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
    struct cl_operand *clone = NULL;

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
            accessor_array_index(ac, indexes.quot);
            // the remainder serves for next index-based-dereferencing rounds
            retval = indexes.rem;
            break;
        }
        MAP_ACCESSOR(ac, TYPE_PTR, ACCESSOR_DEREF) {

            if (insn_offset && op->type->items->type->size) {
                // convert into another accessor then predestined (ptr->arr),
                // but only if resulting index would be 1+
                div_t indexes = div(insn_offset, op->type->items->type->size);
                if (indexes.quot)
                    accessor_array_index(ac, indexes.quot);
                // the remainder serves for next index-based-deref. rounds
                retval = indexes.rem;
            }

            /* {ref, deref} = {} */
            clone = op_cancel_ac(op, CL_ACCESSOR_REF);
            if (clone->type != op->type) {
                /* success */
                *op_composite = clone;
                return retval;
            }
            clone = NULL;

            break;
        }
        default:
            return UINT_MAX;
    }

    if (!clone)
        clone = op_copy(op, copy_shallow_ac_deep);

    op_append_accessor(clone, ac);
    // accessor's type is the operand's type (it itself will be peeled off)
    ac->type = (struct cl_type *) clone->type;
    // peel off one level of type/access decoration from the operand
    clone->type = (struct cl_type *) clone->type->items[i].type;

    *op_composite = clone;

    return retval;
}


/**
    Union-aware wrapper for @c op_dig_step

    Problems/exceptions/notes:
    When digging union, we go through its items, apply a DFS-based search
    in order to get expected type on one, if it ends without success, we try
    another (on the whole, should not end without success).
 */
static unsigned
op_dig_for_type_match(const struct cl_operand **op_composite,
                      const struct cl_type *expected_type,
                      unsigned initial_offset)
{
    unsigned offset = initial_offset;

    const struct cl_operand *op;

#define TRIES 32
    for (int i = 0; TRIES > i; i++) {

        op = *op_composite;
        if (type_match(op->type, expected_type))
            break;

        if (op->type->code == CL_TYPE_UNION) {
            /*
                unions bring non-determinism as there are more ways how to
                "dig" -- use DFS with a sort of backtracking (through stack)
             */
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
                    /* successfull case of digging */
                    break;

                /* restore for the next round */
                op_clone->accessor = NULL;
                op_clone->type = op->type;
            }

            if (UINT_MAX != res) {
                // reflect the changes collected within successful DFS trace
                // (with `op_clone') back to its preimage `op'
                assert(op_clone->accessor);
                assert(type_match(op_clone->type, expected_type));
                if (op->accessor)
                    op_prepend_accessor(op_clone, op->accessor);
                *op_composite = op_clone;
            }

            offset = res;

        } else {

            offset = op_dig_step(op_composite, offset);

        }

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

/**
    Adjust LOAD source operand
 */
static const struct cl_operand *
insn_assignment_load_from(const struct cl_operand *const op_rhs, pseudo_t rhs,
                          const struct cl_type *expected_type, int offset)
{
    struct cl_operand *backup, *ret = op_rhs;
    struct cl_accessor *ac;

    if (!type_match(op_rhs->type, expected_type) || PSEUDO_SYM != rhs->type) {

        if (!op_accessible(op_rhs) || PSEUDO_SYM != rhs->type) {

            /*
                use {ref, deref} = {} in case of prepared array dereference
                (see insn_setops_binop)
             */

            ret = op_cancel_ac(op_rhs, CL_ACCESSOR_REF);
            if (ret->type == op_rhs->type) {
                /* no success with unref */
                ret = op_copy(op_rhs, copy_shallow_ac_deep);
                ac = op_append_accessor(ret, NULL);
                ac->code = CL_ACCESSOR_DEREF;

                if (CL_TYPE_PTR != ret->type->code) {
                    ac->type = build_referenced_type(expected_type);
                    ret->type = expected_type;
                } else {
                    ac->type = ret->type;
                    ret->type = ret->type->items->type;
                }
            }

        } else {

            assert(!ret->accessor);
            ret = op_copy(ret, copy_shallow);  /* backup should be a copy */

        }

        backup = ret;
        if (op_dig_for_type_match((const struct cl_operand **) &ret,
                                  expected_type, offset)) {
            /* throw all effort so far, combine backup and CL_ACCESSOR_OFFSET */
            ret = backup;

            assert(ret->accessor && CL_ACCESSOR_DEREF == ret->accessor->code);
            ac = op_append_accessor(ret, NULL);
            ac->code = CL_ACCESSOR_OFFSET;
            ac->type = ret->accessor->type;
            ac->data.offset.off = offset;
        }
    }

    assert(ret);
    return ret;
}

/**
    Adjust STORE target operand

    Only when "storing" into symbol-like pseudo, we are done when
    the type matches, otherwise we have to either do a simple
    dereference or, together with non-matching symbol-like pseudo,
    dig into into composite type to find the match.
 */
static const struct cl_operand *
insn_assignment_store_to(const struct cl_operand *const op_lhs, pseudo_t lhs,
                         const struct cl_type *expected_type, unsigned offset,
                         const struct cl_insn *cli)
{
    struct cl_operand *ret = op_lhs;

    if (!type_match(op_lhs->type, expected_type) || PSEUDO_SYM != lhs->type) {

        if (!op_accessible(op_lhs) || PSEUDO_SYM != lhs->type) {

            if (CL_OPERAND_CST == op_lhs->code) {
                /* for some reason, constants cannot be deref'd directly */
                assert(CL_TYPE_INT == op_lhs->type->code);

                struct cl_operand *constant = op_copy(op_lhs, copy_shallow);

                ret = op_make_var(NULL);

                ret->code = CL_OPERAND_VAR;
                ret->scope = constant->scope;
                ret->type = constant->type;

                VAR(ret)->loc = cli->loc;
                VAR(ret)->initialized = true;

                VAR(ret)->initial = alloc_cl_initializer_safe();
                VAR(ret)->initial->insn.code = CL_INSN_UNOP;
                VAR(ret)->initial->insn.loc = cli->loc;

                UNOP(&VAR(ret)->initial->insn)->code = CL_UNOP_ASSIGN;
                UNOP(&VAR(ret)->initial->insn)->dst = ret;
                UNOP(&VAR(ret)->initial->insn)->src = constant;
            } else {
                ret = op_copy(op_lhs, copy_shallow);
            }

            struct cl_accessor *ac = op_append_accessor(ret, NULL);
            ac->code = CL_ACCESSOR_DEREF;

            if (CL_TYPE_PTR != ret->type->code) {
                ac->type = build_referenced_type(expected_type);
                ret->type = expected_type;
            } else {
                ac->type = ret->type;
                ret->type = ret->type->items->type;
            }
        }

        offset = op_dig_for_type_match((const struct cl_operand **) &ret,
                                       expected_type, offset);
        assert(0 == offset);
    }

    assert(ret);
    return ret;
}

/**
    Set operands for assigment with special treating of LOAD and STORE
 */
static void
insn_assignment_base(struct cl_insn *cli, const struct instruction *insn,
                     pseudo_t lhs, /* := */ pseudo_t rhs)
{
    struct cl_operand **op_lhs = (struct cl_operand **) &UNOP(cli)->dst,
                      **op_rhs = (struct cl_operand **) &UNOP(cli)->src;
    struct cl_type *expected_type;

    /* LHS */

    *op_lhs = op_from_pseudo(insn, lhs);

    if (OP_STORE == insn->opcode) {
        expected_type = type_from_symbol(insn->type, NULL);
        *op_lhs = insn_assignment_store_to(*op_lhs, lhs, expected_type,
                                           insn->offset, cli);
    }

    /* RHS */

    *op_rhs = op_from_pseudo(insn, rhs);

    if  ((OP_STORE == insn->opcode || OP_PTRCAST == insn->opcode)) {

#if 0
        if (!pseudo_immediate(rhs)
          && !op_dig_for_type_match(op_rhs, expected_type->items->type, 0))
            *op_rhs = op_ref(*op_rhs);
#endif
        if (PSEUDO_SYM == rhs->type)
            *op_rhs = op_ref(*op_rhs);
        else if (CL_TYPE_VOID == (*op_rhs)->type->code)
            /* void can only be obtained by dereference, cancel it */
            *op_rhs = op_cancel_ac(*op_rhs, CL_ACCESSOR_DEREF);


    } else if (OP_PTRCAST == insn->opcode) {

        if (PSEUDO_SYM == rhs->type /*&& op_accessible(op)*/)
            *op_rhs = op_ref(*op_rhs);

    } else if (OP_LOAD == insn->opcode) {

        expected_type = type_from_symbol(insn->type, NULL);
        *op_rhs = insn_assignment_load_from(*op_rhs, rhs, expected_type,
                                            insn->offset);
    }

    /* postprocess */

    /*  NULLs etc. on RHS come from typeless PSEUDO_VAL constants, so steal
        the LHS type, possibly promoting to pointers or, at least, "casting"
        to correct integral type (selected instructions only? */
    if (OP_STORE == insn->opcode || OP_COPY == insn->opcode) {
        if (CL_OPERAND_CST == (*op_rhs)->code
          && CL_TYPE_INT == (*op_rhs)->type->code)
            (*op_rhs)->type = (*op_lhs)->type;
    }

#if 0
    /* kind of workaround: use RHS type for LHS if it is currently VOID,
       should not be needed when sparse does not generate any "type gaps" */
    if (CL_TYPE_VOID == (*op_lhs)->type->code) {
        *op_lhs = op_copy(*op_lhs, copy_shallow);
        (*op_lhs)->type = (*op_rhs)->type;
    }
#endif

    /* finish */

    /*
        FIXME (SPARSE?):  sparse generates (due to memory model?) extra
        instruction, e.g. "store %arg1 -> 0[num]" in case of "num == %arg1"
     */
#if FIX_SPARSE_EXTRA_ARG_TO_MEM
    if (lhs->type != PSEUDO_SYM || rhs->type != PSEUDO_ARG
      || (*op_lhs)->data.var->uid != (*op_rhs)->data.var->uid)
#endif
        return cli;
#if FIX_SPARSE_EXTRA_ARG_TO_MEM
    WARN("instruction omitted: " HIGHLIGHT(_1(s)), show_instruction(insn));
    cli->code = CL_INSN_NOP;  /* nothing to emit */
#endif
}

/**
    Set operands for "load from memory to the new register" assignment
 */
static inline struct instruction *
insn_setops_load(struct cl_insn *cli, const struct instruction *insn)
{
    assert(PSEUDO_REG == insn->target->type);
    insn_assignment_base(cli, insn, insn->target, /* := */ insn->src);

    return NULL;
}

/**
    Set operands for "store indirectly whatever to whatever" assignment
 */
static inline struct instruction *
insn_setops_store(struct cl_insn *cli, const struct instruction *insn)
{
    insn_assignment_base(cli, insn, insn->src, /* := */ insn->target);

    return NULL;
}

/**
    Set operands for simple alias-making assignment

    This always generates artificial operand (but we keep the name).
 */
static inline struct instruction *
insn_setops_copy(struct cl_insn *cli, const struct instruction *insn)
{
    assert(PSEUDO_REG == insn->target->type);
    insn_assignment_base(cli, insn, insn->target, /* := */ insn->src);

    assert(CL_OPERAND_VAR == UNOP(cli)->dst->code);
    VAR(UNOP(cli)->dst)->artificial = true;

    return NULL;
}

/**
    Set operands for assignment with type casting

    In CL, casts are implicit part of assignments.

    If orig_type->bit_size > type->bit_size, something may be lost, but:
    (1) sparse would (?) warn about this if it is not intentional
    (2) sparse does char arithmetics by upcasting to integers first, then
        downcasting the result back to char, so this may be the case
        (carry bit is implictly lost anyway)
    (3) some dangerous castings may be spotted by the analyzers

    The opposite situation is, e.g., with bitfields casted to greater
    type (it might be suitable to zero extra bits due to per-byte
    type size granularity in CL, but this a marginal problem)

 */
static inline struct instruction *
insn_setops_cast(struct cl_insn *cli, const struct instruction *insn)
{
    insn_setops_copy(cli, insn);

    return NULL;
}

/**
    Set operands for assignment with pointer casting
 */
static inline struct instruction *
insn_setops_ptrcast(struct cl_insn *cli, const struct instruction *insn)
{
    insn_setops_copy(cli, insn);

    /* XXX check correctness of inferred type of operands (type, orig_type) */
    return NULL;
}

/**
    Set operands for (not-self-modifying) unary operations
 */
static inline struct instruction *
insn_setops_unop(struct cl_insn *cli, const struct instruction *insn)
{
    /* note: linearize.h talks about src, linearize.c uses src1 (aliasing) */
    insn_assignment_base(cli, insn, insn->target, /* := */ insn->src1);

    assert(!type_composite(UNOP(cli)->dst->type->code));
    assert(!type_composite(UNOP(cli)->src->type->code));
    return NULL;
}

/**
    Set operands for "primitive literal" assignment (cannot use common "base")
 */
static inline struct instruction *
insn_setops_setval(struct cl_insn *cli, const struct instruction *insn)
{
    UNOP(cli)->dst = op_from_pseudo(insn, insn->target);
    UNOP(cli)->src = op_from_primitive_literal(insn->val);

    return NULL;
}

/**
    Set operands for binary operations, taking care of pointer arithmetics
 */
static struct instruction *
insn_setops_binop(struct cl_insn *cli, const struct instruction *insn)
{
    const struct cl_type *t1, *t2;
    struct cl_operand *src1, *src2, *dst, *src1_u, *src2_u;
    struct cl_instruction *next;

    BINOP(cli)->dst  = dst  = op_from_pseudo(insn, insn->target);
    BINOP(cli)->src1 = src1 = op_from_pseudo_ref(insn, insn->src1);
    BINOP(cli)->src2 = src2 = op_from_pseudo_ref(insn, insn->src2);

    src1_u = op_cancel_ac(src1, CL_ACCESSOR_REF);
    src2_u = op_cancel_ac(src2, CL_ACCESSOR_REF);

    t1 = src1->type;
    t2 = src2->type;

    /*
        for pointer vs. non-pointer, there are some special cases
        (first comparison using "referenced" operands as the "ptrlike"
        membership is kept)
     */
    if (cl_type_ptrlike(t1->code) != cl_type_ptrlike(t2->code)) {
        if (CL_TYPE_INT == src1_u->type->code
          && CL_TYPE_ARRAY == src2_u->type->code) {
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
            assert(!src2_u->accessor);

            t1 = dst->type;  /* preserve for future use */

            dst = op_copy(src2_u, copy_shallow_var_deep);

            /* build the accessor chain from the back */

            struct cl_accessor *ac = op_append_accessor(dst, NULL);
            ac->code = CL_ACCESSOR_DEREF_ARRAY;
            ac->type = src2_u->type;

            struct instruction *def_offset = insn->src1->def;
            assert(OP_MULS == def_offset->opcode && def_offset->src1->priv);

            ac->data.array.index = def_offset->src1->priv;

            ac = op_append_accessor(dst, NULL);
            ac->code = CL_ACCESSOR_REF;
            ac->type = dst->type->items->type;

            dst->type = t1;

            /* set adjusted operand for load to see; do not emitting anything */
            insn->target->priv = dst;

            cli->code = CL_INSN_NOP;  /* nothing to emit */
            return NULL;

        } else if (CL_TYPE_INT == src2_u->type->code) {
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

                *next = *insn;                 /* instruction shallow copy */
                next->opcode = OP_ADD;

                *modified_src2 = *insn->src2;  /* int pseudo shallow copy */
                modified_src2->priv = UNOP(cli)->dst;

                next->src2 = modified_src2;
                return next; /* proceed the modified copy in next the round */

            } else if (CL_OPERAND_CST == BINOP(cli)->src2->code) {

                /* compare against NULL, etc. (valid to modify operand) */
                ((struct cl_operand *) BINOP(cli)->src2)->type
                    = BINOP(cli)->src1->type;

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

    return NULL;
}

/**
    Set operand for return statement instruction
 */
static inline struct instruction *
insn_setops_ret(struct cl_insn *cli, const struct instruction *insn)
{
    RET(cli)->src = op_from_pseudo_maybe(insn, insn->src);

    /* XXX type match */

    return NULL;
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
insn_emit_cond(struct cl_insn *cli, pseudo_t cond, struct cl_operand *op_cond,
               const char *then_label, const char *else_label)
{
    /*
        hack: add extra comparison before COND itself as predator
              cannot cope with any other COND use;  if the cond
              is defined by comparison instruction, there is a hope that
              such comparison directly preceeds this COND;
              predator will likely to fail otherwise, but
              (1) it may be fixed at some point
              (2) the counter-example has not been found so far

        we deliberately choose to use NE against 0 (keeping the
        order of labels as this keeps sense of original condition)
     */

    struct cl_operand *zero_comparee, *bool_cond = NULL;

    assert(cond->def);
    switch (cond->def->opcode) {
        case OP_SET_EQ:
        case OP_SET_NE:
        case OP_SET_LE:
        case OP_SET_GE:
        case OP_SET_LT:
        case OP_SET_GT:
        case OP_SET_B:
        case OP_SET_A:
        case OP_SET_BE:
        case OP_SET_AE:
            if (last_insn == cond->def) {
                bool_cond = op_cond;
                assert(&bool_clt == op_cond->type);
                break;
            }
            /* FALLTHROUGH */
        default:
            cli->code = CL_INSN_BINOP;
            BINOP(cli)->code = CL_BINOP_NE;

            bool_cond = op_copy(op_cond, copy_shallow_var_deep_uid);
            bool_cond->type = &bool_clt;
            BINOP(cli)->dst = bool_cond;

            BINOP(cli)->src1 = op_cond;

            zero_comparee = op_make_cst_int(0);
            zero_comparee->type = op_cond->type;
            BINOP(cli)->src2 = zero_comparee;

            DEBUG_INSN_CL(cli);
            API_EMIT(insn, cli);
    }

    cli->code = CL_INSN_COND;
    COND(cli)->src = bool_cond;
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

    UNOP(cli)->dst = op_from_pseudo(insn, lhs);
    UNOP(cli)->src = op_from_pseudo(insn, rhs);

    DEBUG_INSN_CL(cli);
    API_EMIT(insn, cli);
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
    struct cl_operand *op_arg;
    struct pseudo *arg;
    struct cl_type *clt;
    int cnt = 0;

    struct cl_operand *fnc = op_from_pseudo(insn, insn->func),
                      *target = op_from_pseudo_maybe(insn, insn->target);

    DEBUG_INSN_CL_SPECIAL("(call to "_1(s)")", CST_FNC(fnc)->name);

    WITH_CALL_TO_EMIT(&cli->loc, target, fnc)
        FOR_EACH_PTR(insn->arguments, arg) {
            ++cnt;
            op_arg = op_from_pseudo_ref(insn, arg);

            /* adjust type if we know it (not for varargs) */
            if (fnc->type->item_cnt > cnt) {
                clt = fnc->type->items[cnt].type;
                if (!type_match(op_arg->type, clt)) {
                    op_arg = op_copy(op_arg, copy_shallow);
                    op_arg->type = clt;
                }
            }

            DEBUG_CALL_ARG_HEADER(cnt, op_arg);
            API_EMIT(insn_call_arg, cnt, op_arg);
        } END_FOR_EACH_PTR(arg);


    /*
        special handling of non-returning function (forced BB termination);
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
        insn_emit_cond(cli, insn->cond, op_from_pseudo(insn, insn->cond),
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
    insn_emit_cond(cli, insn->src1, op_from_pseudo(insn, insn->src1),
                   bb_true, bb_false);

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
    Switch (incl. GNU C range extension as supported by sparse and also by CL)
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
                val_lo = op_make_cst_int(jmp->begin);
                val_lo->type = op->type;
                val_hi = val_lo;

                if (jmp->begin != jmp->end) {
                    /* actually a range */
                    val_hi = op_make_cst_int(jmp->end);
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
    typedef struct instruction *(*insn_setops)(
        struct cl_insn *,
        const struct instruction *);
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
        INSN_WRN( PHI             ,                     ,                     ),
        INSN_WRN( PHISOURCE       ,                     ,                     ),
        INSN_UNI( CAST            , ASSIGN              , insn_setops_cast    ),
        INSN_UNI( SCAST           , ASSIGN              , insn_setops_cast    ),
        INSN_IGN( FPCAST          , ASSIGN /*not sure*/ , insn_setops_copy    ),
        INSN_UNI( PTRCAST         , ASSIGN              , insn_setops_copy    ),
        INSN_IGN( INLINED_CALL    , /*lin. in-place */  ,                     ),
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
    struct instruction *current;

    if (!insn->bb)
        return ret_negative;  /* zombie instruction */

    DEBUG_INSN_SP(insn);

    do {
        current = insn;
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
            case conv_setops:
                /*
                    this case may cause repeating the loop, depending on
                    whether the conversion is 1:1 or 1:M
                    (e.g., x = ptr - i  ::=  i' = -i;  x = ptr POINTER_PLUS i')
                 */
                cli.code = conversion->insn_code;
                insn = conversion->prop.setops(&cli, insn);
                if (CL_INSN_NOP != cli.code) {
                    DEBUG_INSN_CL(&cli);
                    API_EMIT(insn, &cli);
                    break;
                }
                /* FALLTHROUGH (e.g., int + array(T) BINOP) */

            case conv_ignore:
                DEBUG_INSN_CL_SPECIAL(_1(s), "(ignored)");
                return ret_negative;

            case conv_emit:
                /* at least, initial instruction position may be used */
                cli.code = CL_INSN_NOP;
                conversion->prop.emit(&cli, insn);
                return CL_INSN_ABORT == cli.code ? ret_escape : ret_positive;

            case conv_warn:
            default:
                WARN_UNHANDLED(insn->pos, conversion->prop.string);
                return ret_negative;
        }

        last_insn = current;  /* history predator-cond-workaround */

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
#if FNC_RESET_BB_CNT
            COUNTER(bb) = 0;  /* zero (unused) denotes unexpected BB later on */
#endif
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

                assert(!sym->aux);  /* not already used (may be wrong) */
                sym->aux = fnc_op = op_from_symbol(sym);
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
