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


#define _GNU_SOURCE // asprintf, ...

#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <poll.h>
#include <errno.h>
#include <signal.h>
#include <sys/wait.h>
#include <assert.h>


#define USE_INT3_AS_BRK
#include "trap.h"
#include "code_listener.h"
#include "type_enumerator.h"

// sparse headers
#include "sparse/expression.h"
#include "sparse/linearize.h"
#include "sparse/scope.h"
//#include "sparse/flow.h"
//#include "sparse/parse.h"
//#include "sparse/storage.h"
//#include "sparse/symbol.h"
//#include "sparse/token.h"


// compile options
#define DO_FORK                     1  // "1" recommended

#define DO_PROCEED_INTERNAL         0
#define DO_EXPAND_SYMBOL            1
#define DO_PER_EP_UNSAA             1
#define DO_PER_EP_SET_UP_STORAGE    1

#define SHOW_PSEUDO_INSNS           0



//
// Common macros
//


#define MEM_NEW(type) \
    malloc(sizeof(type))

#define MEM_NEW_ARR(arr, num) \
    malloc(sizeof(*arr) * num)

#define MEM_RESIZE_ARR(arr, newnum) \
    realloc(arr, sizeof(*arr) * (newnum))

#ifndef STREQ
#   define STREQ(s1, s2) (0 == strcmp(s1, s2))
#endif

#define OPTPREFIX_SHORT  "-"
#define OPTPREFIX_LONG   "--"
#define OPTPREFIX_CL     "-cl-"

#define _OPTPREFIXEQ(check, const_prefix, optprefix) \
    strncmp(check, optprefix const_prefix, strlen(optprefix const_prefix)) \
    ? NULL : &check[strlen(optprefix const_prefix)]

#define OPTPREFIXEQ_SHORT(check, const_prefix) \
    (_OPTPREFIXEQ(check, const_prefix, OPTPREFIX_SHORT))

#define OPTPREFIXEQ_LONG(check, const_prefix) \
    (_OPTPREFIXEQ(check, const_prefix, OPTPREFIX_LONG))

#define OPTPREFIXEQ_CL(check, const_prefix) \
    (_OPTPREFIXEQ(check, const_prefix, OPTPREFIX_CL))

#define OPTVALUE(str) \
    ((*str == '=' && *str++ != '\0') ? str : NULL)


//
// ptr_db, for building pointer* hierarchy in order to prevent
// having two semantically same pointers as two different types
//


struct ptr_db_item {
    struct cl_type      *clt;
    struct ptr_db_item  *next;
};

struct ptr_db_arr {
    size_t              alloc_size;
    size_t              remain_size;
    size_t              pos;
    struct ptr_db_item  *heads;
};



//
// Globals
//


const char *GIT_SHA1 = "someversion";
//typedef struct typen_data *type_db_t;

static struct cl_code_listener *cl;

static struct type_ptr_db {
    struct typen_data  *type_db;
    struct ptr_db_arr  ptr_db;
    int                last_base_type_uid;
} type_ptr_db = {
    .type_db = NULL,
    .ptr_db  = { .alloc_size = 0, .remain_size = 0, .pos = 0, .heads = NULL },
    .last_base_type_uid = 0,
};
typedef struct type_ptr_db *type_ptr_db_t;

//static type_db_t type_db = NULL;
//static struct ptr_db_arr
FILE *real_stderr = NULL; /**< used to access "unfaked" stderr */


// associated with respective base types in populate_with_base_types()
static struct cl_type
    void_clt,
    incomplete_clt,
    bad_clt,
    int_clt,  sint_clt,  uint_clt,     short_clt, sshort_clt, ushort_clt,
    long_clt, slong_clt, ulong_clt,    llong_clt, sllong_clt, ullong_clt,
    // lllong_clt, slllong_clt, ulllong_clt
    char_clt, schar_clt, uchar_clt,
    bool_clt,
    float_clt, double_clt, ldouble_clt;



//
// Empty composite values
//


#define EMPTY_LOC  { .file = NULL, .line = -1, .column = -1, .sysp = false }

static const struct cl_operand empty_cl_operand = {
    .code     = CL_OPERAND_VOID,
    .loc      = EMPTY_LOC,
    .scope    = CL_SCOPE_GLOBAL,
    .type     = NULL,
    .accessor = NULL,
};
#define EMPTY_CL_OPERAND(clop)  do { *(clop) = empty_cl_operand; } while (0)

static const struct cl_type empty_cl_type = {
    .uid        = -1,  /**< normally set during the insertion into hash table */
    .code       = CL_TYPE_UNKNOWN,
    .loc        = EMPTY_LOC,
    .scope      = CL_SCOPE_GLOBAL,
    .name       = NULL,
    .size       = 0,
    .item_cnt   = 0,
    .items      = NULL,
    //.array_size = 0,
};
#define EMPTY_CL_TYPE(clt)  do { *(clt) = empty_cl_type; } while (0)


static int cl_verbose = 0;
#define CL_VERBOSE_LOCATION     (1 << 1)
#define CL_VERBOSE_INSTRUCTION  (1 << 2)
#define CL_VERBOSE_INSERT_TYPE  (1 << 3)



//
// Warnings, failures handling
//


#define WARN_UNHANDLED(pos, what) do { \
    warn(pos, "warning: '%s' not handled", what); \
    fprintf(real_stderr, \
            "%s:%d: note: raised from function '%s' [internal location]\n", \
            __FILE__, __LINE__, __FUNCTION__); \
} while (0)

#define WARN_UNHANDLED_SYM(sym) \
    WARN_UNHANDLED((sym)->pos, show_ident((sym)->ident))

#define WARN_VA(pos, fmt, ...) do {\
    warn(pos, "warning: " fmt, __VA_ARGS__); \
    fprintf(real_stderr, \
            "%s:%d: note: raised from function '%s' [internal location]\n", \
            __FILE__, __LINE__, __FUNCTION__); \
} while (0)

#define WARN_CASE_UNHANDLED(pos, what) \
    case what: WARN_UNHANDLED(pos, #what); break;


// Note: should be used only once these resources are initialized

#define NOKILL  (pid_t) 0

#define CLEANUP(pid, cl)  do {         \
        if (pid != NOKILL) kill(pid, SIGKILL);   \
        cl->destroy(cl);               \
        cl_global_cleanup();           \
    } while (0)

#define PERROR_EXIT(str, code) \
    do { perror(str); exit(code); } while (0)

#define PERROR_CLEANUP_EXIT(str, pid, cl, code) \
    do { perror(str); CLEANUP(pid, cl); exit(code); } while (0)

#define ERROR(...)  do {\
        fprintf(real_stderr, __VA_ARGS__); \
        fprintf(real_stderr, "\n"); \
    } while (0)

#define NOTE(...)  do {\
        fprintf(stdout, __VA_ARGS__); \
        fprintf(stdout, "\n"); \
    } while (0)


static void
warn(struct position pos, const char *fmt, ...)
{
    va_list ap;

    fprintf(real_stderr, "%s:%d: ", stream_name(pos.stream), pos.line);

    va_start(ap, fmt);
    vfprintf(real_stderr, fmt, ap);
    va_end(ap);

    fprintf(real_stderr, "\n");
}



//
// CL messaging
//


// FIXME: suboptimal interface of CL messaging
static int cnt_errors = 0;
static int cnt_warnings = 0;

static void dummy_printer(const char *msg)
{
    (void) msg;
}

static void trivial_printer(const char *msg)
{
    fprintf(real_stderr, "%s\n", msg);
}

static void cl_warn(const char *msg)
{
    trivial_printer(msg);
    ++cnt_warnings;
}

static void cl_error(const char *msg)
{
    trivial_printer(msg);
    ++cnt_errors;
}



//
// Freeing resources helper functions
//


static void
cb_free_clt(struct cl_type *clt)
{
    // skip base types that are not on heap
    if (clt->uid > type_ptr_db.last_base_type_uid) {
        enum cl_type_e code = clt->code;
        switch (code) {
            case CL_TYPE_PTR:
            case CL_TYPE_FNC:
                free(clt->items);
                break;

            // TODO
            default:
                break;
        }

        free((char *) clt->name);
        free(clt);
    }
}

static void
free_cl_cst_data(struct cl_operand *op)
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

static void
free_cl_operand_data(struct cl_operand *op)
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



//
// Auxiliary helper functions
//


static int
populate_with_base_types(type_ptr_db_t db);

static void
type_ptr_db_init(type_ptr_db_t db)
{
    db->type_db = typen_create(cb_free_clt);
    if (!db->type_db)
        die("ht_create() failed");

    db->last_base_type_uid = populate_with_base_types(db);
}

static void
type_ptr_db_destroy(type_ptr_db_t db)
{
    typen_destroy(db->type_db);
    free(db->ptr_db.heads);
}

static struct cl_type *
type_ptr_db_insert(type_ptr_db_t db, struct cl_type *clt, void *key, int uid)
#define PTRDBARR_SIZE  128
{
    if (verbose & CL_VERBOSE_INSERT_TYPE) {
        NOTE("add type (uid = %d, clt = %p): %p", uid, clt, key);
        show_symbol((struct symbol *)key);
        NOTE("---");
    }

    struct cl_type *retval = typen_insert_with_uid(db->type_db, clt, key, uid);
    if (!retval)
        die("typen_insert_with_uid() failed");

    if (uid == NEW_UID) {
        // track this really new type also in the pointer hierarchy
        // (at the base level, i.e. no pointer, and respective pointers
        // will be captured in connected singly-linked list)
        struct ptr_db_arr *ptr_db = &db->ptr_db;
        if (!ptr_db->remain_size) {
            ptr_db->alloc_size  += PTRDBARR_SIZE;
            ptr_db->remain_size += PTRDBARR_SIZE;
            ptr_db->heads = MEM_RESIZE_ARR(ptr_db->heads, ptr_db->alloc_size);
            if (!ptr_db->heads)
                die("MEM_RESIZE_ARR");
        }
        ptr_db->remain_size--;
        ptr_db->heads[ptr_db->pos].clt = clt;
        ptr_db->heads[ptr_db->pos].next = NULL;
        ptr_db->pos++;
    }

    // guaranteed to NOT return NULL
    return retval;
}

static bool
redefine_stderr(int target_fd, FILE **backup_stderr)
{
    if (backup_stderr) {
        *backup_stderr = fopen("/dev/stderr", "w");
        if (!*backup_stderr)
            return false;
        else
        setbuf(*backup_stderr, NULL);
    }

    if (close(STDERR_FILENO) == -1
        || dup2(target_fd, STDERR_FILENO) == -1)
        return false;
    else
        return true;
}



//
// Sparse generic helper functions
//


static void
read_location(struct cl_loc *cl_loc, struct position pos)
{
    cl_loc->file   = stream_name(pos.stream);
    cl_loc->line   = pos.line;
    cl_loc->column = pos.pos;
    cl_loc->sysp   = /* not used by SPARSE */ false;
}

static void
read_scope(enum cl_scope_e *cl_scope, struct scope *scope)
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
read_sparse_string(const struct string *str)
{
    return (str->length) ? strndup(str->data, str->length) : NULL;
}

static inline struct symbol *
get_instruction_type(struct instruction *insn)
{
    if (insn->opcode >= OP_BINCMP && insn->opcode <= OP_BINCMP_END)
        return &bool_ctype;
    else
        return insn->type;
}

static struct symbol *
get_arg_at_pos(struct symbol *fn, int pos)
{
    struct symbol *sym, *retval = NULL;

    if (pos <= 0)
        return NULL;

    // FIXME: lot of possible but missing checks
    FOR_EACH_PTR(fn->ctype.base_type->arguments, sym) {
        if (--pos == 0)
            retval = sym;
    } END_FOR_EACH_PTR(sym);
    return retval;
}

static inline bool is_pseudo(pseudo_t pseudo)
{
    return pseudo && pseudo != VOID;
}



//
// Sparse types
//


static inline int sizeof_from_bits(int bits)
{
	return (bits >= 0) ? (bits + bits_in_char - 1) / bits_in_char : 0;
}

static inline bool is_base_type(const struct symbol *type)
{
    return (type->type == SYM_BASETYPE);
}

static int
populate_with_base_types(type_ptr_db_t db)
#define TYPE(sym, clt)  { &sym##_clt, &sym##_ctype, CL_TYPE_##clt, #sym }
{
    const struct {
        struct cl_type  *ref;
        struct symbol   *ctype;
        enum cl_type_e  cl_type;
        const char      *name;
    } base_types[] = {
    /* Synopsis:
     * sparse/symbol.c (ctype_declaration) + sparse/symbol.h
     *
     * Omitted:
     * - type_ctype
     * - string_ctype..lazy_ptr_type (should not be present at all [?])
     *
     * Note:
     * `lllong' represents non-standard "extra long" at specific platforms [?]
     */
        /* CL_TYPE_VOID */
        TYPE(void, VOID),

        /* CL_TYPE_UNKNOWN */
        TYPE(incomplete, UNKNOWN),
        TYPE(bad,        UNKNOWN),

        /* CL_TYPE_INT */
        TYPE(int,    INT),   TYPE(sint,    INT),   TYPE(uint,    INT),
        TYPE(short,  INT),   TYPE(sshort,  INT),   TYPE(ushort,  INT),
        TYPE(long,   INT),   TYPE(slong,   INT),   TYPE(ulong,   INT),
        TYPE(llong,  INT),   TYPE(sllong,  INT),   TYPE(ullong,  INT),
        //TYPE(lllong, INT),   TYPE(slllong, INT),   TYPE(ulllong, INT),

        /* CL_TYPE_CHAR */
        TYPE(char,   CHAR),  TYPE(schar,   CHAR),  TYPE(uchar,  CHAR),

        /* CL_TYPE_BOOL */
        TYPE(bool, BOOL),

        /* CL_TYPE_REAL */
        TYPE(float,   REAL),
        TYPE(double,  REAL),
        TYPE(ldouble, REAL),
    };

    struct symbol *ctype;
    struct cl_type *clt;
    int i;
    for (i = 0; i < ARRAY_SIZE(base_types); i++) {
        clt = base_types[i].ref;
        EMPTY_CL_TYPE(clt);

        ctype = base_types[i].ctype;

        clt->code  = base_types[i].cl_type;
        clt->scope = CL_SCOPE_GLOBAL;
        clt->name  = base_types[i].name;
        clt->size  = sizeof_from_bits(ctype->bit_size);

        // insert into hash table + pointer hierarchy (at base level)
        type_ptr_db_insert(db, clt, ctype, NEW_UID);
    }

    // return uid of the last type inserted so we can skip the freeing
    return clt->uid;
#undef TYPE
}

static struct cl_type *
add_type_if_needed(struct symbol *type, struct instruction *insn,
                   struct ptr_db_item **ptr);

#if 1
static struct cl_type_item *
create_ptr_type_item(struct symbol *type)
{
    struct cl_type_item *item = MEM_NEW(struct cl_type_item);
    if (!item)
        die("MEM_NEW failed");

    item->type = /* FIXME: unguarded recursion */
                 add_type_if_needed(type->ctype.base_type, NULL, NULL);
    item->name = NULL;

    // guaranteed to NOT return NULL
    return item;
}
#endif

static void
add_nested_type(struct cl_type *clt, struct symbol *sym)
{
    if (clt->items == NULL)
        clt->item_cnt = 0;
    clt->items = MEM_RESIZE_ARR(clt->items, clt->item_cnt+1);
    if (!clt->items)
        die("MEM_RESIZE_ARR failed");
    struct cl_type_item *item = &clt->items[clt->item_cnt++];
    if (sym->type == SYM_BASETYPE) {
        // specially for "void" as function argument if no one present
        item->type = add_type_if_needed(sym, NULL, NULL);
        item->name = NULL;
        return;
    }
    item->type = /* recursion */ add_type_if_needed(sym->ctype.base_type, NULL, NULL);
    item->name = (sym->ident) ? strdup(show_ident(sym->ident)) : NULL;
    item->offset = sym->offset;
}

static void
add_struct_elem_types(struct cl_type *clt, struct symbol_list *elems)
{
    struct symbol *sym;

    FOR_EACH_PTR(elems, sym) {
        add_nested_type(clt, sym);
    } END_FOR_EACH_PTR(sym);

    // TODO: not sure why this necessary
    //clt->item_cnt++;
}

static void
add_fn_arguments(struct cl_type *clt, struct symbol_list* args)
{
    struct symbol *sym;
    if (ptr_list_size((struct ptr_list *)args) == 0) {
        add_nested_type(clt, &void_ctype);
        //return;
    }

    FOR_EACH_PTR(args, sym)
        add_nested_type(clt, sym);
    END_FOR_EACH_PTR(sym);

    // TODO: not sure why this necessary
    clt->item_cnt++;
}

static void
read_composite_type(struct cl_type *clt, struct symbol *type)
{
    assert(clt);

    // common with ptr
    read_location(&clt->loc, type->pos);
    read_scope(&clt->scope, type->scope);
    clt->size = sizeof_from_bits(type->bit_size);

#if 0
    // FIXME: this is unwanted "override" behaviour
    if (insn && insn->opcode >= OP_BINCMP && insn->opcode <= OP_BINCMP_END)
        clt->code = CL_TYPE_BOOL;

    if (clt->code == CL_TYPE_UNKNOWN && !clt->name)
        clt->name = strdup("<sparse type not available>");

#endif

    enum type code = type->type;

    switch (code) {
        case SYM_PTR:
            clt->code       = CL_TYPE_PTR;
            // item added back in add_type_if_needed
            break;

        case SYM_STRUCT:
            clt->code       = CL_TYPE_STRUCT;
            clt->name       = strdup(show_ident(type->ident));
            clt->items      = NULL;
            add_struct_elem_types(clt, type->symbol_list);
            break;

        case SYM_UNION:
            clt->code       = CL_TYPE_UNION;
            clt->name       = strdup(show_ident(type->ident));
            clt->item_cnt   = /* TODO */ 0;
            clt->items      = /* TODO */ NULL;
            break;

        case SYM_FN:
            clt->code       = CL_TYPE_FNC;
            clt->item_cnt   = 1;
            clt->items      = create_ptr_type_item(type);
            add_fn_arguments(clt, type->arguments);
            break;

        case SYM_ENUM:
            clt->code       = CL_TYPE_ENUM;
            clt->name       = strdup(show_ident(type->ident));
            break;

        default:
            // e.g., SYM_PTR as well as base types should be already handled
            CL_TRAP;
            clt->code       = CL_TYPE_UNKNOWN;
            clt->name       = strdup(show_typename(type));
    }
}

static void
skip_sparse_accessors(struct symbol **ptype)
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

// for given type "clt", return respective item from pointer hieararchy;
// it is called only when we know such item will be there (already added)
static void
get_ptr_db_item(type_ptr_db_t db, const struct cl_type *clt,
                struct ptr_db_item **ptr)
{
    if (clt->code == CL_TYPE_PTR) {
        struct ptr_db_item *prev = NULL;
        get_ptr_db_item(db, clt->items->type, &prev);
        assert(prev->next);
        *ptr = prev->next;
    } else {
        int i;
        struct ptr_db_arr *ptr_db = &db->ptr_db;
        for (i = ptr_db->alloc_size - ptr_db->remain_size - 1; i >= 0; i--) {
            if (ptr_db->heads[i].clt == clt)
                break;
        }
        if (i >= 0)
            *ptr = &ptr_db->heads[i];
        else {
            // should not happen
            CL_TRAP;
        }
    }
}

// note: the only function that uses type_ptr_db global variable directly
static struct cl_type *
add_type_if_needed(struct symbol *type, struct instruction *insn,
                   struct ptr_db_item **ptr)
{
    struct cl_type *clt;

    if (!type && insn)
        type = get_instruction_type(insn);
    if (!type)
        CL_TRAP;

    // FIXME: this approach is completely wrong since we get type info for the
    // operand's base however we need to get type info in regards to the given
    // accessor
    skip_sparse_accessors(&type);

    // Fastest path, we have the type already in hash table
    clt = typen_get_by_key(type_ptr_db.type_db, type);
    if (clt) {
        // type already hashed
        if (ptr)
            get_ptr_db_item(&type_ptr_db, clt, ptr);
        return clt;
    }

    int uid = NEW_UID;
    struct cl_type **clt_ptr;
    struct cl_type *ptr_type = NULL;
    // Extra handling of pointer symbols, potentially fast circuit for pointer
    // type alias (i.e. no allocation)
    if (type && type->type == SYM_PTR) {
        struct ptr_db_item *prev = NULL;

        ptr_type = add_type_if_needed(type->ctype.base_type, NULL, &prev);
        if (!prev->next) {
            prev->next = MEM_NEW(struct ptr_db_item);
            if (!prev->next)
                die("MEM_NEW");
            prev->next->clt = NULL;
            prev->next->next = NULL;
        } else {
            assert(prev->next->clt);
            uid = prev->next->clt->uid;
        }
        clt_ptr = &prev->next->clt;
        if (ptr)
            *ptr = prev->next;
    } else
        clt_ptr = &clt;

    if (uid == NEW_UID) {
        *clt_ptr = MEM_NEW(struct cl_type);
        if (!*clt_ptr)
            die("MEM_NEW failed");
        EMPTY_CL_TYPE(*clt_ptr);
    }
    clt = type_ptr_db_insert(&type_ptr_db, *clt_ptr, type, uid);
    if (uid != NEW_UID)
        // was pointer alias
        return clt;

    // Slow path for anything (except for pointers) which is being
    // proceeded for the first time (next time, hashed ctl is used instead)
    read_composite_type(clt, type);
    if (type && type->type == SYM_PTR) {
        // use obtained dereferenced type
        clt->item_cnt = 1;
        clt->items = MEM_NEW(struct cl_type_item);
        if (!clt->items)
            die("MEM_NEW");
        assert(ptr_type);
        clt->items->type = ptr_type;
    } else
        if (ptr)
            *ptr = &type_ptr_db.ptr_db.heads[type_ptr_db.ptr_db.pos];
    return clt;
}


static struct cl_type *
clt_from_sym(struct symbol *sym)
{
    if (!sym || !sym->ctype.base_type)
        CL_TRAP;

    return add_type_if_needed(sym->ctype.base_type, NULL, NULL);
}



//
// Symbols/pseudos/operands handling
//


static inline struct cl_cst *
provide_cst(struct cl_operand *op, enum cl_type_e cst_type)
{
    op->code = CL_OPERAND_CST;
    op->data.cst.code = cst_type;
    return &op->data.cst;
}

static inline struct cl_var *
provide_var(struct cl_operand *op)
{
    op->code = CL_OPERAND_VAR;
    op->data.var = MEM_NEW(struct cl_var);
    if (!op->data.var)
        die("MEM_NEW failed");

    // guaranteed not to return NULL
    return op->data.var;
}

static inline struct cl_accessor *
provide_trailing_accessor(struct cl_operand *op)
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

    *retval = MEM_NEW(struct cl_accessor);
    if (!*retval)
        die("MEM_NEW failed");
    (*retval)->next = NULL;

    // guaranteed not to return NULL
    return *retval;
}

static void
read_sym_initializer(struct cl_operand *op, struct expression *expr)
{
    if (!expr)
        return;

    switch (expr->type) {
        case EXPR_STRING:
            op->type = clt_from_sym(expr->ctype);
            provide_cst(op, CL_TYPE_STRING)->data.cst_string.value
                = read_sparse_string(expr->string);
            return;
        default:
            CL_TRAP;
    }
}

static void
read_pseudo_sym(struct cl_operand *op, struct symbol *sym,
                struct symbol *subst_type)
{
    struct symbol *base;

    // read symbol location and scope
    read_location(&op->loc, sym->pos);
    read_scope(&op->scope, sym->scope);

    if (sym->bb_target) {
        WARN_UNHANDLED(sym->pos, "sym->bb_target");
        CL_TRAP;
        op->code = CL_OPERAND_VOID;
        return;
    }

    if (!sym->ident) {
        read_sym_initializer(op, sym->initializer);
        return;
    }

    base = sym->ctype.base_type;
    if (base && base->type == SYM_FN) {
        op->type = clt_from_sym(sym);

        struct cl_cst *cst = provide_cst(op, CL_TYPE_FNC);
        cst->data.cst_fnc.name      = strdup(show_ident(sym->ident));
        cst->data.cst_fnc.is_extern = MOD_EXTERN & sym->ctype.modifiers;
        cst->data.cst_fnc.uid       = (int)(long) sym;
    } else {
        // FIXME: symbols are always to be referenced?
        if (subst_type)
            op->type = clt_from_sym(subst_type);
        else
            op->type = clt_from_sym(sym);

        struct cl_var *var = provide_var(op);
        var->uid     = (int)(long) sym;
        var->name    = strdup(show_ident(sym->ident));

        if (subst_type) {
            struct cl_accessor *ac = provide_trailing_accessor(op);
            ac->code = CL_ACCESSOR_REF;
            ac->type = clt_from_sym(sym);
        }
    }
}

static void
read_pseudo_arg(struct cl_operand *op, const struct instruction *def, int pos)
{
    struct symbol *sym = get_arg_at_pos(def->bb->ep->name, pos);
    if (!sym)
        CL_TRAP;

    op->scope       = CL_SCOPE_FUNCTION;
    op->type        = clt_from_sym(sym);

    struct cl_var *var = provide_var(op);
    var->uid        = (int)(long) sym;
    var->name       = strdup(show_ident(sym->ident));
    var->artificial = false;
}

static void
read_pseudo_reg(struct cl_operand *op, const pseudo_t pseudo)
{/* Synopsis:
  *
  * Note: pseudo->def == NULL for copy.32
  */
    if (pseudo->def)
        op->type = add_type_if_needed(NULL, pseudo->def, NULL);
    else
        op->type = &int_clt;

    struct cl_var *var = provide_var(op);
    var->uid  = (int)(long) pseudo;
    var->name = NULL;
}

static inline struct cl_operand *
read_pseudo(struct cl_operand *op, const pseudo_t pseudo)
{
    EMPTY_CL_OPERAND(op);

    if (!is_pseudo(pseudo))
        return op;

    switch (pseudo->type) {
        case PSEUDO_SYM:  /* union -> sym */
            read_pseudo_sym(op, pseudo->sym, NULL);
            break;
        case PSEUDO_REG:  /* union -> def */
            read_pseudo_reg(op, pseudo);
            break;
        case PSEUDO_VAL: /* union -> val */
            op->type = &int_clt;
            provide_cst(op, CL_TYPE_INT)->data.cst_int.value = pseudo->value;
            break;
        case PSEUDO_ARG:
            read_pseudo_arg(op, pseudo->def, pseudo->nr);
            break;
#if 0
        case PSEUDO_PHI:
            WARN_UNHANDLED(insn->pos, "PSEUDO_PHI");
            break;
#endif
        default:
            CL_TRAP;
    }

    return op;
}

static void
read_insn_op_access(struct cl_operand *op, unsigned insn_offset)
{
    int i = 0;
    struct cl_accessor *ac;

    ac = provide_trailing_accessor(op);

    //CL_TRAP;
    // FIXME: CL_TYPE_VOID (is it, after all, allowed to find this here?)
    #define MAP_ACCESSOR(var, type, acc) case CL_##type: var = CL_##acc; break;
    switch (op->type->code) {
        MAP_ACCESSOR(ac->code, TYPE_PTR,    ACCESSOR_DEREF)
        MAP_ACCESSOR(ac->code, TYPE_STRUCT, ACCESSOR_ITEM)
        default: CL_TRAP;
    }

    // accessor's type is the operand's type (will be peeled off immediately)
    ac->type = (struct cl_type *) op->type;

    if (op->type->code == CL_TYPE_STRUCT) {
        for (i = 0; i < op->type->item_cnt; i++)
            if (op->type->items[i].offset == insn_offset)
                break;
        ac->data.item.id = i;
    }

    // peel off one level of type/access decoration from the operand
    op->type = (struct cl_type *)op->type->items[i].type;

    return;
}

static inline void
adjust_cl_operand_accessors(struct cl_operand *op,
                            const struct cl_type *expected_type,
                            unsigned first_offset)
{
    unsigned offset = first_offset;

    if (op->code == CL_OPERAND_VOID)
        // there is no operand
        return;

    while (op->type != expected_type /*&& op->type->code != CL_TYPE_VOID*/) {
        read_insn_op_access(op, offset);
        // accessing struct element is different with the first level access
        // (use insn_offset) and with other accesses (always zero offset)
        offset = 0;
    }
}



//
// Instructions handling functions
//


static bool
handle_insn_sel(struct cl_insn *cli, const struct instruction *insn)
{/* Synopsis:
  *
  * Problems:
  */
    // at first, create and emit CL_INSN_COND, then
    // create and emit respective basic blocks
    //
    // note: BB label uniqueness: addr(insn) + (1 or 2),
    //       provided that pointer has size of 4+

    struct cl_operand cond, src, dst;
    char *bb_label_true = NULL,
         *bb_label_false = NULL,
         *bb_label_end = NULL;

    // BB labels
    if (   asprintf(&bb_label_true,  "%p", ((char *) insn) + 1) < 0
        || asprintf(&bb_label_false, "%p", ((char *) insn) + 2) < 0
        || asprintf(&bb_label_end,   "%p", ((char *) insn) + 3) < 0)
        die("asprintf failed");

    cli->code                      = CL_INSN_COND;

    cli->data.insn_cond.src        = read_pseudo(&cond, insn->src1);
    cli->data.insn_cond.then_label = bb_label_true;
    cli->data.insn_cond.else_label = bb_label_false;

    //i>
    cl->insn(cl, cli);
    //i>

    free_cl_operand_data(&cond);

    // first BB ("then" branch)

    //b>
    cl->bb_open(cl, bb_label_true);
    //b>

    cli->code                      = CL_INSN_UNOP;
    cli->data.insn_unop.code       = CL_UNOP_ASSIGN;

    cli->data.insn_unop.dst        = read_pseudo(&dst, insn->target);
    cli->data.insn_unop.src        = read_pseudo(&src, insn->src2);

    //i>
    cl->insn(cl, cli);
    //i>

    free_cl_operand_data(&src);

    cli->code                      = CL_INSN_JMP;
    cli->data.insn_jmp.label       = bb_label_end;
    cl->insn(cl, cli);

    // second BB ("else" branch) .. warning: copy-paste from above

    //b>
    cl->bb_open(cl, bb_label_false);
    //b>

    cli->code                      = CL_INSN_UNOP;
    cli->data.insn_unop.code       = CL_UNOP_ASSIGN;

    // reusing already generated operand for insn->target
    cli->data.insn_unop.src        = read_pseudo(&src, insn->src3);

    //i>
    cl->insn(cl, cli);
    //i>

    free_cl_operand_data(&src);
    free_cl_operand_data(&dst);

    cli->code                      = CL_INSN_JMP;
    cli->data.insn_jmp.label       = bb_label_end;

    //i>
    cl->insn(cl, cli);
    //i>

    // merging BB

    //b>
    cl->bb_open(cl, bb_label_end);
    //b>
}

static bool
handle_insn_call(struct cl_insn *cli, const struct instruction *insn)
{/* Synopsis:
  *
  * Problems:
  */
    struct cl_operand dst, fnc;
    struct pseudo *arg;
    int cnt = 0;

    // open call
    read_pseudo(&dst, insn->target);
    read_pseudo(&fnc, insn->func);

    //c>
    cl->insn_call_open(cl, &cli->loc, &dst, &fnc);
    //c>

    free_cl_operand_data(&dst);
    free_cl_operand_data(&fnc);

    // go through arguments
    FOR_EACH_PTR(insn->arguments, arg) {
        struct cl_operand arg_operand;
        if (arg->type == PSEUDO_SYM) {
            EMPTY_CL_OPERAND(&arg_operand);
            read_pseudo_sym(&arg_operand, arg->sym, /*TODO*/ arg->sym);
        } else {
            read_pseudo(&arg_operand, arg);
        }

        //c>
        cl->insn_call_arg(cl, ++cnt, &arg_operand);
        //c>

        free_cl_operand_data(&arg_operand);
    } END_FOR_EACH_PTR(arg);

    //c>
    cl->insn_call_close(cl);
    //c>

    if (insn->func->sym->ctype.modifiers & MOD_NORETURN) {
        // this call never returns --> end of BB!!

        cli->code    = CL_INSN_ABORT;

        //i>
        cl->insn(cl, cli);
        //i>

        return false;
    }

    return true;
}

static bool
handle_insn_br(struct cl_insn *cli, const struct instruction *insn)
{/* Synopsis:
  *
  * Problems:
  */
    char *bb_name_true = NULL;
    char *bb_name_false = NULL;
    struct cl_operand op;

    if (asprintf(&bb_name_true, "%p", insn->bb_true) < 0)
        die("asprintf failed");

    if (!is_pseudo(insn->cond)) {
        cli->code                = CL_INSN_JMP;
        cli->data.insn_jmp.label = bb_name_true;

        //i>
        cl->insn(cl, cli);
        //i>

        free(bb_name_true);
        return true;
    }

    if (asprintf(&bb_name_false, "%p", insn->bb_false) < 0)
        die("asprintf failed");

    cli->code                      = CL_INSN_COND;
    cli->data.insn_cond.src        = read_pseudo(&op, insn->cond);
    cli->data.insn_cond.then_label = bb_name_true;
    cli->data.insn_cond.else_label = bb_name_false;

    //i>
    cl->insn(cl, cli);
    //i>

    free_cl_operand_data(&op);
    free(bb_name_true);
    free(bb_name_false);

    return true;
}

static bool
handle_insn_switch(struct cl_insn *cli, const struct instruction *insn)
{/* Synopsis:
  *
  * Problems:
  */
    struct cl_operand op;
    struct multijmp *jmp;

    // emit insn_switch_open
    read_pseudo(&op, insn->target);

    //s>
    cl->insn_switch_open(cl, &cli->loc, &op);
    //s>

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
            val_lo.type = &int_clt;
            val_hi.type = &int_clt;

            val_lo.data.cst.code = CL_TYPE_INT;
            val_hi.data.cst.code = CL_TYPE_INT;

            val_lo.data.cst.data.cst_int.value = jmp->begin;
            val_hi.data.cst.data.cst_int.value = jmp->end;
        }

        if (asprintf(&label, "%p", jmp->target) < 0)
            die("asprintf failed");

        // emit insn_switch_case
        // FIXME: not enough accurate location info from SPARSE for switch/case

        //s>
        cl->insn_switch_case(cl, &cli->loc, &val_lo, &val_hi, label);
        //s>

        free_cl_operand_data(&val_lo);
        free_cl_operand_data(&val_hi);
        free(label);

    } END_FOR_EACH_PTR(jmp);

    //s>
    cl->insn_switch_close(cl);
    //s>

    return true;
}

static bool
handle_insn_ret(struct cl_insn *cli, const struct instruction *insn)
{/* Synopsis -- input:
  * insn->src  ... value to be used as a return value
  * insn->type ... type of return value
  *
  * Synopsis -- output:
  * CL_INSN_RET
  *     cl_insn.insn_ret.src ... cl_operand representing return value
  *
  * Problems: << FIXME: this is in common for all assignments, solved globally
  * 1. One-element struct -- how to represent return value correctly?
  * S. Use insn->type to deduce the right one.  Because the resulting type
  *    should have been already inserted into hash table, we can compare
  *    struct cl_type pointers directly and keep accessing the original type
  *    until we get the same as a resulting one.
  */
    struct cl_operand op;
    const struct cl_type *resulting_type;

    // src operand
    cli->data.insn_ret.src = read_pseudo(&op, insn->src);

    resulting_type = add_type_if_needed(insn->type, NULL, NULL);
    assert(resulting_type);
    adjust_cl_operand_accessors(&op, resulting_type, insn->offset);

    //i>
    cl->insn(cl, cli);
    //i>

    free_cl_operand_data(&op);

    return true;
}

static bool
insn_assignment_base(struct cl_insn *cli, const struct instruction *insn,
                     pseudo_t lhs,     /* := */  pseudo_t rhs,
                     bool lhs_access,            bool rhs_access
)
{/* Synopsis -- input:
  * insn->type ... type of value to assign
  *
  * Problems:
  */
    struct cl_operand op_lhs;
    struct cl_operand op_rhs;

    // FIXME: why?
    if (insn->opcode == OP_PTRCAST && rhs->type == PSEUDO_SYM) {
        EMPTY_CL_OPERAND(&op_rhs);
        read_pseudo_sym(&op_rhs, insn->src->sym, insn->orig_type);
    } else {
        read_pseudo(&op_rhs, rhs);

        if (rhs_access && (rhs->type != PSEUDO_VAL)) {
            const struct cl_type *resulting_type;
            int insn_offset = insn->offset;
            resulting_type = add_type_if_needed(insn->type, NULL, NULL);
            if (insn->opcode == OP_STORE) {
                // remove one level of indirection of target type
                // (will be compensated by adding reference to rhs operand)
                resulting_type = resulting_type->items[0].type;
                insn_offset = 0;
            }
            assert(resulting_type);
            adjust_cl_operand_accessors(&op_rhs, resulting_type, insn_offset);
        }
    }

    if (lhs->type == PSEUDO_VAL /* && lhs->value == 0 */
        && op_rhs.type->code == CL_TYPE_PTR) {
        op_lhs.code = CL_OPERAND_CST;
        op_lhs.type = op_rhs.type;
        op_lhs.accessor = NULL;
        op_lhs.data.cst.code = CL_TYPE_INT;
        op_lhs.data.cst.data.cst_int.value = lhs->value;
    } else {
        read_pseudo(&op_lhs, lhs);
        if (lhs_access) {
            const struct cl_type *resulting_type;
            resulting_type = add_type_if_needed(insn->type, NULL, NULL);
            assert(resulting_type);
            adjust_cl_operand_accessors(&op_lhs, resulting_type, insn->offset);
        }
    }

    if (insn->opcode == OP_STORE) {
        // add promised reference
        if (op_rhs.code == CL_OPERAND_VAR) {
            // accessor only when operand is variable
            struct cl_accessor *ac = provide_trailing_accessor(&op_rhs);
            ac->code = CL_ACCESSOR_REF;
            ac->type = op_rhs.type;
        }
        op_rhs.type = add_type_if_needed(insn->type, NULL, NULL);
    }

#if 0
    if (op_lhs.access && op_lhs.name && op_lhs.offset
            && 0 == strcmp(op_lhs.name, op_lhs.offset))
        CL_TRAP;

    if (op_rhs.access && op_rhs.name && op_rhs.offset
            && 0 == strcmp(op_rhs.name, op_rhs.offset))
        CL_TRAP;
#endif

    // FIXME SPARSE?: hack because sparse generates extra instruction
    //         e.g. store %arg1 -> 0[in], in case of "in" == "%arg1"
#if 1
    if (lhs->type != PSEUDO_SYM || rhs->type != PSEUDO_ARG
         || op_lhs.data.var->uid != op_rhs.data.var->uid) {
#endif
        cli->data.insn_unop.dst = &op_lhs;
        cli->data.insn_unop.src = &op_rhs;

        //i>
        cl->insn(cl, cli);
        //i>
#if 1
    } else {
        WARN_VA(insn->pos, "instruction omitted: %s",
                show_instruction((struct instruction *) insn));
    }
#endif

    free_cl_operand_data(&op_lhs);
    free_cl_operand_data(&op_rhs);

    return true;
}

static inline bool
handle_insn_store(struct cl_insn *cli, const struct instruction *insn)
{/* Synopsis -- input:
  * insn->symbol ... mem. address where to assign value
  * insn->target ... source value
  * insn->type   ... type of value to be assigned
  *
  * Synopsis -- output:
  * CL_INSN_UNOP
  *     CL_UNOP_ASSIGN
  *
  * Problems:
  * 1. Register is not immediately connected with type information
  * S. Use insn->type
  */
    //CL_TRAP;
    return
    insn_assignment_base(cli, insn,
        insn->symbol,  /* := */  insn->target,
        true,                    insn->target->type != PSEUDO_REG
    );
}

static inline bool
handle_insn_load(struct cl_insn *cli, const struct instruction *insn)
{/* Synopsis -- input:
  * insn->target ... register (XXX: only?) to be assigned
  * insn->src    ... mem. address containing source value
  * insn->type   ... type of value to be assigned
  *
  * Synopsis -- output:
  * CL_INSN_UNOP
  *     CL_UNOP_ASSIGN
  *
  * Problems:
  * 1. Register is not immediately connected with type information
  * S. Use insn->type
  */
    return
    insn_assignment_base(cli, insn,
        insn->target,  /* := */  insn->src,
        false,                   true
    );
}

static inline bool
handle_insn_copy(struct cl_insn *cli, const struct instruction *insn)
{
    return
    insn_assignment_base(cli, insn,
        insn->target,  /* := */  insn->src,
        false,                   false
    );
}

static inline bool
handle_insn_ptrcast(struct cl_insn *cli, const struct instruction *insn)
{
    //CL_TRAP;
    return
    insn_assignment_base(cli, insn,
        insn->target,  /* := */  insn->src,
        false,                   false
    );
}

static bool
handle_insn_binop(struct cl_insn *cli, const struct instruction *insn)
{/* Synopsis:
  *
  * Problems:
  */
    struct cl_operand dst, src1, src2;

    cli->data.insn_binop.dst  = read_pseudo(&dst,  insn->target);
    cli->data.insn_binop.src1 = read_pseudo(&src1, insn->src1  );
    cli->data.insn_binop.src2 = read_pseudo(&src2, insn->src2  );

    //i>
    cl->insn(cl, cli);
    //i>

    free_cl_operand_data(&dst);
    free_cl_operand_data(&src1);
    free_cl_operand_data(&src2);

    return true;
}

typedef bool insn_handler(struct cl_insn *, const struct instruction *);
static bool handle_insn(struct instruction *insn)
#define INSN_GEN(spi, cli, hnd) \
    [OP_##spi] = { .insn_code = CL_INSN_##cli, .data.handler = hnd }
#define INSN_UNI(spi, unop_code, hnd) \
    [OP_##spi] = { .insn_code = CL_INSN_UNOP,                                \
                   .code.unop = CL_UNOP_##unop_code, .data.handler = hnd }
#define INSN_BIN(spi, binop_code, hnd)                                       \
    [OP_##spi] = { .insn_code = CL_INSN_BINOP,                               \
                   .code.binop = CL_BINOP_##binop_code , .data.handler = hnd }
#define INSN_IGN(spi, _, __) \
    [OP_##spi] = { .insn_code = CL_INSN_ABORT, .data.insn_str = "OP_" #spi }
{
    const struct insn_handlers {
        enum cl_insn_e       insn_code;
        union {
            enum cl_unop_e   unop;
            enum cl_binop_e  binop;
        } code;
        union {
            insn_handler     *handler;
            const char       *insn_str;
        } data;
    } insn_handlers[] = {
    /* Synopsis:
     * sparse/linearize.h
     *
     * Note:
     * Instructions with more complicated rules (more instructions are emitted
     * per the single original one) are denoted with NOP, unhandled with ABORT.
     */
        //action| sparse insn.    | cl insn. (+ uni/bin)| handler            |
        //------+-----------------+---------------------+--------------------|

        INSN_GEN( RET             , RET                 , handle_insn_ret    ),
        INSN_IGN( BADOP           ,                     ,                    ),

        /* Entry */
        INSN_IGN( ENTRY           ,                     ,                    ),

        /* Terminator */
        // OP_TERMINATOR = OP_RET
        INSN_GEN( RET             , RET                 , handle_insn_ret    ),
        INSN_GEN( BR              , NOP /*JMP or COND*/ , handle_insn_br     ),
        INSN_GEN( SWITCH          , NOP /*another way*/ , handle_insn_switch ),
        INSN_IGN( INVOKE          ,                     ,                    ),
        INSN_IGN( COMPUTEDGOTO    ,                     ,                    ),
        INSN_IGN( UNWIND          ,                     ,                    ),
        // OP_TERMINATOR_END = OP_UNWIND

        /* Binary */
        // OP_BINARY = OP_ADD
        INSN_BIN( ADD             , PLUS                , handle_insn_binop  ),
        INSN_BIN( SUB             , MINUS               , handle_insn_binop  ),
        INSN_BIN( MULU            , MULT                , handle_insn_binop  ),
        INSN_BIN( MULS            , MULT                , handle_insn_binop  ),
        INSN_BIN( DIVU            , TRUNC_DIV           , handle_insn_binop  ),
        INSN_BIN( DIVS            , TRUNC_DIV           , handle_insn_binop  ),
        INSN_BIN( MODU            , TRUNC_MOD           , handle_insn_binop  ),
        INSN_BIN( MODS            , TRUNC_MOD           , handle_insn_binop  ),
        INSN_IGN( SHL             ,                     ,                    ),
        INSN_IGN( LSR             ,                     ,                    ),
        INSN_IGN( ASR             ,                     ,                    ),

        /* Logical */
        INSN_BIN( AND             , BIT_AND             , handle_insn_binop  ),
        INSN_BIN( OR              , BIT_IOR             , handle_insn_binop  ),
        INSN_IGN( XOR             ,                     ,                    ),
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
        INSN_IGN( SET_B           ,                     ,                    ),
        INSN_IGN( SET_A           ,                     ,                    ),
        INSN_IGN( SET_BE          ,                     ,                    ),
        INSN_IGN( SET_AE          ,                     ,                    ),
        // OP_BINCMP_END = OP_SET_AE,

        /* Uni */
        INSN_IGN( NOT             ,                     ,                    ),
        INSN_IGN( NEG             ,                     ,                    ),

        /* Select - three input values */
        INSN_GEN( SEL             , NOP /*COND*/        , handle_insn_sel    ),

        /* Memory */
        INSN_IGN( MALLOC          ,                     ,                    ),
        INSN_IGN( FREE            ,                     ,                    ),
        INSN_IGN( ALLOCA          ,                     ,                    ),
        INSN_UNI( LOAD            , ASSIGN              , handle_insn_load   ),
        INSN_UNI( STORE           , ASSIGN              , handle_insn_store  ),
        INSN_IGN( SETVAL          ,                     ,                    ),
        INSN_IGN( SYMADDR         ,                     ,                    ),
        INSN_IGN( GET_ELEMENT_PTR ,                     ,                    ),

        /* Other */
        INSN_IGN( PHI             ,                     ,                    ),
        INSN_IGN( PHISOURCE       ,                     ,                    ),
            // FIXME: this might be a SPARSE bug if DO_PER_EP_UNSAA is set
            // NOTE: really encountered (hash_table.c)
        INSN_UNI( CAST            , ASSIGN              , handle_insn_copy   ),
        INSN_UNI( SCAST           , ASSIGN              , handle_insn_copy   ),
        INSN_UNI( FPCAST          , ASSIGN              , handle_insn_copy   ),
        INSN_UNI( PTRCAST         , ASSIGN              , handle_insn_ptrcast),
        INSN_IGN( INLINED_CALL    ,                     ,                    ),
        INSN_GEN( CALL            , NOP /*another way*/ , handle_insn_call   ),
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
    };

    struct cl_insn cli;
    const struct insn_handlers* insn_handler;

    enum opcode code = insn->opcode;
    //assert(OP_BADOP <= code && code <= OP_COPY);

    if (verbose & CL_VERBOSE_INSTRUCTION)
        NOTE("\t%d: instruction to be processed: %s", insn->pos.line,
                                                      show_instruction(insn));

    insn_handler = &insn_handlers[code];

    read_location(&cli.loc, insn->pos);
    cli.code = insn_handler->insn_code;

    switch (insn_handler->insn_code) {
        case CL_INSN_ABORT:
            WARN_UNHANDLED(insn->pos, insn_handler->data.insn_str);
            break;
        case CL_INSN_UNOP:
            cli.data.insn_unop.code = insn_handler->code.unop;
            break;
        case CL_INSN_BINOP:
            cli.data.insn_binop.code = insn_handler->code.binop;
            break;
        default:
            break;
    }

    assert(insn_handler->data.handler);
    return insn_handler->data.handler(&cli, insn);
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



//
// Functions for lower granularity/higher level handling
//


static void handle_bb(struct basic_block *bb)
{
    struct instruction *insn;
    char *bb_name;

    if (!bb)
        return;

    if (asprintf(&bb_name, "%p", bb) < 0)
        die("asprintf failed");

    //b>
    cl->bb_open(cl, bb_name);
    //b>

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

    struct cl_insn cli;
    cli.code                = CL_INSN_JMP;
    cli.data.insn_jmp.label = entry_name;
    read_location(&cli.loc, entry->pos);

    //i>
    cl->insn(cl, &cli);
    //i>

    free(entry_name);

    // go through basic blocks
    FOR_EACH_PTR(ep->bbs, bb) {
        if (!bb)
            continue;

        if (bb->parents || bb->children || bb->insns
            || /* FIXME: is the following actually useful? */ 2 < verbose) {
            handle_bb(bb);
        }
    } END_FOR_EACH_PTR(bb);
}

static void handle_fnc_body(struct symbol *sym)
{
    struct entrypoint *ep = linearize_symbol(sym);
    if (!ep)
        CL_TRAP;

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
        op.data.var = MEM_NEW(struct cl_var);
#if 1
        op.data.var->uid             = /* TODO */ (int)(long) arg;
        op.data.var->name            = strdup(show_ident(arg->ident));
#endif

        read_location(&op.loc, arg->pos);

        //f>
        cl->fnc_arg_decl(cl, ++argc, &op);
        //f>

        free_cl_operand_data(&op);
    } END_FOR_EACH_PTR(arg);
}

static void handle_fnc_def(struct symbol *sym)
{
    struct cl_operand fnc;
    read_location(&fnc.loc, sym->pos);
    read_scope(&fnc.scope, sym->scope);

    fnc.code                            = CL_OPERAND_CST;
    fnc.type                            = clt_from_sym(sym);
    fnc.accessor                        = NULL;
    fnc.data.cst.code                   = CL_TYPE_FNC;
    fnc.data.cst.data.cst_fnc.name      = show_ident(sym->ident);
    fnc.data.cst.data.cst_fnc.is_extern = false;

    //f>
    cl->fnc_open(cl, &fnc);
    //f>

    /* no need to call free_cl_operand_data() */

    // dump argument list
    handle_fnc_arg_list(sym->ctype.base_type->arguments);

    // handle fnc body
    handle_fnc_body(sym);

    //f>
    cl->fnc_close(cl);
    //f>
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

static void clean_up_symbols(struct symbol_list *list)
{
    struct symbol *sym;

    FOR_EACH_PTR(list, sym) {

#if DO_EXPAND_SYMBOL
        expand_symbol(sym);
#endif

        handle_top_level_sym(sym);
    } END_FOR_EACH_PTR(sym);
}



//
// Code listener setup and related helpers
//


static void print_help(const char *cmd)
#define _(...) printf(__VA_ARGS__); printf("\n");
#define __ _("");
{
_("sparse-based code listener frontend"                                      )
__
_("usage: %s (cl frontend args | sparse args)*"                          ,cmd)
__
_("For `sparse args', see sparse documentation; these args are generally"    )
_("compatible with those for gcc and unrecognized ones are ignored anyway."  )
__
_("This code listener fronted also defines few args/options on its own:"     )
__
_("-h, --help               Prints this help text"                           )
_("-cl-verbose[=MASK]       Be verbose (selectively if MASK provided)"       )
_("-cl-dump-pp              Dump pretty-printed linearized code"             )
_("-cl-dump-types           Add type information to such pretty-printed code")
_("-cl-gen-dot[=MAIN_FILE]  Generate control flow graphs"                    )
_("-cl-type-dot[=OUT_FILE]  Generate type graphs"                            )
#undef __
#undef _
}

struct cl_plug_options {
    bool        dump_types;
    bool        use_dotgen;
    bool        use_pp;
    bool        use_typedot;
    const char  *gl_dot_file;
    const char  *pp_out_file;
    const char  *type_dot_file;
};

static int
handle_cl_args(int argc, char *argv[], struct cl_plug_options *opt)
{
    char *value;

    // initialize opt data
    memset(opt, 0, sizeof(*opt));

    // handle plug-in args
    int i = 0;
    while (++i < argc) {

        /* help and other merely local args */

        if (((value = OPTPREFIXEQ_SHORT(argv[i],     "h"))
              || (value = OPTPREFIXEQ_LONG(argv[i],  "help")))
            && *value == '\0') {
            print_help(argv[0]);
            return EXIT_FAILURE;
        } else if ((value = OPTPREFIXEQ_CL(argv[i],  "verbose"))) {
            verbose = OPTVALUE(value)
                ? atoi(value)
                : ~0;

        /* args affecting code listener behaviour */

        } else if ((value = OPTPREFIXEQ_CL(argv[i],  "dump-pp"))) {
            opt->use_pp         = true;
            opt->pp_out_file    = OPTVALUE(value);
        } else if (OPTPREFIXEQ_CL(argv[i],           "dump-types")) {
            opt->dump_types     = true;
            // TODO: warn about ignoring extra value?
        } else if ((value = OPTPREFIXEQ_CL(argv[i],  "gen-dot"))) {
            opt->use_dotgen     = true;
            opt->gl_dot_file    = OPTVALUE(value);
        } else if ((value = OPTPREFIXEQ_CL(argv[i],  "type-dot"))) {
            if (OPTVALUE(value)) {
                opt->use_typedot    = true;
                opt->type_dot_file  = value;
            } else {
                ERROR("mandatory value omitted for type-dot");
                return EXIT_FAILURE;
            }
        }

        // TODO: remove?
        /*} else if ((value = OPTPREFIXEQ_CL(argv[i], "args"))) {
            opt->peer_args = OPTVALUE(value)
                ? value
                : "";*/
    }

    return EXIT_SUCCESS;
}

static bool
cl_append_listener(struct cl_code_listener *chain, const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);

    char *config_string;
    int rv = vasprintf(&config_string, fmt, ap);
    assert(0 < rv);
    va_end(ap);

    struct cl_code_listener *cl = cl_code_listener_create(config_string);
    free(config_string);

    if (!cl) {
        // FIXME: deserves a big comment (subtle)
        chain->destroy(chain);
        return false;
    }

    cl_chain_append(chain, cl);
    return true;
}

static bool
cl_append_def_listener(struct cl_code_listener *chain, const char *listener,
                       const char *args, const struct cl_plug_options *opt)
{
    const char *clf = (/*opt->use_peer*/ true)
        ? "unfold_switch,unify_labels_gl"
        : "unify_labels_fnc";

    return cl_append_listener(chain,
            "listener=\"%s\" listener_args=\"%s\" clf=\"%s\"",
            listener, args, clf);
}

static struct cl_code_listener*
create_cl_chain(const struct cl_plug_options *opt)
{
    struct cl_code_listener *chain = cl_chain_create();
    if (!chain)
        // error message already emitted
        return NULL;

    if (CL_VERBOSE_LOCATION & verbose) {
        if (!cl_append_listener(chain, "listener=\"locator\""))
            return NULL;
    }

    if (opt->use_pp) {
        const char *use_listener = (opt->dump_types)
            ? "pp_with_types"
            : "pp";

        const char *out = (opt->pp_out_file)
            ? opt->pp_out_file
            : "";

        if (!cl_append_def_listener(chain, use_listener, out, opt))
            return NULL;
    }

    if (opt->use_dotgen) {
        const char *gl_dot = (opt->gl_dot_file)
            ? opt->gl_dot_file
            : "";
        if (!cl_append_def_listener(chain, "dotgen", gl_dot, opt))
            return NULL;
    }

    if (opt->use_typedot
            && !cl_append_def_listener(chain, "typedot", opt->type_dot_file, opt))
        return NULL;

    /*if (opt->use_peer
            && !cl_append_def_listener(chain, "easy", opt->peer_args, opt))
        return NULL;*/

    return chain;
}



//
// Worker/master loops
//


// Worker loop (sparse -> linearized code -> processing -> code_listener)
// Note: used in both fork and fork-free setups
static int
worker_loop(struct cl_code_listener *cl, int argc, char **argv)
{
    char *file;
    struct string_list *filelist = NULL;
    struct symbol_list *symlist;

    // initialize sparse
    symlist = sparse_initialize(argc, argv, &filelist);

    // initialize type database
    type_ptr_db_init(&type_ptr_db);

#if DO_PROCEED_INTERNAL
    // proceed internal symbols
    cl->file_open(cl, "sparse-internal-symbols");
    clean_up_symbols(symlist);
    cl->file_close(cl);
#endif

    // proceed the rest, file by file
    FOR_EACH_PTR_NOTAG(filelist, file) {
        if (0 < verbose)
            fprintf(real_stderr, "about to process '%s'...\n", file);
        cl->file_open(cl, file);
        clean_up_symbols(sparse(file));
        cl->file_close(cl);
    } END_FOR_EACH_PTR_NOTAG(file);

    // finalize and destroy
    type_ptr_db_destroy(&type_ptr_db);
    cl->acknowledge(cl);
    CLEANUP(NOKILL, cl);

    return EXIT_SUCCESS;
}


#if DO_FORK
// Master loop (grab worker's stderr via read_fd, print it after work is over)
static int
master_loop(struct cl_code_listener *cl, int read_fd, pid_t pid)
#define MASTER_BUFFSIZE  4096
{
      int stat_loc, res = 0;
      char *buffer = NULL;
      size_t alloc_size = 0, remain_size = 0;
      ssize_t read_size;
      struct pollfd fds = { .fd = read_fd, .events = POLLIN };

      for (;;) {
          if (poll(&fds, 1, -1) < 0) {
              if (errno == EINTR)
                  continue;
              PERROR_CLEANUP_EXIT("pol", pid, cl, 2);
          } else if (fds.revents & POLLHUP)
            // worker has finished
            break;

          if (!remain_size) {
              alloc_size += MASTER_BUFFSIZE;
              remain_size = MASTER_BUFFSIZE;
              buffer = MEM_RESIZE_ARR(buffer, alloc_size);
              if (!buffer)
                  PERROR_CLEANUP_EXIT("MEM_RESIZE_ARR", pid, cl, 2);
          }
          read_size = read(read_fd, &buffer[alloc_size-remain_size],
                           remain_size);
          if (read_size < 0)
              PERROR_CLEANUP_EXIT("read", pid, cl, 2);
          remain_size -= read_size;
      }

      if (wait(&stat_loc) == (pid_t)-1)
          PERROR_CLEANUP_EXIT("wait", pid, cl, 2);
      if (WIFEXITED(stat_loc)) {
          res = WEXITSTATUS(stat_loc);
          fprintf(real_stderr, "sparse returned %i\n", res);
          if (res)
              // sparse ended prematurely (probably caused by invalid syntax)
              CLEANUP(NOKILL, cl);
      }

      if (alloc_size-remain_size) {
          fprintf(real_stderr, "-------------------\nsparse diagnostics:\n");
          write(STDERR_FILENO, buffer, alloc_size-remain_size);
      }
      free(buffer);

      return res;
}
#endif



//
// Main
//


int main(int argc, char *argv[])
{
    int retval;
    struct cl_plug_options opt;

    real_stderr = stderr; // use this if you need "unfaked" stderr

    // handle this code listener frontend specific arguments
    if (retval = handle_cl_args(argc, argv, &opt))
        return retval;

    // initialize code listener
    struct cl_init_data init = { .debug = trivial_printer,
                                 .warn  = cl_warn,
                                 .error = cl_error,
                                 .note  = trivial_printer,
                                 .die   = trivial_printer  };
    cl_global_init(&init);
    cl = create_cl_chain(&opt);
    if (!cl)
        // error message already emitted
        return EXIT_FAILURE;

#if DO_FORK
    // set up pipe
    int fildes[2];
    if (pipe(fildes) < 0)
        PERROR_CLEANUP_EXIT("pipe", NOKILL, cl, 2);
    // master-worker fork
    pid_t pid = fork();
    if (pid == -1)
        PERROR_CLEANUP_EXIT("fork", NOKILL, cl, 2);
    else if (pid == 0) {
        /* child = worker, use fildes[1] for writing */

        if (close(fildes[0]) < 0)
            PERROR_EXIT("close", 2);
        if (!redefine_stderr(fildes[1], &real_stderr))
            PERROR_EXIT("Redefining stderr", 2);
#endif

        // main processing loop
        retval = worker_loop(cl, argc, argv);

#if DO_FORK
        if (fclose(real_stderr) == EOF || close(fildes[1]) < 0)
            PERROR_EXIT("fclose/close", 2);
    } else {
        /* parent = master, use fildes[0] for reading */

        if (close(fildes[1]) < 0)
            PERROR_CLEANUP_EXIT("close", pid, cl, 2);
        // master loop -- gather what sparse produce to stderr
        retval = master_loop(cl, fildes[0], pid);
    }
#endif

    return retval;
}
