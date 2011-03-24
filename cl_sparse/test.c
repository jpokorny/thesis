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
    (type *) malloc(sizeof(type))

#define MEM_NEW_VECTOR(type, num) \
    (type *) malloc(sizeof(type) * num)

#define MEM_RESIZE(ptr, newnum) \
    realloc(ptr, sizeof(*ptr) * (newnum))

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
// ptr_slist, for building pointer* hierarchy in order to prevent
// having two semantically same pointers as two different types
//

#define PTRSLISTARR_SIZE          128

struct ptr_slist_item {
    struct cl_type *clt;
    struct ptr_slist_item *next;
};

struct ptr_slist_arr {
    size_t alloc_size;
    size_t remain_size;
    size_t pos;
    struct ptr_slist_item *heads;
};



//
// Globals
//


const char *GIT_SHA1 = "someversion";
typedef struct typen_data *type_db_t;

static struct cl_code_listener *cl;
static type_db_t type_db = NULL;
static struct ptr_slist_arr ptr_slist = { .alloc_size = 0, .remain_size = 0,
                                          .pos = 0, .heads = NULL };

FILE *real_stderr = NULL; /**< used to access "unfaked" stderr */

static int cl_verbose = 0;
#define CL_VERBOSE_LOCATION         (1 << 1)
#define CL_VERBOSE_INSTRUCTION      (1 << 2)
#define CL_VERBOSE_INSERT_TYPE      (1 << 3)



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


static void warn(struct position pos, const char *fmt, ...)
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
static bool preserve_ec;
static int cnt_errors;
static int cnt_warnings;

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


static void cb_free_clt(struct cl_type *clt)
{
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



//
// Auxiliary helper functions
//


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
                                      void *key, int uid)
{
    if (verbose & CL_VERBOSE_INSERT_TYPE) {
        NOTE("add type (uid = %d, clt = %p): %p", uid, clt, key);
        show_symbol((struct symbol *)key);
        NOTE("---");
    }

    struct cl_type *rv = typen_insert_with_uid(db, clt, key, uid);
    if (!rv)
        die("typen_insert_as_new() failed");

    // guaranteed to NOT return NULL
    return rv;
}

static bool redefine_stderr(int target_fd, FILE **backup_stderr)
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


static void read_sparse_location(struct cl_loc *cl_loc, struct position pos)
{
    cl_loc->file   = stream_name(pos.stream);
    cl_loc->line   = pos.line;
    cl_loc->column = pos.pos;
    cl_loc->sysp   = /* not used by SPARSE */ false;
}

static void read_sparse_scope(enum cl_scope_e *cl_scope, struct scope *scope)
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

static const char *read_sparse_string(const struct string *str)
{
    return (str->length) ? strndup(str->data, str->length) : NULL;
}

static struct symbol *get_instruction_type(struct instruction *insn)
{
    if (insn->opcode >= OP_BINCMP && insn->opcode <= OP_BINCMP_END)
        return &bool_ctype;
    else
        return insn->type;
}

static struct symbol *get_arg_at_pos(struct symbol *fn, int pos)
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

static bool is_pseudo(pseudo_t pseudo)
{
    return pseudo && pseudo != VOID;
}



//
// Sparse types
//

static void empty_cl_type(struct cl_type *clt)
{
    clt->code       = CL_TYPE_UNKNOWN;
    clt->name       = NULL;
    clt->size       = 0;
    clt->item_cnt   = 0;
    clt->items      = NULL;
    clt->scope      = CL_SCOPE_GLOBAL;
    clt->loc.file   = NULL;
    clt->loc.line   = -1;
}

static void read_bytesize(int *bytes, int bits)
{
	*bytes = (bits >= 0) ? (bits + bits_in_char - 1) / bits_in_char : 0;
}

#define TYPE(c, cl)  { &c##_ctype, CL_TYPE_##cl }
static void populate_with_scalar_types(type_db_t tdb,
                                       struct ptr_slist_arr *ptr_arr)
{
    struct {
        struct symbol *ctype;
        enum cl_type_e cl_type;
    } scalar_types[] = {
        TYPE(void, VOID), TYPE(bool, BOOL),
        // CL_TYPE_INT
        TYPE(int, INT),    TYPE(sint, INT),    TYPE(uint, INT),
        TYPE(short, INT),  TYPE(sshort, INT),  TYPE(ushort, INT),
        TYPE(long, INT),   TYPE(slong, INT),   TYPE(ulong, INT),
        TYPE(llong, INT),  TYPE(sllong, INT),  TYPE(ullong, INT),
        TYPE(lllong, INT), TYPE(slllong, INT), TYPE(ulllong, INT),
        // CL_TYPE_CHAR
        TYPE(char, CHAR),  TYPE(schar, CHAR),  TYPE(uchar, CHAR),
    };

    struct cl_type *clt;
    struct symbol *ctype;
    int i;
    for (i = 0; i < ARRAY_SIZE(scalar_types); i++) {
        clt = MEM_NEW(struct cl_type);
        if (!clt)
            die("MEM_NEW failed");
        empty_cl_type(clt);

        ctype = scalar_types[i].ctype;

        clt->code = scalar_types[i].cl_type;
        read_bytesize(&clt->size, ctype->bit_size);
        clt->item_cnt = 0;
        clt->items = NULL;

        type_db_insert(tdb, clt, ctype, NEW_UID);

        // no duplicity checks
        if (ptr_arr->remain_size == 0) {
            ptr_arr->alloc_size += PTRSLISTARR_SIZE;
            ptr_arr->remain_size += PTRSLISTARR_SIZE;
            ptr_arr->heads = MEM_RESIZE(ptr_arr->heads, ptr_arr->alloc_size);
            if (!ptr_arr->heads)
                die("MEM_RESIZE");
        }
        ptr_arr->remain_size--;
        ptr_arr->heads[ptr_arr->pos].clt = clt;
        ptr_arr->heads[ptr_arr->pos].next = NULL;
        ptr_arr->pos++;
    }

}
#undef TYPE

static struct cl_type* add_type_if_needed(struct symbol *type,
                                          struct instruction *insn,
                                          struct ptr_slist_item **ptr);

#if 1
static struct cl_type_item* create_ptr_type_item(struct symbol *type)
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

static void add_nested_type(struct cl_type *clt, struct symbol *sym)
{
    if (clt->items == NULL)
        clt->item_cnt = 0;
    clt->items = MEM_RESIZE(clt->items, clt->item_cnt+1);
    if (!clt->items)
        die("MEM_RESIZE failed");
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

static void add_struct_elem_types(struct cl_type *clt,
                                  struct symbol_list *elems)
{
    struct symbol *sym;

    FOR_EACH_PTR(elems, sym) {
        add_nested_type(clt, sym);
    } END_FOR_EACH_PTR(sym);

    // TODO: not sure why this necessary
    //clt->item_cnt++;
}

static void add_fn_arguments(struct cl_type *clt, struct symbol_list* args)
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

static void read_sparse_type(struct cl_type *clt, struct symbol *type)
{
    enum type code = type->type;

    switch (code) {
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
            // e.g., SYM_PTR should be already handled
            CL_TRAP;
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

// for given type "clt", return respective item from pointer hieararchy;
// it is called only when we know such item will be there (already added)
static void get_ptr_slist(const struct cl_type *clt, struct ptr_slist_item **ptr)
{
    if (clt->code == CL_TYPE_PTR) {
        struct ptr_slist_item *prev = NULL;
        get_ptr_slist(clt->items->type, &prev);
        assert(prev->next);
        *ptr = prev->next;
    } else {
        int i;
        for (i = ptr_slist.alloc_size - ptr_slist.remain_size - 1;
             i >= 0; i--) {
            if (ptr_slist.heads[i].clt == clt)
                break;
        }
        if (i >= 0)
            *ptr = &ptr_slist.heads[i];
        else {
            // should not happen
            CL_TRAP;
        }
    }
}

static struct cl_type* add_type_if_needed(struct symbol *type,
                                          struct instruction *insn,
                                          struct ptr_slist_item **ptr)
{
    struct cl_type *clt;

    if (!type && insn)
        type = get_instruction_type(insn);

    // FIXME: this approach is completely wrong since we get type info for the
    // operand's base however we need to get type info in regards to the given
    // accessor
    skip_sparse_accessors(&type);

    // Fastest path, we have the type already in hash table
    clt = typen_get_by_key(type_db, type);
    if (clt) {
        // type already hashed
        if (ptr)
            get_ptr_slist(clt, ptr);
        return clt;
    }

    // Extra handling of pointer symbols, potentially fast circuit for pointer
    // type alias (i.e. no allocation)
    if (/*ptr &&*/ type && type->type == SYM_PTR) {
        struct ptr_slist_item *prev = NULL;
        struct cl_type *ptr_type, **clt_ptr;
        int uid = NEW_UID;

        ptr_type = add_type_if_needed(type->ctype.base_type, NULL, &prev);
        if (!prev->next) {
            prev->next = MEM_NEW(struct ptr_slist_item);
            if (!prev->next)
                die("MEM_NEW");
            prev->next->next = NULL;
            clt_ptr = &prev->next->clt;
            *clt_ptr = MEM_NEW(struct cl_type);
            if (!*clt_ptr)
                die("MEM_NEW failed");
            empty_cl_type(*clt_ptr);

            // setup ctl
            (*clt_ptr)->code = CL_TYPE_PTR;
            (*clt_ptr)->item_cnt = 1;
            (*clt_ptr)->items = MEM_NEW(struct cl_type_item);
            if (!(*clt_ptr)->items)
                die("MEM_NEW");
            (*clt_ptr)->items->type = ptr_type;
            (*clt_ptr)->items->name = NULL;
        } else
            uid = prev->next->clt->uid;

        if (ptr)
            *ptr = prev->next;
        return type_db_insert(type_db, prev->next->clt, type, uid);
    }

    // Slow path for anything (except for pointers) which is being
    // proceeded for the first time (next time, hashed ctl is used instead)

    clt = MEM_NEW(struct cl_type);
    if (!clt)
        die("MEM_NEW failed");
    empty_cl_type(clt);

    // read type info if available
    if (type) {
        read_sparse_type(clt, type);
        read_sparse_location(&clt->loc, type->pos);
        read_sparse_scope(&clt->scope, type->scope);
        read_bytesize(&clt->size, type->bit_size);
    } else if (insn) {
        // TODO...
        CL_TRAP;
#if 0
        // FIXME: bool et al. not properly handled (sparse does not offer this)
        if (insn->opcode == OP_CALL) {
            CL_TRAP;  // should not get there
            //struct symbol *arg = get_arg_at_pos(insn->func->sym, fargn+1);
            //read_sparse_scalar_type(&clt->code, arg);
        }
        else
#endif
        clt->code = CL_TYPE_INT;
        read_sparse_location(&clt->loc, insn->pos);
        read_bytesize(&clt->size, insn->size);
    }
    // FIXME: this is unwanted "override" behaviour
    if (insn && insn->opcode >= OP_BINCMP && insn->opcode <= OP_BINCMP_END)
        clt->code = CL_TYPE_BOOL;

    if (clt->code == CL_TYPE_UNKNOWN && !clt->name)
        clt->name = strdup("<sparse type not available>");

    // FIXME: no duplicity checks, really not necessary?
    if (ptr_slist.remain_size == 0) {
        ptr_slist.alloc_size += PTRSLISTARR_SIZE;
        ptr_slist.remain_size += PTRSLISTARR_SIZE;
        ptr_slist.heads = MEM_RESIZE(ptr_slist.heads, ptr_slist.alloc_size);
        if (!ptr_slist.heads)
            die("MEM_RESIZE");
    }
    ptr_slist.remain_size--;
    ptr_slist.heads[ptr_slist.pos].clt = clt;
    ptr_slist.heads[ptr_slist.pos].next = NULL;
    ptr_slist.pos++;

    if (ptr)
        *ptr = &ptr_slist.heads[ptr_slist.pos];

    // hash the just read type for next round
    return type_db_insert(type_db, clt, type, NEW_UID);
}


static struct cl_type* clt_from_sym(struct symbol *sym)
{
    if (!sym || !sym->ctype.base_type)
        CL_TRAP;

    return add_type_if_needed(sym->ctype.base_type, NULL, NULL);
}



//
// Symbols/pseudos/operands handling
//

static void read_sym_initializer(struct cl_operand *op, struct expression *expr)
{
    if (!expr)
        return;

    switch (expr->type) {
        case EXPR_STRING:
            op->code          = CL_OPERAND_CST;
            op->type          = clt_from_sym(expr->ctype);
            op->data.cst.code = CL_TYPE_STRING;
            op->data.cst.data.cst_string.value
                              = read_sparse_string(expr->string);
            return;

        default:
            CL_TRAP;
    }
}

static void read_pseudo_sym(struct cl_operand *op, struct symbol *sym, struct symbol *subst_type)
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
        // FIXME: symbols are always to be referenced?
        if (subst_type)
            op->type                            = clt_from_sym(subst_type);
        else
            op->type                            = clt_from_sym(sym);
        op->data.var                        = MEM_NEW(struct cl_var);
        op->data.var->uid                     = /* TODO */ (int)(long) sym;
        op->data.var->name                   = strdup(show_ident(sym->ident));
        if (subst_type) {
            struct cl_accessor *ac = MEM_NEW(struct cl_accessor);
            if (!ac)
                die("NEW_MEM failed");
            ac->code = CL_ACCESSOR_REF;
            ac->type = clt_from_sym(sym);
            ac->next = NULL;
            op->accessor = ac;
        }
    }
}

static void read_pseudo(struct cl_operand *op, pseudo_t pseudo)
{
    switch(pseudo->type) {
        case PSEUDO_SYM:  /* union -> sym */
            read_pseudo_sym(op, pseudo->sym, NULL);
            break;

        case PSEUDO_REG: { /* union -> def */
            op->code                = CL_OPERAND_VAR;
            // note: pseudo->def == NULL for copy.32
            if (pseudo->def)
                op->type                = add_type_if_needed(NULL, pseudo->def, NULL);
            else
                op->type                = add_type_if_needed(&int_ctype, NULL, NULL);
            op->data.var            = MEM_NEW(struct cl_var);
            op->data.var->uid       = /* TODO */ (int)(long) pseudo->def;
            op->data.var->name      = NULL;
            break;
        }

        case PSEUDO_VAL: { /* union -> val */
            long long value = pseudo->value;

            op->code                = CL_OPERAND_CST;
            op->type                = add_type_if_needed(&int_ctype, NULL, NULL);
            op->data.cst.code       = CL_TYPE_INT;
            op->data.cst.data.cst_int.value  = value;
            return;
        }

        case PSEUDO_ARG: { /* union -> def */
            struct symbol *sym = get_arg_at_pos(pseudo->def->bb->ep->name, pseudo->nr);
            if (!sym)
                CL_TRAP;

            op->code                = CL_OPERAND_VAR;
            op->scope               = CL_SCOPE_FUNCTION;
            if (!sym) {
                op->type                = add_type_if_needed(&int_ctype, NULL, NULL);
                op->data.var            = MEM_NEW(struct cl_var);
                op->data.var->uid       = /* TODO */ (int)(long) pseudo->def;
                op->data.var->name      = NULL;
            } else {
                op->type                = clt_from_sym(sym);
                op->data.var            = MEM_NEW(struct cl_var);
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
            CL_TRAP;
    }
}

static void read_insn_op_access(struct cl_operand *op, struct instruction *insn)
{
    struct cl_accessor *ac;
#if 0
    if (insn->type
            && insn->type->ident
            && 0 != strcmp("__ptr", show_ident(insn->type->ident)))
#endif
#if 0
    if (insn->opcode == OP_LOAD &&
        op->type->items[0].type->code != CL_TYPE_STRUCT)
    {
        //WARN_UNHANDLED(insn->pos, "CL_ACCESSOR_ITEM");
        CL_TRAP;
        return;
    }
#endif
    if (op->type->code == CL_TYPE_STRUCT) {
        // struct.elem
        ac = MEM_NEW(struct cl_accessor);
        if (!ac)
            die("MEM_NEW failed");
        ac->code = CL_ACCESSOR_ITEM;
        ac->next = NULL;
        int i;
        for (i = 0; i < op->type->item_cnt; i++)
            if (op->type->items[i].offset == insn->offset)
                break;
        ac->data.item.id = i;
        ac->type = /*dangerous?*/ (struct cl_type *) op->type;
        op->accessor = ac;
        op->type = (struct cl_type *)op->type->items[i].type;
    } else if (op->type->code == CL_TYPE_PTR
               && op->type->items[0].type->code == CL_TYPE_STRUCT) {
        ac = MEM_NEW(struct cl_accessor);
        if (!ac)
            die("MEM_NEW failed");

        ac->code = CL_ACCESSOR_DEREF;
        ac->type = /* TODO */ op->type;
        ac->next = NULL;

        op->accessor = ac;

        ac = MEM_NEW(struct cl_accessor);
        if (!ac)
            die("MEM_NEW failed");
        ac->code = CL_ACCESSOR_ITEM;
        ac->next = NULL;
        int i;
        for (i = 0; i < op->type->items[0].type->item_cnt; i++)
            if (op->type->items[0].type->items[i].offset == insn->offset)
                break;
        ac->data.item.id = i;
        ac->type = /*dangerous?*/ (struct cl_type *) op->type->items[0].type;
        //ac->type = (struct cl_type *) op->type->items[0].type->items[i].type;
        op->accessor->next = ac;
    }
    return;
}

static void empty_cl_operand(struct cl_operand *op)
{
    op->code        = CL_OPERAND_VOID;
    op->scope       = CL_SCOPE_GLOBAL;
    op->loc.file    = NULL;
    op->loc.line    = -1;
    op->type        = NULL;
    op->accessor    = NULL;
}

static void pseudo_to_cl_operand(struct instruction *insn, pseudo_t pseudo,
                                 struct cl_operand *op, bool access)
{
    empty_cl_operand(op);

    if (!is_pseudo(pseudo))
        return;

    if (insn->opcode == OP_PTRCAST && pseudo == insn->src
        /* && pseudo->type == PSEUDO_SYM*/ ) {
        read_pseudo_sym(op, insn->src->sym, insn->orig_type);
    } else {
        read_pseudo(op, pseudo);
        if (access)
            read_insn_op_access(op, insn);
    }
}



//
// Instructions handling functions
//


static void handle_insn_sel(struct instruction *insn)
{
    // at first, create and emit CL_INSN_COND, then
    // create and emit respective basic blocks
    //
    // note: BB label uniqueness: addr(insn) + (1 or 2),
    //       provided that pointer has size of 4+

    struct cl_insn cli;
    struct cl_operand cond, src, dst;
    char *bb_label_true = NULL,
         *bb_label_false = NULL,
         *bb_label_end = NULL;

    // BB labels
    if (asprintf(&bb_label_true, "%p", ((char *) insn) + 1) < 0)
        die("asprintf failed");
    if (asprintf(&bb_label_false, "%p", ((char *) insn) + 2) < 0)
        die("asprintf failed");
    if (asprintf(&bb_label_end, "%p", ((char *) insn) + 3) < 0)
        die("asprintf failed");

    cli.code = CL_INSN_COND;
    read_sparse_location(&cli.loc, insn->pos);

    pseudo_to_cl_operand(insn, insn->src1 , &cond, false);
    cli.data.insn_cond.src = &cond;

    cli.data.insn_cond.then_label = bb_label_true;
    cli.data.insn_cond.else_label = bb_label_false;

    cl->insn(cl, &cli);

    free_cl_operand_data(&cond);

    // first BB ("then" branch)
    cl->bb_open(cl, bb_label_true);

    cli.code = CL_INSN_UNOP;
    cli.data.insn_unop.code = CL_UNOP_ASSIGN;

    pseudo_to_cl_operand(insn, insn->target , &dst, false);
    cli.data.insn_unop.dst = &dst;
    pseudo_to_cl_operand(insn, insn->src2 , &src, false);
    cli.data.insn_unop.src = &src;

    cl->insn(cl, &cli);
    free_cl_operand_data(&src);

    cli.code = CL_INSN_JMP;
    cli.data.insn_jmp.label = bb_label_end;
    cl->insn(cl, &cli);

    // second BB ("else" branch) .. warning: copy-paste from above
    cl->bb_open(cl, bb_label_false);

    cli.code = CL_INSN_UNOP;
    cli.data.insn_unop.code = CL_UNOP_ASSIGN;

    pseudo_to_cl_operand(insn, insn->src3 , &src, false);
    cli.data.insn_unop.src = &src;

    cl->insn(cl, &cli);
    free_cl_operand_data(&src);
    free_cl_operand_data(&dst);

    cli.code = CL_INSN_JMP;
    cli.data.insn_jmp.label = bb_label_end;
    cl->insn(cl, &cli);

    // merging BB
    cl->bb_open(cl, bb_label_end);
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
        struct cl_operand arg_operand;
        if (arg->type == PSEUDO_SYM) {
            empty_cl_operand(&arg_operand);
            read_pseudo_sym(&arg_operand, arg->sym, /*TODO*/ arg->sym);
        } else {
            pseudo_to_cl_operand(insn, arg, &arg_operand, false);
        }

        cl->insn_call_arg(cl, ++cnt, &arg_operand);
        free_cl_operand_data(&arg_operand);
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
            val_lo.type = add_type_if_needed(&int_ctype, NULL, NULL);
            val_hi.type = add_type_if_needed(&int_ctype, NULL, NULL);

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

    pseudo_to_cl_operand(insn, insn->src, &op, true);
    cli.code                = CL_INSN_RET;
    cli.data.insn_ret.src   = &op;
    read_sparse_location(&cli.loc, insn->pos);
    cl->insn(cl, &cli);
    free_cl_operand_data(&op);
}

static void insn_assignment_base(struct instruction *insn,
                                 pseudo_t lhs       ,  pseudo_t rhs       ,
                                 bool     lhs_access,  bool     rhs_access)
{
    struct cl_operand op_lhs;
    struct cl_operand op_rhs;

    pseudo_to_cl_operand(insn, rhs, &op_rhs, rhs_access);

    if (lhs->type == PSEUDO_VAL /* && lhs->value == 0 */
        && op_rhs.type->code == CL_TYPE_PTR) {
        op_lhs.code = CL_OPERAND_CST;
        op_lhs.type = op_rhs.type;
        op_lhs.accessor = NULL;
        op_lhs.data.cst.code = CL_TYPE_INT;
        op_lhs.data.cst.data.cst_int.value = lhs->value;
    } else {
        pseudo_to_cl_operand(insn, lhs, &op_lhs, lhs_access);
    }
    if (rhs->type == PSEUDO_VAL /* && rhs->value == 0 */
        && op_lhs.type->code == CL_TYPE_PTR) {
        op_rhs.code = CL_OPERAND_CST;
        op_rhs.type = op_lhs.type;
        op_rhs.accessor = NULL;
        op_rhs.data.cst.code = CL_TYPE_INT;
        op_rhs.data.cst.data.cst_int.value = rhs->value;
    }


#if 0
    if (op_lhs.access && op_lhs.name && op_lhs.offset
            && 0 == strcmp(op_lhs.name, op_lhs.offset))
        CL_TRAP;

    if (op_rhs.access && op_rhs.name && op_rhs.offset
            && 0 == strcmp(op_rhs.name, op_rhs.offset))
        CL_TRAP;
#endif

    // TODO: move to function?
    // FIXME SPARSE?: hack because sparse generates extra instruction
    //         e.g. store %arg1 -> 0[in] if "in" == "%arg1"
#if 1
    if (lhs->type != PSEUDO_SYM || rhs->type != PSEUDO_ARG
         || op_lhs.data.var->uid != op_rhs.data.var->uid) {
#endif
        struct cl_insn cli;
        cli.code                    = CL_INSN_UNOP;
        cli.data.insn_unop.code     = CL_UNOP_ASSIGN;
        cli.data.insn_unop.dst      = &op_lhs;
        cli.data.insn_unop.src      = &op_rhs;
        read_sparse_location(&cli.loc, insn->pos);
        cl->insn(cl, &cli);
#if 1
    } else {
        WARN_VA(insn->pos, "instruction omitted: %s", show_instruction(insn));
    }
#endif

    free_cl_operand_data(&op_lhs);
    free_cl_operand_data(&op_rhs);
}

static void handle_insn_store(struct instruction *insn)

{
    //CL_TRAP;
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

static void handle_insn_ptrcast(struct instruction *insn)
{
    //CL_TRAP;
    insn_assignment_base(insn,
            insn->target, insn->src,
            false        , false);
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
    if (verbose & CL_VERBOSE_INSTRUCTION)
        NOTE("\t%d: instruction to be processed: %s", insn->pos.line, show_instruction(insn));

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
        case OP_SUB:
            handle_insn_binop(insn, CL_BINOP_MINUS);
            break;
        case OP_MULU:
        case OP_MULS:
            handle_insn_binop(insn, CL_BINOP_MULT);
            break;
        case OP_DIVU:
        case OP_DIVS:
            handle_insn_binop(insn, CL_BINOP_TRUNC_DIV);
            break;
        case OP_MODU:
        case OP_MODS:
            handle_insn_binop(insn, CL_BINOP_TRUNC_MOD);
            break;

        WARN_CASE_UNHANDLED(insn->pos, OP_SHL)
        WARN_CASE_UNHANDLED(insn->pos, OP_LSR)
        WARN_CASE_UNHANDLED(insn->pos, OP_ASR)

        /* Logical */
        case OP_AND:
            handle_insn_binop(insn, CL_BINOP_BIT_AND);
            break;
        case OP_OR:
            handle_insn_binop(insn, CL_BINOP_BIT_IOR);
            break;
        WARN_CASE_UNHANDLED(insn->pos, OP_XOR)

        case OP_AND_BOOL:
            handle_insn_binop(insn, CL_BINOP_TRUTH_AND);
            break;
        case OP_BINARY_END:
            handle_insn_binop(insn, CL_BINOP_TRUTH_OR);
            break;

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
        case OP_SEL:
            handle_insn_sel(insn);
            break;

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
            handle_insn_copy(insn);
            break;

        case OP_PTRCAST:
            handle_insn_ptrcast(insn);
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
        // TODO: JP
        op.data.var = MEM_NEW(struct cl_var);
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
// Code listener related setup
//


#define _(...) printf(__VA_ARGS__); printf("\n");
#define __ _("");
static void print_help(const char *cmd)
{
    _("sparse-based code listener frontend")
    __
    _("usage: %s (cl frontend args | sparse args)*", cmd)
    __
    _("For sparse args, see sparse documentataion; these args are generally")
    _("compatible with those for gcc and sparse ignores unrecognized ones.")
    __
    _("This code listener fronted also defines few args/options on its own:")
    __
    _("-h, --help              Prints this help text")
    _("-cl-verbose[=MASK]      Be verbose (selectively if MASK provided)")
    _("-cl-dump-pp             Dump pretty-printed linearized code")
    _("-cl-dump-type           Add type information to such pretty-printed code")
    _("... TODO ...")
}
#undef __
#undef _

struct cl_plug_options {
    bool                    dump_types;
    bool                    use_dotgen;
    bool                    use_pp;
    bool                    use_typedot;
    const char              *gl_dot_file;
    const char              *pp_out_file;
    const char              *type_dot_file;
};

static int handle_cl_args(int argc, char *argv[],
                          struct cl_plug_options *opt)
{
    char *value;

    // initialize opt data
    memset(opt, 0, sizeof(*opt));

    // handle plug-in args
    int i = 0;
    while (++i < argc) {
        if ((value = OPTPREFIXEQ_CL(argv[i], "verbose"))) {
            verbose = OPTVALUE(value)
                ? atoi(value)
                : ~0;

        } else if (((value = OPTPREFIXEQ_SHORT(argv[i], "h"))
                    || (value = OPTPREFIXEQ_LONG(argv[i], "help")))
                   && *value == '\0') {
            print_help(argv[0]);
            return EXIT_FAILURE;

        /*} else if ((value = OPTPREFIXEQ_CL(argv[i], "args"))) {
            opt->peer_args = OPTVALUE(value)
                ? value
                : "";*/
        /*} else if (OPTPREFIXEQ_CL(argv[i], "dry-run")) {
            opt->use_peer       = false;
            // TODO: warn about ignoring extra value? */

        } else if ((value = OPTPREFIXEQ_CL(argv[i], "dump-pp"))) {
            opt->use_pp         = true;
            opt->pp_out_file    = OPTVALUE(value);

        } else if (OPTPREFIXEQ_CL(argv[i], "dump-types")) {
            opt->dump_types     = true;
            // TODO: warn about ignoring extra value?

        } else if ((value = OPTPREFIXEQ_CL(argv[i], "gen-dot"))) {
            opt->use_dotgen     = true;
            opt->gl_dot_file    = OPTVALUE(value);

        /*} else if (OPTPREFIXEQ_CL(argv[i], "preserve-ec")) {
            // FIXME: do not use gl variable, use the pointer user_data instead
            preserve_ec = true;
            // TODO: warn about ignoring extra value?*/

        } else if ((value = OPTPREFIXEQ_CL(argv[i], "type-dot"))) {
            if (OPTVALUE(value)) {
                opt->use_typedot    = true;
                opt->type_dot_file  = value;
            } else {
                ERROR("mandatory value omitted for type-dot");
                return EXIT_FAILURE;
            }
        }
    }

    return EXIT_SUCCESS;
}

static bool cl_append_listener(struct cl_code_listener *chain,
                               const char *fmt, ...)
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

static bool cl_append_def_listener(struct cl_code_listener *chain,
                                   const char *listener, const char *args,
                                   const struct cl_plug_options *opt)
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
static
int worker_loop(struct cl_code_listener *cl, int argc, char **argv)
{
    char *file;
    struct string_list *filelist = NULL;
    struct symbol_list *symlist;

    // initialize sparse
    symlist = sparse_initialize(argc, argv, &filelist);

    // initialize type database
    type_db = type_db_create();
    populate_with_scalar_types(type_db, &ptr_slist);

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
    type_db_destroy(type_db);
    cl->acknowledge(cl);
    CLEANUP(NOKILL, cl);

    return EXIT_SUCCESS;
}

// Master loop (grab worker's stderr via read_fd, when work is over, print it)
// Note: used in fork setup only
#define BUFFSIZE          4096
static
int master_loop(struct cl_code_listener *cl, int read_fd, pid_t pid)
{
      size_t alloc_size = 0, remain_size = 0;
      ssize_t read_size;
      char *buffer = NULL;
      int stat_loc, res = 0;
      struct pollfd fds = { .fd = read_fd, .events = POLLIN };

      for (;;) {
          if (poll(&fds, 1, -1) == -1) {
              if (errno == EINTR)
                  continue;
              PERROR_CLEANUP_EXIT("pol", pid, cl, 2);
          } else if (fds.revents & POLLHUP)
            // worker has finished
            break;

          if (!remain_size) {
              alloc_size += BUFFSIZE;
              remain_size = BUFFSIZE;
              buffer = MEM_RESIZE(buffer, alloc_size);
              if (!buffer) PERROR_CLEANUP_EXIT("MEM_RESIZE", pid, cl, 2);
          }
          read_size = read(read_fd, &buffer[alloc_size-remain_size],
                           remain_size);
          if (read_size == -1) PERROR_CLEANUP_EXIT("read", pid, cl, 2);
          remain_size -= read_size;
      }

      if (wait(&stat_loc) == (pid_t)-1) PERROR_CLEANUP_EXIT("wait", pid, cl, 2);
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



//
// Main
//


int main(int argc, char *argv[])
{
    int retval;
    struct cl_plug_options opt;

    real_stderr = stderr; // use this if you need "unfaked" stderr

    // initialize code listener
    if (retval = handle_cl_args(argc, argv, &opt))
        return retval;
    static struct cl_init_data init = {
        .debug = trivial_printer,
        .warn  = cl_warn,
        .error = cl_error,
        .note  = trivial_printer,
        .die   = trivial_printer
    };

    cl_global_init(&init);
    cl = create_cl_chain(&opt);
    if (!cl)
        // error message already emitted
        return EXIT_FAILURE;

#if DO_FORK
    // set up pipe
    int fildes[2];
    if (pipe(fildes) == -1) PERROR_CLEANUP_EXIT("pipe", NOKILL, cl, 2);
    // master-worker fork
    pid_t pid = fork();
    if (pid == -1) PERROR_CLEANUP_EXIT("fork", NOKILL, cl, 2);
    else if (pid == 0) {
        // child = worker, use fildes[1] for writing

        if (close(fildes[0]) == -1) PERROR_EXIT("close", 2);
        if (!redefine_stderr(fildes[1], &real_stderr))
            PERROR_EXIT("Redefining stderr", 2);
#endif

        // main processing loop
        retval = worker_loop(cl, argc, argv);

#if DO_FORK
        if (fclose(real_stderr) == EOF || close(fildes[1]) == -1)
            PERROR_EXIT("fclose/close", 2);
    } else {
        // parent = master, use fildes[0] for reading

        if (close(fildes[1]) == -1) PERROR_CLEANUP_EXIT("close", pid, cl, 2);
        // master loop -- gather what sparse produce to stderr
        retval = master_loop(cl, fildes[0], pid);
    }
#endif

    return retval;
}
