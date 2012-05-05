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

#include "clsp-emit.h"
#include "type_enumerator.h"

// yo, Dawg...
#define OR  : case
#define IN(choices)  , choices
#define COND_WHICH(cond, which) \
    switch (cond) {             \
        case which:
#define BEGIN_WHEN(cond_which)  \
        COND_WHICH(cond_which)
#define END_WHEN                \
            ; break;            \
        default:                \
            break;              \
    }


// TODO: cl_loc_unknown
#define EMPTY_LOC  { .file=NULL, .line=-1, .column=-1, .sysp=false }

const struct cl_type pristine_cl_type = {
    .uid        = NEW_UID,  ///< in control of type_enumerator
    .code       = CL_TYPE_UNKNOWN,
    .loc        = EMPTY_LOC,
    .scope      = CL_SCOPE_GLOBAL,
    .name       = NULL,
    .size       = 0,
    .item_cnt   = 0,
    .items      = NULL,
    //.array_size = 0,
};

/* sparse - code listener types mapping */

// associated with respective base types in `populate_with_base_types()'
struct cl_type
    void_clt,
    incomplete_clt,
    bad_clt,
    int_clt,  sint_clt,  uint_clt,     short_clt, sshort_clt, ushort_clt,
    long_clt, slong_clt, ulong_clt,    llong_clt, sllong_clt, ullong_clt,
    // lllong_clt, slllong_clt, ulllong_clt
    char_clt, schar_clt, uchar_clt,
    bool_clt,
    float_clt, double_clt, ldouble_clt,
    string_clt;


static const struct {
    struct cl_type  *ref;
    struct symbol   *type;
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
#define TYPE(sym, clt) \
    { &sym##_clt, &sym##_ctype, CL_TYPE_##clt, #sym }
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

    /* CL_TYPE_STRING */
    TYPE(string,  STRING),

#undef TYPE
};


static inline struct ptr_db_item *
empty_ptr_db_item(struct ptr_db_item *item, struct cl_type *clt)
{
    item->clt       = clt;
    item->next      = NULL;
    item->arr_cnt   = 0;
    item->arr       = NULL;
    item->free_type = false;

    return item;
}

struct ptr_db_item *
new_ptr_db_item(void)
{
    struct ptr_db_item *retval;
    // guaranteed not to return NULL
    return empty_ptr_db_item((MEM_NEW(retval)), NULL);
}



/* freeing resources connected with type */

static void
free_type(struct cl_type *clt)
{
    // skip base types that are not on heap
    if (clt->uid > TYPEPTRDB->last_base_type_uid) {

        /* clt->name */
        free((char *) clt->name);

        /* clt->items */
        // selective approach can expose wrong usage through leaked memory
        BEGIN_WHEN(clt->code IN (CL_TYPE_PTR    OR
                                 CL_TYPE_STRUCT OR
                                 CL_TYPE_UNION  OR
                                 CL_TYPE_ARRAY  OR
                                 CL_TYPE_FNC    ))
        {
            int i;
            for (i = 0; i < clt->item_cnt; i++) {
                /* clt->items[i].type (skipped) */

                /* clt->items[i].name */
                free((char *) clt->items[i].name);
            }
            free(clt->items);
        }
        END_WHEN

        /* clt (heap!) */
        free(clt);
    }
}


// for given type "clt", return respective item from pointer hierarchy;
// it is called only when we know such item will be there (already added)
static struct ptr_db_item *
type_ptr_db_lookup_ptr(struct ptr_db_arr *ptr_db, const struct cl_type *clt)
{
    if (clt->code == CL_TYPE_PTR)
        return type_ptr_db_lookup_ptr(ptr_db, clt->items->type)->next;

    size_t i;
    for (i = 0; i < ptr_db->last; i++)
        if (ptr_db->heads[i].clt == clt)
            break;

    if (i < ptr_db->last)
        return &ptr_db->heads[i];

    // not found ... should not happen
    CL_TRAP;
    return NULL;
}




// Note: use `build_referenced_type' when possible
static inline struct cl_type *
referenced_type(const struct cl_type* orig_type)
{/* Problems/exceptions/notes:
  * 1. Reusing location and scope from `orig_type'.
  */

    struct cl_type *retval;
    MEM_NEW(retval);  // guaranteed not to return NULL

    retval->uid   = TYPEPTRDB->last_base_type_uid+1;
    retval->code  = CL_TYPE_PTR;
    retval->loc   = orig_type->loc;
    retval->scope = orig_type->scope;
    retval->name  = NULL;
    retval->size  = sizeof_from_bits(bits_in_pointer);
    retval->items = NULL;

    struct cl_type_item *item = type_append_item(retval);
    item->type    = orig_type;
    item->name    = NULL;

    // guaranteed not to return NULL
    return retval;
}



struct cl_type *
build_referenced_type(struct cl_type *orig_clt)
{
    struct ptr_db_arr *ptr_db = &(TYPEPTRDB->ptr_db);

    struct ptr_db_item *prev;
    prev = type_ptr_db_lookup_ptr(ptr_db, orig_clt);

    if (!prev->next) {
        prev->next = new_ptr_db_item();
        prev->next->clt = referenced_type(orig_clt);
        prev->next->free_type = true;
    }

    return prev->next->clt;
}


struct cl_type *
type_ptr_db_insert(struct type_ptr_db *db, struct cl_type *clt,
                   const struct symbol *type, struct ptr_db_item **ptr)
#define PTRDBARR_SIZE  (128)
{
    WITH_DEBUG_LEVEL(d_tins) {
        if (clt->loc.file)
            PUT(debug, CLPOSFMT_1 ": " HIGHLIGHT("type-db") ": add "
                       HIGHLIGHT(_4(s)) " (uid="_5(d)", clt="_6(p)
                       ", type="_7(p)")",
                       CLPOS(clt->loc), SP(show_typename, type),
                       clt->uid, (void *) clt, (void *) type);
        else
            PUT(debug, "\t" HIGHLIGHT("type-db") ": add "
                       HIGHLIGHT(_1(s))
                       " (uid="_2(d)", clt="_3(p)", type="_4(p)")",
                       SP(show_typename, type),
                       clt->uid, (void *) clt, (void *) type);
    }

    struct cl_type *retval;
    int uid = clt->uid;

    retval = typen_insert_with_uid(db->type_db, clt, (void *) type);
    if (!retval)
        DIE( ECODE(TDB, "typen_insert_with_uid failed") );

    if (uid == NEW_UID && type->type != SYM_PTR) {
        // track this really new type also in the pointer hierarchy
        // (at the base level, i.e. no pointer, and respective pointers
        // will be captured in connected singly-linked list)
        struct ptr_db_arr *ptr_db = &db->ptr_db;
        if (!(ptr_db->alloc_size - ptr_db->last)) {
            ptr_db->alloc_size += PTRDBARR_SIZE;
            // guaranteed to continue only in case of success
            MEM_ARR_RESIZE(ptr_db->heads, ptr_db->alloc_size);
        }
        empty_ptr_db_item(&ptr_db->heads[ptr_db->last], clt);

        if (ptr)
            *ptr = &ptr_db->heads[ptr_db->last];

        ptr_db->last++;
    } else if (type->type == SYM_ARRAY /* && uid != NEW_UID */) {
        if (ptr)
            *ptr = type_ptr_db_lookup_ptr(&db->ptr_db, clt);
    }

    // guaranteed to NOT return NULL
    return retval;
}


static void
populate_with_base_types(struct type_ptr_db *db)
{
    struct symbol *type;
    struct cl_type *clt;
    int i;
    for (i = 0; i < ARRAY_SIZE(base_types); i++) {
        clt = base_types[i].ref;
        empty_type(clt);

        type = base_types[i].type;

        clt->code        = base_types[i].cl_type;
        clt->scope       = CL_SCOPE_GLOBAL;
        clt->name        = base_types[i].name;
        clt->size        = sizeof_from_bits(type->bit_size);
        clt->is_unsigned = type->ctype.modifiers & MOD_UNSIGNED;

        // insert into hash table + pointer hierarchy (at base level)
        type_ptr_db_insert(db, clt, type, NULL);
    }

    // set uid of the last type inserted so we can skip the freeing for these
    db->last_base_type_uid = clt->uid;
}


struct cl_type *
type_ptr_db_lookup_item(struct type_ptr_db *db, const struct symbol *type,
                        struct ptr_db_item **ptr)
{
    struct cl_type *clt = typen_get_by_key(db->type_db, (void *) type);
    if (clt && ptr)
        *ptr = type_ptr_db_lookup_ptr(&db->ptr_db, clt);

    return clt;
}


void
type_ptr_db_init(struct type_ptr_db* db)
{
    db->type_db = typen_create(free_type);
    if (!db->type_db)
        DIE( ECODE(TDB, "ht_create() failed") );

    // fill with base types
    populate_with_base_types(db);
}

void
type_ptr_db_destroy(struct type_ptr_db* db)
{
    /* todo: check double free, etc. */

    if (db->type_db)
        typen_destroy(db->type_db);

    // destroy pointer hierarchy
    struct ptr_db_arr *ptr_db = &db->ptr_db;
    struct ptr_db_item *item, *item_next;
    int i;
    for (i = 0; i < ptr_db->last; i++) {
        item = &ptr_db->heads[i];

        /* item->clt (skipped, except for those explicitly flagged) */
        if (item->free_type)
            free_type(item->clt);

        /* item->arr */
        //int j;

        /* XXX off-by-one error */
        //PUT(err, "cnt is:" _1(d), item->arr_cnt);
        //for (j = 1; j < item->arr_cnt; j++)
        //    free(item->arr[j-1]);
        free(item->arr);

        // move onto next items, this one captured by `free(db->ptr_db.heads)'
        item = item->next;

        while (item) {
            item_next = item->next;

            /* item->clt (skipped, except for those explicitly flagged) */
            if (item->free_type)
                free_type(item->clt);

            free(item);
            item = item_next;
        }
    }
    free(ptr_db->heads);
}
