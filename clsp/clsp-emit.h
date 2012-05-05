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
#ifndef CLSP_EMIT_H_GUARD
#define CLSP_EMIT_H_GUARD

#include "clsp.h"
#include "clsp-types.h"
#include "clsp-use-sparse.h"
#include "clsp-use-cl.h"

/**
    Enumerated mutually exclusive flag positions denoting the effort
    regarding defective files.
 */
enum emit_props {
    emit_vanilla               = 0,
    /* applies to top-level */
    emit_dry_run               = 1 << 1,
    emit_skip_initial          = 1 << 2,  /**< whether to skip "initial file" */
    /* applies to files proceeding level */
    emit_files_keep_going      = 1 << 3,
    /* applies to file proceeding level */
    emit_file_try_hard         = 1 << 4,
    emit_file_private          = 1 << 5,  /**< as usual, but w/o emitting */
    emit_file_interactive      = 1 << 6,
};

/**
    Feed Code Listener with (heavy-lifted) sparse linearized code.

    In case of unrecoverable error (on both local and sparse side),
    dies immediately.

    @param[in] filelist    List of files selected for proceeding
    @param[in] symlist     Internal symbols obtained by sparse_initialize call
    @param[in] emit_props  OR'ed @c emit_props enumeration values
    @return    See @c retval enumeration
    @note  Expects @c sparse_initialize to be the only function called so far.
 */
extern enum retval emit(struct string_list *filelist,
                        struct symbol_list *symlist, int props);


/**
    xxx
 */
extern void type_ptr_db_init(struct type_ptr_db *db);

/**
    xxx
 */
extern void type_ptr_db_destroy(struct type_ptr_db *db);


/* type "constructor" */


extern const struct cl_type pristine_cl_type;

static inline struct cl_type *
empty_type(struct cl_type* clt)
{
    *clt = pristine_cl_type;
    return clt;
}

static inline struct cl_type *
new_type(void)
{
    struct cl_type *retval;
    return empty_type(MEM_NEW(retval));  // guaranteed not to return NULL
}



static inline int
sizeof_from_bits(int bits)
{/* Alternative:
  * bytes_to_bits (sparse/target.h)
  *     - cons: we need the ceil value (1 bit ~ 1 byte), 0 in "strange" cases
  */
    return (bits > 0)
        ? (bits + bits_in_char - 1) / bits_in_char
        : 0;
}


// NOTE: clt->item_cnt can be uninitialized provided that clt->items is NULL
static inline struct cl_type_item *
type_append_item(struct cl_type *clt)
{
    struct cl_type_item *ret;

    if (!clt->items)
        clt->item_cnt = 0;

    // guaranteed to continue only in case of success
    ret = MEM_ARR_APPEND(clt->items, clt->item_cnt);
    ret->type = NULL;
    return ret;
}


extern struct cl_type *build_referenced_type(struct cl_type *orig_clt);


extern struct cl_type *type_ptr_db_insert(struct type_ptr_db *db,
                                          struct cl_type *clt,
                                          const struct symbol *type,
                                          struct ptr_db_item **ptr);

extern struct cl_type *type_ptr_db_lookup_item(struct type_ptr_db *db,
                                               const struct symbol *type,
                                               struct ptr_db_item **ptr);

extern struct ptr_db_item *new_ptr_db_item(void);

#endif
