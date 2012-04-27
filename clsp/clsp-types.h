/*
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
#ifndef CLSP_TYPES_H_GUARD
#define CLSP_TYPES_H_GUARD


#include "clsp-use-cl.h"  /* cl_type */

//
// pointer and array DB, for building pointer* and array hierarchy in order to
// prevent having two semantically same pointers/arrays as two different types
//

struct arr_db_item {
    int             arr_size;
    struct cl_type  *clt;
};

struct ptr_db_item {
    struct cl_type      *clt;
    struct ptr_db_item  *next;
    size_t              arr_cnt;
    struct arr_db_item  **arr;
    bool                free_type;  ///< whether we are responsible for clt
};

struct ptr_db_arr {
    size_t              alloc_size;
    size_t              last;
    struct ptr_db_item  *heads;
};


extern struct type_ptr_db {
    int                 last_base_type_uid;
    struct typen_data   *type_db;
    struct ptr_db_arr   ptr_db;
} type_ptr_db;


#endif
