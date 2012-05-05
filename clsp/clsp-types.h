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


extern struct cl_type
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

#endif
