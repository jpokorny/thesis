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
#ifndef CLSP_ALLOC_H_GUARD
#define CLSP_ALLOC_H_GUARD
/**
    Convenient malloc/realloc error-handling encapsulation
 */

#include "clsp.h"

#include <stdlib.h>  /* malloc, realloc */


#define MEM_NEW(var)                    \
   ((((var) = malloc(sizeof(*(var))))   \
     ? (void) 0  /* NOOP */             \
     : DIE( ERRNOCODE(MEM, "MEM_NEW") ) \
   ) , (var))

#define MEM_ARR_RESIZE(arr, newcnt)                       \
    ((((arr) = realloc((arr), sizeof(*(arr)) * (newcnt))) \
      ? (void) 0 /* NOOP */                               \
      : DIE( ERRNOCODE(MEM, "MEM_ARR_RESIZE") )           \
    ) , (arr))

/* NOTE: one-liner for item append: *(MEM_ARR_APPEND(arr,size)) = item */
#define MEM_ARR_APPEND(arr, oldcnt)                           \
    ((((arr) = realloc((arr), sizeof(*(arr)) * (++(oldcnt)))) \
      ? (void) 0 /* NOOP */                                   \
      : DIE( ERRNOCODE(MEM, "MEM_ARR_APPEND") )               \
    ) , (arr + oldcnt - 1))

#define MEM_ARR_APPEND_NEW(arr, oldcnt)                       \
    ((((arr) = realloc((arr), sizeof(*(arr)) * (++(oldcnt)))) \
      ? (void) 0 /* NOOP */                                   \
      : DIE( ERRNOCODE(MEM, "MEM_ARR_APPEND_NEW") )           \
    ) , MEM_NEW(*(arr + oldcnt - 1)))

#endif
