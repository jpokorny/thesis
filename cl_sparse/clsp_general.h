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

#ifndef CLSP_GENERAL_H_GUARD
#define CLSP_GENERAL_H_GUARD

#define NORETWRN(expr)  (void)(expr)
#define APPLY(next,...)      next(__VA_ARGS__)

#define JOIN(a,b)  a##b
#define IDENTITY(what)  (what)


#define STRINGIFY(arg)              #arg
#define TOSTRING(arg)               STRINGIFY(arg)

//#define PRAGMA_MSG(arg)             PRAGMA_MSG_(arg)
#define PRAGMA_MSG(arg)             PRAGMA_MSGSTR(TOSTRING(arg))
#define PRAGMA_MSGSTR(arg)          PRAGMA_MSGSTR_(message arg)
#define PRAGMA_MSGSTR_(arg)         _Pragma(#arg)
#define COMPILE_TIME_ASSERT(pred)   switch(0){case 0:case pred:;}


// beware of side-effects
#define PARTIALLY_ORDERED(a, b, c)  (a <= b && b <= c)

/* common macros */

#ifndef STREQ
# define STREQ(s1, s2)  (!strcmp(s1, s2))
#endif

/*
 *  allocation
 */

/*  NOTE: use "MEM_NEW(foo)" for direct access ("MEM_NEW(foo).bar = 42")
          and "(MEM_NEW(foo))" as a function parameter ("bar((MEM_NEW(foo)))")
 */


#define MEM_NEW(var)                        \
   (((var) = malloc(sizeof(*(var))))        \
     ? (void) 0  /* NOOP */                 \
     : DIE( ERRNOCODE(EC_MEM, "MEM_NEW") )  \
   ) , (var)

#define MEM_ARR_RESIZE(arr, newcnt)                      \
    (((arr) = realloc((arr), sizeof(*(arr)) * (newcnt))) \
      ? (void) 0 /* NOOP */                              \
      : DIE( ERRNOCODE(EC_MEM, "MEM_ARR_RESIZE") )       \
    ) , (arr)

/* NOTE: one-liner for item append: *(MEM_ARR_APPEN(arr,size)) = item */
#define MEM_ARR_APPEND(arr, oldcnt)                          \
    (((arr) = realloc((arr), sizeof(*(arr)) * (++(oldcnt)))) \
      ? (void) 0 /* NOOP */                                  \
      : DIE( ERRNOCODE(EC_MEM, "MEM_ARR_APPEND") )           \
    ) , (arr + oldcnt - 1)


#endif
/* vim:set ts=4 sts=4 sw=4 et: */
