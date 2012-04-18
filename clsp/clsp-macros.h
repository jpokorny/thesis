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
#ifndef CLSP_MACROS_H_GUARD
#define CLSP_MACROS_H_GUARD
/**
    Little basic macros.
 */

/* Makes ignoring return value explicit */
#define NORETWRN(expr)         (void)(expr)

/* Basic "functions" */
#define APPLY(next,...)        next(__VA_ARGS__)
#define APPLY_INNER(next,...)  next(__VA_ARGS__)
#define IDENTITY(what)         what

/* Preprocessor lexical manipulations */
#define JOIN(a,b)              a##b
#define STRINGIFY(arg)         #arg
#define TOSTRING(arg)          STRINGIFY(arg)

/*
    Compile-time (unless applied on VLAs) strlen with several checks:
    - arg is a pointer/array:          sizeof(*arg)
    - arg is not a NULL pointer:       arg[sizeof(arg)-1]
    - arg is array of "byte-type":     sizeof(char)-sizeof(*arg)
    - last byte of arg has zero value (null-terminated string):
                                       (result so far) - !!arg[sizeof(arg)-1]
 */
#define CONST_STRLEN(arg) \
    sizeof(arg) - sizeof(char[1+sizeof(char)-sizeof(*arg)-!!arg[sizeof(arg)-1]])


/* Pragma as a function-like macro (promoted from directive) */
#define PRAGMA_MSG(arg)        PRAGMA_MSG_(arg)
#define PRAGMA_MSG_(arg)       PRAGMA_MSGSTR(TOSTRING(arg))
#define PRAGMA_MSGSTR(arg)     PRAGMA_MSGSTR_(message arg)
#define PRAGMA_MSGSTR_(arg)    _Pragma(#arg)

/*
#define COMPILE_TIME_ASSERT(pred)   switch(0){case 0:case pred:;}
#define BUILD_BUG_ON(condition) ((void)sizeof(char[1 - 2*!!(condition)]))
*/

/* NOTE: beware of side-effects */
#define PARTIALLY_ORDERED(a, b, c)  (a <= b && b <= c)

/* Guarded due to possible collision with Code Listener */
#ifndef STREQ
# define STREQ(s1, s2)  (!strcmp(s1, s2))
#endif

/* Variable length arguments macros */

/*
  see, e.g.,
  http://cplusplus.co.il/2010/07/17/variadic-macro-to-count-number-of-arguments/
  but the technique is wide-spread
*/
#define VA_ARGS_CNT(...)  VA_ARGS_CNT_IMPL(__VA_ARGS__,9,8,7,6,5,4,3,2,1,_)
#define VA_ARGS_CNT_IMPL(_1,_2,_3,_4,_5,_6,_7,_8,_9,N,...)  N


#endif
