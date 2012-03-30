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

/* Basic "functions". */
#define APPLY(next,...)        next(__VA_ARGS__)
#define IDENTITY(what)         what

/* Preprocessor lexical manipulations */
#define JOIN(a,b)              a##b
#define STRINGIFY(arg)         #arg
#define TOSTRING(arg)          STRINGIFY(arg)

/* Pragma as a function-like macro (promoted from directive) */
#define PRAGMA_MSG(arg)        PRAGMA_MSG_(arg)
#define PRAGMA_MSG_(arg)       PRAGMA_MSGSTR(TOSTRING(arg))
#define PRAGMA_MSGSTR(arg)     PRAGMA_MSGSTR_(message arg)
#define PRAGMA_MSGSTR_(arg)    _Pragma(#arg)

/*
#define COMPILE_TIME_ASSERT(pred)   switch(0){case 0:case pred:;}
*/

/* NOTE: beware of side-effects */
#define PARTIALLY_ORDERED(a, b, c)  (a <= b && b <= c)

/* Guarded due to possible collision with Code Listener */
#ifndef STREQ
# define STREQ(s1, s2)  (!strcmp(s1, s2))
#endif


#endif
/* vim:set ts=4 sts=4 sw=4 et: */
