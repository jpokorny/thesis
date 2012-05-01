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
#ifndef CLSP_MACROS_H_GUARD
#define CLSP_MACROS_H_GUARD
/**
    Handful of basic macros with (almost) no dependencies
 */

#include "assert.h"

/* makes ignoring return value explicit */
#define NORETWRN(e_)          (void)(e_)

/* basic functional programming */
#define APPLY(f_, ...)        f_(__VA_ARGS__)
#define APPLY_INNER(f_, ...)  f_(__VA_ARGS__)
#define IDENTITY(a_)          a_

/* preprocessor lexical manipulations */
#ifndef DO_STRINGIFY  /* the same defined on sparse side */
# define DO_STRINGIFY(a_)     #a_
# define STRINGIFY(a_)        DO_STRINGIFY(a_)
#endif
#define JOIN(a_,b_)           a_##b_


/*
    _Pragma operator made convenient
 */
#define PRAGMA_MSG(arg)       PRAGMA_MSG_(arg)
#define PRAGMA_MSG_(arg)      PRAGMA_MSGSTR(STRINGIFY(arg))
#define PRAGMA_MSGSTR(arg)    PRAGMA_(message arg)
#define PRAGMA_RAW(arg)       PRAGMA_(arg)
#define PRAGMA_(arg)          _Pragma(#arg)


/*
    string manipulations
 */


/*
    compile-time (unless applied on VLAs) strlen with several checks:
    - arg is a pointer/array:          sizeof(*arg)
    - arg is not a NULL pointer:       arg[sizeof(arg)-1]
    - arg is array of "byte-type":     sizeof(char)-sizeof(*arg)
    - last byte of arg has zero value (null-terminated string):
                                       (result so far) - !!arg[sizeof(arg)-1]
 */
#define CONST_STRLEN(arg) \
    sizeof(arg) - sizeof(char[1+sizeof(char)-sizeof(*arg)-!!arg[sizeof(arg)-1]])


/*
    a bit of mathemathics
 */

/* NOTE: beware of side-effects */
#define ORDERED(a_, b_, c_)            ((a_) <= (b_) && (b_) <= (c_))
#define STRICTLY_ORDERED(a_, b_, c_)   ((a_) <  (b_) && (b_) <  (c_))


/*
    enums
 */

/*
    NOTE: temporarily turning off one innocent and otherwise annoying warning
 */
#define ASSERT_ENUM_RANGE(first, test, last)           \
    do {                                               \
    PRAGMA_RAW(GCC diagnostic push)                    \
    PRAGMA_RAW(GCC diagnostic ignored "-Wtype-limits") \
    assert(ORDERED(first, test, last));                \
    PRAGMA_RAW(GCC diagnostic pop)                     \
    } while (0)

/*
    This expects enum "enum" to be arranged such that the first valid
    element is accessible via "enum_first" item and the last valid
    element via "enum_last" (both may be aliases, indeed).
 */
#define ASSERT_ENUM_RANGE_INTERNAL(enum, test) \
    ASSERT_ENUM_RANGE(enum##_first, test, enum##_last)

/*
    Example usage: enum "frobs" comprises "frob_foo" and "frob_bar", "frob_baz"
                   FOR_ENUM_RANGE(i, frob, foo, baz)
                       printf("%d\n", i);
 */
#define FOR_ENUM_RANGE(i, en, first, last) \
    for (enum en##s i = en##_##first; en##_##last >= i; i++)


/* Variable length arguments macros */

/*
  wide-spread technique, see, e.g.,
  http://cplusplus.co.il/2010/07/17/variadic-macro-to-count-number-of-arguments
 */
#define VA_ARGS_CNT(...)  VA_ARGS_CNT_IMPL(__VA_ARGS__,13,12,11,10,9,8,7,6,5,4,3,2,1,_)
#define VA_ARGS_CNT_IMPL(_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,N,...)  N


#endif
