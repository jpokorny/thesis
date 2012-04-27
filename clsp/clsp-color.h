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
#ifndef CLSP_ENUM_COLOR_H_GUARD
#define CLSP_ENUM_COLOR_H_GUARD

#include "clsp-macros.h"  /* APPLY */


#define CLR_TERMINATE     "\033[0m"

/*
    color and its (deliberately chosen) "highlight" counterpart + ANSI code

    NOTE: one color should not be prefix of another (such as "dark")
 */
#define CLR_NONE          none,      none,      ""           /* no change */
#define CLR_DEFAULT       default,   default,   "\033[0;39m" /* default color */
#define CLR_DARKSOME      darksome,  black,     "\033[0;30m"
#define CLR_RED           red,       darkred,   "\033[0;31m"
#define CLR_GREEN         green,     darkgreen, "\033[0;32m"
#define CLR_BROWN         brown,     darkbrown, "\033[0;33m"
#define CLR_BLUE          blue,      darkblue,  "\033[0;34m"
#define CLR_PURPLE        purple,    darkpurple,"\033[0;35m"
#define CLR_CYAN          cyan,      darkcyan,  "\033[0;36m"
#define CLR_LIGHTGRAY     lightgray, gray,      "\033[0;37m"
#define CLR_DARKGRAY      darkgray,  darksome,  "\033[1;30m"
#define CLR_BOLDRED       boldred,   red,       "\033[1;31m"
#define CLR_BOLDGREEN     boldgreen, green,     "\033[1;32m"
#define CLR_BOLDBROWN     boldbrown, brown,     "\033[1;33m"
#define CLR_BOLDBLUE      boldblue,  blue,      "\033[1;34m"
#define CLR_BOLDPURPLE    boldpurple,purple,    "\033[1;35m"
#define CLR_BOLDCYAN      boldcyan,  cyan,      "\033[1;36m"
#define CLR_WHITE         white,     lightgray, "\033[1;37m"
#define CLR_BLACK         black,     darksome,  "\033[2;30m"
#define CLR_DARKRED       darkred,   red,       "\033[2;31m"
#define CLR_DARKGREEN     darkgreen, green,     "\033[2;32m"
#define CLR_DARKBROWN     darkbrown, brown,     "\033[2;33m"
#define CLR_DARKBLUE      darkblue,  blue,      "\033[2;34m"
#define CLR_DARKPURPLE    darkpurple,purple,    "\033[2;35m"
#define CLR_DARKCYAN      darkcyan,  cyan,      "\033[2;36m"
#define CLR_GRAY          gray,      lightgray, "\033[2;37m"

#define CLRLIST(x)           \
    APPLY(x, CLR_NONE      ) \
    APPLY(x, CLR_DEFAULT   ) \
    APPLY(x, CLR_DARKSOME  ) \
    APPLY(x, CLR_RED       ) \
    APPLY(x, CLR_GREEN     ) \
    APPLY(x, CLR_BROWN     ) \
    APPLY(x, CLR_BLUE      ) \
    APPLY(x, CLR_PURPLE    ) \
    APPLY(x, CLR_CYAN      ) \
    APPLY(x, CLR_LIGHTGRAY ) \
    APPLY(x, CLR_DARKGRAY  ) \
    APPLY(x, CLR_BOLDRED   ) \
    APPLY(x, CLR_BOLDGREEN ) \
    APPLY(x, CLR_BOLDBROWN ) \
    APPLY(x, CLR_BOLDBLUE  ) \
    APPLY(x, CLR_BOLDPURPLE) \
    APPLY(x, CLR_BOLDCYAN  ) \
    APPLY(x, CLR_WHITE     ) \
    APPLY(x, CLR_BLACK     ) \
    APPLY(x, CLR_DARKRED   ) \
    APPLY(x, CLR_DARKGREEN ) \
    APPLY(x, CLR_DARKBROWN ) \
    APPLY(x, CLR_DARKBLUE  ) \
    APPLY(x, CLR_DARKPURPLE) \
    APPLY(x, CLR_DARKCYAN  ) \
    APPLY(x, CLR_GRAY      )

/**
    Enumeration of available colors
 */
enum color {
#define X(norm, high, code)  clr_##norm,
    CLRLIST(X)
#undef X
    clr_cnt,
    clr_terminate = clr_cnt,

    clr_first = 0,
    clr_last = clr_cnt-1,

    clr_first_empty = clr_first,
    clr_last_empty = clr_default,

    clr_first_real = clr_last_empty + 1,
    clr_last_real = clr_last
};


/**
    Simple "normal color -- highlighted color" palette
 */
struct palette {
    enum color norm;
    enum color high;
};

/* palette "constructors" */
#define PALETTE(norm_, high_) \
    (struct palette) { .norm = clr_##norm_, .high = clr_##high_ }
#define PALETTE_NONE  PALETTE(none, none)


/*
    compile-time assets
 */

#define CLR_PRETTY(norm,high,code)  code #norm CLR_TERMINATE

#define PALETTE_PRETTY(nnorm, nhigh, ncode, hnorm, hhigh, hcode) \
    CLR_PRETTY(nnorm, nhigh, ncode) ":" CLR_PRETTY(hnorm, hhigh, hcode)


/*
    run-time assets
 */

extern const char *const clr_codes[clr_terminate+1];
extern const char *const clr_str[clr_cnt];

/* postponed inclusion due to clsp.h bootstrap arrangement (struct palette) */
#include "clsp-out-base.h"  /* _1(), ... */

#define CLR_PRINTARG_FMT    "%s%s%s"
#define CLR_PRINTARG_FMT_1  _1(s)_2(s)_3(s)
#define CLR_PRINTARG_FMT_2  _2(s)_3(s)_4(s)
#define CLR_PRINTARG_FMT_3  _3(s)_4(s)_5(s)
#define CLR_PRINTARG_FMT_4  _4(s)_5(s)_6(s)
#define CLR_PRINTARG_FMT_5  _5(s)_6(s)_7(s)
#define CLR_PRINTARG_FMT_6  _6(s)_7(s)_8(s)
#define CLR_PRINTARG_FMT_7  _7(s)_8(s)_9(s)
#define CLR_PRINTARG_FMT_8  _8(s)_9(s)_10(s)

#define CLR_PRINTARG(x)     clr_codes[x],clr_str[x],clr_codes[clr_terminate]


/*
    unused eye-candies
 */

#define CLR_FAINT_ON       "\033[2m"
#define CLR_FAINT_OFF      "\033[22m"
#define CLR_UNDERLINE_ON   "\033[4m"
#define CLR_UNDERLINE_OFF  "\033[24m"
#define CLR_INVERSE_ON     "\033[7m"
#define CLR_INVERSE_OFF    "\033[27m"

#define FAINT(what)        CLR_FAINT_ON     what CLR_FAINT_OFF
#define UNDERLINE(what)    CLR_UNDERLINE_ON what CLR_UNDERLINE_OFF
#define INVERSE(what)      CLR_INVERSE_ON   what CLR_INVERSE_OFF


#endif
