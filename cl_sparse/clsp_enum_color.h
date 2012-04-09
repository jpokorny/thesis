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

#include "clsp_macros.h"  /* APPLY */

#define CLR_BLACK       black,     "\033[0;30m"
#define CLR_RED         red,       "\033[0;31m"
#define CLR_GREEN       green,     "\033[0;32m"
#define CLR_BROWN       brown,     "\033[0;33m"
#define CLR_BLUE        blue,      "\033[0;34m"
#define CLR_PURPLE      purple,    "\033[0;35m"
#define CLR_CYAN        cyan,      "\033[0;36m"
#define CLR_LIGHTGRAY   lightgray, "\033[0;37m"
#define CLR_DARKGRAY    darkgray,  "\033[1;30m"
#define CLR_BOLDRED     boldred,   "\033[1;31m"
#define CLR_BOLDGREEN   boldgreen, "\033[1;32m"
#define CLR_BOLDBROWN   boldbrown, "\033[1;33m"
#define CLR_BOLDBLUE    boldblue,  "\033[1;34m"
#define CLR_BOLDPURPLE  boldpurple,"\033[1;35m"
#define CLR_BOLDCYAN    boldcyan,  "\033[1;36m"
#define CLR_WHITE       white,     "\033[1;37m"
#define CLR_TERMINATE   /*special*/"\033[0m"
#define CLRLIST(x)           \
    APPLY(x, CLR_BLACK     ) \
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
    APPLY(x, CLR_WHITE     )


enum color {
#define X(name,code)  clr_##name,
    CLRLIST(X)
#undef X
    clr_terminate,
    clr_last = clr_terminate,
    clr_undef,
    clr_last_all = clr_undef,
    clr_first = 0
};

extern const char *clr_codes[clr_last_all];
extern const char *clr_str[clr_last];

#define CLR_PRINTARG(x)  clr_codes[x], clr_str[x], clr_codes[clr_terminate]

#endif
