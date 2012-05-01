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

#include "clsp-color.h"

const char *const clr_codes[clr_terminate+1] = {
#define X(norm,high,code)  [clr_##norm] = code,
    CLRLIST(X)
#undef X
    [clr_terminate] = CLR_TERMINATE
};

const char *const clr_str[clr_cnt] = {
#define X(norm,high,code)  [clr_##norm] = #norm,
    CLRLIST(X)
#undef X
};
