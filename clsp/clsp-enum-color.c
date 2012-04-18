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

#include "clsp-enum-color.h"

const char *clr_codes[clr_terminate+1] = {
#define X(norm,high,code)  [clr_##norm] = code,
    CLRLIST(X)
#undef X
    [clr_terminate] = CLR_TERMINATE
};

const char *clr_str[clr_terminate] = {
#define X(norm,high,code)  [clr_##norm] = #norm,
    CLRLIST(X)
#undef X
};
