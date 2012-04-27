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

#include "clsp.h"
#include "clsp-conv.h"

#include <assert.h>


enum cl_scope_e
conv_scope(const struct symbol *src)
{
    struct scope *scope = src->scope;

    if (!scope)
        /* defaults to the smallest possible scope */
        return CL_SCOPE_BB;

    /* the order of comparisons is important */
    if (SP(global_scope) == scope) {

        assert(src->ctype.modifiers & MOD_TOPLEVEL);
        return CL_SCOPE_GLOBAL;

    } else if (SP(file_scope) == scope) {

        /*
            this is not iff-relation:
            assert(src->ctype.modifiers & MOD_STATIC);
         */
        return CL_SCOPE_STATIC;

    } else if (SP(function_scope) == scope) {

        return CL_SCOPE_FUNCTION;

    } else if (SP(block_scope) == scope) {

        return CL_SCOPE_BB;

    } else if (SP(is_outer_scope, scope)) {

        /* function argument, seems reliable */
        return CL_SCOPE_FUNCTION;

    } else {

        return CL_SCOPE_GLOBAL;

    }
}
