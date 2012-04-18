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
#ifndef CLSP_VERSION_H_GUARD
#define CLSP_VERSION_H_GUARD

#ifdef HAS_CL
#  define VER_HAS_CL "CL part built-in"
#else
#  define VER_HAS_CL "no CL part built-in"
#endif

#define VER_DETAILS_  VER_HAS_CL ""
#define VER_DETAILS   (VER_DETAILS_[0] == '\0') ? "" : " (" VER_DETAILS_ ")"

#endif
