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

#include 

inline void
swap_stream(FILE *f1, FILE *f2) {
    int fd1 = fileno(f1), fd2 = fileno(f2);
    if (fd1 == fd2)
        return;

    fflush(f1); fflush(f2);
    int temp = dup(fd1);
    if (-1 == temp
     || -1 == close(fd1)
     || -1 == dup2(fd2, fd1)
     || -1 == dup2(temp, fd2)
     || -1 == close(temp)
    )
        DIE( ERRNO("swap_stream") );
}
