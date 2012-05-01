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

/*
    Program to test througput when swapping streams.
    Use "time" utility to compare the total times for various modes:

    SWAPPED = 0 ... disable
    SWAPPED = 1 ... stdout-stderr
    SWAPPED = 2 ... stdout-stdout

    Experimental results for 1000000 iterations:

    mode |    0     |     1    |    2
    -----+----------+----------+----------
    real | 0m0.224s | 0m6.460s | 0m0.250s
    -cmp |  1.00x   | ~ 28.84x | ~1.11x
    user | 0m0.201s | 0m1.616s | 0m0.225s
    -cmp |  1.00x   | ~  8.04x | ~1.12x
    sys  | 0m0.014s | 0m5.176s | 0m0.018s
    -cmp |  1.00x   | ~369.71x | ~1.29x
    -----+----------+----------+----------

 */

#define stddebug      stderr
#define STREAM(which) std##which
#define STREAMSTRUCT(which)  (void) 0
#define STREAMCLRNORM(which) ""
#define STREAMCLRHIGH(which) ""
#define STREAMCLREND(which)  ""

#define NLOG
#include "clsp-out-ext.h"


#ifndef SWAPPED
# define SWAPPED     1
#endif

#ifndef ITERATIONS
# define ITERATIONS  1000000L
#endif

int main(int argc, char *argv[])
{
    for (unsigned long i = 0; i < ITERATIONS; i++) {
#if SWAPPED != 0
# if SWAPPED == 1
        WITH_SWAPPED_STREAM(out, err) {
# else
        WITH_SWAPPED_STREAM(out, out) {
# endif
#endif
            fprintf(stdout, "%lu\n", i);
#if SWAPPED != 0
        }
#endif
    }
}
