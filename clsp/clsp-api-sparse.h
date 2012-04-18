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
#ifndef CLSP_API_SPARSE_H_GUARD
#define CLSP_API_SPARSE_H_GUARD
/**
    Sparse API (subset of it) internal mirroring
*/

#include "clsp-apis.h"

/* see below which API elements need which header */
#include "lib.h"
#include "expression.h"
#include "linearize.h"
#include "ptrlist.h"
#include "scope.h"
#include "storage.h"
#include "symbol.h"
#include "target.h"
#include "token.h"
#if !defined(ptr_list_empty)
# pragma message "missing some macros that should be defined already"
#endif


#define API_SPARSE_NAME   sparse
#define API_SPARSE_FQ     IDENTITY
#define API_SPARSE_KIND(type,                                          \
                        kind,                                          \
                        argcnt,                                        \
                        out /* E=uses stderr, D=stdout for debug msgs, \
                               X=no decoration */                      \
                       )  kind##argcnt

/*lib.h:        int die_if_error*/
#define API_SPARSE_die_if_error         A,S,0,X
/*token.h:      int input_stream_nr*/
#define API_SPARSE_input_stream_nr      A,S,0,X
/*token.h:      struct stream *input_streams*/
#define API_SPARSE_input_streams        A,S,0,X
/*lib.h:        int preprocess_only*/
#define API_SPARSE_preprocess_only      A,S,0,X
/*lib.h:        int preprocessing*/
#define API_SPARSE_preprocessing        A,S,0,X

/*scope.h:      struct scope block_scope, function_scope, file_scope, global_scope */
#define API_SPARSE_block_scope          A,S,0,X
#define API_SPARSE_function_scope       A,S,0,X
#define API_SPARSE_file_scope           A,S,0,X
#define API_SPARSE_global_scope         A,S,0,X

/*symbol.h:     void show_symbol (struct symbol *sym)*/
#define API_SPARSE_show_symbol          C,N,1,D
/*parse.h:      int show_expression (struct expression *)*/
#define API_SPARSE_show_expression      C,N,1,D  /* unuseful retval */

/*symbol.h:     struct symbol *examine_symbol_type (struct symbol *)*/
#define API_SPARSE_examine_symbol_type  C,N,1,E  /* unuseful retval */
/*expression.h: int expand_symbol (struct symbol *)*/
#define API_SPARSE_expand_symbol        C,N,1,E  /* unuseful retval ("cost") */
/*linearize.h:  struct entrypoint *linearize_symbol (struct symbol *sym)*/
#define API_SPARSE_linearize_symbol     C,R,1,E
/*lib.h:        struct symbol_list *sparse (char *filename)*/
#define API_SPARSE_sparse_initialize    C,R,3,E
/*linearize.h:  int unssa (struct entrypoint *ep)*/
#define API_SPARSE_sparse               C,R,1,E
/*lib.h:        struct symbol_list *sparse_initialize (int argc, char **argv, struct string_list **files)*/
#define API_SPARSE_unssa                C,N,1,E  /* unuseful retval (0) */

/*ptrlist.h:    void *first_ptr_list (struct ptr_list *list)*/
#define API_SPARSE_first_ptr_list       C,U,1,X
/*storage.h:    void free_storage (void)*/
#define API_SPARSE_free_storage         C,N,0,X
/*ptrlist.h:    int ptr_list_size (struct ptr_list *)*/
#define API_SPARSE_ptr_list_size        C,U,1,X
/*storage.h:    void set_up_storage(struct entrypoint *)*/
#define API_SPARSE_set_up_storage       C,N,1,X
/*symbol.h:     const char *show_typename (struct symbol *sym)*/
#define API_SPARSE_show_typename        C,U,1,X
/*linearize.h:  const char *show_instruction (struct instruction *insn)*/
#define API_SPARSE_show_instruction     C,U,1,X
/*token.h:      const char *stream_name (int stream)*/
#define API_SPARSE_stream_name          C,U,1,X

#define API_SPARSE_LAST                 Z,_,_,_
#define API_SPARSE_LIST                                    \
    /* globals */                                          \
    die_if_error,                                          \
    input_stream_nr,                                       \
    input_streams,                                         \
    preprocess_only,                                       \
    preprocessing,                                         \
    block_scope, function_scope, file_scope, global_scope, \
    /* functions printing debug info to stdout */          \
    show_symbol,                                           \
    show_expression,                                       \
    /* functions printing errors/warnings to stderr*/      \
    examine_symbol_type,                                   \
    expand_symbol,                                         \
    linearize_symbol,                                      \
    sparse_initialize,                                     \
    sparse,                                                \
    unssa,                                                 \
    /* silent functions */                                 \
    first_ptr_list,                                        \
    free_storage,                                          \
    ptr_list_size,                                         \
    show_typename,                                         \
    show_instruction,                                      \
    set_up_storage,                                        \
    stream_name

#if (API_SHOW > 0)
API_PRAGMA_OVERVIEW(SPARSE)
# if (API_SHOW > 1)
API_PRAGMA_DETAILS(SPARSE)
# endif
#endif

/* access other properties */
#define API_SPARSE_OUT(type,kind,argcnt,out)  out

/* hard-coded otherwise unexposed constants */
#define SPARSE_OPT_PREPROCESSOR  "-E"

#endif
