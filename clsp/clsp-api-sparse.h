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
#ifndef CLSP_API_SPARSE_H_GUARD
#define CLSP_API_SPARSE_H_GUARD
/**
    Sparse API (subset of it) internal mirroring
*/

#include "clsp-apis.h"

/* in case we have already local aliases defined (1:1) */
#undef DO_STRINGIFY
#undef STRINGIFY

/* see below which API elements need which header */
#include "lib.h"
#include "allocate.h"
#include "expression.h"
#include "linearize.h"
#include "ptrlist.h"
#include "scope.h"
#include "symbol.h"
#include "target.h"
#include "token.h"
#if DO_PER_EP_SET_UP_STORAGE
# include "storage.h"
#endif
#if !defined(ptr_list_empty)    \
 || !defined(DECLARE_ALLOCATOR) \
 || !defined(ALLOCATOR)         \
 || !defined(VOID)
char error_ = "missing some macros that should be defined already"[-1];
#endif


/* FIXME: missing prototypes */
pseudo_t linearize_expression(struct entrypoint *ep, struct expression *expr);


#define API_SPARSE_NAME   sparse
#define API_SPARSE_FQ     IDENTITY
#define API_SPARSE_KIND(type,                                       \
                        kind,                                       \
                        argcnt,                                     \
                        out /* C = prevent from modification        \
                               D = uses stdout for debug messages,  \
                               E = may use stderr,                  \
                               F = uses stderr for debug messages   \
                               X = no decoration */                 \
                       )  kind##argcnt

/*lib.h:        int die_if_error*/
#define API_SPARSE_die_if_error               A,S,0,X
/*token.h:      struct stream *input_streams*/
#define API_SPARSE_input_streams              A,S,0,X

/*token.h:      int input_stream_nr*/
#define API_SPARSE_input_stream_nr            A,S,0,C
/*lib.h:        int preprocess_only*/
#define API_SPARSE_preprocess_only            A,S,0,C
/*lib.h:        int preprocessing*/
#define API_SPARSE_preprocessing              A,S,0,C

/*scope.h:      struct scope block_scope, function_scope, file_scope, global_scope */
#define API_SPARSE_block_scope                A,S,0,C
#define API_SPARSE_function_scope             A,S,0,C
#define API_SPARSE_file_scope                 A,S,0,C
#define API_SPARSE_global_scope               A,S,0,C

/*symbol.h:     void show_symbol (struct symbol *sym)*/
#define API_SPARSE_show_symbol                C,N,1,D
/*parse.h:      int show_expression (struct expression *)*/
#define API_SPARSE_show_expression            C,N,1,D  /* unuseful retval */

/*symbol.h:     void debug_symbol(struct symbol *)*/
#define API_SPARSE_debug_symbol               C,N,1,F
/*allocate.c:   void show_*_alloc(void)*/
#define API_SPARSE_show_ident_alloc           C,N,0,F
#define API_SPARSE_show_token_alloc           C,N,0,F
#define API_SPARSE_show_context_alloc         C,N,0,F
#define API_SPARSE_show_symbol_alloc          C,N,0,F
#define API_SPARSE_show_expression_alloc      C,N,0,F
#define API_SPARSE_show_statement_alloc       C,N,0,F
#define API_SPARSE_show_string_alloc          C,N,0,F
#define API_SPARSE_show_scope_alloc           C,N,0,F
#define API_SPARSE_show_bytes_alloc           C,N,0,F
#define API_SPARSE_show_basic_block_alloc     C,N,0,F
#define API_SPARSE_show_entrypoint_alloc      C,N,0,F
#define API_SPARSE_show_instruction_alloc     C,N,0,F
#define API_SPARSE_show_multijmp_alloc        C,N,0,F
#define API_SPARSE_show_pseudo_alloc          C,N,0,F
#define API_SPARSE_clear_ident_alloc          C,N,0,X
#define API_SPARSE_clear_token_alloc          C,N,0,X
#define API_SPARSE_clear_context_alloc        C,N,0,X
#define API_SPARSE_clear_symbol_alloc         C,N,0,X
#define API_SPARSE_clear_expression_alloc     C,N,0,X
#define API_SPARSE_clear_statement_alloc      C,N,0,X
#define API_SPARSE_clear_string_alloc         C,N,0,X
#define API_SPARSE_clear_scope_alloc          C,N,0,X
#define API_SPARSE_clear_bytes_alloc          C,N,0,X
#define API_SPARSE_clear_basic_block_alloc    C,N,0,X
#define API_SPARSE_clear_entrypoint_alloc     C,N,0,X
#define API_SPARSE_clear_instruction_alloc    C,N,0,X
#define API_SPARSE_clear_multijmp_alloc       C,N,0,X
#define API_SPARSE_clear_pseudo_alloc         C,N,0,X
/*linearize.c:  void show_*_alloc(void)*/
#define API_SPARSE_show_pseudo_user_alloc     C,N,0,F
#define API_SPARSE_show_asm_constraint_alloc  C,N,0,F
#define API_SPARSE_show_asm_rules_alloc       C,N,0,F
#define API_SPARSE_clear_pseudo_user_alloc    C,N,0,X
#define API_SPARSE_clear_asm_constraint_alloc C,N,0,X
#define API_SPARSE_clear_asm_rules_alloc      C,N,0,X

/*symbol.h:     struct symbol *examine_symbol_type (struct symbol *)*/
#define API_SPARSE_examine_symbol_type        C,N,1,E  /* unuseful retval */
/*expression.h: int expand_symbol (struct symbol *)*/
#define API_SPARSE_expand_symbol              C,N,1,E  /* unuseful retval ("cost") */
/*FIXME:        pseudo_t linearize_expression(struct entrypoint *ep, struct expression *expr)*/
#define API_SPARSE_linearize_expression       C,N,2,E  /* unuseful retval */
/*linearize.h:  struct entrypoint *linearize_symbol (struct symbol *sym)*/
#define API_SPARSE_linearize_symbol           C,R,1,E
/*lib.h:        struct symbol_list *sparse (char *filename)*/
#define API_SPARSE_sparse_initialize          C,R,3,E
/*linearize.h:  int unssa (struct entrypoint *ep)*/
#define API_SPARSE_sparse                     C,R,1,E
/*lib.h:        struct symbol_list *sparse_initialize (int argc, char **argv, struct string_list **files)*/
#define API_SPARSE_unssa                      C,N,1,E  /* unuseful retval (0) */

/*ptrlist.h:    void *first_ptr_list (struct ptr_list *list)*/
#define API_SPARSE_first_ptr_list             C,U,1,X
/*storage.h:    void free_storage (void)*/
#define API_SPARSE_free_storage               C,N,0,X
/*symbol.h:     const char* get_type_name(enum type type)*/
#define API_SPARSE_get_type_name              C,U,1,X
/*scope.h       int is_outer_scope(struct scope *)*/
#define API_SPARSE_is_outer_scope             C,U,1,X
/*ptrlist.h:    int ptr_list_size (struct ptr_list *)*/
#define API_SPARSE_ptr_list_size              C,U,1,X
/*storage.h:    void set_up_storage(struct entrypoint *)*/
#define API_SPARSE_set_up_storage             C,N,1,X
/*symbol.h:     const char *show_typename (struct symbol *sym)*/
#define API_SPARSE_show_typename              C,U,1,X
/*linearize.h:  const char *show_instruction (struct instruction *insn)*/
#define API_SPARSE_show_instruction           C,U,1,X
/*linearize.h:  const char *show_pseudo(pseudo_t pseudo)*/
#define API_SPARSE_show_pseudo                C,U,1,X
/*token.h:      const char *stream_name (int stream)*/
#define API_SPARSE_stream_name                C,U,1,X

#define API_SPARSE_LAST                       Z,_,_,_

#define API_SPARSE_ALLOC_LIST(which) \
    show_##which##_alloc, clear_##which##_alloc

#define API_SPARSE_LIST                                    \
    /* globals that are purposefully read-write */         \
    die_if_error,                                          \
    input_streams,                                         \
    /* globals treated as constants */                     \
    input_stream_nr,                                       \
    preprocess_only,                                       \
    preprocessing,                                         \
    block_scope, function_scope, file_scope, global_scope, \
    /* functions printing debug info to stdout */          \
    show_symbol,                                           \
    show_expression,                                       \
    /* alloc funtions (show prints debug info to stderr) */\
    API_SPARSE_ALLOC_LIST(ident),                          \
    API_SPARSE_ALLOC_LIST(token),                          \
    API_SPARSE_ALLOC_LIST(context),                        \
    API_SPARSE_ALLOC_LIST(symbol),                         \
    API_SPARSE_ALLOC_LIST(expression),                     \
    API_SPARSE_ALLOC_LIST(statement),                      \
    API_SPARSE_ALLOC_LIST(string),                         \
    API_SPARSE_ALLOC_LIST(bytes),                          \
    API_SPARSE_ALLOC_LIST(basic_block),                    \
    API_SPARSE_ALLOC_LIST(entrypoint),                     \
    API_SPARSE_ALLOC_LIST(instruction),                    \
    API_SPARSE_ALLOC_LIST(multijmp),                       \
    API_SPARSE_ALLOC_LIST(pseudo),                         \
    /* functions printing errors/warnings to stderr*/      \
    examine_symbol_type,                                   \
    expand_symbol,                                         \
    linearize_expression,                                  \
    linearize_symbol,                                      \
    sparse_initialize,                                     \
    sparse,                                                \
    unssa,                                                 \
    /* silent functions */                                 \
    first_ptr_list,                                        \
    free_storage,                                          \
    get_type_name,                                         \
    is_outer_scope,                                        \
    ptr_list_size,                                         \
    show_typename,                                         \
    show_instruction,                                      \
    show_pseudo,                                           \
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
