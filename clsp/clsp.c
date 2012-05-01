/*
 * Copyright (C) 2009 Kamil Dudka <kdudka@redhat.com>
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

#define  API_SHOW 1
#include "clsp.h"

#include <unistd.h>           /* STD*_FILENO */
#include <dlfcn.h>            /* dlopen, dlsym, dlclose */
#include <assert.h>

#include "clsp-options.h"
#include "clsp-emit.h"        /* enum emit_effort */
#include "clsp-use-cl.h"      /* CL_BUILTIN_LOCATOR_STR */
#include "type_enumerator.h"


/* maximum length of configuration string for creating Code Listener object */
#define CLSP_CONFIG_STRING_MAX  512
/* passed to cl_global_init_defaults (when built-in msg printers requested) */
#define CLSP_NAME               __FILE__
/* flags to dlopen(3) */
#define CLSP_DLOPEN_FLAGS       (RTLD_LAZY|RTLD_LOCAL)


struct globals globals;
/*
 = { .type_ptr_db = {
    .last_base_type_uid = 0,  // to prevent free of non-heap based types
    .type_db            = NULL,
    .ptr_db             = { .alloc_size=0, .last=0, .heads=NULL },
}};
*/


const char *GIT_SHA1 = "someversion";


/** Code Listener messaging functions *************************************/


/** Code Listener messaging: standard print */
static void clmsg_print(const char *msg)     {PUT(cl, _1(s), msg);            }
/** Code Listener messaging: highlighted print */
static void clmsg_highlight(const char *msg) {PUT(cl, HIGHLIGHT(_1(s)), msg); }
/** Code Listener messaging: debug print */
static void clmsg_debug(const char *msg)     {PUT(cl_debug, _1(s), msg);      }
/** Code Listener messaging: print and exit */
static void clmsg_die(const char *msg)       {DIE( ECODE(CL, "cl: %s", msg) );}


/** Atexit handlers and resource releasing ********************************/

/*
    Two things must be met for atexit handler or its part:
    - no (hidden) aborting call
    - the function must be ready to be called multiple times
      (set pointers to NULL, etc.)
 */


/**
    Early atexit handler for worker-related resources.

    @note: stdout and stderr skipped
 */
static void
atexit_worker_early(void)
{
#if 0
    // TODO
    type_ptr_db_destroy();
#endif

    /* restore error stream which may yet be useful */
    FILE **cur = &(STREAMSTRUCT(err).stream);
    assert(cur);
    int fileno_cur = fileno(*cur);

    if (STDERR_FILENO != fileno_cur) {
        if (STDERR_FILENO < fileno_cur)
            fclose(*cur);
        stream_setup(GLOBALS(outstreams), outstream_err,
                     OUTSTREAM_PROPS(STDERR_FILENO, PALETTE_NONE));
    }
    if (STREAMSTRUCT(err).isatty)
        PUT(err, CLR_TERMINATE);  /* reset color for sure */
}

/**
    Atexit handler for remaining worker-related resources.
 */
static void
atexit_worker_late(void)
{
    GLOBALS(basename) = NULL;
    free((void *) GLOBALS(basename_free));
    GLOBALS(basename_free) = NULL;

    /* close the streams used by the worker */
    int fileno_cur;
    FILE *cur;

    FOR_ENUM_RANGE(i, outstream, first, last) {
        /* if deferred, merge into the right one, closing the former */
        cur = stream_output_deferred(GLOBALS(outstreams), i);
        assert(cur);

        fileno_cur = fileno(cur);
        if (fileno_cur > STDERR_FILENO) {
#if 0
            /* prevent double-close of particular file descriptor */
            enum outstreams inext = i+1;
            for ( ; outstream_last >= inext; inext++)
                if (fileno_cur == fileno(GLOBALS(outstreams)[inext].stream))
                    break;
            if (outstream_last <= inext - 1)
                fclose(cur);
#else
            fclose(cur);
#endif
        }
    }
}

/**
    Atexit handler for Code Listener related resources.
 */
static void
atexit_cl(void)
{
    if (GLOBALS(cl)) {
        API_EMIT(destroy);
        GLOBALS(cl) = NULL;
        API_CL(global_cleanup);
    }

    /* close the access to dynamic libraries */
    for (size_t i=0; i < GLOBALS(cl_libs.cnt); i++)
        if (GLOBALS(cl_libs.handles)[i]
          && 0 != dlclose(GLOBALS(cl_libs.handles)[i]))
            PUT(err, "dlclose: "_1(s), dlerror());
}


/**
    Release resources used by sparse

    The only allocated memory we are unable to free properly are
    initial tokens (those before actually proceeding provided source
    files).  So valgrind will not ever show the ideal state.

    (Possible sparse fix: extended allocator descriptor with stack
     to push current blob heads [size of 1 = single extra item is OK
     currently] and push the head insteand of losing it completely)
 */
static void
release_sparse(void)
{
    free(SP(input_streams));
    SP(input_streams) = NULL;

    /* common */
    SP(clear_ident_alloc);
    /*SP(clear_token_alloc); NOOP + see the comment */
    SP(clear_context_alloc);
    SP(clear_symbol_alloc);
    SP(clear_expression_alloc);
    SP(clear_statement_alloc);
    SP(clear_string_alloc);
    SP(clear_scope_alloc);
    SP(clear_bytes_alloc);
    SP(clear_basic_block_alloc);
    SP(clear_entrypoint_alloc);
    SP(clear_instruction_alloc);
    SP(clear_multijmp_alloc);
    SP(clear_pseudo_alloc);

    /* linearize */
    SP(clear_pseudo_user_alloc);
    SP(clear_asm_constraint_alloc);
    SP(clear_asm_rules_alloc);
}

/**
    Atexit handler for sparse related resources.
 */
static void
atexit_sparse(void)
{
    release_sparse();

    if (SP(preprocessing))
        /* if this is set on sparse side, we know the failure happened */
        PUT(err, "error: failure in sparse preprocessing");
}


/** setup *****************************************************************/


/* convenient shortcuts */
#define OPTS_INTERNALS(what)   (opts->internals.what)
#define OPTS_OUTSTREAM_PROPS(which) \
    OPTS_OUTSTREAM_PROPS_RAW(outstream_##which)
#define OPTS_OUTSTREAM_PROPS_RAW(which) \
    opts->outstreams[which - outstream_last_base]
#define OPTS_CL(what)          (opts->cl.what)
#define OPTS_SPARSE(what)      (opts->sparse.what)


static enum retval
setup_sparse(const struct options *opts, struct symbol_list **symlist,
             struct string_list **filelist)
{
    int ret = ret_positive;
    struct symbol_list *loc_symlist;
    struct string_list *loc_filelist = NULL;
    char *file;

    if (GLOBALS(unexposed.register_atexit)
      && 0 != atexit(atexit_sparse))
        DIE("atexit");

    SP(sparse_initialize,
               /*out*/ loc_symlist,
               /* in*/ OPTS_SPARSE(argc), OPTS_SPARSE(argv), &loc_filelist);

    if (SP(preprocess_only)) {
        FOR_EACH_PTR_NOTAG(loc_filelist, file)
            SP(sparse, /*out*/ loc_symlist, /*in*/ file);
        END_FOR_EACH_PTR_NOTAG(file);

        loc_symlist = NULL;
        loc_filelist = NULL;
        ret = SP(die_if_error) ? ret_negative : ret_escape;
    }

    *symlist = loc_symlist;
    *filelist = loc_filelist;
    return ret;
}


static bool
clsp_append_via_config(struct cl_code_listener *chain,
                       const char *config_string)
{
    struct cl_code_listener *cl;
    API_CL(code_listener_create, /*out*/ cl, /*in*/ config_string);
    if (!cl) {
        chain->destroy(chain);
        return false;
    }
    API_CL(chain_append, chain, cl);
    return true;
}

static bool
clsp_append_via_config_args(struct cl_code_listener *chain,
                            const char *listener, const char *args,
                            const char *clf)
{
    char config_string[CLSP_CONFIG_STRING_MAX];

    if (!clf)
        clf = (/*opts->use_peer*/ true)
            ? "unfold_switch,unify_labels_gl"
            : "unify_labels_fnc";

    if (0 >= snprintf(config_string, sizeof(config_string),
                      "listener=\"%s\" listener_args=\"%s\" clf=\"%s\"",
                      listener, args, clf))
        DIE("snprintf");

    return clsp_append_via_config(chain, config_string);
}

// NOTE: using the base Code Listener
static struct cl_code_listener*
clsp_chain_init(const struct options *opts)
{
    struct cl_code_listener *chain;
    API_CL(chain_create, /*out*/ chain);
    if (!chain)
        return NULL;  // error message already emitted

    /* location as a first step throughout the run/locator */
    if (OPTS_CL(debug.location))
        if (!clsp_append_via_config(chain, CL_BUILTIN_LOCATOR_STR))
            return NULL;
    /* pretty printing/pp */
    if (OPTS_CL(pprint.enable)) {
        const char *listener = OPTS_CL(pprint.types)
            ? "pp_with_types"
            : "pp";
        const char *file = OPTS_CL(pprint.file)
            ? OPTS_CL(pprint.file)
            : "";
        const char *clf = OPTS_CL(pprint.switch_to_if)
            ? "unify_labels_gl"
            : "unfold_switch,unify_labels_gl";
        if (!clsp_append_via_config_args(chain, listener, file, clf))
            return NULL;
    }
    /* control flow graphs generator/dotgen */
    if (OPTS_CL(gencfg.enable)) {
        const char *file = OPTS_CL(gencfg.file)
            ? OPTS_CL(gencfg.file)
            : "";
        if (!clsp_append_via_config_args(chain, "dotgen", file, NULL))
            return NULL;
    }
    /* type graphs generator/typedot */
    if (OPTS_CL(gentype.enable))
        if (!clsp_append_via_config_args(chain, "typedot",
                                         OPTS_CL(gentype.file), NULL))
            return NULL;
    /*if (OPTS(use_peer)
        && !clsp_append_via_config_args(chain, "easy", OPTS(peer_args)))
          return NULL;*/

    /* TODO: DLOPEN */

    return chain;
}

static inline void
setup_cl(const struct options *opts)
{
    if (GLOBALS(unexposed.register_atexit)
      && 0 != atexit(atexit_cl))
        DIE("atexit");

    GLOBALS(cl_libs.cnt) = OPTS_CL(listeners.cnt);
    NORETWRN(MEM_ARR_RESIZE((GLOBALS(cl_libs.handles)), GLOBALS(cl_libs.cnt)));

#ifndef HAS_CL
    if (0 == OPTS_CL(listeners.cnt))
        DIE( ECODE(OPT,"no Code Listener specified") );

    /* use the symbols from the base Code Listener provided externally
    / TODO: strip args ("lib:args") */
    void *handle = dlopen(OPTS_CL(listeners.arr)[0], CLSP_DLOPEN_FLAGS);
    if (!handle)
        DIE("dlopen: %s", dlerror());

    if (!dlsym(handle, "plugin_is_GPL_compatible"))
        DIE("sorry, it seems the plugin is not GPL-compatible:\n%s",
            dlerror());

    DLOG(d_plug,
         _1(s)": " HIGHLIGHT("cl-plugin") ": loaded successfully",
         OPTS_CL(listeners.arr)[0]);

    GLOBALS(cl_libs.handles)[0] = handle;

# define PROCEED(pfx,item,cnt)                                                 \
    ((*(void **)(&GLOBALS(cl_api.item)) = dlsym(handle, API_FQ_STR(pfx,item))) \
        ? (void) 0  /* NOOP */                                                 \
        : DIE("dlopen: %s: %s", API_FQ_STR(pfx,item), dlerror())               \
    );
    API_PROCEED(CL, PROCEED)
# undef PROCEED
#endif

    if (OPTS_CL(default_output)) {
        API_CL(global_init_defaults, CLSP_NAME, OPTS_CL(debug.level));
    } else {
        struct cl_init_data init = {
            .debug       = clmsg_debug,
            .warn        = clmsg_print,
            .error       = clmsg_highlight,
            .note        = clmsg_print,
            .die         = clmsg_die,
            .debug_level = OPTS_CL(debug.level),
        };
        API_CL(global_init, &init);
    }

    GLOBALS(cl) = clsp_chain_init(opts);
    if (!GLOBALS(cl))
        DIE("clsp_chain_init");
}


/** top-level feeder functions ********************************************/


/**
    Code Listener emit manager using (heavily transformed) code from sparse

    It is also responsible for any and all resources incl. @c opts.

    @param[in] opts  Target options representation
    @return          Presumably final exit code
 */
int
emitter(struct options *opts)
{
    int ret, emit_props = OPTS_INTERNALS(emit_props);
    struct string_list *filelist = NULL;
    struct symbol_list *symlist;

    /* last thing on the way out: close streams incl. those setup below */
    if (GLOBALS(unexposed.register_atexit) && 0 != atexit(atexit_worker_late))
        DIE("atexit");
    FOR_ENUM_RANGE(i, outstream, first_custom, last_custom)
        stream_setup(GLOBALS(outstreams), i, OPTS_OUTSTREAM_PROPS_RAW(i));

    ret = setup_sparse(opts, &symlist, &filelist);
    /* positive = std. path, escape = preprocess and quit, negative = fail */
    if (ret_escape == ret)
        emit_props |= emit_dry_run;
    else if (ret_positive == ret)
        setup_cl(opts);

    /* options no longer needed (preserving some bits already) */
    options_dispose(opts);

    /* take care of streams (incl. cleanup in atexit handler) */
    if (GLOBALS(unexposed.register_atexit) && 0 != atexit(atexit_worker_early))
        DIE("atexit");

    if (ret_positive == ret) {
        /* with temporary DB, emit what you can as per emit_props */
        type_ptr_db_init(TYPEPTRDB);
        ret = emit(filelist, symlist, emit_props);
        type_ptr_db_destroy(TYPEPTRDB);
    }

    release_sparse();

    if (ret_negative != ret && !(emit_props & emit_dry_run))
        /* hand over the processing business */
        API_EMIT(acknowledge);

    return (ret_negative == ret) ? ec_sparse_code : ec_ok;
}


/** init ******************************************************************/


/**
    Initializes values within object reprezenting globals

    @param[in,out]  globals_obj  Object representing globals
    @note Presumes empty object as in case of default static
          initialization and only minimal set of values is treated here.
 */
static void
globals_initialize(struct globals *globals_obj)
{
    globals_obj->basename = NULL;
    globals_obj->indent = 8/INDENT_MULT;
    globals_obj->debug = d_none;  /* overwritten as per command-line */

    /* out and error streams hard-mapped to stdout and stderr, no coloring */
    stream_setup(globals_obj->outstreams, outstream_out,
                 OUTSTREAM_PROPS(STDOUT_FILENO, PALETTE_NONE));
    stream_setup(globals_obj->outstreams, outstream_err,
                 OUTSTREAM_PROPS(STDERR_FILENO, PALETTE_NONE));

    /* temporary defaults so we can warn in options gathering phase */
    stream_setup(globals_obj->outstreams, outstream_warn,
                 OUTSTREAM_PROPS(STDERR_FILENO, PALETTE_NONE));

    globals_obj->unexposed.register_atexit = true;

#ifdef HAS_CL
    /* set Code Listener symbols we know are available at link-time */
# define PROCEED(prefix,item,cnt) \
    globals_obj->cl_api.item = &API_FQ(prefix,item);
    API_PROCEED(CL, PROCEED)
# undef PROCEED
#endif

}

/**
    Finalizes values within object reprezenting globals

    Setup debug stream (if suitable) so we can use it early on.

    @param[in,out] globals_obj  Object representing globals
    @param[in]     opts         Gathered options
 */
static void
globals_finalize(struct globals *globals_obj, const struct options *opts)
{
    globals_obj->basename = OPTS_INTERNALS(basename);
    globals_obj->basename_free = OPTS_INTERNALS(basename_free);

    /* first stream to be set up when we enable debugging has to be debug one */
    globals_obj->debug = OPTS_INTERNALS(debug);
    if (globals_obj->debug)
        stream_setup(globals_obj->outstreams, outstream_debug,
                     OPTS_OUTSTREAM_PROPS(debug));

    /* it was already temporarily set up, but now the properties are clear */
    stream_setup(globals_obj->outstreams, outstream_warn,
                 OPTS_OUTSTREAM_PROPS(warn));
}

int
main(int argc, char *argv[])
{
    int ret;
    struct options *options;

    globals_initialize(&globals);
    ret = options_gather(&options, argc, argv);
    if (ret_positive != ret)
        return (ret_escape == ret) ? EXIT_SUCCESS : ec_opt;
    globals_finalize(&globals, options);

    WITH_DEBUG_LEVEL(d_opts)
        options_dump(options);

    /* acquires opts */
    return emitter(options);
}
