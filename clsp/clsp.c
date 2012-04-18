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
#include "clsp.h"             /* bootstrap most of the dependencies */

#include <unistd.h>           /* STD*_FILENO */
#include <dlfcn.h>            /* dlopen, dlsym, dlclose */
#include <assert.h>

#include "clsp-options.h"
#include "clsp-emit.h"        /* enum emit_effort */
#include "type_enumerator.h"


/* maximum length of configuration string for creating Code Listener object */
#define CLSP_CONFIG_STRING_MAX  512
/* passed to cl_global_init_defaults (when built-in msg printers requested) */
#define CLSP_NAME               __FILE__
/* flags to dlopen(3) */
#define CLSP_DLOPEN_FLAGS       (RTLD_LAZY|RTLD_LOCAL)


/* Context Managers ala Python */

#define WITH_OBJECT(object)               \
    for (int i=0; 0==i                    \
         ? (object##_init(&object), 1)    \
         : (object##_destroy(&object), 0) \
         ; i++)

struct globals globals;


const char *GIT_SHA1 = "someversion";


/** Code Listener messaging functions + atexit handlers *******************/


/* "globals" are real globals for these */


/** Code Listener messaging: standard print */
static void clmsg_print(const char *msg)     {PUT(cl, _1(s), msg);            }
/** Code Listener messaging: highlighted print */
static void clmsg_highlight(const char *msg) {PUT(cl, HIGHLIGHT(_1(s)), msg); }
/** Code Listener messaging: debug print */
static void clmsg_debug(const char *msg)     {PUT(cl_debug, _1(s), msg);      }
/** Code Listener messaging: print and exit */
static void clmsg_die(const char *msg)       {DIE( ECODE(CL, "cl: %s", msg) );}


/**
    Wrapper for fdopen, safely short-circuiting for stdout and stderr.

    @param[in,out] stream  Stream to be assigned
    @param[in]     fd      File descriptor number to be fdopen'd

    @return  Stream as was just assigned within stream argument
 */
static inline FILE *
stream_setup(struct outstream *stream, const char *which, int fd,
             const struct palette palette)
{
    FILE **s = &stream->stream;
    stream->deferred_dest = fd_undef;
    switch (fd) {
        case STDOUT_FILENO:
            *s = stdout;
            break;
        case STDERR_FILENO:
            *s = stderr;
            break;
        case fd_deferred_unspec:
            /* unspecified deferred -> deferred, merge into stderr */
            fd = -STDERR_FILENO;
            /*FALLTHROUGH*/
        default:
            if (fd == 0) {
                *s = fopen("/dev/null", "a");
            } else if (fd > 0) {
                *s = fdopen(fd, "a");
            } else {
                *s = tmpfile();
                fd = stream->deferred_dest = -fd;
            }
            if (!*s)
                DIE( ERRNO("fopen/fdopen/tmpfile (%s, fd=%d)",which,fd) );
#if 1
            if (0 != setvbuf(*s, NULL, _IOLBF, 0))
                DIE( ERRNO("setvbuf (%s, fd=%d)",which,fd) );
#endif
    }

    stream->clr_norm = clr_codes[palette.norm];
    stream->clr_high = clr_codes[palette.high];

    stream->isatty = isatty(fd);
    if (stream->isatty && (palette.norm || palette.high))
        /* zero-th color is "none", at least one has to be set for this */
        stream->clr_end  = clr_codes[clr_terminate];
    else
        stream->clr_end  = clr_codes[clr_none];

    if (which)
        /*
            avoid using uninitialized stream during initial phase,
            debug stream should be a first one with "which" defined
         */
        DLOG(d_stream, "\t" HIGHLIGHT("stream") ": set up " HIGHLIGHT(_1(s))
                       ": fd="_2(d)", isatty="_3(c) ", color=" CLR_PRINTARG_FMT_4,
                       which, fd, GET_YN(stream->isatty),
                       CLR_PRINTARG(palette.norm));

    return *s;
}


static inline void
stream_output_deferred(FILE **stream, int deferred_fd)
{
    char *buf;
    long size;
    struct outstream outstream;

    size = ftell(*stream);
    if (-1 == size)
        DIE( ERRNO("ftell") );

    errno = 0;
    rewind(*stream);
    if (0 != errno)
        DIE( ERRNO("rewind") );

    buf = malloc(sizeof(*buf) * size);
    if (!buf)
        DIE( ERRNO("malloc") );

    clearerr(*stream);
    size = fread(buf, sizeof(*buf), size, *stream);
    if (ferror(*stream))
        DIE( ERRNO("fread") );

    fclose(*stream);
    *stream = stream_setup(&outstream, "deferred", deferred_fd, PALETTE_NONE);

    fwrite(buf, sizeof(*buf), size, *stream);

    free(buf);
}


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
    int fno = fileno(*cur);

    if (STDERR_FILENO != fno) {
        if (STDERR_FILENO < fno)
            fclose(*cur);
        stream_setup(&(STREAMSTRUCT(err)), NULL, STDERR_FILENO, PALETTE_NONE);
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
    /* close the streams used by the worker */
    int fileno_cur;
    FILE **cur;

    for (enum streams s = stream_first; s < stream_last; s++) {
        cur = &GLOBALS(stream)[s].stream;

        if (!*cur)
            continue;

        if (fd_undef != GLOBALS(stream)[s].deferred_dest)
            /* merge deferred into the right one, closing the former */
            stream_output_deferred(cur, GLOBALS(stream)[s].deferred_dest);

        fileno_cur = fileno(*cur);
        if (fileno_cur > STDERR_FILENO) {
#if 0
            /* prevent double-close of particular file descriptor */
            enum streams sn = s+1;
            for ( ; sn < stream_last; sn++)
                if (fileno_cur == fileno(GLOBALS(stream)[sn].stream))
                    break;
            if (sn == stream_last) {
                fclose(*cur);
                *cur = NULL;
            }
#else
            fclose(*cur);
            *cur = NULL;
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
    Atexit handler for sparse related resources.
 */
static void
atexit_sparse(void)
{
    free(SP(input_streams));

    if (SP(preprocessing))
        /* if this is set on sparse side, we know the failure happened */
        PUT(err, "error: failure in sparse preprocessing");
}



/** setup *****************************************************************/


/* convenient shortcuts */
#define OPTS_INTERNALS(what)  (opts->internals.what)
#define OPTS_CL(what)         (opts->cl.what)
#define OPTS_SPARSE(what)     (opts->sparse.what)


static enum retval
setup_sparse(const struct options *opts, struct symbol_list **symlist,
             struct string_list **filelist)
{
    int ret = ret_continue;
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
        ret = SP(die_if_error) ? ret_fail : ret_bye;
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
        if (!clsp_append_via_config(chain, "listener=\"locator\""))
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

    DLOG(d_plugin, _1(s)": " HIGHLIGHT("cl-plugin") ": loaded successfully",
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
    Code Listener emit manager using (heavily transformed) code from sparse.

    @param[in] opts  Target options representation
    @return          Presumably final exit code
 */
int
emitter(struct options *opts)
{
    int emit_props = OPTS_INTERNALS(emit_props), ret;
    struct string_list *filelist = NULL;
    struct symbol_list *symlist;

    /* take care of streams (incl. cleanup in atexit handler) */

    if (GLOBALS(unexposed.register_atexit) && 0 != atexit(atexit_worker_late))
        DIE("atexit");

    stream_setup(&STREAMSTRUCT(sp), "sp",
                 OPTS_INTERNALS(fd.sp), OPTS_INTERNALS(clr.sp));
    stream_setup(&STREAMSTRUCT(cl), "cl",
                 OPTS_INTERNALS(fd.cl), OPTS_INTERNALS(clr.cl));
    stream_setup(&STREAMSTRUCT(cl_debug), "cl-debug",
                 OPTS_INTERNALS(fd.cl_debug), OPTS_INTERNALS(clr.cl_debug));

    /* setup sparse (run preprocessing if requested and exit promptly then) */
    ret = setup_sparse(opts, &symlist, &filelist);
    if (ret == ret_continue)
        /* setup Code Listener when it is definitely needed */
        setup_cl(opts);
    else
        emit_props |= (ret == ret_bye) ? emit_dry_run : 0;

    /* options no longer needed, what's needed is already preserved */
    options_dispose(opts);

    /* take care of streams (incl. cleanup in atexit handler) */
    if (GLOBALS(unexposed.register_atexit) && 0 != atexit(atexit_worker_early))
        DIE("atexit");

    if (ret == ret_continue)
        /* with temporary DB, emit what you can as per emit_props */
        WITH_OBJECT(type_ptr_db)
            ret = emit(filelist, symlist, emit_props);

    if (ret_fail != ret && !(emit_props & emit_dry_run))
        /* hand over the processing business */
        API_EMIT(acknowledge);

    return (ret_fail == ret) ? ec_sparse_code : ec_ok;
}


/** init ******************************************************************/


/**
    Initializes values within object reprezenting globals.

    @param[in,out]  globals_obj  Object representing globals
    @note Presumes empty object as in case of default static
          initialization and only minimal set of values is treated here.
 */
static void
globals_initialize(struct globals *globals_obj)
{
    globals_obj->stream[stream_debug].stream = NULL;
    globals_obj->stream[stream_sp].stream    = NULL;
    globals_obj->stream[stream_cl].stream    = NULL;

    /* out and error streams hard-mapped to stdout and stderr, no coloring */
    stream_setup(&globals_obj->stream[stream_out], NULL,
                 STDOUT_FILENO, PALETTE_NONE);
    stream_setup(&globals_obj->stream[stream_err], NULL,
                 STDERR_FILENO, PALETTE_NONE);

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

    globals_obj->debug = OPTS_INTERNALS(debug);

    if (globals_obj->debug)
        stream_setup(&globals_obj->stream[stream_debug], "debug",
                     OPTS_INTERNALS(fd.debug), OPTS_INTERNALS(clr.debug));

    stream_setup(&globals_obj->stream[stream_warn], "warn",
                 OPTS_INTERNALS(fd.warn), OPTS_INTERNALS(clr.warn));
}

int
main(int argc, char *argv[])
{
    int ret;
    struct options *opts;

    globals_initialize(&globals);
    ret = options_gather(&opts, argc, argv);
    globals_finalize(&globals, opts);

    WITH_DEBUG_LEVEL(d_options)
        options_dump(opts);

    if (ret_continue != ret) {
        options_dispose(opts);
        ret = (ret_bye == ret) ? EXIT_SUCCESS : ec_opt;
    } else {
        /* acquires opts */
        ret = emitter(opts);
    }

    return ret;
}
