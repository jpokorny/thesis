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
#include "clsp.h"          /* bootstrap all other dependencies... */
#include "clsp_options.h"  /* ...except for this */
#include "type_enumerator.h" /* ...and this */

#include <assert.h>    /* assert */
#include <unistd.h>    /* STD*_FILENO */
#include <dlfcn.h>     /* dlopen, dlsym, dlclose */
#include <sys/wait.h>  /* wait */
#include <signal.h>    /* kill */


/* maximum length of configuration string for creating Code Listener object */
#define CLSP_CONFIG_STRING_MAX  512
/* passed to cl_global_init_defaults */
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


/* static void clmsg_ignore(const char *msg) { (void) msg;                   } */
/**
    Code Listener messaging: standard print.
 */
static void clmsg_print(const char *msg)  { PUT(cl, "%s", msg);          }
/**
    Code Listener messaging: print and exit.
 */
static void clmsg_die(const char *msg)    { DIE( ECODE(CL, "%s", msg) ); }


/**
    Atexit handler for worker-related resources.

    @note: stdout and stderr skipped
 */
static void
atexit_worker(void)
{
#if 0
    // TODO
    type_ptr_db_destroy();
#endif
    /* close the streams used by the worker */
    int fileno_cur;
    for (enum streams s=stream_first; s < stream_last; s++) {
        fileno_cur = fileno(GLOBALS(stream)[s].stream);
        if (GLOBALS(stream)[s].stream && fileno_cur > STDERR_FILENO) {
            // prevent double-close of particular file descriptor
            enum streams sn = s+1;
            for ( ; sn < stream_last; sn++)
                if (fileno_cur == fileno(GLOBALS(stream)[s].stream))
                    break;
            if (sn == stream_last)
                fclose(GLOBALS(stream)[s].stream);
        }
    }
    /* close the access to dynamic libraries */
    for (size_t i=0; i < GLOBALS(cl_libs.cnt); i++)
        if (GLOBALS(cl_libs.handles)[i]
          && 0 != dlclose(GLOBALS(cl_libs.handles)[i]))
            PUT(err, "dlclose: %s", dlerror());
}

/**
    Atexit handler for Code Listener related resources.
 */
static void
atexit_cl(void) {
    if (GLOBALS(cl)) {
        API_EMIT(destroy);
        GLOBALS(cl) = NULL;
        API_CL(global_cleanup);
    }
}

/**
    Atexit handler for Sparse related resources.

    Close (->flush to buffer) deferred sparse output and print it.

    @note more options here, e.g., extra line prefix
    @note must be called before atexit_worker
 */
static void
atexit_sparse(void) {
    /*
        To be sure if deferred stream was opened and used,
        we fflush the stream and examine size of respective buffer.
     */
    errno = 0;
    if (STREAM(sparse)
      && EOF != fflush(STREAM(sparse))
      && 0 < GLOBALS(deferred.size)) {
        if (EOF == fclose(STREAM(sparse)))
            PUT(err, "%s: could not fclose", __func__);
        STREAM(sparse) = NULL;

        PUT(err, "sparse output:");
        PUT(err, "%.*s", (int) GLOBALS(deferred.size),
                 GLOBALS(deferred.buffer));

        free(GLOBALS(deferred.buffer));
        GLOBALS(deferred) = (struct g_deferred) { .buffer=NULL, .size=0 };
    } else if (0 != errno)
        PUT(err, "%s: could not fflush", __func__);

#if DO_SPARSE_FREE
    free(API_SPARSE(input_streams));
#endif
}


/** setup *****************************************************************/


/* convenient shortcuts */
#define OPTS_INTERNALS(what)   (opts->internals.what)
#define OPTS_CL(what)      (opts->cl.what)
#define OPTS_SPARSE(what)  (opts->sparse.what)


/**
    Wrapper for fdopen, safely short-circuiting for stdout and stderr.

    @param[in,out] stream  Stream to be assigned.
    @param[in]     fd      File descriptor number to be fdopen'd.
 */
static inline void
setup_stream(struct outstream *stream, int fd, enum color clr)
{
    /* this is why we don't close anything first */
    FILE **s = &stream->stream;
    switch (fd) {
        case STDOUT_FILENO:  *s = stdout;                 break;
        case STDERR_FILENO:  *s = stderr;                 break;
        case opts_fd_undef:  fd = fileno(stream->stream); break;
        default:
            /* opts_fd_deferred (unexpected) -> fail */
            *s = fdopen(fd, "a");
            if (!*s)
                /* incl. opts_fd_undef */
                DIE( ERRNO("fdopen") );
#if 0
            if (0 != setvbuf(*s, NULL, _IOLBF, 0))
                DIE( ERRNO("setvbuf") );
#endif
            break;
    }

    if (isatty(fd) && clr < clr_last) {
        /* clr != clr_undef */
        stream->clr_begin = clr_codes[clr];
        stream->clr_end   = clr_codes[clr_terminate];
    } else {
        stream->clr_begin = "";
        stream->clr_end   = "";
    }
}

static struct symbol_list *
setup_sparse(const struct options *opts, struct string_list **filelist)
{
    struct symbol_list *symlist = NULL;

    if (GLOBALS(unexposed.register_atexit)
      && 0 != atexit(atexit_worker))
        DIE("atexit");

    if (opts_fd_deferred != OPTS_INTERNALS(fd.sparse))
        setup_stream(&STREAMSTRUCT(sparse), OPTS_INTERNALS(fd.sparse),
                     OPTS_INTERNALS(clr.sparse));
    else {
        /* setup deferred output incl. atexit handler for printing it */
        if (GLOBALS(unexposed.register_atexit)
          && 0 != atexit(atexit_sparse))
            DIE("atexit");
        STREAM(sparse) = open_memstream(&GLOBALS(deferred.buffer),
                                        &GLOBALS(deferred.size));
        if (!STREAM(sparse))
            DIE( ERRNO("open_memstream") );
    }

    API_SPARSE(sparse_initialize,
              /*out*/ symlist,
              /* in*/ OPTS_SPARSE(argc), OPTS_SPARSE(argv), filelist);
    return symlist;
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
    setup_stream(&STREAMSTRUCT(cl), OPTS_INTERNALS(fd.cl),
                 OPTS_INTERNALS(clr.cl));

    if (GLOBALS(unexposed.register_atexit)
      && 0 != atexit(atexit_cl))
        DIE("atexit");

    GLOBALS(cl_libs.cnt) = OPTS_CL(listeners.cnt);
    NORETWRN(MEM_ARR_RESIZE((GLOBALS(cl_libs.handles)), GLOBALS(cl_libs.cnt)));

#ifndef HAS_CL
    /* use the symbols from the base Code Listener provided externally
    / TODO: strip args ("lib:args") */
    void *handle = dlopen(OPTS_CL(listeners.arr)[0], CLSP_DLOPEN_FLAGS);
    if (!handle)
        DIE("dlopen: %s", dlerror());

    DLOG(d_plugin, "plugin %s loaded", OPTS_CL(listeners.arr)[0]);

    if (!dlsym(handle, "plugin_is_GPL_compatible"))
        DIE("Sorry, it seems the plugin is not GPL-compatible:\n%s",
            dlerror());

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
            .debug       = clmsg_print,
            .warn        = clmsg_print,
            .error       = clmsg_print,
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


/** master+worker top-level functions *************************************/


/**
    Routine of worker (main proceeding).

    @param[in] opts  Target options representation.
    @return          Presumably final exit code.
 */
int
worker(struct options *opts)
{
    struct string_list *filelist = NULL;
    struct symbol_list *symlist = NULL;

    symlist = setup_sparse(opts, &filelist);
    if (ptr_list_empty(filelist))
        /* short-circuit when nothing to proceed */
        return ec_general;

    setup_cl(opts);

    free(opts);
    opts = NULL;

    WITH_OBJECT(type_ptr_db)
        proceed(filelist, symlist);

    /* rest of cleanup in registered atexit handler(s) */
    API_EMIT(acknowledge);
    return ec_ok;
}

/**
    Routine of master.

    @return Presumably final exit code.

    @note The only reason to use the "fork" arrangement is probably
          showing the exit code of sparse explicitly.
 */
static int
master(pid_t pid, struct options *opts)
{
    int status, ret = 0;
    pid_t p;

    free(opts);
    opts = NULL;

    while ((pid_t) -1 == (p = waitpid(pid, &status, 0)) && EINTR != errno)
        /* loop */
        ;

    if ((pid_t) -1 == p) {
        ret = errno;
        NORETWRN(kill(pid, SIGKILL));  /* unchecked on purpose */
        errno = ret;
        DIE( ERRNO("wait") );
    }

    if (WIFEXITED(status)) {
        ret = WEXITSTATUS(status);
        PUT(debug, "sparse returned %i", ret);
        return ret;
    }

    DIE("worker ended in an unexpected way");
}


/** init ******************************************************************/


/**
    Initializes values within object reprezenting globals.

    @param[in,out]  globals_obj  Object representing globals.
    @note Presumes empty object as in case of default static
          initialization and only minimal set of values is treated here.
 */
static void
globals_initialize(struct globals *globals_obj)
{
    globals_obj->stream[stream_out]    = (struct outstream){ stdout, "", "" };
    globals_obj->stream[stream_err]    = (struct outstream){ stderr, "", "" };

    globals_obj->unexposed.register_atexit = true;
#ifdef HAS_CL
    /* set symbols we know are available at link-time */
# define PROCEED(prefix,item,cnt) \
    globals_obj->cl_api.item = &API_FQ(prefix,item);
    API_PROCEED(CL, PROCEED)
# undef PROCEED
#endif
}

/**
    Finalizes values within object reprezenting globals.

    @param[in,out]  globals_obj  Object representing globals.
    @param[in]      opts         Gathered options.
 */
static void
globals_finalize(struct globals *globals_obj, const struct options *opts)
{
    globals_obj->debug = OPTS_INTERNALS(debug);
    if (globals_obj->debug)
        setup_stream(&globals_obj->stream[stream_debug],
                     OPTS_INTERNALS(fd.debug), OPTS_INTERNALS(clr.debug));
}

int
main(int argc, char *argv[])
{
    int ret;
    pid_t pid;
    struct options *opts;

    globals_initialize(&globals);
    ret = options_gather(&opts, argc, argv);
    globals_finalize(&globals, opts);

    WITH_DEBUG_LEVEL(d_options)
        options_dump(opts);

    if (ret)
        return (0 > ret) ? EXIT_SUCCESS : ret;


    if (!OPTS_INTERNALS(fork))
        ret = worker(opts);
    else if ((pid_t) -1 == (pid = fork()))
        DIE( ERRNO("fork") );
    else if ((pid_t) 0 == pid)
        ret = worker(opts);
    else
        ret = master(pid, opts);

    options_dispose(opts);
    return ret;
}
