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


const char *GIT_SHA1 = "someversion";


#define GLOBALS(what)     (opts->globals->what)
#define INTERNALS(what)   (opts->internals.what)
#define CLOPTS(what)      (opts->cl.what)
#define SPARSEOPTS(what)  (opts->sparse.what)

static struct symbol_list *
setup_sparse(const struct options *opts, struct symbol_list **filelist)
{
    struct symbol_list *symlist = NULL;

    if (GLOBALS(unexposed.register_atexit)
      && 0 != atexit(atexit_worker))
        DIE("atexit");

    if (FD_DEFERRED != INTERNALS(imm.fd.sparse))
        setup_stream(&STREAM(sparse), INTERNALS(imm.fd.sparse));
    else {
        // setup deferred output incl. atexit handler for printing it
        if (GLOBALS(unexposed.register_atexit)
          && 0 != atexit(atexit_sparse))
            DIE("atexit");
        STREAM(sparse) = open_memstream(&GLOBALS(deferred.buffer),
                                        &GLOBALS(deferred.size));
        if (!STREAM(sparse))
            DIE( ERRNO("open_memstream") );
    }

    SPARSE_API(sparse_initialize, /*OUT*/ symlist, /*IN*/ SPARSEOPTS(set.argc),
               SPARSEOPTS(set.argv), filelist);
    return symlist;
}

static bool
clsp_append_via_config(struct cl_code_listener *chain,
                       const char *config_string, const struct options *opts)
{
    struct cl_code_listener *cl = CL_API(code_listener_create, config_string);
    if (!cl) {
        chain->destroy(chain);
        return false;
    }
    CL_API(chain_append, chain, cl);
    return true;
}

static bool
clsp_append_via_config_args(struct cl_code_listener *chain,
                            const char *listener, const char *args,
                            const char *clf, const struct options *opts)
{
    char config_string[CLSP_CONFIG_STRING_MAX];

    if (!clf)
        clf = (/*opts->use_peer*/ true)
            ? "unfold_switch,unify_labels_gl"
            : "unify_labels_fnc";

    if (0 >= snprintf(config_string, CLSP_CONFIG_STRING_MAX,
                      "listener=\"%s\" listener_args=\"%s\" clf=\"%s\"",
                      listener, args, clf))
        DIE("snprintf");

    return clsp_append_via_config(chain, config_string, opts);
}

// NOTE: using the base Code Listener
static struct cl_code_listener*
clsp_chain_init(const struct options *opts)
{
    struct cl_code_listener *chain = CL_API(chain_create);
    if (!chain)
        return NULL;  // error message already emitted

    // location as a first step throughout the run/locator
    if (CLOPTS(imm.debug.location))
        if (!clsp_append_via_config(chain, "listener=\"locator\"", opts))
            return NULL;
    // pretty printing/pp
    if (CLOPTS(imm.pprint.enable)) {
        const char *listener = CLOPTS(imm.pprint.types)
            ? "pp_with_types"
            : "pp";
        const char *file = CLOPTS(imm.pprint.file)
            ? CLOPTS(imm.pprint.file)
            : "";
        const char *clf = CLOPTS(imm.pprint.switch_to_if)
            ? "unify_labels_gl"
            : "unfold_switch,unify_labels_gl";
        if (!clsp_append_via_config_args(chain, listener, file, clf, opts))
            return NULL;
    }
    // control flow graphs generator/dotgen
    if (CLOPTS(imm.gencfg.enable)) {
        const char *file = CLOPTS(imm.gencfg.file)
            ? CLOPTS(imm.gencfg.file)
            : "";
        if (!clsp_append_via_config_args(chain, "dotgen", file, NULL, opts))
            return NULL;
    }
    // type graphs generator/typedot
    if (CLOPTS(imm.gentype.enable))
        if (!clsp_append_via_config_args(chain, "typedot",
                                         CLOPTS(imm.gentype.file), NULL, opts))
            return NULL;
    /*if (OPTS(use_peer)
        && !clsp_append_via_config_args(chain, "easy", OPTS(peer_args), opts))
          return NULL;*/

    // TODO: DLOPEN

    return chain;
}

static inline void
setup_cl(const struct options *opts)
{
    if (GLOBALS(unexposed.register_atexit)
      && 0 != atexit(atexit_cl))
        DIE("atexit");

    GLOBALS(cl_libs.cnt) = CLOPTS(imm.listeners.cnt);
    NORETWRN(MEM_ARR_RESIZE((GLOBALS(cl_libs.handles)), GLOBALS(cl_libs.cnt)));

#ifndef HAS_CL
    // use the symbols from the base Code Listener provided externally
    // TODO: strip args ("lib:args")
    void *handle = dlopen(CLOPTS(imm.listeners.arr)[0], DL_OPEN_FLAGS);
    if (!handle)
        DIE("dlopen: %s", dlerror());
    GLOBALS(cl_libs.handles)[0] = handle;
# define PROCEED(what)                                                        \
    ((*(void **)(&GLOBALS(cl_api.what)) = dlsym(handle, TOSTRING(cl_##what))) \
        ? (void) 0  /* NOOP */                                                \
        : DIE("dlopen: %s", dlerror())                                        \
    )
    CL_API_PROCEED(PROCEED);
# undef PROCEED
#endif

    if (CLOPTS(imm.default_output))
        CL_API(global_init_defaults, CLSP_NAME, CLOPTS(imm.debug.level));
    else {
        struct cl_init_data init = {
            .debug       = clmsg_print,
            .warn        = clmsg_print,
            .error       = clmsg_print,
            .note        = clmsg_print,
            .die         = clmsg_die,
            .debug_level = CLOPTS(imm.debug.level),
        };
        CL_API(global_init, &init);
    }

    GLOBALS(cl) = clsp_chain_init(opts);
    if (!GLOBALS(cl))
        DIE("clsp_chain_init");
}


/** master+worker top-level functions ************************************/


/** Routine of worker (main proceeding).

    @param[in] opts  Target options representation.

    @todo setup_sparse here
 */
int
worker(struct options *opts)
{
    struct string_list *filelist = NULL;
    struct symbol_list *symlist = NULL;

    setup_stream(&STREAM(debug), INTERNALS(imm.fd.debug));
    setup_stream(&STREAM(cl),    INTERNALS(imm.fd.cl));

    symlist = setup_sparse(opts, &filelist);
    if (SPARSE_API(ptr_list_empty, filelist))
        /* nothing to proceed */
        return -1;

    setup_cl(opts);

    WITH_OBJECT(type_ptr_db)
        proceed_sparse(symlist, filelist);

    /* rest of cleanup in registered atexit handler(s) */
    EMIT(acknowledge);
    return 0;
}

/** Routine of master.

    @note The only reason to use the "fork" arrangement is probably
          showing the exit code of sparse explicitly.
 */
static int
master(pid_t pid, const struct options *opts)
{
    int status, ret = 0;
    pid_t p;

    while ((pid_t) -1 == (p = waitpid(pid, &status, 0)) && EINTR != errno)
        /* loop */
        ;

    if ((pid_t) -1 == p) {
        ret = errno;
        /* unchecked on purpose */
        kill(pid, SIGKILL);
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


/** options/arguments handling ********************************************/


#define OPTISBIN(what)             APPLY(OPTISBIN_,OPT_##what)
#define OPTPREFIX(what)            APPLY(OPTPREFIX_,OPT_##what)
#define OPTPREFIX_(is_bin,prefix)  prefix
#define OPTISBIN_(is_bin,prefix)   is_bin

#define OPT_SHORT      false,"-"
#define OPT_SHORT_BIN  true ,"-"
#define OPT_LONG       false,"--"
#define OPT_LONG_BIN   true ,"--"
#define OPT_CL         false,"-cl-"
#define OPT_CL_BIN     true ,"-cl-"

/** Version printer. */
int
print_version(const struct options *opts)
{
    PUT(out, "%s", GIT_SHA1);
    return 0;
}

/** Help printer. */
int
print_help(const char *cmd, const struct options *opts, int ret)
{
#define _(...)       PUT(out, __VA_ARGS__);
#define __           PUT(out, "");
#define ___          ""
#define L(lo, cmt)   PUT(out, "%-32s%s", OPTPREFIX(LONG) lo, cmt);
#define S(so, cmt)   PUT(out, "%-32s%s", OPTPREFIX(SHORT) so, cmt);
#define I(ign, cmt)  PUT(out, "%-32s%s", ign, cmt);
#define B(so, lo, cmt) \
    PUT(out, "%-32s%s", OPTPREFIX(SHORT) so ", " OPTPREFIX(LONG) lo, cmt);
#define C(co, cmt)   PUT(out, "%-32s%s", OPTPREFIX(CL) co, cmt);
#define V(v, cmt)    PUT(out, "%-32d%s", v, cmt);
    _("sparse-based Code Listener frontend, version"                           )
    __                                                                         ;
    _("usage: %s ( cl-global-opts | cl-plugin[:args] | sparse-opts-args )*",cmd)
    __                                                                         ;
#ifndef HAS_CL
    _("As no Code Listener plugin was built-in (-> no one to serve as a base"  )
    _("one at hand), at least one such has to be provided in the form of a"    )
    _("shared library containing necessary symbols of Code Listener interface" )
    _("(Code Listener based plugins for GCC are compatible);"                  )
    _("see `%s' option below.",                          OPTPREFIX(CL) "plugin")
#endif
    __                                                                         ;
    __                                                                         ;
    _("This Code Listener front-end defines a few internal options:"           )
    __                                                                         ;
    B("h", "help"          , "Prints this help text"                           )
    L("version"            , "Prints the version information"                  )
    B("f", "fork"          , "Do fork (only to show sparse exit status {0,1})" )
    _("[specification of file descriptors, use `FD>file' redirection for FD>2]")
    L("cl-fd=FD"           , "Redefine stderr used to display to cl messages"  )
    I(___                  , "(fatal errors are always produced on stderr)"    )
    L("sparse-fd=FD"       , "Redefine stderr used by sparse, `D' for deferred")
    L("debug-fd=FD"        , "Redefine stdout used for debugging messages"     )
    __                                                                         ;
    B("d", "debug[=MASK]"  , "Debug (selectively if MASK specified)"           )
    _("MASK:")             ;              for (int i = d_first; i < d_last; i++)
    V(DVALUE(i)            ,                                  d_str[DACCESS(i)])
    __                                                                         ;
    __                                                                         ;
    _("From the options affecting the run through Code Listener interface,"    )
    _("one particularly important is a way to import other Code Listener(s):"  )
    __                                                                         ;
    C("plugin=FILE[:ARGS]" , "Path to a shared library containg symbols of"    )
    I(___                  , "Code Listener (for instance, GCC plugins can be" )
#ifdef HAS_CL
    I(___                  , "used directly), passing it optional ARGS"        )
#else
    I(___                  , "used directly), passing it optional ARGS;"       )
    I(___                  , "the first one is a base one and must be provided")
#endif
    __                                                                         ;
    __                                                                         ;
#ifndef HAS_CL
    _("and specifically these options are for a base (built-in) Code Listener:")
#else
    _("and specifically these options are for a base (provided) Code Listener:")
#endif
    __                                                                         ;
    C("default-output"     , "Use Code Listener's built-ins to print messages" )
    C("pprint[=FILE]"      , "Dump pretty-printed linearized code"             )
    C("pprint-types"       , "Add type information to pretty-printed code"     )
    C("pprint-switch-to-if", "Unfold `switch' into series of `if' statements " )
    C("gen-cfg[=MAIN_FILE]", "Generate control flow graphs (as per MAIN_FILE)" )
    C("gen-type[=FILE]"    , "Generate type graphs (to FILE if specified)"     )
    C("debug-location"     , "Output location as first step throughout the run")
    C("debug-level[=LEVEL]", "Debug (according to LEVEL if specified)"         )
    __                                                                         ;
    __                                                                         ;
    _("For `sparse-opts-args' (including the specification of the target[s])," )
    _("see sparse documentation;  generally, there is some level of"           )
    _("compatibility with GCC and unrecognized options are ignored anyway."    )
    _("To name a few notable notable ones (referring to current version):"     )
    __                                                                         ;
    S("v"                  , "Report more defects, more likely false positives")
    S("m64"                , "Suppose 64bit architecture (32bit by default)"   )
    S("DNAME[=VALUE]"      , "Define macro NAME (holding value VALUE if spec.)")
    S("W[no[-]]WARNING"    , "Request/not to report WARNING-related issues;"   )
    I(___                  , "`sparse-all' covers all available warnings"      )
    _("%s"                 , "..."                                             )
    __                                                                         ;
    __                                                                         ;
    _("Return values:")    ;            for (int i = ec_first; i < ec_last; i++)
    V(ECVALUE(i)           ,                                ec_str[ECACCESS(i)])
#undef V
#undef C
#undef B
#undef I
#undef S
#undef L
#undef ___
#undef __
#undef _
    return ret;
}

/**  The first/initializing phase of gathering options.
 */
static void
opts_initialize(struct options *opts, const struct globals *globals_obj)
{
    INTERNALS(imm.fork) = false;
    INTERNALS(imm.fd)
      = (struct oi_fd) { .cl=FD_UNDEF, .sparse=FD_UNDEF, .debug=FD_UNDEF };
    INTERNALS(imm.debug) = 0;

    CLOPTS(imm.listeners.cnt)  = 0;
    CLOPTS(imm.listeners.arr)  = NULL;
    CLOPTS(imm.default_output) = false;
    CLOPTS(imm.pprint.enable)  = false;
    CLOPTS(imm.gencfg.enable)  = false;
    CLOPTS(imm.gentype.enable) = false;
    CLOPTS(imm.debug) = (struct oc_debug) { .location=false, .level=0 };

    /* const was meant to denote "no modification to globals" (yet) */
    opts->globals = (struct globals*) globals_obj;
}

/** Positive number converter (from string).

    @todo  More checks.
 */
static inline int
get_positive_num(const struct options *opts, const char *what,
                 const char *value)
{
    if (!isdigit(value[0]))
        DIE( ECODE(ec_opt, "option %s: not a numeric value: %s", what, value) );
    int ret = strtol(value, NULL, 10);
    if (ret < 0)
        DIE( ECODE(ec_opt, "option %s: must be positive number", what) );
    return ret;
}

/** File descriptor specification converter (from string).

    @note Single @c D characters stands for special "deferred" stream
          available as per  @c accept_deferred argument.
 */
static inline int
get_fd(const char *what, const char *value, bool accept_deferred,
       const struct options *opts)
{
    if (accept_deferred && value[0] == 'D' && value[1] == '\0')
        return FD_DEFERRED;
    return get_positive_num(what, value, opts);
}

/** The main phase of gathering options.

    We only handle known options/arguments and leave argv untouched
    behind us (our options should be guaranteed not to collide with
    sparse, though).
 */
static int
proceed_options(struct options *opts, int argc, char *argv[])
{
#define OPTPREFIXEQ(argv, i, type, opt)                                      \
    (strncmp(argv[i],OPTPREFIX(type) opt,strlen(OPTPREFIX(type) opt))        \
        ? NULL                                                               \
        : (((OPTISBIN(type) && argv[i][strlen(OPTPREFIX(type) opt)] != '\0') \
            ? PUT(out, "option %s: binary option with argument (or clash?)", \
                  argv[i])                                                   \
            : 0)                                                             \
            , &argv[i][strlen(OPTPREFIX(type) opt)]))
#define OPTVALUE(argv, i, str)                                               \
    (*str != '\0'                                                            \
        ? (((*str != '=' || *++str != '\0')) ? str : NULL)                   \
        : (argv[i+1][0] != '\0' && argv[i+1][0] != '-')                      \
            ? (str = argv[++i])                                              \
            : NULL)
    if (1 >= argc)
        return print_help(argv[0], opts, 1);

    char *value;
    for (int i=1; i < argc; i++) {

        /* internal options */

        if (OPTPREFIXEQ(argv,i,SHORT_BIN,"h")
          || OPTPREFIXEQ(argv,i,LONG_BIN,"help")) {
            return print_help(argv[0], opts, -1);
        }
        else if ((value = OPTPREFIXEQ(argv,i,LONG_BIN,"version"))) {
            return print_version(opts);
        }
        else if ((value = OPTPREFIXEQ(argv,i,SHORT_BIN,"f"))
          || (value = OPTPREFIXEQ(argv,i,LONG_BIN,"fork"))) {
            INTERNALS(imm.fork) = true;
        }
        else if ((value = OPTPREFIXEQ(argv,i,LONG,"cl-fd"))
          || (value = OPTPREFIXEQ(argv,i,LONG,"sparse-fd"))
          || (value = OPTPREFIXEQ(argv,i,LONG,"debug-fd"))) {
            char *arg = argv[i];  /* preserve across OPTVALUE */
            if (OPTVALUE(argv,i,value)) {
                /* exploiting the difference of initial chars */
                int *to_set;
                switch (arg[strlen(OPTPREFIX(LONG))]) {
                    case 'c': to_set = &INTERNALS(imm.fd.cl);     break;
                    case 's': to_set = &INTERNALS(imm.fd.sparse); break;
                    case 'd': to_set = &INTERNALS(imm.fd.debug);  break;
                    default: DIE("unexpected case");
                }
                *to_set = get_fd(arg, value,
                                 (to_set == &INTERNALS(imm.fd.sparse)), opts);
            } else
                DIE( ECODE(ec_opt, "option %s: omitted value", arg) );
        }
        else if ((value = OPTPREFIXEQ(argv,i,SHORT,"d"))
          || (value = OPTPREFIXEQ(argv,i,LONG,"debug"))) {
            INTERNALS(imm.debug) = OPTVALUE(argv,i,value)
                ? get_positive_num("debug", value, opts)
                : ~0;
        }

        /* Code Listener options */

        else if ((value = OPTPREFIXEQ(argv,i,CL,"plugin"))) {
            *(MEM_ARR_APPEND(CLOPTS(imm.listeners.arr),
                             CLOPTS(imm.listeners.cnt)))
              = OPTVALUE(argv,i,value);
        }
        else if (OPTPREFIXEQ(argv,i,CL_BIN,"default-output")) {
            CLOPTS(imm.default_output) = true;
        }
        else if ((value = OPTPREFIXEQ(argv,i,CL,"pprint"))) {
            CLOPTS(imm.pprint.enable)       = true;
            CLOPTS(imm.pprint.file)         = OPTVALUE(argv,i,value);
            CLOPTS(imm.pprint.types)        = false;
            CLOPTS(imm.pprint.switch_to_if) = false;
        }
        else if (OPTPREFIXEQ(argv,i,CL_BIN,"pprint-types")) {
            if (!CLOPTS(imm.pprint.enable))
                PUT(err, "option %s: cannot be used before %s",
                    OPTPREFIX(CL) "pprint-types", OPTPREFIX(CL) "pprint");
            else
                CLOPTS(imm.pprint.types) = true;
        }else if (OPTPREFIXEQ(argv,i,CL_BIN,"pprint-switch-to-if")) {
            if (!CLOPTS(imm.pprint.enable))
                PUT(err, "option %s: cannot be used before %s",
                    OPTPREFIX(CL) "pprint-switch-to-if",
                    OPTPREFIX(CL) "pprint");
            else
                CLOPTS(imm.pprint.switch_to_if) = true;
        }
        else if ((value = OPTPREFIXEQ(argv,i,CL,"gen-cfg"))) {
            CLOPTS(imm.gencfg.enable) = true;
            CLOPTS(imm.gencfg.file)   = OPTVALUE(argv,i,value);
        }
        else if ((value = OPTPREFIXEQ(argv,i,CL,"gen-type"))) {
            CLOPTS(imm.gentype.enable) = true;
            CLOPTS(imm.gentype.file)   = OPTVALUE(argv,i,value);
        }
        else if (OPTPREFIXEQ(argv,i,CL_BIN,"debug-location")) {
            CLOPTS(imm.debug.location) = true;
        }
        else if ((value = OPTPREFIXEQ(argv,i,CL,"debug-level"))) {
            CLOPTS(imm.debug.level) = OPTVALUE(argv,i,value)
                ? get_positive_num("debug-level", value, opts)
                : ~0;
        }
        /* TODO: remove? */
        /*} else if ((value = OPTPREFIXEQ(argv,i,CL,"cl-args"))) {
            OPTS(peer_args) = OPTVALUE(value)
                ? value
                : "";*/
    }

    return 0;
#undef OPTPREFIXEQ
#undef OPTVALUE
}

/** The last/finalizing phase of gathering options.

    @todo Check code listener if !HAS_CL, ...
 */
static void
opts_initialize(struct options *opts, int argc, char *argv[])
{
#ifndef HAS_CL
    if (0 == CLOPTS(imm.listeners.cnt))
        DIE("no Code Listener specified");
#endif
    if (CLOPTS(imm.default_output) && FD_UNDEF != INTERNALS(imm.fd.cl))
        PUT(err, "option %s: does not make sense with %s",
            OPTPREFIX(CL) "pprint-switch-to-if",
            OPTPREFIX(CL) "pprint");
    SPARSEOPTS(set.argc) = argc;
    SPARSEOPTS(set.argv) = argv;
}

/** Initializes values within object reprezenting globals.

    @param[in,out]  globals_obj  Object representing globals.
    @note Presumes empty object as in case of default static
          initialization and only minimal set of values is treated here.
 */
struct globals *
globals_init(struct globals *globals_obj)
{
    globals_obj->stream[stream_out] = stdout;
    globals_obj->stream[stream_err] = stderr;
    globals_obj->unexposed.register_atexit = true;
#ifdef HAS_CL
    /* set symbols we know are available out-of-the-box */
# define PROCEED(what)  globals_obj->cl_api.what = &cl_##what
    CL_API_PROCEED(PROCEED);
# undef PROCEED
#endif
    return globals_obj;
}

/** Gather options in the structure which is also pre-initialized.

    @param[out] opts         Target options representation.
    @param[in]  globals_obj  Object (read-only in this stage) representing
                             globals.
    @param[in]  argc         argc
    @param[in]  argv         argv
    @return     0 = continue, <0 = exit ok, >0 = exit fail
 */
int
gather_opts(struct options *opts, const struct globals *globals_obj,
     int argc, char *argv[])
{
    int ret;

    opts_initialize(opts, globals_obj);

    if (!(ret = opts_proceed(opts, argc, argv))
        opts_finalize(opts, argc, argv);

    return ret;
}

int
main(int argc, char *argv[])
{
    struct options options, *opts = &options;
    int ret;
    pid_t pid;

    if (!(ret = gather_opts(opts, globals_init(&globals), argc, argv));
        return (ret < 0) ? EXIT_SUCCESS : ret;

    if (!INTERNALS(imm.fork))
        ret = worker(opts);
    else if ((pid_t) -1 == (pid = fork()))
        DIE( ERRNO("fork") );
    else if (pid == 0)
        ret = worker(opts);
    else
        ret = master(pid, opts);

    return ret;
}

/* vim:set ts=4 sts=4 sw=4 et: */
