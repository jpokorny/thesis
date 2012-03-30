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

#include <ctype.h>     /* isdigit */
#include <assert.h>    /* assert */

#include "clsp.h"
#include "clsp_options.h"

/* see options_gather */
#define EXIT_OK    -1
#define EXIT_BAD   ec_opt
#define CONTINUE   0

/* "is binary option" X option prefix product */
#define OPT_SHORT      false,"-"
#define OPT_SHORT_BIN  true ,"-"
#define OPT_LONG       false,"--"
#define OPT_LONG_BIN   true ,"--"
#define OPT_CL         false,"-cl-"
#define OPT_CL_BIN     true ,"-cl-"

#define ISBIN(what)            APPLY(ISBIN_,OPT_##what)
#define PREFIX(what)           APPLY(PREFIX_,OPT_##what)
#define ISBIN_(isbin,prefix)   isbin
#define PREFIX_(isbin,prefix)  prefix


static const char *ec_str[ec_last] = {
#define X(num,name,desc)  [num] = desc,
    ECLIST(X)
#undef X
};

static const char *d_str[d_last] = {
#define X(num,name,desc)  [num] = desc,
    DLIST(X)
#undef X
};


/** general helpers *******************************************************/

/**
    Positive number converter (from string).

    @todo  More checks.
 */
static inline int
get_positive_num(const char *what, const char *value)
{
    if (!isdigit(value[0]))
        DIE( ECODE(OPT,"option %s: not a numeric value: %s",what,value) );
    int ret = strtol(value, NULL, 10);
    if (ret < 0)
        DIE( ECODE(OPT,"option %s: must be positive number",what) );
    return ret;
}

/**
    File descriptor specification converter (from string).

    @note Single @c 'D' characters stands for special "deferred" stream
          available as per @c accept_deferred argument.
 */
static inline int
get_fd(const char *what, const char *value, bool accept_deferred)
{
    if (accept_deferred && value[0] == 'D' && value[1] == '\0')
        return opts_fd_deferred;
    return get_positive_num(what, value);
}


/** output ****************************************************************/


/**
    Version printer.
 */
static void
print_version()
{
    PUT(out, "%s", GIT_SHA1);
}

/**
    Help printer.

    @param[in] cmd   Name of the program (like in argv[0]).
 */
static void
print_help(const char *cmd)
{
#define _(...)       PUT(out, __VA_ARGS__);
#define __           PUT(out, "");
#define ___          ""
#define L(lo, cmt)   PUT(out, "%-30s%s", PREFIX(LONG) lo, cmt);
#define S(so, cmt)   PUT(out, "%-30s%s", PREFIX(SHORT) so, cmt);
#define I(ign, cmt)  PUT(out, "%-30s%s", ign, cmt);
#define B(so, lo, cmt) \
    PUT(out, "%-30s%s", PREFIX(SHORT) so ", " PREFIX(LONG) lo, cmt);
#define C(co, cmt)   PUT(out, "%-30s%s", PREFIX(CL) co, cmt);
#define V(v, cmt)    PUT(out, "%8d                      %s", v, cmt);
    _("Sparse-based Code Listener frontend, version %s"               ,GIT_SHA1)
    __                                                                         ;
    _("usage: %s (intern-opts|cl-opts|cl-plugin[:args]|sparse-opts)* files",cmd)
    _("[files and option args may be ambiguous, use %s separator]",PREFIX(LONG))
    __                                                                         ;
#ifndef HAS_CL
    _("As no Code Listener plugin was built-in (-> no one to serve as a base"  )
    _("one at hand), at least one such has to be provided in the form of a"    )
    _("shared library containing necessary symbols of Code Listener interface" )
    _("(Code Listener based plugins for GCC are compatible);"                  )
    _("see `%s' option below.",                          PREFIX(CL) "plugin")
    __                                                                         ;
#endif
    _("This Code Listener front-end defines a few internal options:"           )
    __                                                                         ;
    B("h", "help"          , "Prints this help text"                           )
    L("version"            , "Prints the version information"                  )
    B("f", "fork"          , "Do fork (only to show sparse exit status {0,1})" )
    _("[specification of file descriptors: use `FD>file' redirection for FD>2]")
    L("cl-fd=FD"           , "Redefine stderr used to display to cl messages"  )
    I(___                  , "(fatal errors are always produced on stderr)"    )
    L("sparse-fd=FD"       , "Redefine stderr used by sparse, `D' for deferred")
    L("debug-fd=FD"        , "Redefine stdout used for debugging messages"     )
    __                                                                         ;
    B("d", "debug[=MASK]"  , "Debug (selectively if MASK specified)"           )
    _("MASK:")             ;              for (int i = d_first; i < d_last; i++)
    V(DVALUE(i)            ,                                           d_str[i])
    __                                                                         ;
    _("From the options affecting the other end of Code Listener interface,"   )
    _("one particularly important is a way to load other listeners as plugins:")
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
    _("%s"                 , "[...]"                                           )
    __                                                                         ;
    _("Return values:")    ;            for (int i = ec_first; i < ec_last; i++)
    V(ECVALUE(i)           ,                                          ec_str[i])
#undef V
#undef C
#undef B
#undef I
#undef S
#undef L
#undef ___
#undef __
#undef _
}


/** options processing ****************************************************/


/* convenient shortcuts (expects using "opts" for "struct options *") */
#define INTERNALS(what)  (opts->internals.what)
#define CL(what)         (opts->cl.what)
#define SPARSE(what)     (opts->sparse.what)


/**
    The first/initializing phase of gathering options.
 */
static void
options_initialize(struct options *opts)
{
    INTERNALS(fork) = false;
    INTERNALS(fd) = (struct oi_fd)
        { .cl=opts_fd_undef, .sparse=opts_fd_undef, .debug=opts_fd_undef };
    INTERNALS(debug) = 0;

    CL(listeners.cnt)  = 0;
    CL(listeners.arr)  = NULL;
    CL(default_output) = false;
    CL(pprint.enable)  = false;
    CL(gencfg.enable)  = false;
    CL(gentype.enable) = false;
    CL(debug) = (struct oc_debug) { .location=false, .level=0 };
}

/**
    The main phase of gathering options.

    We only handle known options/arguments and leave argv untouched
    behind us (our options should be guaranteed not to collide with
    sparse, though).
 */
static int
options_proceed(struct options *opts, int argc, char *argv[])
{
#define PREFIXEQ(argv, i, type, opt)                                         \
    (strncmp(argv[i],PREFIX(type) opt,strlen(PREFIX(type) opt))              \
        ? NULL                                                               \
        : (((ISBIN(type) && argv[i][strlen(PREFIX(type) opt)] != '\0')       \
            ? PUT(err, "option %s: binary option with argument (or clash?)", \
                  argv[i])                                                   \
            : 0)                                                             \
            , &argv[i][strlen(PREFIX(type) opt)]))
#define VALUE(argv, i, str)                                                  \
    (*str != '\0'                                                            \
        ? (((*str != '=' || *++str != '\0')) ? str : NULL)                   \
        : (argv[i+1] && argv[i+1][0] != '-')                                 \
            ? (str = argv[++i])                                              \
            : NULL)
    char *value;
    int args = 0, ret = CONTINUE;
    for (int i=1; i < argc; /*manually*/ ) {

        /* internal options */

        if (PREFIXEQ(argv,i,SHORT_BIN,"h")
          || PREFIXEQ(argv,i,LONG_BIN,"help")) {
            return print_help(argv[0]), EXIT_OK;
        }
        else if ((value = PREFIXEQ(argv,i,LONG_BIN,"version"))) {
            return print_version(opts), EXIT_OK;
        }
        else if ((value = PREFIXEQ(argv,i,SHORT_BIN,"f"))
          || (value = PREFIXEQ(argv,i,LONG_BIN,"fork"))) {
            INTERNALS(fork) = true;
        }
        else if ((value = PREFIXEQ(argv,i,LONG,"cl-fd"))
          || (value = PREFIXEQ(argv,i,LONG,"sparse-fd"))
          || (value = PREFIXEQ(argv,i,LONG,"debug-fd"))) {
            char *arg = argv[i];  /* preserve across VALUE */
            if (VALUE(argv,i,value)) {
                /* exploiting the difference of initial chars */
                int *to_set;
                switch (arg[strlen(PREFIX(LONG))]) {
                    case 'c': to_set = &INTERNALS(fd.cl);     break;
                    case 's': to_set = &INTERNALS(fd.sparse); break;
                    case 'd': to_set = &INTERNALS(fd.debug);  break;
                    default: DIE( ECODE(OPT,"unexpected case") );
                }
                *to_set = get_fd(arg, value,
                                 (to_set == &INTERNALS(fd.sparse)));
            } else {
                DIE( ECODE(OPT,"option %s: omitted value",arg) );
            }
        }
        else if ((value = PREFIXEQ(argv,i,SHORT,"d"))
          || (value = PREFIXEQ(argv,i,LONG,"debug"))) {
            if (!VALUE(argv,i,value)) {
                INTERNALS(debug) = ~0;
                continue;
            }
            INTERNALS(debug) = get_positive_num("debug", value);
        }

        /* Code Listener options */

        else if ((value = PREFIXEQ(argv,i,CL,"plugin"))) {
            char *arg = argv[i];  /* preserve across VALUE */
            if (VALUE(argv,i,value))
                *(MEM_ARR_APPEND(CL(listeners.arr),
                                 CL(listeners.cnt))) = VALUE(argv,i,value);
            else
                DIE( ECODE(OPT,"option %s: omitted value",arg) );
        }
        else if (PREFIXEQ(argv,i,CL_BIN,"default-output")) {
            CL(default_output) = true;
        }
        else if ((value = PREFIXEQ(argv,i,CL,"pprint"))) {
            CL(pprint.enable)       = true;
            CL(pprint.file)         = VALUE(argv,i,value);
            CL(pprint.types)        = false;
            CL(pprint.switch_to_if) = false;
        }
        else if (PREFIXEQ(argv,i,CL_BIN,"pprint-types")) {
            if (!CL(pprint.enable))
                PUT(err, "option %s: cannot be used before %s",
                    PREFIX(CL) "pprint-types", PREFIX(CL) "pprint");
            else
                CL(pprint.types) = true;
        }
        else if (PREFIXEQ(argv,i,CL_BIN,"pprint-switch-to-if")) {
            if (!CL(pprint.enable))
                PUT(err, "option %s: cannot be used before %s",
                    PREFIX(CL) "pprint-switch-to-if",
                    PREFIX(CL) "pprint");
            else
                CL(pprint.switch_to_if) = true;
        }
        else if ((value = PREFIXEQ(argv,i,CL,"gen-cfg"))) {
            CL(gencfg.enable) = true;
            CL(gencfg.file)   = VALUE(argv,i,value);
        }
        else if ((value = PREFIXEQ(argv,i,CL,"gen-type"))) {
            CL(gentype.enable) = true;
            CL(gentype.file)   = VALUE(argv,i,value);
        }
        else if (PREFIXEQ(argv,i,CL_BIN,"debug-location")) {
            CL(debug.location) = true;
        }
        else if ((value = PREFIXEQ(argv,i,CL,"debug-level"))) {
            if (!VALUE(argv,i,value)) {
                CL(debug.level) = ~0;
                continue;
            }
            CL(debug.level) = get_positive_num("debug-level", value);
        }
        /* TODO: remove? */
        /*} else if ((value = PREFIXEQ(argv,i,CL,"cl-args"))) {
            OPTS(peer_args) = VALUE(value)
                ? value
                : "";*/

        else if (PREFIXEQ(argv,i,CL,"" /* prefix only */)) {
            PUT(err, "option %s: this alone does not make sense", argv[i]);
        }
        else if (PREFIXEQ(argv,i,LONG,"" /* prefix only -> separator */)) {
            ++i;
            break;
        }
        else {
            args++;
        }
        ++i;
    }

    if (!args) {
        if (1 < argc)
            PUT(err, "missing arguments (while some options specified)");
        else
            print_help(argv[0]);
        ret = EXIT_BAD;
    }

    return ret;
}

/**
    The last/finalizing phase of gathering options.

    @todo Check code listener if !HAS_CL, ...
 */
static void
options_finalize(struct options *opts, int argc, char *argv[])
{
#ifndef HAS_CL
    if (0 == CL(listeners.cnt))
        DIE( ECODE(OPT,"no Code Listener specified") );
#endif

    if (opts_fd_undef != INTERNALS(fd.cl)
      && CL(default_output)) {
        PUT(err, "option %s: does not make sense with %s",
                 PREFIX(LONG) "cl-fd",
                 PREFIX(CL) "default-output");
        INTERNALS(fd.cl) = opts_fd_undef;
    }

    if (opts_fd_undef == INTERNALS(fd.debug)
      && 0 != INTERNALS(debug)) {
        PUT(err, "option %s: does not make sense without %s",
                 PREFIX(LONG) "debug-fd",
                 PREFIX(LONG) "debug");
        INTERNALS(fd.cl) = opts_fd_undef;
    }

    SPARSE(argc) = argc;
    SPARSE(argv) = argv;

    GLOBALS(debug) = INTERNALS(debug);
}

/* see clsp_options.h */
int
options_gather(struct options **opts, int argc, char *argv[])
{
    assert(opts);
    assert(argv != NULL);

    struct options *new_opts;
    int ret;

    new_opts = malloc(sizeof(**opts));
    if (!new_opts)
        DIE( ERRNOCODE(OPT,"malloc") );

    options_initialize(new_opts);

    ret = options_proceed(new_opts, argc, argv);
    if (CONTINUE == ret)
        options_finalize(new_opts, argc, argv);

    *opts = new_opts;
    return ret;
}

/* see clsp_options.h */
void
options_dump(const struct options *opts)
{
#define GET_YN(b)  (b) ? 'Y' : 'N'

    assert(opts);

    PUT(out, "------------\noptions dump\n------------");


    PUT(out, "internals");
    PUT(out, "\tfork:\t%c", GET_YN(INTERNALS(fork)));
    PUT(out, "\tfd:\t{cl=%d,sparse=%d,debug=%d}",
             INTERNALS(fd.cl),
             INTERNALS(fd.sparse),
             INTERNALS(fd.debug));
    PUT(out, "\tdebug:\t%d", INTERNALS(debug));
    PUT(out, "");


    PUT(out, "cl");
    PUT(out, "\tlisteners:\t%zu", CL(listeners.cnt));
    for (size_t i = 0; i < CL(listeners.cnt); i++)
        PUT(out, "\t\t%s", CL(listeners.arr[i]));
    PUT(out, "\tdefault_output:\t%c", GET_YN(CL(default_output)));

    if (CL(pprint.enable))
        PUT(out, "\tpprint:\t{types=%c, switch_to_if=%c, file=%s}",
                 GET_YN(CL(pprint.types)),
                 GET_YN(CL(pprint.switch_to_if)),
                 CL(pprint.file));
    else
        PUT(out, "\tpprint:\tN/A");

    if (CL(gencfg.enable))
        PUT(out, "\tgencfg:\t{file=%s}",
                 CL(gencfg.file));
    else
        PUT(out, "\tgencfg:\tN/A");

    if (CL(gentype.enable))
        PUT(out, "\tgentype:\t{file=%s}",
                 CL(gentype.file));
    else
        PUT(out, "\tgentype:\tN/A");

    PUT(out, "\tdebug:\t{location=%c, level=%d}",
             GET_YN(CL(debug.location)),
             CL(debug.level));
    PUT(out, "");


    PUT(out, "sparse");
    PUT(out, "\targc:\t%d", SPARSE(argc));
    PUT(out, "\targv:\t%s",SPARSE(argv[0]));
    for (int i = 1; i < SPARSE(argc); i++)
        PUT(out, "\t\t%s", SPARSE(argv[i]));


    PUT(out, "------------");
}

#ifdef TEST
const char *const GIT_SHA1 = "someversion";
struct globals globals;

int
main(int argc, char *argv[])
{
    int ret;
    struct options *opts;

    STREAM(out) = stdout;
    STREAM(err) = stderr;

    ret = options_gather(&opts, argc, argv);

    if (ret)
        return (0 > ret) ? EXIT_SUCCESS : ret;

    options_dump(opts);

    return ret;
}
#endif

/* vim:set ts=4 sts=4 sw=4 et: */
