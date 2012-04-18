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

#include "clsp.h"

#include <ctype.h>   /* isdigit */
#include <assert.h>
#include <libgen.h>  /* POSIX basename */
#include <stddef.h>  /* ptrdiff_t */

#include "clsp-options.h"
#include "clsp-version.h"
#include "clsp-defaults.h"
#include "clsp-output.h"     /* GET_YN */
#include "clsp-emit.h"       /* enum emit_props */
#include "clsp-api-sparse.h" /* SPARSE_OPT_PP */


#define OPTSARGS_SEP  "--"

/* "is binary option" X option prefix product */
#define OPT_RAW        false,""
#define OPT_RAW_BIN    true ,""
#define OPT_SHORT      false,"-"
#define OPT_SHORT_BIN  true ,"-"
#define OPT_LONG       false,"--"
#define OPT_LONG_BIN   true ,"--"
#define OPT_CL         false,"-cl-"
#define OPT_CL_BIN     true ,"-cl-"

#define ISBIN(what)            APPLY_INNER(ISBIN_,OPT_##what)
#define PREFIX(what)           APPLY_INNER(PREFIX_,OPT_##what)
#define ISBIN_(isbin,prefix)   isbin
#define PREFIX_(isbin,prefix)  prefix


static const char *d_str[d_last] = {
#define X(name,desc)  [d_##name] = desc,
    DLIST(X)
#undef X
};


/* command-line arguments used internally are exchanged for this */
static const char *const empty = "";


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
    File descriptor specification converter (from string to int).

    @note Single @c 'D' characters stands for special "deferred" stream
          (denoted by @c fd_deferred_unspec) and @c 'D' followed by number
          is deferred stream that is finally merged into such specified
          descriptor (denoted by negative value of such descriptor as
          a return value.  Both available only @c accept_deferred is set.
 */
static inline int
get_fd(const char *what, const char *value, bool accept_deferred)
{
    if (accept_deferred && value[0] == 'D') {
        if (value[1] == '\0')
            return fd_deferred_unspec;
        else
            return -get_positive_num(what, &value[1]);
    }
    return get_positive_num(what, value);
}

/**
    Color specification converter (from string to enum color).
 */
static inline struct palette
get_palette(const char *what, const char *value)
{
    const char *idx = value;
    struct palette palette;
    enum color *clr;

    if (!value)
        return PALETTE_NONE;

#define X(norm, high, code)                                        \
    if (!strncmp(idx, #norm, CONST_STRLEN(#norm))) {               \
        palette = PALETTE(norm, high); idx += CONST_STRLEN(#norm); \
    } else
    CLRLIST(X) /*else*/ DIE( ECODE(OPT,"option %s: unknown color",what) );
#undef X

    clr = &palette.high;

    if (':' == *idx) {
        ++idx;
#define X(norm, high, code)                            \
    if (!strncmp(idx, #high, CONST_STRLEN(#high))) {   \
        *clr = clr_##high; idx += CONST_STRLEN(#high); \
    } else
        CLRLIST(X) /*else*/ DIE( ECODE(OPT,"option %s: unknown color",what) );
#undef X
    }

    if ('\0' != *idx)
        DIE( ECODE(OPT,"option %s: not specified correctly",what) );
   
    return palette;
}


/** output ****************************************************************/


/**
    Version printer.
 */
static void
print_version()
{
    PUT(out, _1(s)_2(s), GIT_SHA1, VER_DETAILS);
}

/**
    Help printer.

    @param[in] cmd   Name of the program (like in argv[0]).
 */
static void
print_help(const char *cmd)
{
#define _(...)       PUT(out, __VA_ARGS__);
#define __           PUT(out);
#define ___          ""
#define L(lo, cmt)   PUT(out, "  "_1(-28s)_2(s), PREFIX(LONG) lo, cmt);
#define S(so, cmt)   PUT(out, "  "_1(-28s)_2(s), PREFIX(SHORT) so, cmt);
#define I(ign, cmt)  PUT(out, "  "_1(-28s)_2(s), ign, cmt);
#define B(so, lo, cmt) \
    PUT(out, "  "_1(-28s)_2(s), PREFIX(SHORT) so ", " PREFIX(LONG) lo, cmt);
#define C(co, cmt)   PUT(out, "  "_1(-28s)_2(s), PREFIX(CL) co, cmt);
#define V(v, cmt)    PUT(out, _1(8d)"                      "_2(s), v, cmt);
#define X(stmt)      stmt
#define O(cmt)       PUT(out, "  |: " _1(-71s) " :|", cmt);
    char buf[1024], *ptr = buf;
    int i,j;
    NORETWRN(setvbuf(STREAM(out), NULL, _IOFBF, 0));  /* flush block later */
    _("Sparse-based Code Listener frontend, version "_1(s)            ,GIT_SHA1)
    __                                                                         ;
    _("usage: "_1(s)" (INT-OPTS|CL-OPTS-OR-PLUGIN|SPARSE-OPTS)* file [...]",cmd)
    __                                                                         ;
#ifndef HAS_CL
    _("As no Code Listener plugin was built-in (no one to serve as a base one" )
    _("at hand), at least one such has to be provided in the form of a shared" )
    _("library containing the symbols of the interface (plugins targeted for"  )
    _("GCC should be compatible);  see `"_1(s)"' below.", PREFIX(CL) "plugin"  )
    __                                                                         ;
#endif
    _("This Code Listener front-end defines a few internal options (INT-OPTS):")
    B("h", "help"          , "Prints this help text"                           )
    L("version"            , "Prints the version information"                  )
    B("k", "keep-going"    , "Defect file does not end the run, it is skipped" )
    B("t", "try-hard"      , "Make best effort to proceed even defective file" )
    B("n", "dry-run"       , "Skip the final confirmation of emitted code"     )
    B("E", "preprocessor"  , "Terminate showing output of sparse preprocessor" )
    O("file descriptors, use FD>file redirection for FD > 2, empty/0:/dev/null")
    O("sparse: `D[FD]' for output to optional FD (none=stderr) to be deferred" )
    L("fd-warn[=FD]"       , "Abnormalities warnings ["DEF_FD_STR(WARN)"]"     )
    L("fd-debug[=FD]"      , "Debugging (if enabled) ["DEF_FD_STR(DEBUG)"]"    )
    L("fd-sp[=FD]"         , "Sparse warnings/errors ["DEF_FD_STR(SPARSE)"]"   )
    L("fd-cl[=FD]"         , "CL standard messages   ["DEF_FD_STR(CL)"]"       )
    I(___                  , "(fatal errors are always produced on stderr)"    )
    L("fd-cl-debug[=FD]"   , "CL debug messages      ["DEF_FD_STR(CL_DEBUG)"]" )
    O("specification of colors (terminal only), empty or 'none' for no color;" )
    O("CLR format: NORMAL-CLR[:HIGHLIGHT-CLR] (latter autoselected otherwise)" )
    L("clr-warn[=CLR]"     , "Abnormalities warnings ["DEF_PLT_STR(WARN)"]"    )
    L("clr-debug[=CLR]"    , "Debugging (if enabled)   ["DEF_PLT_STR(DEBUG)"]" )
    L("clr-sp[=CLR]"       , "Sparse defects/entities["DEF_PLT_STR(SP)"]"      )
    L("clr-cl[=CLR]"       , "CL standard messages   ["DEF_PLT_STR(CL)"]"      )
    L("clr-cl-debug[=CLR]" , "CL debug messages      ["DEF_PLT_STR(CL_DEBUG)"]")
    X(for (i=0             ;                           i < (clr_last-1)/8; i++))
    X(for (j=0, *ptr++='\n'; j <= ((clr_last-j)/8-i ? 7 : (clr_last-2)%8); j++))
    X(ptr+=snprintf(ptr,sizeof(buf)+buf-ptr,"%s%-10s%s",CLR_PRINTARG(i*8+j+1));)
    X(*ptr = '\0'          ;                         ptr = buf; _(_1(s),++ptr);)
    B("d", "debug[=MASK]"  , "Internal debug; selectively with MASK, see below")
    X(for (i = d_first     ;                                   i < d_last; i++))
    V(DVALUE(i)            ,                                           d_str[i])
    __                                                                         ;
    _("From the options affecting CL infrastructure (CL-OPTS-OR-PLUGIN), one"  )
    _("particularly important is a way to load other listeners as plugins:"    )
    C("plugin=FILE[:ARGS]" , "Path to a shared library containg symbols of"    )
    I(___                  , "Code Listener (for instance, GCC plugins can be" )
#ifdef HAS_CL
    I(___                  , "used directly), passing it optional ARGS"        )
#else
    I(___                  , "used directly), passing it optional ARGS;"       )
    I(___                  , "the first one is a base one and must be provided")
#endif
    __                                                                         ;
#ifdef HAS_CL
    _("and specifically these options are for a base (built-in) Code Listener:")
#else
    _("and specifically these options are for a base (provided) Code Listener:")
#endif
    C("default-output"     , "Use Code Listener's built-in message printers"   )
    C("pprint[=FILE]"      , "Pretty-print code along the run (stdout by def.)")
    C("pprint-types"       , "Add type information to pretty-printed code"     )
    C("pprint-switch-to-if", "Unfold `switch' into series of `if' statements " )
    C("gen-cfg[=MAIN_FILE]", "Generate control flow graphs (as per MAIN_FILE)" )
    C("gen-type[=FILE]"    , "Generate type graphs (to FILE if specified)"     )
    C("debug-location"     , "Keep printing location along the run"            )
    C("debug-level[=LEVEL]", "Debug (according to LEVEL if specified)"         )
    __                                                                         ;
    _("Sparse options (SPARSE-OPTS) are generally compatible with the common"  )
    _("compilers (notably GCC) and unrecognized options are ignored anyway;"   )
    _("some are highlighted below (for the rest refer to sparse itself):"      )
    S("v"                  , "Report more defects, more likely false positives")
    S("m64"                , "Suppose 64bit architecture (32bit by default)"   )
    S("W[no[-]]WARNING"    , "Request/not to report WARNING-related issues;"   )
    I(___                  , "`sparse-all' covers all available warnings"      )
    __                                                                         ;
    _("Tip:bash completion: eval \"$("_1(s)" "_2(s)")\"",cmd,PREFIX(LONG)"bash")
    __                                                                         ;
    _("Return values:")    ;            for (int i = ec_first; i < ec_last; i++)
    V(ECVALUE(i)           ,                                          ec_str[i])
    NORETWRN(fflush(STREAM(out)));
    /* about to exit, thus not bothering with buffering restoration */
#undef O
#undef X
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

#define OPTSLIST(x)                      \
    APPLY(x, SHORT, h)                   \
    APPLY(x, LONG,  help)                \
    APPLY(x, LONG,  version)             \
    APPLY(x, LONG,  bash)                \
    APPLY(x, SHORT, k)                   \
    APPLY(x, LONG,  keep-going)          \
    APPLY(x, SHORT, t)                   \
    APPLY(x, LONG,  try-hard)            \
    APPLY(x, SHORT, n)                   \
    APPLY(x, LONG,  dry-run)             \
    APPLY(x, SHORT, E)                   \
    APPLY(x, LONG,  preprocessor)        \
    APPLY(x, LONG,  fd-warn)             \
    APPLY(x, LONG,  fd-debug)            \
    APPLY(x, LONG,  fd-sp)               \
    APPLY(x, LONG,  fd-cl)               \
    APPLY(x, LONG,  fd-cl-debug)         \
    APPLY(x, LONG,  clr-warn)            \
    APPLY(x, LONG,  clr-debug)           \
    APPLY(x, LONG,  clr-sp)              \
    APPLY(x, LONG,  clr-cl)              \
    APPLY(x, LONG,  clr-cl-debug)        \
    APPLY(x, SHORT, d)                   \
    APPLY(x, LONG,  debug)               \
    APPLY(x, CL,    plugin)              \
    APPLY(x, CL,    default-output)      \
    APPLY(x, CL,    pprint)              \
    APPLY(x, CL,    pprint-types)        \
    APPLY(x, CL,    pprint-switch-to-if) \
    APPLY(x, CL,    gen-cfg)             \
    APPLY(x, CL,    gen-type)            \
    APPLY(x, CL,    debug-location)      \
    APPLY(x, CL,    debug-level)         \
    APPLY(x, SHORT, v)                   \
    APPLY(x, SHORT, m64)

static void
print_completion_bash(const char *cmd)
{
#define X1(type, name)        PREFIX(type) STRINGIFY(name) " "
#define X2(norm, high, code)  STRINGIFY(norm) " "

    /* (POSIX) basename may modify in-place which is not desired */
    char *copy = strdup(cmd);
    if (copy)
        cmd = basename(copy);

    PUT(out,"\
# "_1(s)" bash completion start\n\
# add me to ~/.profile persistently or eval on-the-fly in the bash\n\
\n\
_clsp()\n\
{\n\
    local prev cur opts colors\n\
    COMPREPLY=()\n\
    cur=\"${COMP_WORDS[COMP_CWORD]}\"\n\
    prev=\"${COMP_WORDS[COMP_CWORD-1]}\"\n\
\n\
    opts=\"" OPTSLIST(X1) "\"\n\
    colors=\"" CLRLIST(X2) "\"\n\
\n\
    if [[ ${cur} == -* ]]; then\n\
        COMPREPLY=( $(compgen -W \"${opts}\" -- ${cur}) )\n\
    elif [[ ${prev} == " PREFIX(LONG) "clr-[a-z-]* ]]; then\n\
        COMPREPLY=( $(compgen -W \"${colors}\" -- ${cur}) )\n\
    elif [[ ${prev} == " PREFIX(CL) "plugin ]]; then\n\
        COMPREPLY=( $(compgen -f -X '!*.so' ${cur}) )\n\
    else\n\
        COMPREPLY=( $(compgen -f -X '!*.c' ${cur}) )\n\
    fi\n\
}\n\
complete -o plusdirs -F _clsp \""_1(s)"\"\n\
\n\
# "_2(s)" bash completion end", cmd, cmd);

    free(copy);

#undef X2
#undef X1
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
    opts->finalized = false;

    INTERNALS(emit_props) = emit_vanilla;
    INTERNALS(fd) = (struct oi_fd) {
        .warn     = DEF_FD_VAL(WARN),
        .debug    = DEF_FD_VAL(DEBUG),
        .sp       = DEF_FD_VAL(SPARSE),
        .cl       = DEF_FD_VAL(CL),
        .cl_debug = DEF_FD_VAL(CL_DEBUG),
    };
    INTERNALS(clr) = (struct oi_clr) {
        .warn      = DEF_PLT(WARN),
        .debug     = DEF_PLT(DEBUG),
        .sp        = DEF_PLT(SP),
        .cl        = DEF_PLT(CL),
        .cl_debug  = DEF_PLT(CL_DEBUG),
    };
    INTERNALS(debug) = 0;

    CL(listeners.cnt)  = 0;
    CL(listeners.arr)  = NULL;
    CL(default_output) = false;
    CL(pprint.enable)  = false;
    CL(gencfg.enable)  = false;
    CL(gentype.enable) = false;
    CL(debug) = (struct oc_debug) { .location=false, .level=0 };
}


#define PREFIXEQ(arg, type, opt)                                               \
    (strncmp(arg, PREFIX(type) opt, CONST_STRLEN(PREFIX(type) opt))            \
        ? NULL                                                                 \
        : (((ISBIN(type) && (arg)[CONST_STRLEN(PREFIX(type) opt)] != '\0')     \
            ? PUT(err, "option "_1(s)": binary option with argument (clash?)", \
                  arg)                                                         \
            : 0)                                                               \
            , &(arg)[CONST_STRLEN(PREFIX(type) opt)]))
#define VALUE_(args, i, str, testnextchar)                         \
    (*str != '\0'                                                  \
        ? (((*str != '=' || *++str != '\0')) ? str : (str = NULL)) \
        : (args[i+1] && testnextchar(args[i+1][0]))                \
            ? (args[i++] = empty, str = args[i])                   \
            : (args[i] = empty, str = NULL))
/* NOTE: no explicit check whether str == NULL */
#define NONOPT(x)             x != '-'
#define VALUE(args, i, str)   VALUE_(args, i, str, NONOPT)
#define ISNUM(x)              isdigit(x)
#define NUMVAL(args, i, str)  VALUE_(args, i, str, ISNUM)

enum {
    proceeded_exit            = -2,
    proceeded_unconsumed      = -1,
    proceeded_nothing         =  0,
    proceeded_single_consumed =  1
};


static inline int
options_proceed_internal(struct options *opts, const char *args[],
                         const char *argv0)
{
    int i = 0, ret = proceeded_single_consumed;  /* optimistic default */
    const char *value;

    if ((value = PREFIXEQ(*args, SHORT_BIN, "h"))
      || (value = PREFIXEQ(*args, LONG_BIN, "help"))) {

        ret = (*value == '\0')
                ? (print_help(argv0), proceeded_exit)
                : proceeded_unconsumed;  /* return back as unconsumed */

    } else if ((value = PREFIXEQ(*args, LONG_BIN, "version"))) {

        print_version();
        ret = proceeded_exit;

    } else if ((value = PREFIXEQ(*args, LONG_BIN, "bash"))) {

        print_completion_bash(argv0);
        ret = proceeded_exit;

    } else if ((value = PREFIXEQ(*args, SHORT_BIN, "k"))
      || (value = PREFIXEQ(*args, LONG_BIN, "keep-going"))) {

        if (*value == '\0')
            INTERNALS(emit_props) |= emit_keep_going;
        else
            ret = proceeded_unconsumed;  /* return back as unconsumed */

    } else if ((value = PREFIXEQ(*args, SHORT_BIN, "t"))
      || (value = PREFIXEQ(*args, LONG_BIN, "try-hard"))) {

        if (*value == '\0')
            INTERNALS(emit_props) |= emit_try_hard;
        else
            ret = proceeded_unconsumed;  /* return back as unconsumed */

    } else if ((value = PREFIXEQ(*args, SHORT_BIN, "n"))
      || (value = PREFIXEQ(*args, LONG_BIN, "dry-run"))) {

        if (*value == '\0')
            INTERNALS(emit_props) |= emit_dry_run;
        else
            ret = proceeded_unconsumed;  /* return back as unconsumed */

    } else if ((value = PREFIXEQ(*args, LONG_BIN, "preprocessor"))) {

        /* just overwrite to canonical sparse preprocessing option */
        if (*value == '\0')
            *args = SPARSE_OPT_PREPROCESSOR;
        ret = proceeded_unconsumed;  /* return back as unconsumed */

    } else if ((value = PREFIXEQ(*args, LONG, "fd-warn"))
      || (value = PREFIXEQ(*args, LONG, "fd-debug"))
      || (value = PREFIXEQ(*args, LONG, "fd-sp"))
      || (value = PREFIXEQ(*args, LONG, "fd-cl"))) {

        /* exploiting the difference of initial chars (nested levels) */
        const char *arg = args[i];  /* preserve across VALUE */
        int *to_set;
        const char *c = arg;
        c += CONST_STRLEN(PREFIX(LONG)) + CONST_STRLEN("fd-");
        switch (*c) {
            case 'w': to_set = &INTERNALS(fd.warn);   break;
            case 'd': to_set = &INTERNALS(fd.debug);  break;
            case 's': to_set = &INTERNALS(fd.sp);     break;
            case 'c':
                c += CONST_STRLEN("cl");
                if ('\0' == *c) { to_set = &INTERNALS(fd.cl); break; }
                switch (*++c) {
                    case 'd': to_set = &INTERNALS(fd.cl_debug); break;
                    default: DIE( ECODE(OPT,"unexpected case: %s",arg) );
                }
                break;
            default: DIE( ECODE(OPT,"unexpected case: %s",arg) );
        }
        if (VALUE(args, i, value))
            *to_set = get_fd(arg, value,(to_set == &INTERNALS(fd.sp)));
        else
            *to_set = 0;  /* turned to /dev/null */

    } else if ((value = PREFIXEQ(*args, LONG, "clr-warn"))
      || (value = PREFIXEQ(*args, LONG, "clr-debug"))
      || (value = PREFIXEQ(*args, LONG, "clr-sp"))
      || (value = PREFIXEQ(*args, LONG, "clr-cl"))) {

        /* exploiting the difference of initial chars (nested levels) */
        const char *arg = args[i];  /* preserve across VALUE */
        struct palette *to_set;
        const char *c = arg;
        c += CONST_STRLEN(PREFIX(LONG)) + CONST_STRLEN("clr-");
        switch (*c) {
            case 'w': to_set = &INTERNALS(clr.warn);   break;
            case 'd': to_set = &INTERNALS(clr.debug);  break;
            case 's': to_set = &INTERNALS(clr.sp);  break;
            case 'c':
                c += CONST_STRLEN("cl");
                if ('\0' == *c) { to_set = &INTERNALS(clr.cl); break; }
                switch (*++c) {
                    case 'd': to_set = &INTERNALS(clr.cl_debug); break;
                    default: DIE( ECODE(OPT,"unexpected case: %s",arg) );
                }
                break;
            default: DIE( ECODE(OPT,"unexpected case: %s",arg) );
        }
        *to_set = get_palette(arg, VALUE(args, i, value));

    } else if ((value = PREFIXEQ(*args, SHORT, "d"))
      || (value = PREFIXEQ(*args, LONG, "debug"))) {

        if (!NUMVAL(args, i, value))
            INTERNALS(debug) = ~0;
        else
            INTERNALS(debug) = get_positive_num("debug", value);

    } else {
        /* nothing we recognise */
        ret = proceeded_nothing;
    }

    return ret + i;
}

#if 0
/*
    If we ever need to look into sparse's pile.
 */
static inline int
options_proceed_sparse(struct options *opts, const char *args[])
{
    const char *value;

    if (value = PREFIXEQ(*args, SHORT_BIN, "E")) {

        if (*value == '\0')
            pp = true;
    }
    
    return proceed_unconsumed;
}
#endif

static inline int
options_proceed_cl(struct options *opts, const char *args[])
{
    int i = 0, ret = proceeded_single_consumed;  /* optimistic default */
    const char *value;

    if ((value = PREFIXEQ(*args, CL, "plugin"))) {

        const char *arg = args[i];  /* preserve across VALUE */
        if (VALUE(args,i,value))
            *(MEM_ARR_APPEND(CL(listeners.arr), CL(listeners.cnt)))
                = value;
        else
            DIE( ECODE(OPT,"option %s: omitted value",arg) );

    } else if (PREFIXEQ(*args, CL_BIN, "default-output")) {

        CL(default_output) = true;

    } else if ((value = PREFIXEQ(*args, CL, "pprint"))) {

        CL(pprint.enable)       = true;
        CL(pprint.file)         = VALUE(args,i,value);
        CL(pprint.types)        = false;
        CL(pprint.switch_to_if) = false;

    } else if (PREFIXEQ(*args, CL_BIN, "pprint-types")) {

        if (!CL(pprint.enable))
            PUT(err, "option "_1(s)": cannot be used before "_2(s),
                PREFIX(CL) "pprint-types", PREFIX(CL) "pprint");
        else
            CL(pprint.types) = true;

    } else if (PREFIXEQ(*args, CL_BIN, "pprint-switch-to-if")) {

        if (!CL(pprint.enable))
            PUT(err, "option "_1(s)": cannot be used before "_2(s),
                PREFIX(CL) "pprint-switch-to-if",
                PREFIX(CL) "pprint");
        else
            CL(pprint.switch_to_if) = true;

    } else if ((value = PREFIXEQ(*args, CL, "gen-cfg"))) {

        CL(gencfg.enable) = true;
        CL(gencfg.file)   = VALUE(args,i,value);

    } else if ((value = PREFIXEQ(*args, CL, "gen-type"))) {

        CL(gentype.enable) = true;
        CL(gentype.file)   = VALUE(args,i,value);

    } else if (PREFIXEQ(*args, CL_BIN, "debug-location")) {

        CL(debug.location) = true;

    } else if ((value = PREFIXEQ(*args, CL, "debug-level"))) {

        if (!NUMVAL(args,i,value))
            CL(debug.level) = ~0;
        else
            CL(debug.level) = get_positive_num("debug-level", value);

    /* TODO: remove? */
    /*} else if ((value = PREFIXEQ(*args,CL,"cl-args"))) {

        OPTS(peer_args) = VALUE(value)
            ? value
            : "";

      }*/
    } else {
        /* nothing we recognise (i == 0) */
        ret = proceeded_nothing;
    }

    return ret + i;
}

/**
    The main phase of gathering options.

    We only handle known options/arguments and picking them out of argv
    (our options should be guaranteed not to collide with sparse).

    @return  How many options/arguments were kept in modified argv
             (special values aliasing with @c retval enumeration)
    @note    argv[0] 
 */
static int
options_proceed(struct options *opts, int argc, const char *argv[])
{
    bool consume_options = true, consumed;
    int i = 0, kept = 0, ret;
    const char *value;

    while (++i < argc) {
        assert(empty != argv[i]);

        if (consume_options) {
            consumed = true;

            ret = options_proceed_internal(opts, &argv[i], argv[0]);
            if (proceeded_nothing == ret)
                ret = options_proceed_cl(opts, &argv[i]);

            if (proceeded_nothing != ret) {
                switch (ret) {
                    case proceeded_unconsumed:
                        consumed = false;
                        break;
                    case proceeded_exit:
                        /* help and the like, bail out */
                        return ret_bye;
                    case proceeded_single_consumed:
                        /* no extra internally consumed args */
                        break;
                    default:
                        /* ret == 1 + number of extra internally consumed args */
                        assert(0 < ret);
                        i += ret - proceeded_single_consumed;
                }
            } else if (PREFIXEQ(argv[i], CL, "" /* prefix only */)) {
                PUT(err, "option "_1(s)": this alone does not make sense",
                         argv[i]);
            } else if ((value = PREFIXEQ(argv[i], RAW, OPTSARGS_SEP))) {
                if (*value == '\0')
                    consume_options = false;
                else
                    consumed = false;
            } else {
                /* unhandled opt/arg (probably for sparse) continue below */
                consumed = false;
            }

            if (consumed) {
                /* current item consumed (maybe more previous, not our deal) */
                argv[i] = empty;
                continue;
            }
        }

        /* probably sparse options/argument (may be forced with "--") */ 
        if (++kept != i) {
            argv[kept] = argv[i];
            argv[i] = empty;
        }
    }

    /* make modified argv fully standard-compliant again */
    if (kept <= i-1)
        argv[++kept] = NULL;
    assert(!argv[kept]);

    return kept;
}

/**
    The last/finalizing phase of gathering options.

    @todo Check code listener if !HAS_CL, ...
 */
static void
options_finalize(struct options *opts, int argc, char *argv[])
{
    /* XXX or enable automatically.. */
    if (0 == CL(debug.level) && CL(debug.location))
        PUT(err, "location will not be shown without explicitly"
                 " requiring use of CL debug messages");

    SPARSE(argc) = argc;
    SPARSE(argv) = argv;

    opts->finalized = true;
}

/* see clsp_options.h */
int
options_gather(struct options **opts, int argc, char *argv[])
{
    assert(opts);
    assert(argv != NULL);

    int ret;
    struct options *new_opts;

    new_opts = malloc(sizeof(*new_opts));
    if (!new_opts)
        DIE( ERRNOCODE(OPT,"malloc") );

    options_initialize(new_opts);
    ret = options_proceed(new_opts, argc, (const char **) argv);

    switch (ret) {
        case ret_bye:
            break;
        case 0:
            if (1 < argc)
                PUT(err, "missing arguments (while some options specified)");
            else
                print_help(argv[0]);
            ret = ret_fail;
            break;
        default:
            assert(0 < ret);
            options_finalize(new_opts, ret, argv);
            ret = ret_continue;
    }

    *opts = new_opts;
    return ret;
}

/* see clsp_options.h */
void
options_dump(const struct options *opts)
{

    assert(opts && opts->finalized);

    char buf[512], *ptr = buf;

    PUT(debug, "------------\n" HIGHLIGHT("options dump") "\n------------");

    PUT(debug, HIGHLIGHT("internals"));
    PUT(debug, "\tfd:\t{warn="_1(d)", debug="_2(d)", sp="_3(d)", cl="_4(d)
               ", cl-debug="_5(d)"}",
               INTERNALS(fd.warn),
               INTERNALS(fd.debug),
               INTERNALS(fd.sp),
               INTERNALS(fd.cl),
               INTERNALS(fd.cl_debug));

    ptr += snprintf(ptr, sizeof(buf)+buf-ptr, "%sdebug=%s",
                    STREAMCLRNORM(debug), STREAMCLREND(debug));
    ptr += snprintf(ptr, sizeof(buf)+buf-ptr, CLR_PRINTARG_FMT,
                    CLR_PRINTARG(INTERNALS(clr.warn.norm)));
    ptr += snprintf(ptr, sizeof(buf)+buf-ptr, "%s:%s",
                    STREAMCLRNORM(debug), STREAMCLREND(debug));
    ptr += snprintf(ptr, sizeof(buf)+buf-ptr, CLR_PRINTARG_FMT,
                    CLR_PRINTARG(INTERNALS(clr.warn.high)));

    ptr += snprintf(ptr, sizeof(buf)+buf-ptr, "%s, debug=%s",
                    STREAMCLRNORM(debug), STREAMCLREND(debug));
    ptr += snprintf(ptr, sizeof(buf)+buf-ptr, CLR_PRINTARG_FMT,
                    CLR_PRINTARG(INTERNALS(clr.debug.norm)));
    ptr += snprintf(ptr, sizeof(buf)+buf-ptr, "%s:%s",
                    STREAMCLRNORM(debug), STREAMCLREND(debug));
    ptr += snprintf(ptr, sizeof(buf)+buf-ptr, CLR_PRINTARG_FMT,
                    CLR_PRINTARG(INTERNALS(clr.debug.high)));

    ptr += snprintf(ptr, sizeof(buf)+buf-ptr, "%s, sp=%s",
                    STREAMCLRNORM(debug), STREAMCLREND(debug));
    ptr += snprintf(ptr, sizeof(buf)+buf-ptr, CLR_PRINTARG_FMT,
                    CLR_PRINTARG(INTERNALS(clr.sp.norm)));
    ptr += snprintf(ptr, sizeof(buf)+buf-ptr, "%s:%s",
                    STREAMCLRNORM(debug), STREAMCLREND(debug));
    ptr += snprintf(ptr, sizeof(buf)+buf-ptr, CLR_PRINTARG_FMT,
                    CLR_PRINTARG(INTERNALS(clr.sp.high)));

    ptr += snprintf(ptr, sizeof(buf)+buf-ptr, "%s, cl=%s",
                    STREAMCLRNORM(debug), STREAMCLREND(debug));
    ptr += snprintf(ptr, sizeof(buf)+buf-ptr, CLR_PRINTARG_FMT,
                    CLR_PRINTARG(INTERNALS(clr.cl.norm)));
    ptr += snprintf(ptr, sizeof(buf)+buf-ptr, "%s:%s",
                    STREAMCLRNORM(debug), STREAMCLREND(debug));
    ptr += snprintf(ptr, sizeof(buf)+buf-ptr, CLR_PRINTARG_FMT,
                    CLR_PRINTARG(INTERNALS(clr.cl.high)));

    ptr += snprintf(ptr, sizeof(buf)+buf-ptr, "%s, cl-debug=%s",
                    STREAMCLRNORM(debug), STREAMCLREND(debug));
    ptr += snprintf(ptr, sizeof(buf)+buf-ptr, CLR_PRINTARG_FMT,
                    CLR_PRINTARG(INTERNALS(clr.cl_debug.norm)));
    ptr += snprintf(ptr, sizeof(buf)+buf-ptr, "%s:%s",
                    STREAMCLRNORM(debug), STREAMCLREND(debug));
    ptr += snprintf(ptr, sizeof(buf)+buf-ptr, CLR_PRINTARG_FMT,
                    CLR_PRINTARG(INTERNALS(clr.cl_debug.high)));
    ptr += snprintf(ptr, sizeof(buf)+buf-ptr, "%s", STREAMCLRNORM(debug));
    assert(ptr-buf < (ptrdiff_t) sizeof(buf));

    PUT(debug, "\tclr:\t{"_1(s)"}", buf);
    PUT(debug, "\tdebug:\t"_1(d), INTERNALS(debug));
    PUT(debug);


    PUT(debug, HIGHLIGHT("cl"));
    PUT(debug, "\tlisteners:\t"_1(zu), CL(listeners.cnt));
    for (size_t i = 0; i < CL(listeners.cnt); i++)
        PUT(debug, "\t\t"_1(s), CL(listeners.arr[i]));
    PUT(debug, "\tdefault_output:\t"_1(c), GET_YN(CL(default_output)));

    if (CL(pprint.enable))
        PUT(debug, "\tpprint:\t{types="_1(c)", switch_to_if="_2(c)", "
                   "file="_3(s)"}",
                   GET_YN(CL(pprint.types)),
                   GET_YN(CL(pprint.switch_to_if)),
                   CL(pprint.file));
    else
        PUT(debug, "\tpprint:\tN/A");

    if (CL(gencfg.enable))
        PUT(debug, "\tgencfg:\t{file="_1(s)"}",
                   CL(gencfg.file));
    else
        PUT(debug, "\tgencfg:\tN/A");

    if (CL(gentype.enable))
        PUT(debug, "\tgentype:\t{file="_1(s)"}",
                   CL(gentype.file));
    else
        PUT(debug, "\tgentype:\tN/A");

    PUT(debug, "\tdebug:\t{location="_1(c)", level="_2(d)"}",
               GET_YN(CL(debug.location)),
               CL(debug.level));
    PUT(debug, "");


    PUT(debug, HIGHLIGHT("sparse"));
    PUT(debug, "\targc:\t"_1(d), SPARSE(argc));
    PUT(debug, "\targv:\t"_1(s),SPARSE(argv[0]));
    for (int i = 1; i < SPARSE(argc); i++)
        PUT(debug, "\t\t"_1(s), SPARSE(argv[i]));


    PUT(debug, "------------");
}

/* see clsp_options.h */
void
options_dispose(struct options *opts)
{
    free(CL(listeners.arr));
    free(opts);
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
