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
#ifndef CLSP_APIS_H_GUARD
#define CLSP_APIS_H_GUARD
/**
    External API functions are commonly not distinguished from local
    ones.  This is straightforward usage.  But there is no flexibility
    in it.  For instance, some or all the API calls need to be wrapped
    by some setup--teardown code on the client (our) side (e.g., logging
    of the call arguments and then of the return code).  This requires
    taking care about the each such call separately (bad maintenance).

    Thus, seemingly inconvenient, following constructs allow us to
    encapsulate external API dependencies into uniform "internalized"
    mirrored API, which buys us some deal of flexibility in terms of
    having its local usage under control (decorating the calls, perhaps
    even selectively, etc.).  Also lookup of calls from particular API
    in the code is much easier using a single search query, but what's
    more important is the in-code documentation aspect.

    Many macros are being defined here (prefixed with "API_" for sanity),
    but mostly they are only private dependencies for the ones meant for
    public usage (they help to get across some preprocessor language
    limitations).  Such internal macros start with "API__" and are not
    intended to be used directly outside this file.


    Internally, each API (set of functions and other symbols) is tight
    together around common prefix (e.g., FOO) which denotes the prefix
    of macros (analogy to methods in OOP) expected to have meaningful
    definition; such suffixes are (skipping interconnecting '_'):

    - NAME: abitrary identifier of the whole API
    - FQ:   function-like macro mapping internalized identifiers to
            fully-qualified API identifiers for end use (e.g., add
            a common, internally stripped prefix)
    - KIND: function-like macro returning the kind of API item
            based on its properties passed as arguments;  kind is encoded:
            [SNR][0-9]; S = non-functional
                        N = functional non-returing
                        R = functional returning API item
    - identifiers of API items (there can be a morphism of these
      internalized identifiers used here and the identifiers for end use,
      such as thecommon prefixes stripped -- they can be compensated back
      with FQ method, see above) tied together with their properties
      in the form of tuple of values;  first of them always denotes
      the type of the item, specified by a single letter 'A' through 'W':
      - A: function
      - B: function-like macro
      - C: global variable
    - LAST: tuple of the same size as properties of API items (see above),
            starting with 'Z' which denotes the last item
    - LIST: comma-separated enumeration of the identifiers defined as per
            above

    What is defined here is only a common minimum.  When using facilities
    provided by this file, specific function-like macros have to be defined
    (also functions maybe used directly when it makes sense) and passed as
    arguments to the main public macros:

    - API_USE
    - API_PROCEED[_SAFE]

    Such usage-specific function-like macros can be built on top of these:

    - API_NAME[_STR]
    - API_PROPS
    - API[_PROPS]_TYPE
    - API_KIND
    - API_FQ[_STR]

    Additionally, these ready-to-use macros are available:

    - API_PRAGMA_{OVERVIEW,DETAILS}

    Also, there may be API-specific function where it makes sense (e.g.,
    to access additional API item properties).
 */

#include "clsp-macros.h"
#if !defined(IDENTITY)      \
 || !defined(APPLY)         \
 || !defined(JOIN)          \
 || !defined(TOSTRING)      \
 || !defined(PRAGMA_MSGSTR)
# pragma message "Missing some macros that should be defined already"
#endif


#if 0
# define API_SHOW 1  /* 0=nothing, 1=overview only, 2=details */
#endif

#if (API_SHOW > 0)
# if (API_SHOW > 1)
PRAGMA_MSGSTR("OVERVIEW OF APIs USED FOLLOWS (A/B=std/macro func,C=global)")
# else
PRAGMA_MSGSTR("OVERVIEW OF APIs USED FOLLOWS:")
# endif
#endif


/*  (internal)
    Each internalized use of covered API part is defined indirectly through
    one of the following macros.  Their form comes partly from effort to avoid
    "ISO C99 requires rest arguments to be used" warning.

    The last argument "map" offers manipulation with the item before
    actually using it.  This is useful in some cases and when not, just
    use IDENTITY.
 */
#define API__USE(...)        API__USE_(__VA_ARGS__)
#define API__USE_(kind,...)  API__USE_##kind(__VA_ARGS__)
/* non-functional symbol */
#define API__USE_S0(sym,               map)  map(sym)
/* NOTE: this cannot be decorated, but is eligible for direct retval use */
#define API__USE_U0(fnc,               map)  map(fnc)(              )
#define API__USE_U1(fnc,a1,            map)  map(fnc)(a1            )
#define API__USE_U2(fnc,a1,a2,         map)  map(fnc)(a1,a2         )
#define API__USE_U3(fnc,a1,a2,a3,      map)  map(fnc)(a1,a2,a3      )
#define API__USE_U4(fnc,a1,a2,a3,a4,   map)  map(fnc)(a1,a2,a3,a4   )
#define API__USE_U5(fnc,a1,a2,a3,a4,a5,map)  map(fnc)(a1,a2,a3,a4,a5)
/* non-returning function/function-like macro (or retval ignored) */
#define API__USE_N0(...)  (void) API__USE_U0(__VA_ARGS__)
#define API__USE_N1(...)  (void) API__USE_U1(__VA_ARGS__)
#define API__USE_N2(...)  (void) API__USE_U2(__VA_ARGS__)
#define API__USE_N3(...)  (void) API__USE_U3(__VA_ARGS__)
#define API__USE_N4(...)  (void) API__USE_U4(__VA_ARGS__)
#define API__USE_N5(...)  (void) API__USE_U5(__VA_ARGS__)
/* returning function/function-like macro */
#define API__USE_R0(fnc,ret,...)  ret = API__USE_U0(fnc,__VA_ARGS__)
#define API__USE_R1(fnc,ret,...)  ret = API__USE_U1(fnc,__VA_ARGS__)
#define API__USE_R2(fnc,ret,...)  ret = API__USE_U2(fnc,__VA_ARGS__)
#define API__USE_R3(fnc,ret,...)  ret = API__USE_U3(fnc,__VA_ARGS__)
#define API__USE_R4(fnc,ret,...)  ret = API__USE_U4(fnc,__VA_ARGS__)
#define API__USE_R5(fnc,ret,...)  ret = API__USE_U5(fnc,__VA_ARGS__)


/*  (internal)
    Iteration (if it can be called like this) through the API items,
    perfoming selected action (defined, e.g., via another macro) on each
    (aka mapping).

    The iteration is performed as a tail recursion, applying "map" on the
    current "head" (and thus expanding it into final product) along.
    It stops if API item of reserved type 'Z' ('A' through 'W' is used to
    distinguish the type of the API item, see below) is encountered.
    This stop-mark is appended (in two instances in order to suppress
    mentioned "ISO C99" warning) by the facade function automatically.
   
    To incorporate this unavoidable branching, each iteration X consists
    of two parts:
    - API__PROCEED_X:  select whether to continue (see the next item) or end
    - API__PROCEED_X_: perform "map" (makes the final product) on the "head"
                       and recurse with the rest
 */
#define  API__PROCEED_BEGIN(...)  API__PROCEED_0(0,__VA_ARGS__)
#define  API__PROCEED_0(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)( 1,map,prefix,head,__VA_ARGS__)
#define  API__PROCEED_1(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)( 2,map,prefix,head,__VA_ARGS__)
#define  API__PROCEED_2(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)( 3,map,prefix,head,__VA_ARGS__)
#define  API__PROCEED_3(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)( 4,map,prefix,head,__VA_ARGS__)
#define  API__PROCEED_4(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)( 5,map,prefix,head,__VA_ARGS__)
#define  API__PROCEED_5(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)( 6,map,prefix,head,__VA_ARGS__)
#define  API__PROCEED_6(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)( 7,map,prefix,head,__VA_ARGS__)
#define  API__PROCEED_7(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)( 8,map,prefix,head,__VA_ARGS__)
#define  API__PROCEED_8(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)( 9,map,prefix,head,__VA_ARGS__)
#define  API__PROCEED_9(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)(10,map,prefix,head,__VA_ARGS__)
#define API__PROCEED_10(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)(11,map,prefix,head,__VA_ARGS__)
#define API__PROCEED_11(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)(12,map,prefix,head,__VA_ARGS__)
#define API__PROCEED_12(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)(13,map,prefix,head,__VA_ARGS__)
#define API__PROCEED_13(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)(14,map,prefix,head,__VA_ARGS__)
#define API__PROCEED_14(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)(15,map,prefix,head,__VA_ARGS__)
#define API__PROCEED_15(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)(16,map,prefix,head,__VA_ARGS__)
#define API__PROCEED_16(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)(17,map,prefix,head,__VA_ARGS__)
#define API__PROCEED_17(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)(18,map,prefix,head,__VA_ARGS__)
#define API__PROCEED_18(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)(19,map,prefix,head,__VA_ARGS__)
#define API__PROCEED_19(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)(20,map,prefix,head,__VA_ARGS__)
#define API__PROCEED_20(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)(21,map,prefix,head,__VA_ARGS__)
#define API__PROCEED_21(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)(22,map,prefix,head,__VA_ARGS__)
#define API__PROCEED_22(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)(23,map,prefix,head,__VA_ARGS__)
#define API__PROCEED_23(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)(24,map,prefix,head,__VA_ARGS__)
#define API__PROCEED_24(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)(25,map,prefix,head,__VA_ARGS__)
#define API__PROCEED_25(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)(26,map,prefix,head,__VA_ARGS__)
#define API__PROCEED_26(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)(27,map,prefix,head,__VA_ARGS__)
#define API__PROCEED_27(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)(28,map,prefix,head,__VA_ARGS__)
#define API__PROCEED_28(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)(29,map,prefix,head,__VA_ARGS__)
#define API__PROCEED_29(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)(30,map,prefix,head,__VA_ARGS__)
#define API__PROCEED_30(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)(31,map,prefix,head,__VA_ARGS__)
#define API__PROCEED_31(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)(32,map,prefix,head,__VA_ARGS__)
#define API__PROCEED_32(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)(33,map,prefix,head,__VA_ARGS__)
#define API__PROCEED_33(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)(34,map,prefix,head,__VA_ARGS__)
#define API__PROCEED_34(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)(35,map,prefix,head,__VA_ARGS__)
#define API__PROCEED_35(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)(36,map,prefix,head,__VA_ARGS__)
#define API__PROCEED_36(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)(37,map,prefix,head,__VA_ARGS__)
#define API__PROCEED_37(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)(38,map,prefix,head,__VA_ARGS__)
#define API__PROCEED_38(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)(39,map,prefix,head,__VA_ARGS__)
#define API__PROCEED_39(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)(40,map,prefix,head,__VA_ARGS__)
#define API__PROCEED_40(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)(41,map,prefix,head,__VA_ARGS__)
#define API__PROCEED_41(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)(42,map,prefix,head,__VA_ARGS__)
#define API__PROCEED_42(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)(43,map,prefix,head,__VA_ARGS__)
#define API__PROCEED_43(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)(44,map,prefix,head,__VA_ARGS__)
#define API__PROCEED_44(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)(45,map,prefix,head,__VA_ARGS__)
#define API__PROCEED_45(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)(46,map,prefix,head,__VA_ARGS__)
#define API__PROCEED_46(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)(47,map,prefix,head,__VA_ARGS__)
#define API__PROCEED_47(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)(48,map,prefix,head,__VA_ARGS__)
#define API__PROCEED_48(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)(49,map,prefix,head,__VA_ARGS__)
#define API__PROCEED_49(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)(50,map,prefix,head,__VA_ARGS__)
#define API__PROCEED_50(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)(51,map,prefix,head,__VA_ARGS__)
#define API__PROCEED_51(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)(52,map,prefix,head,__VA_ARGS__)
#define API__PROCEED_52(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)(53,map,prefix,head,__VA_ARGS__)
#define API__PROCEED_53(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)(54,map,prefix,head,__VA_ARGS__)
#define API__PROCEED_54(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)(55,map,prefix,head,__VA_ARGS__)
#define API__PROCEED_55(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)(56,map,prefix,head,__VA_ARGS__)
#define API__PROCEED_56(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)(57,map,prefix,head,__VA_ARGS__)
#define API__PROCEED_57(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)(58,map,prefix,head,__VA_ARGS__)
#define API__PROCEED_58(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)(59,map,prefix,head,__VA_ARGS__)
#define API__PROCEED_59(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)(60,map,prefix,head,__VA_ARGS__)
#define API__PROCEED_60(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)(61,map,prefix,head,__VA_ARGS__)
#define API__PROCEED_61(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)(62,map,prefix,head,__VA_ARGS__)
#define API__PROCEED_62(cnt,map,prefix,head,...) API__PROCEED_SELECT(API_TYPE(prefix,head),cnt)(63,map,prefix,head,__VA_ARGS__)
#define API__PROCEED_63(cnt,map,prefix,head,...) "NOT IMPLEMENTED YET ;)"

#define API__PROCEED_SELECT(type,cnt)   JOIN(API__PROCEED_SELECT_,type)(type,cnt)
/* A-W: continue (add missing types when needed), Z: stop iteration/recursion */
#define API__PROCEED_SELECT_A(type,cnt)  API__PROCEED_##cnt##_
#define API__PROCEED_SELECT_B(type,cnt)  API__PROCEED_##cnt##_
#define API__PROCEED_SELECT_C(type,cnt)  API__PROCEED_##cnt##_
#define API__PROCEED_SELECT_Z(type,cnt)  API__PROCEED_END_

#define  API__PROCEED_0_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define  API__PROCEED_1_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define  API__PROCEED_2_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define  API__PROCEED_3_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define  API__PROCEED_4_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define  API__PROCEED_5_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define  API__PROCEED_6_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define  API__PROCEED_7_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define  API__PROCEED_8_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define  API__PROCEED_9_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define API__PROCEED_10_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define API__PROCEED_11_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define API__PROCEED_12_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define API__PROCEED_13_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define API__PROCEED_14_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define API__PROCEED_15_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define API__PROCEED_16_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define API__PROCEED_17_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define API__PROCEED_18_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define API__PROCEED_19_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define API__PROCEED_20_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define API__PROCEED_21_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define API__PROCEED_22_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define API__PROCEED_23_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define API__PROCEED_24_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define API__PROCEED_25_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define API__PROCEED_26_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define API__PROCEED_27_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define API__PROCEED_28_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define API__PROCEED_29_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define API__PROCEED_30_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define API__PROCEED_31_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define API__PROCEED_32_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define API__PROCEED_33_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define API__PROCEED_34_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define API__PROCEED_35_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define API__PROCEED_36_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define API__PROCEED_37_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define API__PROCEED_38_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define API__PROCEED_39_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define API__PROCEED_40_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define API__PROCEED_41_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define API__PROCEED_42_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define API__PROCEED_43_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define API__PROCEED_44_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define API__PROCEED_45_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define API__PROCEED_46_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define API__PROCEED_47_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define API__PROCEED_48_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define API__PROCEED_49_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define API__PROCEED_50_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define API__PROCEED_51_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define API__PROCEED_52_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define API__PROCEED_53_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define API__PROCEED_54_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define API__PROCEED_55_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define API__PROCEED_56_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define API__PROCEED_57_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define API__PROCEED_58_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define API__PROCEED_59_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define API__PROCEED_60_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define API__PROCEED_61_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define API__PROCEED_62_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define API__PROCEED_63_(cnt,map,prefix,head,...) map(prefix,head,cnt) API__PROCEED_##cnt(cnt,map,prefix,__VA_ARGS__)
#define API__PROCEED_END_(cnt,map,prefix,head,last)  /* NOOP */


/*  
    Main mean of using the internally mirrored API.
  
    It is intended that the consumer compilation unit first makes
    the specific instance, then it can be used uniformly to access
    particular API:
  
    #include clsp_apis.h
    #define FOO(...)   FOO_(__VA_ARGS__,IDENTITY)
    #define FOO_(item,...)                   \
        do {                                 \
            printf("%s\n", STRINGIFY(item)); \
            API_USE(FOO,item,__VA_ARGS__);   \
        while (0)
    #define FRU(...)   API_USE(FRU,__VA_ARGS__,IDENTITY)
  
    FOO(bar, frob);      ==> do { printf("%s\n", "bar"); bar(frob); };
    FRU(fru, ret, baz);  ==> ret = fru(baz);
 */
#define API_USE(prefix,item,...)  \
    API__USE(API_KIND(prefix,item),item,__VA_ARGS__)


/*  
    A way to apply function/macro on each item of specified API.
  
    The second variant is a convenient wrapper to make expanded
    part safe when used anywhere in the functional (but apparently,
    cannot be used outside).
 */
#define API_PROCEED(prefix,map)      \
    API__PROCEED_BEGIN(map,prefix,API_##prefix##_LIST,LAST,LAST)

#define API_PROCEED_SAFE(prefix,map) \
    do {                             \
        API_PROCEED2(prefix,map)     \
    } while (0)


/*  helper macros  */

/* get API name (raw or stringified) of specified API */
#define API_NAME(prefix)         API_##prefix##_NAME
#define API_NAME_STR(prefix)     TOSTRING(API_NAME(prefix))

/* get properties of specified item of specified API */
#define API_PROPS(prefix,item)    API_##prefix##_##item

/* get type specification of specified item of specified API/its properties */
#define API_TYPE(prefix,item)     APPLY(API_PROPS_TYPE,API_PROPS(prefix,item))
#define API_PROPS_TYPE(type,...)  type

/* get kind specification of specified item of specified API */
#define API_KIND(prefix,item)     APPLY(API_##prefix##_KIND,API_PROPS(prefix,item))

/* internalized -> fully-qualified API identifier (raw/stringified) */
#define API_FQ(prefix,item)       API_##prefix##_FQ(item)
#define API_FQ_STR(prefix,item)   TOSTRING(API_FQ(prefix,item))


/*  ready-to-use macros  */

/* expose overview of specified API via pragma messages */
#define API_PRAGMA_OVERVIEW(prefix)                    \
    PRAGMA_MSGSTR("API:" API_NAME_STR(prefix) " =>"    \
                  API_PROCEED(prefix,API__SHOW_STRING))
#define API__SHOW_STRING(prefix,item,cnt)  " " API_FQ_STR(prefix,item) ","

/* expose details of specified API via pragma messages */
#define API_PRAGMA_DETAILS(prefix)  API_PROCEED(prefix,API__PRAGMA_DETAILS)
#define API__PRAGMA_DETAILS(prefix,item,cnt)                                  \
    PRAGMA_MSGSTR("API:" API_NAME_STR(prefix) ":"                             \
                  TOSTRING(API_TYPE(prefix,item)) ":" API_FQ_STR(prefix,item))


#endif
