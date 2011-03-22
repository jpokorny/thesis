#!/bin/bash

TESTSUITE_DIR=tests
TESTSUITE_OUTPUTS=.dots
TESTSUITE_LOGS=.logs
TESTSUITE_IGNORE=.testignore

ISOMORPHISM_TESTER="$PWD"/isomorphic
DIFF=colordiff

CL_SPARSE="$PWD"/test
CL_GCC=gcc
CL_GCC_PLUGIN=libcl_test

KEEP_DOT_FILES=1
GENERATE_BAD=1
SHOW_OK=0
SHOW_IGNORE=1


for i in $(seq 0 63); do SLINE="-$SLINE"; done
for i in $(seq 0 63); do DLINE="=$DLINE"; done


declare -i CNT_OK=0
declare -i CNT_BAD=0


function ok() {
    let CNT_OK+=1
    if [ $SHOW_OK -ne 0 ]; then
        printf "%-61s" "$(basename $1)"
        echo -e " \033[32mOK\033[0m"
    fi
}

function bad() {
    let CNT_BAD+=1
    echo $1 >> $3
    printf "%-61s" "$(basename $1)"
    echo -e "\033[31mBAD\033[0m"
    if [ $# -gt 3 ]; then
        echo $2
    elif [ $GENERATE_BAD -ne 0 ]; then
        dot -Tpdf $1 > $1.pdf
        dot -Tpdf $2 > $2.pdf
    fi
}


function proceed_sparse() {
    echo $1 $2 $3
    $CL_SPARSE $1 -cl-verbose=0 -cl-gen-dot=$2 -cl-type-dot=$3 2>&1 >/dev/null
}

function proceed_gcc() {
    $CL_GCC -c $1 -o /dev/null                      \
            -fplugin=$CL_GCC_PLUGIN.so              \
            -fplugin-arg-$CL_GCC_PLUGIN-verbose=0   \
            -fplugin-arg-$CL_GCC_PLUGIN-gen-dot=$2  \
            -fplugin-arg-$CL_GCC_PLUGIN-type-dot=$3 2>&1 >/dev/null
}

function prelude() {
    PATH="`readlink -f ../predator_cl/gcc-install/bin`:$PATH"
    LD_LIBRARY_PATH="`readlink -f ../predator_cl/cl_build`:$LD_LIBRARY_PATH"
    export PATH LD_LIBRARY_PATH
    echo $LD_LIBRARY_PATH
}


function do_tests() {
    prelude
    clean >/dev/null

    ###
    pushd $TESTSUITE_DIR >/dev/null

    mkdir -p $TESTSUITE_OUTPUTS/sparse $TESTSUITE_OUTPUTS/gcc $TESTSUITE_LOGS
    PREV_SUMMARY_LOG=$TESTSUITE_LOGS/$(ls -1 -r $TESTSUITE_LOGS | head -n1)
    SUMMARY_LOG=$TESTSUITE_LOGS/$(date +%y%m%d_%H%M%S).log.unsorted
    echo $PREV_SUMMARY_LOG

    for SRC in $(find . -name "*.c" | sed "s/\.\//|/1" | cut -d"|" -f2); do
        echo $DLINE
        proceed_sparse $SRC $SRC.flow.dot $SRC.type.dot 2>&1 >/dev/null
            tar cf - $SRC*.dot | ( cd $TESTSUITE_OUTPUTS/sparse; tar xfp -)
            rm -f -- $SRC*.dot
        proceed_gcc    $SRC $SRC.flow.dot $SRC.type.dot 2>&1 >/dev/null
            tar cf - $SRC*.dot | ( cd $TESTSUITE_OUTPUTS/gcc; tar xfp -)
            rm -f -- $SRC*.dot
        GCC_DOTS=$(find $TESTSUITE_OUTPUTS/gcc/. -path "*$SRC*.dot")
        printf "%-61s %2i\n" $(echo -n $SRC | tr [:lower:] [:upper:]) \
                             $(echo $GCC_DOTS | wc -w)
        #echo $SLINE
        for DOT in $GCC_DOTS; do
            PATTERN=$(echo $DOT | sed "s/\.\//|/1" | cut -d"|" -f2)
            IGNORE=$(grep -F "$PATTERN" $TESTSUITE_IGNORE) \
                && test "$(echo $IGNORE | cut -c1)" != "#"
            if [ $? -eq 0 ]; then
                if [ $SHOW_IGNORE -ne 0 ]; then
                    echo "ignore: $PATTERN: $(echo $IGNORE | cut -d";" -f2)"
                fi
                continue
            fi
            DOT_SPARSE=$(echo $DOT | sed s/gcc/sparse/1)
            $ISOMORPHISM_TESTER $DOT $DOT_SPARSE >/dev/null
            case $? in 0) ok  $DOT $DOT_SPARSE;;
                       1) bad $DOT $DOT_SPARSE $SUMMARY_LOG;;
                       *) bad $DOT $DOT_SPARSE $SUMMARY_LOG "unexpected retval";;
            esac
            [ $KEEP_DOT_FILES -eq 0 ] && rm -f -- $DOT $DOT_SPARSE
        done
    done

    echo $DLINE

    printf "Good %59d\n" $CNT_OK
    printf "Bad  %59d\n" $CNT_BAD
    printf "SUM  %59d\n" $(($CNT_OK+$CNT_BAD))

    echo $DLINE

    NEW_SUMMARY_LOG=$(echo $SUMMARY_LOG | sed "s/\.unsorted//1")
    sort $SUMMARY_LOG -o $NEW_SUMMARY_LOG
    rm -f $SUMMARY_LOG
    $DIFF $NEW_SUMMARY_LOG $PREV_SUMMARY_LOG

    [ $? -ne 0 ] && echo $DLINE

    popd >/dev/null
    ###
}


function clean() {
    echo "cleaning"; rm -r $TESTSUITE_DIR/$TESTSUITE_OUTPUTS || :
}

case $1 in "clean") $1;;
                 *) do_tests;;
esac
