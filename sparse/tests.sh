#!/bin/bash

TESTSUITE_DIR=tests
TESTSUITE_OUTPUTS=.dots
TESTSUITE_IGNORE=.testignore

ISOMORPHISM_TESTER="$PWD"/isomorphic

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
    printf "%-61s" "$(basename $1)"
    echo -e "\033[31mBAD\033[0m"
    if [ $# -gt 2 ]; then
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
    PATH="`readlink -f ../../gcc-install/bin`:$PATH"
    LD_LIBRARY_PATH="`readlink -f ../../cl_build`:LD_LIBRARY_PATH"
    export PATH LD_LIBRARY_PATH
}


function do_tests() {
    #prelude
    pushd $TESTSUITE_DIR >/dev/null
    mkdir -p $TESTSUITE_OUTPUTS/sparse $TESTSUITE_OUTPUTS/gcc
    for SRC in $(find . -name "*.c"); do
        echo $DLINE
        proceed_sparse $SRC $SRC.flow.dot $SRC.type.dot 2>&1 >/dev/null
            tar cf - $SRC*.dot | ( cd $TESTSUITE_OUTPUTS/sparse; tar xfp -)
            rm -f -- $SRC*.dot
        proceed_gcc    $SRC $SRC.flow.dot $SRC.type.dot 2>&1 >/dev/null
            tar cf - $SRC*.dot | ( cd $TESTSUITE_OUTPUTS/gcc; tar xfp -)
            rm -f -- $SRC*.dot
        GCC_DOTS=$(find $TESTSUITE_OUTPUTS/gcc/. -path "*$SRC*.dot")       
        echo "$SRC ($(echo $GCC_DOTS | wc -w) dot files for gcc variant)"
        echo $SLINE
        for DOT in $GCC_DOTS; do
            PATTERN=$(echo $DOT | sed "s/\/\.\//|/1" | cut -d"|" -f2)
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
                       1) bad $DOT $DOT_SPARSE;;
                       *) bad $DOT $DOT_SPARSE "unexpected retval";;
            esac
            [ $KEEP_DOT_FILES -eq 0 ] && rm -f -- $DOT $DOT_SPARSE
        done
    done
    popd >/dev/null

    echo $DLINE
    printf "Good %59d\n" $CNT_OK
    printf "Bad  %59d\n" $CNT_BAD
    printf "SUM  %59d\n" $(($CNT_OK+$CNT_BAD))
}


case $1 in "clean") echo "cleaning"; rm -r $TESTSUITE_DIR/$TESTSUITE_OUTPUTS;;
                 *) do_tests;;
esac