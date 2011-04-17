#!/bin/bash
#
# Prerequisities:
# * python (2.6+ perhaps)
# * NetworkX (http://networkx.lanl.gov/)

TESTSUITE_DIR=tests

TESTSUITE_DOTS=.dots
GCC_DOTS=$TESTSUITE_DOTS/gcc
TESTSUITE_LOGS=.logs
TESTSUITE_SPARSE_PP=.sparse_pp

TESTSUITE_IGNORE=.testignore

ISOMORPHISM_TESTER="$PWD"/isomorphic
SIMILARITY_TESTER="$PWD"/simdiff
DIFF=colordiff

CL_SPARSE="$PWD"/cl_sparse
CL_GCC=gcc
CL_GCC_PLUGIN=libcl_test

KEEP_DOT_FILES=1
GENERATE_BAD=1
SHOW_OK=1
SHOW_IGNORE_REASON=0

DIRDIFF=meld


for i in $(seq 0 63); do SLINE="-$SLINE"; done
for i in $(seq 0 63); do DLINE="=$DLINE"; done


declare -i CNT_OK_PREV=0
declare -i CNT_BAD_PREV=0

declare -i CNT_OK_GCC=0
declare -i CNT_BAD_GCC=0


function print_file () {
    printf "%-56s" "$(basename $1)"
}

function ok() {
    if [ $SHOW_OK -ne 0 ]; then
        echo -en "  \033[32mOK\033[0m"
    fi
}

function bad() {
    echo $(echo $DOT | sed "s/\.\//|/1" | cut -d"|" -f2) >> $3
    echo -en " \033[31mBAD\033[0m"
    if [ $# -gt 3 ]; then
        echo $4
    elif [ $GENERATE_BAD -ne 0 ]; then
        dot -Tpdf $1 > $1.pdf
        dot -Tpdf $2 > $2.pdf
    fi
}

function ignore() {
    echo -e " \033[33mIGN\033[0m"
    if [ $SHOW_IGNORE_REASON -ne 0 ]; then
        echo "!ignore! $1"
    fi
}

function ok_prev()  { let CNT_OK_PREV+=1;  print_file $1; ok  $@; }
function bad_prev() { let CNT_BAD_PREV+=1; print_file $1; bad $@; }

function ok_gcc()   { let CNT_OK_GCC+=1;   ok  $@; echo; }
function bad_gcc()  { let CNT_BAD_GCC+=1;  bad $@; echo; }


function proceed_sparse() {
    if [ $# -gt 3 ]; then
        SINK=$4
        mkdir -p $(dirname $SINK)
    else
        SINK=/dev/null
    fi
    $CL_SPARSE $1 -cl-verbose=0        \
        -cl-gen-dot=$2 -cl-type-dot=$3 \
        -cl-dump-pp -cl-dump-types     \
        2>/dev/null >$SINK
}

function proceed_gcc() {
    $CL_GCC -c $1 -o /dev/null                      \
            -fplugin=$CL_GCC_PLUGIN.so              \
            -fplugin-arg-$CL_GCC_PLUGIN-verbose=0   \
            -fplugin-arg-$CL_GCC_PLUGIN-gen-dot=$2  \
            -fplugin-arg-$CL_GCC_PLUGIN-type-dot=$3 2>&1 >/dev/null
}

function gcc_prelude() {
    PATH="`readlink -f ../predator_cl/gcc-install/bin`:$PATH"
    LD_LIBRARY_PATH="`readlink -f ../predator_cl/cl_build`:$LD_LIBRARY_PATH"
    export PATH LD_LIBRARY_PATH
}


function do_tests() {
    gcc_prelude
    clean >/dev/null

    pushd $TESTSUITE_DIR >/dev/null
    ###

    DATE=$(date +%y%m%d_%H%M%S)

    mkdir -p $TESTSUITE_DOTS
    PREV_SPARSE_DOTS=$TESTSUITE_DOTS/$(ls -1 -t $TESTSUITE_DOTS | head -n1)
    SPARSE_DOTS=$TESTSUITE_DOTS/$DATE
    mkdir -p $GCC_DOTS $SPARSE_DOTS

    mkdir -p $TESTSUITE_SPARSE_PP
    PREV_SPARSE_PP=$TESTSUITE_SPARSE_PP/$(ls -1 -t $TESTSUITE_SPARSE_PP | head -n1)
    SPARSE_PP=$TESTSUITE_SPARSE_PP/$DATE
    mkdir $SPARSE_PP

    mkdir -p $TESTSUITE_LOGS
    PREV_SUMMARY_LOG=$(ls -1 -t $TESTSUITE_LOGS/*.log | head -n1)
    SUMMARY_LOG=$TESTSUITE_LOGS/$DATE.log.unsorted

    # for each source file from testsuite, generate pretty prints
    # and flow graphs + type graphs and compare them with those
    # from previous run/with gcc cl frontend output
    for SRC in $(find . -name "*.c" | sed "s/\.\//|/1" | cut -d"|" -f2); do
        echo $DLINE
        proceed_sparse $SRC $SRC.flow.dot $SRC.type.dot $SPARSE_PP/$SRC.log #2>&1 >/dev/null
            tar cf - $SRC*.dot | ( cd $SPARSE_DOTS; tar xfp -)
            rm -f -- $SRC*.dot
        proceed_gcc    $SRC $SRC.flow.dot $SRC.type.dot # 2>&1 >/dev/null
            tar cf - $SRC*.dot | ( cd $GCC_DOTS; tar xfp -)
            rm -f -- $SRC*.dot
        GCC_DOT_FILES=$(find $GCC_DOTS/. -path "*$SRC*.dot")
        SPARSE_DOT_FILES=$(find $SPARSE_DOTS/. -path "*$SRC*.dot")
        [ $(echo $GCC_DOT_FILES | wc -w) -ne $(echo $SPARSE_DOT_FILES | wc -w) ] \
            && echo "Warning: #GCC dot files != #sparse dot files"

        # pretty-print similarity with previous stored
        TITLE="$(echo -n $SRC | tr [:lower:] [:upper:])"
        printf "%-44s pp similarity: %1.2f\n"                          \
            "$TITLE [$(echo $SPARSE_DOT_FILES | wc -w) sparse dot f.]" \
            "$($SIMILARITY_TESTER $SPARSE_PP/$SRC.log  $PREV_SPARSE_PP/$SRC.log)"

        # test isomorphism between graphs
        for DOT in $SPARSE_DOT_FILES; do
            # a) with previous graph by sparse cl frontend
            DOT_PREV=$(echo $DOT | sed "s|$SPARSE_DOTS|$PREV_SPARSE_DOTS|1")
            $ISOMORPHISM_TESTER $DOT $DOT_PREV >/dev/null
            case $? in 0) ok_prev  $DOT $DOT_PREV;;
                       1) bad_prev $DOT $DOT_PREV $SUMMARY_LOG;;
                       *) bad_prev $DOT $DOT_PREV $SUMMARY_LOG "unexpected retval";;
            esac

            # b) with current graph by gcc cl frontend
            #    (if not ignored or if it is not a type graph)
            echo "$DOT" | grep -F "type.dot" >/dev/null && ign=1 || ign=0
            if [ $ign -eq 0 ]; then
                ign=1
                PATTERN=$(echo "$DOT" | sed "s/\.\//|/1" | cut -d"|" -f2)
                IGNORE=$(grep -F "$PATTERN" $TESTSUITE_IGNORE) \
                    && [ "$(echo $IGNORE | cut -c1)" != "#" ] && ign=0
            fi
            if [ $ign -eq 1 ]; then
                ignore "$(echo $IGNORE | cut -d";" -f2)"
                continue
            fi
            DOT_GCC=$(echo $DOT | sed "s|$SPARSE_DOTS|$GCC_DOTS|1")
            $ISOMORPHISM_TESTER $DOT $DOT_GCC >/dev/null
            case $? in 0) ok_gcc  $DOT $DOT_GCC;;
                       1) bad_gcc $DOT $DOT_GCC $SUMMARY_LOG;;
                       *) bad_gcc $DOT $DOT_GCC $SUMMARY_LOG "unexpected retval";;
            esac
            [ $KEEP_DOT_FILES -eq 0 ] && rm -f -- $DOT $DOT_SPARSE
        done
    done

    echo $DLINE

    printf "Good %55d %3d\n" $CNT_OK_PREV  $CNT_OK_GCC
    printf "Bad  %55d %3d\n" $CNT_BAD_PREV $CNT_BAD_GCC
    printf "SUM  %55d %3d\n" $(($CNT_OK_PREV+$CNT_BAD_PREV)) $(($CNT_OK_GCC+$CNT_BAD_GCC))

    echo $DLINE

    NEW_SUMMARY_LOG=$(echo $SUMMARY_LOG | sed "s/\.unsorted//1")
    sort $SUMMARY_LOG -o $NEW_SUMMARY_LOG
    rm -f $SUMMARY_LOG
    $DIFF $NEW_SUMMARY_LOG $PREV_SUMMARY_LOG

    [ $? -ne 0 ] && echo $DLINE

    nohup $DIRDIFF $SPARSE_PP $PREV_SPARSE_PP 2>&1 >/dev/null &

    ###
    popd >/dev/null
}


function clean() {
    echo "cleaning"
    rm -r -- $TESTSUITE_DIR/$GCC_DOTS || :
}

function clean_prev() {
    echo "cleaning previous"

    # XXX: copy'n'paste from above

    pushd $TESTSUITE_DIR >/dev/null
    ###

    PREV_SPARSE_DOTS=$TESTSUITE_DOTS/$(ls -1 -t $TESTSUITE_DOTS | head -n1)
    PREV_SPARSE_PP=$TESTSUITE_SPARSE_PP/$(ls -1 -t $TESTSUITE_SPARSE_PP | head -n1)
    PREV_SUMMARY_LOG=$(ls -1 -t $TESTSUITE_LOGS | head -n1)

    rm -rf -- $PREV_SPARSE_DOTS $PREV_SPARSE_PP $PREV_SUMMARY_LOG

    ###
    popd >/dev/null
}

case $1 in "clean") $1;;
           "clean_prev") $1;;
           "gcc_prelude") $1;;
           "") do_tests;;
           *) echo "$0 [clean|gcc_prelude (note: use \"source $0\" for this)]"
esac
