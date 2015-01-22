#!/bin/bash
set -e
set -o pipefail

RELEASE=$1
INPUT_SIZE=$2
OUTPUT_FORMAT=$3

WORKDIR=/tmp
APP_ROOT=$(realpath $(dirname $0)/../..)
ERL_RELEASES_PATH=$APP_ROOT/priv/erl_installations

TMP_IN_MOD=in
TMP_IN_FILE=${TMP_IN_MOD}.erl

cd $WORKDIR

head -c $INPUT_SIZE > $TMP_IN_FILE

MAYBE_MOD=$(cat $TMP_IN_FILE | grep '\-module(' | sed -e 's/-module(\([0-9a-z]\+\))\..*/\1/')
if [ -n "$MAYBE_MOD" ]; then
    IN_MOD=$MAYBE_MOD
    IN_FILE=${IN_MOD}.erl
    mv $TMP_IN_FILE $IN_FILE
else
    IN_MOD=$TMP_IN_MOD
    IN_FILE==${IN_MOD}.erl
fi

source $ERL_RELEASES_PATH/$RELEASE/activate

# maybe send erlc's stderr with some 'packet prefix' from stdout?
case $OUTPUT_FORMAT in
    beam)
        erlc -Wall $IN_FILE
        ;;
    P | E | S)
        erlc -Wall -$OUTPUT_FORMAT $IN_FILE
        cat ${IN_MOD}.${OUTPUT_FORMAT}
        ;;
    # E)
    #     erlc -Wall -E -o $OUT_FILE in.erl
    #     cat $OUT_FILE
    #     ;;
    # S)
    #     erlc -Wall -S -o $OUT_FILE in.erl
    #     cat $OUT_FILE
    #     ;;
    core)
        erlc -Wall +to_core $IN_FILE
        cat ${IN_MOD}.core
        ;;
        
    dis)
        erlc -Wall $IN_FILE
        erl -noshell -eval "erts_debug:df(${IN_MOD}), erlang:halt(0)."
        cat ${IN_MOD}.dis
        ;;
esac
