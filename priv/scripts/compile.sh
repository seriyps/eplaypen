#!/bin/bash
set -e
set -o pipefail

MODULE=$1
RELEASE=$2
INPUT_SIZE=$3
OUTPUT_FORMAT=$4

WORKDIR=/tmp
APP_ROOT=$(realpath $(dirname $0)/../..)
ERL_RELEASES_PATH=$APP_ROOT/priv/erl_installations

IN_FILE=${MODULE}.erl

cd $WORKDIR

head -c $INPUT_SIZE > "$IN_FILE"

source $ERL_RELEASES_PATH/$RELEASE/activate

# maybe send erlc's stderr with some 'packet prefix' from stdout?
case $OUTPUT_FORMAT in
    beam)
        erlc -Wall "$IN_FILE"
        ;;
    P | E | S)
        erlc -Wall -$OUTPUT_FORMAT "$IN_FILE"
        cat "${MODULE}.${OUTPUT_FORMAT}"
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
        erlc -Wall +to_core "$IN_FILE"
        cat "${MODULE}.core"
        ;;
        
    dis)
        erlc -Wall $IN_FILE
        erl -noshell -eval "erts_debug:df('${MODULE}'), erlang:halt(0)."
        cat "${MODULE}.dis"
        ;;
esac
