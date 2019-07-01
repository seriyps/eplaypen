#!/bin/sh
set -e
# set -o pipefail

MODULE=$1
INPUT_SIZE=$2
OUTPUT_FORMAT=$3

WORKDIR=~

IN_FILE=${MODULE}.erl

cd $WORKDIR

head -c $INPUT_SIZE > "$IN_FILE"

# maybe send erlc's stderr with some 'packet prefix' from stdout?
case $OUTPUT_FORMAT in
    beam)
        erlc -Wall "$IN_FILE"
        ;;
    P | E | S)
        erlc -Wall -$OUTPUT_FORMAT "$IN_FILE"
        cat "${MODULE}.${OUTPUT_FORMAT}"
        ;;
    ssa)
        erlc -Wall +dssa "$IN_FILE"
        cat "${MODULE}.ssa"
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
        # TODO: erlc +to_dis $IN_FILE works on Erlang >= 20
        erlc -Wall $IN_FILE
        erl -noshell -eval "erts_debug:df('${MODULE}'), erlang:halt(0)."
        cat "${MODULE}.dis"
        ;;
esac
