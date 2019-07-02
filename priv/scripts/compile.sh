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
        erl -noshell +A 0 -eval "c:m('${MODULE}'), erlang:halt(0)."
        ;;
    P | E | S)
        erlc -Wall -$OUTPUT_FORMAT "$IN_FILE"
        cat "${MODULE}.${OUTPUT_FORMAT}"
        ;;
    abstr | ssa)
        erlc -Wall +d${OUTPUT_FORMAT} "$IN_FILE"
        cat "${MODULE}.${OUTPUT_FORMAT}"
        ;;
    kernel)
        erlc -Wall +dkern "$IN_FILE"
        cat "${MODULE}.${OUTPUT_FORMAT}"
        ;;
    core)
        erlc -Wall +to_core "$IN_FILE"
        cat "${MODULE}.core"
        ;;
    dis_gte20)
        # Works on OTP >= 20
        erlc -Wall +to_dis $IN_FILE
        cat "${MODULE}.dis"
        ;;
    dis)
        erlc -Wall $IN_FILE
        erl -noshell +A 0 -eval "erts_debug:df('${MODULE}'), erlang:halt(0)."
        cat "${MODULE}.dis"
        ;;
esac
