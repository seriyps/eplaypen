#!/bin/sh
set -e
# set -o pipefail

MODULE=$1
INPUT_SIZE=$2
OUTPUT_FORMAT=$3
ERL_EXTRA_OPTS=$4

WORKDIR=~

IN_FILE=${MODULE}.erl

cd $WORKDIR

head -c $INPUT_SIZE > "$IN_FILE"

# maybe send erlc's stderr with some 'packet prefix' from stdout?
case $OUTPUT_FORMAT in
    beam)
        erlc -Wall "$IN_FILE"
        ;;
    beam_info)
        erlc -Wall +debug_info "$IN_FILE"
        CODE='
c:m('${MODULE}'),
io:format("~n~n===.beam chunks===~n~n"),
ChunkNames = [attributes, compile_info, exports, labeled_exports , imports, indexed_imports , locals, labeled_locals, atoms, abstract_code, case erlang:system_info(otp_release) > "19" of true -> debug_info; _ -> "Dbgi" end, "LitT", "StrT", "FunT", "Line", "Code"],
{ok, {_, Chunks}} = beam_lib:chunks(code:which('${MODULE}'), ChunkNames, [allow_missing_chunks]),
lists:foreach(fun({Name, Val}) -> io:format("~s:~n~p~n~n", [Name, Val]) end, Chunks),
erlang:halt(0).'
        erl -noshell +A 0 $ERL_EXTRA_OPTS -eval "${CODE}"
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
    dis)
        # Works on OTP >= 20
        erlc -Wall +to_dis $IN_FILE
        cat "${MODULE}.dis"
        ;;
    dis_lt20)
        erlc -Wall $IN_FILE
        erl -noshell +A 0 $ERL_EXTRA_OPTS -eval "erts_debug:df('${MODULE}'), erlang:halt(0)."
        cat "${MODULE}.dis"
        ;;
esac
