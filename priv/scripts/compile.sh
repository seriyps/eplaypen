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
        escript $(dirname "$0")/beam_chunks.erl "${MODULE}"
        ;;
    tokens)
        CODE='{ok, B} = file:read_file("'$IN_FILE'"),
               Res = case erl_scan:string(unicode:characters_to_list(B), 0, [text, return]) of
                 {ok, Tokens, _End} -> Tokens;
                 Other -> Other
               end,
               io:format("~p~n", [Res]),
               erlang:halt(0).'
        erl -noshell +A 0 ${ERL_EXTRA_OPTS} -eval "$CODE"
        ;;
    P | E | S)
        erlc -Wall -${OUTPUT_FORMAT} "$IN_FILE"
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
        erlc -Wall +to_dis "$IN_FILE"
        cat "${MODULE}.dis"
        ;;
    dis_lt20)
        erlc -Wall "$IN_FILE"
        erl -noshell +A 0 ${ERL_EXTRA_OPTS} -eval "erts_debug:df('${MODULE}'), erlang:halt(0)."
        cat "${MODULE}.dis"
        ;;
    asmdump)
        erlc -Wall "$IN_FILE"
        erl +JDdump true -noshell +A 0 ${ERL_EXTRA_OPTS} -eval "code:load_file('${MODULE}'), erlang:halt(0)."
        cat "${MODULE}.asm"
        ;;
esac
