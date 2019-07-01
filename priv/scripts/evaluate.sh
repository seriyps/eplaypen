#!/bin/sh
set -e
#set -o pipefail

MODULE=$1
INPUT_SIZE=$2

WORKDIR=~

IN_FILE=${MODULE}.erl

cd $WORKDIR

head -c $INPUT_SIZE > "$IN_FILE"

erlc -Wall "$IN_FILE"
# `+pc unicode` only >=R16
# ERL_CRASH_DUMP=/dev/null erl -noshell -s $MODULE main -s erlang halt

RUNSNIP="
io:setopts([{encoding, utf8}]),
try '$MODULE':main()
catch T:R ->
    io:format(standard_error, \"~p(~p)~n~p\", [T, R, erlang:get_stacktrace()])
end,
erlang:halt()."

ERL_CRASH_DUMP=/dev/null erl -noshell -noinput +pc unicode +A 0 -eval "$RUNSNIP"
