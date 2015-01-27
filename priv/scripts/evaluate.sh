#!/bin/bash
set -e
set -o pipefail

MODULE=$1
RELEASE=$2
INPUT_SIZE=$3

WORKDIR=~
APP_ROOT=$(realpath $(dirname $0)/../..)
ERL_RELEASES_PATH=$APP_ROOT/priv/erl_installations

IN_FILE=${MODULE}.erl

cd $WORKDIR

head -c $INPUT_SIZE > "$IN_FILE"

source $ERL_RELEASES_PATH/$RELEASE/activate

erlc -Wall "$IN_FILE"
# `+pc unicode` only >=R16
# ERL_CRASH_DUMP=/dev/null erl -noshell -s $MODULE main -s erlang halt

RUNSNIP="
try '$MODULE':main()
catch T:R ->
    io:format(standard_error, \"~p(~p)~n~p\",
              [T, R, erlang:get_stacktrace()])
end,
erlang:halt()."

ERL_CRASH_DUMP=/dev/null erl -noshell +A 0 -eval "$RUNSNIP"
