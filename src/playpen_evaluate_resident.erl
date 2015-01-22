%%% @author Sergey Prokhorov <me@seriyps.ru>
%%% @copyright (C) 2015, Sergey Prokhorov
%%% @doc
%%%
%%% @end
%%% Created : 22 Jan 2015 by Sergey Prokhorov <me@seriyps.ru>

-module(playpen_evaluate_resident).

-export([main/1]).

main([InputSizeStr]) ->
    InputSize = list_to_integer(InputSizeStr),
    io:setopts([string, {encoding, unicode}]),
    Sources = stdin_read(InputSize),
    Output = eval(Sources),
    stdout_write(Output).


stdin_read(InputSize) ->
    io:get_chars([], InputSize).
stdout_write(Data) ->
    io:put_chars(Data).

eval(Sources) ->
    %% erl_scan:string()
    %% epp:... ????
    %% erl_parse:parse_form()
    %% compile:forms
    Sources.
