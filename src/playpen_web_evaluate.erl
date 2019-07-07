%%% @author Sergey Prokhorov <me@seriyps.ru>
%%% @copyright (C) 2015, Sergey Prokhorov
%%% @doc
%%%
%%% @end
%%% Created : 20 Jan 2015 by Sergey Prokhorov <me@seriyps.ru>

-module(playpen_web_evaluate).

-export([init/2, terminate/3]).

-export([from_urlencoded/2, from_json/2]).

-include_lib("hut/include/hut.hrl").

-define(BODY_LIMIT, 102400).


-spec init(cowboy_req:req(), [evaluate | compile]) ->
                  {ok, cowboy_req:req(), any()}.
init(Req, Opts) ->
    ?log(debug, "handle"),
    case cowboy_req:parse_header(<<"content-type">>, Req) of
        {<<"application">>, <<"x-www-form-urlencoded">>, _} ->
            handle_body(Req, Opts, from_urlencoded);
        {<<"application">>, <<"json">>, _} ->
            handle_body(Req, Opts, from_json);
        {error, badarg} ->
            Req2 = cowboy_req:reply(415, Req),
            {ok, Req2, Opts}
    end.

terminate(_, _Req, _State) ->
    ok.

handle_body(Req, State, ParserCallback) ->
    case cowboy_req:has_body(Req) of
        false ->
            Req2 = cowboy_req:reply(204, Req),
            {ok, Req2, State};
        true ->
            ?MODULE:ParserCallback(Req, State)
    end.


from_urlencoded(Req, State) ->
    ?log(debug, "Urlencoded req"),
    {ok, KV, Req1} = cowboy_req:read_urlencoded_body(Req, #{length => ?BODY_LIMIT}),
    handle_arguments(Req1, State, maps:from_list(KV)).

from_json(Req, State) ->
    ?log(debug, "JSON req"),
    case cowboy_req:read_body(Req, #{length => ?BODY_LIMIT}) of
        {ok, Data, Req1} ->
            try jiffy:decode(Data, [return_maps]) of
                KV when is_map(KV) ->
                    handle_arguments(Req1, State, KV)
            catch throw:_ ->
                    Req2 = cowboy_req:reply(400, #{}, "Invalid JSON", Req1),
                    {ok, Req2, State}
            end;
        {more, _, Req1} ->
            Req2 = cowboy_req:reply(400, #{}, "Request body too long", Req1),
            {ok, Req2, State}
    end.

handle_arguments(Req, [evaluate] = State, #{<<"code">> := SourceCode, <<"release">> := Release}) ->
    Releases = playpen:available_releases(),
    case {maps:find(Release, Releases), find_module_name(SourceCode)} of
        {{ok, {ReleaseTag, Features}}, {ok, Mod}} ->
            Script = filename:join(["/mnt", "scripts", "evaluate.sh"]),
            Argv = [Script, Mod, integer_to_binary(iolist_size(SourceCode)), extra_erl_args(Features)],
            run(Req, State, ReleaseTag, Argv, SourceCode);
        {error, _} ->
            Req2 = cowboy_req:reply(400, #{}, io_lib:format("Unknown release '~s'", [Release]), Req),
            {ok, Req2, State};
        {_, error} ->
            Req2 = cowboy_req:reply(400, #{}, "Missing or invalid '-module' attribute.", Req),
            {ok, Req2, State}
    end;
handle_arguments(Req, [compile] = State, #{<<"code">> := SourceCode, <<"release">> := Release} = KV) ->
    Emit0 = maps:get(<<"emit">>, KV, <<"beam">>),
    Formats = playpen:available_outputs(),
    Releases = playpen:available_releases(),
    case {lists:member(Emit0, Formats),
          maps:find(Release, Releases),
          find_module_name(SourceCode)} of
        {true, {ok, {ReleaseTag, Features}}, {ok, Mod}} ->
            Script = filename:join(["/mnt", "scripts", "compile.sh"]),
            case convert_emit(Emit0, Features) of
                not_supported ->
                    Req2 = cowboy_req:reply(
                             400, #{},
                             io_lib:format("Output format '~s' not supported by '~s'",
                                           [Emit0, Release]), Req),
                    {ok, Req2, State};
                Emit ->
                    Argv = [Script, Mod, integer_to_binary(iolist_size(SourceCode)), Emit, extra_erl_args(Features)],
                    run(Req, State, ReleaseTag, Argv, SourceCode)
            end;
        {_, _, error} ->
            Req2 = cowboy_req:reply(400, #{}, "Missing or invalid '-module' attribute.", Req),
            {ok, Req2, State};
        {_, _, _} ->
            %% Invalid output format ~p or release ~p.
            Req2 = cowboy_req:reply(
                     400, #{},
                     io_lib:format("Invalid output format '~s' or release '~s'",
                                   [Emit0, Release]), Req),
            {ok, Req2, State}
    end;
handle_arguments(Req, State, Payload) ->
    %% Invalid payload ~p.
     Req2 = cowboy_req:reply(400, #{}, io_lib:format("Invalid payload ~p", [Payload]), Req),
    {ok, Req2, State}.

extra_erl_args(Features) ->
    Opts = lists:foldl(
             fun(dirty_io, Acc) ->
                     ["+SDio 1" | Acc];
                (_, Acc) ->
                     Acc
             end, [], Features),
    iolist_to_binary(lists:join(" ", Opts)).

convert_emit(<<"ssa">> = E, Features) ->
    case lists:member(ssa, Features) of
        true -> E;
        _ -> not_supported
    end;
convert_emit(<<"dis">> = E, Features) ->
    case lists:member(to_dis, Features) of
        true -> E;
        false -> <<"dis_lt20">>
    end;
convert_emit(Emit, _) ->
    Emit.



-spec run(cowboy_req:req(), term(), unicode:chardata(), list(), iodata()) -> {ok, cowboy_req:req(), term()}.
run(Req, State, Release, Argv, SourceCode) ->
    OutForm = text,
    ContentType = case OutForm of
                      text -> <<"text/plain">>;
                      binary -> <<"octet/stream">>
                  end,
    Req1 = cowboy_req:stream_reply(200, #{<<"content-type">> => ContentType}, Req),
    Callback = fun(Chunk, Req2) ->
                       ?log(debug, "Chunk ~p", [iolist_size(Chunk)]),
                       ok = cowboy_req:stream_body(output_frame(Chunk, OutForm), nofin, Req2),
                       Req2
               end,

    PrivDir = filename:absname(code:priv_dir(eplaypen)),
    BindMount = [
                 {filename:join(PrivDir, "scripts"), "/mnt/scripts", ro}
                ],
    PPOpts = #{timeout => 15,
               memory_limit => 100,
               image => iolist_to_binary(lists:join(":", ["erlang", Release])),
               cpu_limit => 1,
               mount => BindMount},
    IOOpts = #{max_output_size => 512 * 1024,
               collect_output => false,
               timeout => 10000,
               output_callback => Callback,
               output_callback_state => Req1},
    case playpen_cmd:cmd(Argv, SourceCode, PPOpts, IOOpts) of
        {ok, {Code, _, Req3}} ->
            ok = cowboy_req:stream_body(retcode_frame(Code, OutForm), fin, Req3),
            {ok, Req3, State};
        {error, {output_too_large, _, Req3}} ->
            ok = cowboy_req:stream_body(output_too_large_frame(OutForm), fin, Req3),
            {ok, Req3, State};
        {error, {timeout, _, Req3}} ->
            ok = cowboy_req:stream_body(timeout_frame(OutForm), fin, Req3),
            {ok, Req3, State}
    end.

%% 1 - stdout/stderr     <<1, Size:24/little, Payload:Size/binary>>
%% 2 - return code       <<2, Code:8>>
%% 3 - output too large  <<3>>
%% 4 - timeout           <<4>>
output_frame(Chunk, binary) ->
    Size = <<1, (size(Chunk)):24/little>>,
    [Size, Chunk];
output_frame(Chunk, text) ->
    %% ["1|",
    %%  integer_to_binary(size(Chunk)),
    %% "|", Chunk].
    Chunk.

retcode_frame(Code, binary) ->
    <<2, Code>>;
retcode_frame(Code, text) ->
    %% ["2|", integer_to_binary(Code)].
    [<<"\n------\nReturn code is ">>, integer_to_binary(Code)].

output_too_large_frame(binary) ->
    <<3>>;
output_too_large_frame(text) ->
    <<"\n------\nOutput too large. Truncated.">>.

timeout_frame(binary) ->
    <<4>>;
timeout_frame(text) ->
    <<"\n------\nTimeout">>.


find_module_name(Body) ->
    %% May use `erl_scan:string/1', but this requires `binary_to_string'
    %% for sources.
    %% " \t - \t \n module \n ( \n\r my_mod09 \n ) \n. "
    %% "-module(m)."
    %% "-module(mY_MOD09)."
    %% "-module('!@#$%')."
    %% "-'module'(m).
    %% Don't allow $/, $" and $. in name (used in FS path).
    Rule = <<"\\s*-\\s*'?module'?\\s*\\(\\s*(([a-z][0-9a-z_A-Z]*)|'([^'/.\"\\\\]+)')\\s*\\)\\s*\\.">>,
    case re:run(Body, Rule, [multiline, {capture, [2, 3], binary}]) of
        {match, [<<>>, Mod]} ->
            {ok, Mod};
        {match, [Mod, <<>>]} ->
            {ok, Mod};
        nomatch ->
            error
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

find_module_name_test() ->
    ?assertEqual({ok, <<"m">>}, find_module_name(<<"-module(m).">>)),
    ?assertEqual({ok, <<"m">>}, find_module_name(<<"\t \n-module\t\n(\t\nm\t\n)\t\n.">>)),
    ?assertEqual({ok, <<"mY_MOD09">>}, find_module_name(<<"-module(mY_MOD09).">>)),
    ?assertEqual({ok, <<"!@#$%">>}, find_module_name(<<"-module('!@#$%').">>)),
    ?assertEqual({ok, <<"m">>}, find_module_name(<<"-'module'(m).">>)),
    ?assertEqual(error, find_module_name(<<>>)),
    ?assertEqual(error, find_module_name(<<"m">>)),
    ?assertEqual(error, find_module_name(<<"-module('/').">>)),
    ?assertEqual(error, find_module_name(<<"-module('\\wasd').">>)).

-endif.
