%%% @author Sergey Prokhorov <me@seriyps.ru>
%%% @copyright (C) 2015, Sergey Prokhorov
%%% @doc
%%%
%%% @end
%%% Created : 26 Jan 2015 by Sergey Prokhorov <me@seriyps.ru>

-module(playpen).
-behaviour(application).

-export([start/0, cowboy_reload_routes/0]). %, reload_releases/0]).
-export([available_outputs/0, available_releases/0]).
%% Application callbacks
-export([start/2, stop/1]).

-define(APP, eplaypen).


%% API

start() ->
    {ok, _} = application:ensure_all_started(?APP),
    %% reload_releases(),
    start_cowboy().

start_cowboy() ->
    Port = 8080,
    Ip = {127, 0, 0, 1},
    Concurrency = 10,
	Dispatch = cowboy_router:compile(cowboy_routes()),
	{ok, _} = cowboy:start_clear(
                http, [{port, Port}, {ip, Ip}, {max_connections, Concurrency}],
                #{env => #{dispatch => Dispatch}}),
    ok.

cowboy_reload_routes() ->
    Dispatch = cowboy_routes(),
    cowboy:set_env(http, dispatch, cowboy_router:compile(Dispatch)).

%% reload_releases() ->
%%     Releases = read_releases(),
%%     application:set_env(?APP, releases, Releases).

%% some info
available_outputs() ->
    [<<"beam">>, % erl_lint                                 | erlc mod.erl
     <<"beam_info">>, % beam_lib:chunks(...)
     <<"tokens">>, % erl_scan:string(...)
     <<"P">>,    % compile:forms(.., ['P'])                 | erlc -P mod.erl
     <<"abstr">>, % compile:forms(.., [dabstr])             | erlc +dabstr mod.erl
     <<"E">>,    % compile:forms(.., ['E'])                 | erlc -E mod.erl
     <<"S">>,    % compile:forms(.., ['S'])                 | erlc -S mod.erl
     <<"dis">>,  % compile:forms(.., []), erts_debug:df(..) | erlc mod.erl && erl -eval 'erts_debug:df(mod).'
     <<"kernel">>, % compile:forms(.., [dkernel])           | erlc +dkernel mod.erl
     <<"ssa">>,  % compile:forms(.., [dssa])                | erlc +dssa mod.erl
     <<"core">>, % compile:forms(.., [to_core])             | erlc +to_core mod.erl
     <<"asmdump">> % erl +asmdump
    ].

-spec available_releases() -> #{Name :: binary() => Image :: string()}.
available_releases() ->
    {ok, Releases} = application:get_env(?APP, releases),
    Releases.


%% applicaion callback
start(_, _) ->
    Res = playpen_sup:start_link(),
    start_cowboy(),
    Res.

stop(_) ->
    ok.

%% Internals

%% read_releases() ->
%%     ReleasesListPath = filename:join([code:priv_dir(?APP), "RELEASES.txt"]),
%%     {ok, ReleasesBin} = file:read_file(ReleasesListPath),
%%     [L1 | _] = binary:split(ReleasesBin, [<<"\n">>, <<"\r">>]),
%%     lists:reverse(binary:split(L1, <<" ">>, [global, trim])).

cowboy_routes() ->
    [{'_',
      [
       {"/api/evaluate", playpen_web_evaluate, [evaluate]},
       {"/api/compile", playpen_web_evaluate, [compile]},
       {"/api/pastebin/[:paste_id]", playpen_web_pastebin, []},
       %% this 2 is for development, on prod Nginx should be used
       {"/", cowboy_static, {priv_file, ?APP, "htdocs/index.html"}},
       {"/assets/[...]", cowboy_static, {priv_dir, ?APP, "htdocs/assets"}}
      ]}].
