-module(playpen).

%% playpen: playpen library's entry point.

-export([start/0]).

-define(APP, eplaypen).

%% API

start() ->
    lager:start(),
    {ok, _} = application:ensure_all_started(?APP),
    Releases = read_releases(),
    application:set_env(?APP, releases, Releases),
    Port = 8080,
    Ip = {127, 0, 0, 1},
	Dispatch = cowboy_router:compile(cowboy_routes()),
	{ok, _} = cowboy:start_http(http, 100, [{port, Port}, {ip, Ip}],
                                [{env, [{dispatch, Dispatch}]}]),
    ok.

%% Internals

read_releases() ->
    ReleasesListPath = filename:join([code:priv_dir(?APP), "RELEASES.txt"]),
    {ok, ReleasesBin} = file:read_file(ReleasesListPath),
    [L1 | _] = binary:split(ReleasesBin, [<<"\n">>, <<"\r">>]),
    lists:reverse(binary:split(L1, <<" ">>, [global, trim])).

cowboy_routes() ->
    [{'_',
      [
       {"/api/evaluate", playpen_web_evaluate, [evaluate]},
       {"/api/compile", playpen_web_evaluate, [compile]},
       {"/api/pastebin", playpen_web_pastebin, []},
       %% this 2 is for development, on prod Nginx should be used
       {"/", cowboy_static, {priv_file, ?APP, "htdocs/index.html"}},
       {"/assets/[...]", cowboy_static, {priv_dir, ?APP, "htdocs/assets"}}
      ]}].
