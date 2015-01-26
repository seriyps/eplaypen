%%% @author Sergey Prokhorov <me@seriyps.ru>
%%% @copyright (C) 2015, Sergey Prokhorov
%%% @doc
%%%
%%% @end
%%% Created : 26 Jan 2015 by Sergey Prokhorov <me@seriyps.ru>

-module(playpen_sup).
-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    %% Postgres pool params
    {ok, DbPoolOpts} = application:get_env(db_pool),
    DbPoolName = playpen_db,
    DbPoolArgs = [{name, {local, DbPoolName}},
                  {worker_module, DbPoolName},
                  {size, proplists:get_value(size, DbPoolOpts, 2)},
                  {max_overflow, proplists:get_value(max_overflow, DbPoolOpts, 20)}],
    {ok, DbWorkerArgs} = application:get_env(db),

    Childs = [
              poolboy:child_spec(DbPoolName, DbPoolArgs, DbWorkerArgs)
             ],
    {ok, { {one_for_one, 5, 10}, Childs} }.

