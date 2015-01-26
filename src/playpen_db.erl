%%% @author Sergey Prokhorov <me@seriyps.ru>
%%% @copyright (C) 2015, Sergey Prokhorov
%%% @doc
%%%
%%% @end
%%% Created : 27 Jan 2015 by Sergey Prokhorov <me@seriyps.ru>

-module(playpen_db).
-behaviour(poolboy_worker).

-export([start_link/1]).


start_link([Host, Username, Password, Opts]) ->
    pgsql:connect(Host, Username, Password, Opts).
