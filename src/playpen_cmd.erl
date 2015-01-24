%%% @author Sergey Prokhorov <me@seriyps.ru>
%%% @copyright (C) 2015, Sergey Prokhorov
%%% @doc
%%%
%%% @end
%%% Created : 20 Jan 2015 by Sergey Prokhorov <me@seriyps.ru>

-module(playpen_cmd).

-export([cmd/4]).

-export_type([playpen_opts/0, io_opts/0]).

-type playpen_opts() ::
        #{root => file:filename(),
          mount_proc => boolean(),
          mount_dev => boolean(),
          bind => boolean(),
          bind_rw => boolean(),
          user => iodata(),
          hostname => iodata(),
          timeout => non_neg_integer(),
          memory_limit => pos_integer(),
          devices => [{file:filename(), [r | w | m]}],
          syscalls => [binary()],
          syscalls_file => file:filename()}.

-type io_opts() ::
        #{max_output_size => pos_integer(),
          timeout => non_neg_integer() | infinity,
          collect_output => boolean(),
          output_callback_state => term(),
          output_callback =>
              fun( (Chunk :: iodata(), State :: term()) -> State :: term() )}.



-spec cmd([string() | binary()], iodata(), playpen_opts(), io_opts()) ->
                 {ok, {Code, Output, CallbackState}}
                 | {error, {output_too_large | timeout, Output, CallbackState}}
                     when
      Code :: non_neg_integer(),
      Output :: iodata(),
      CallbackState :: term().
cmd([_Callable | _Argv] = Cmd, Stdin, PPOpts, IOOpts) ->
    [Callable1 | Argv1] = build_playpen_argv(Cmd, PPOpts),
    lager:debug("<~p> | ~p ~p", [iolist_size(Stdin), Callable1, Argv1]),
    Port = erlang:open_port(
             {spawn_executable, Callable1},
             [{args, Argv1},
              binary, exit_status, use_stdio, stderr_to_stdout, hide]),
    case Stdin of
        [] -> ok;
        _ ->
            Port ! {self(), {command, Stdin}}
    end,
    Response = port_loop(Port, IOOpts),
    %% This flush should loop to read all extra messages
    receive {Port, Payload} ->
            lager:warning("Extra port data flushed ~p", [Payload])
    after 0 -> ok
    end,
    Response.

%% TODO prepend `playpen <opts> --`
build_playpen_argv(Cmd, _Opts) ->
    Cmd.

port_loop(Port, Opts) ->
    CallbackState = maps_get(output_callback_state, Opts, undefined),
    port_loop(Port, Opts, {0, [], CallbackState}).

port_loop(Port, #{max_output_size := Max}, {Size, Acc, CbState}) when Size > Max ->
    lager:info("Too large output. Max: ~p, Size: ~p", [Max, Size]),
    erlang:port_close(Port),
    {error, {output_too_large, lists:reverse(Acc), CbState}};
port_loop(Port, Opts, {Size, Acc, CbState}) ->
    Timeout = maps_get(timeout, Opts, infinity),
    receive
        {Port, {data, Data}} ->
            CbState1 =
                case Opts of
                    #{output_callback := Cb} ->
                        Cb(Data, CbState);
                    _ ->
                        CbState
                end,
            Acc1 =
                case Opts of
                    #{collect_output := true} ->
                        [Data | Acc];
                    _ ->
                        Acc
                end,
            port_loop(Port, Opts, {Size + byte_size(Data), Acc1, CbState1});
        {Port, {exit_status, Status}} ->
            {ok, {Status, lists:reverse(Acc), CbState}}
    after Timeout ->
            lager:info("Port timeout. Timeout: ~p, Size: ~p", [Timeout, Size]),
            erlang:port_close(Port),
            {error, {timeout, lists:reverse(Acc), CbState}}
    end.


maps_get(Key, Map, Default) ->
    case maps:find(Key, Map) of
        error ->
            Default;
        {ok, Val} ->
            Val
    end.
