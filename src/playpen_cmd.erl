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
        #{executable => file:filename(),
          root => file:filename(),              % required
          mount_proc => boolean(),
          mount_dev => boolean(),
          bind => [file:filename()],
          bind_rw => [file:filename()],
          user => iodata(),
          hostname => iodata(),
          timeout => non_neg_integer(),
          memory_limit => pos_integer(),
          devices => [{file:filename(), [r | w | m]}],
          syscalls => [string()],
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

build_playpen_argv(Cmd, #{root := Root} = Opts) ->
    Executable = case maps:find(executable, Opts) of
                     {ok, P} -> P;
                     error ->
                         %% may return false!
                         os:find_executable("playpen")
                 end,
    Cmd1 = [Root, "--" | Cmd],
    PPOpts = maps:without([executable, root], Opts),
    Cmd2 = add_playpen_opts(Cmd1, maps:to_list(PPOpts)),
    [Executable | Cmd2].
    %% Cmd.

add_playpen_opts(Cmd, []) ->
    Cmd;
add_playpen_opts(Cmd, [{mount_proc, true} | Opts]) ->
    ["--mount-proc" | add_playpen_opts(Cmd, Opts)];
add_playpen_opts(Cmd, [{mount_dev, true} | Opts]) ->
    ["--mount-dev" | add_playpen_opts(Cmd, Opts)];
add_playpen_opts(Cmd, [{bind, V} | Opts]) ->
    Flags = ["-b" || _ <- V],
    WithFlags = lists:zip(Flags, V),
    WithFlags ++ add_playpen_opts(Cmd, Opts);
add_playpen_opts(Cmd, [{bind_rw, V} | Opts]) ->
    Flags = ["-B" || _ <- V],
    WithFlags = lists:zip(Flags, V),
    WithFlags ++ add_playpen_opts(Cmd, Opts);
add_playpen_opts(Cmd, [{user, V} | Opts]) ->
    ["--user", V | add_playpen_opts(Cmd, Opts)];
add_playpen_opts(Cmd, [{hostname, V} | Opts]) ->
    ["--hostname", V | add_playpen_opts(Cmd, Opts)];
add_playpen_opts(Cmd, [{timeout, V} | Opts]) ->
    ["--timeout", integer_to_list(V) | add_playpen_opts(Cmd, Opts)];
add_playpen_opts(Cmd, [{memory_limit, V} | Opts]) ->
    ["--memory-limit", integer_to_list(V) | add_playpen_opts(Cmd, Opts)];
add_playpen_opts(Cmd, [{devices, V} | Opts]) ->
    WithAttrs = [begin
                     SAccess = lists:map(fun erlang:atom_to_list/1, Access),
                     AccessStr = lists:flatten(SAccess),
                     lists:flatten([Dev, $\:, AccessStr])
                 end || {Dev, Access} <- V],
    Csv = string:join(WithAttrs, ","),
    ["--devices", Csv | add_playpen_opts(Cmd, Opts)];
add_playpen_opts(Cmd, [{syscalls, V} | Opts]) ->
    Csv = string:join(V, ","),
    ["--syscalls", Csv | add_playpen_opts(Cmd, Opts)];
add_playpen_opts(Cmd, [{syscalls_file, V} | Opts]) ->
    ["--syscalls-file", V | add_playpen_opts(Cmd, Opts)];
add_playpen_opts(Cmd, [{K, false} | Opts]) when (K == mount_proc) orelse (K == mount_dev) ->
    add_playpen_opts(Cmd, Opts).


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
