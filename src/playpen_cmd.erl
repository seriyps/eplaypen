%%% @author Sergey Prokhorov <me@seriyps.ru>
%%% @copyright (C) 2015, Sergey Prokhorov
%%% @doc
%%%
%%% @end
%%% Created : 20 Jan 2015 by Sergey Prokhorov <me@seriyps.ru>

-module(playpen_cmd).

-export([cmd/4]).

-export_type([playpen_opts/0, io_opts/0]).

-include_lib("hut/include/hut.hrl").
-define(MAX_TIME_S, 600).

-type playpen_opts() ::
        #{executable => file:filename(),
          sudo => boolean(),
          image => unicode:chardata(),
          mount => [{From :: file:filename(), To :: file:filename(), Mode :: ro | rw}],
          timeout => non_neg_integer(),
          uniq_id => unicode:chardata(), % should be shell-safe! no validation done
          memory_limit => pos_integer(),
          cpu_limit => pos_integer()}.

-type io_opts() ::
        #{max_output_size => pos_integer(),
          timeout => non_neg_integer() | infinity,
          collect_output => boolean(),
          output_callback_state => term(),
          output_callback =>
              fun( (Chunk :: iodata(), State :: term()) -> State :: term() )}.

-record(loop_st,
        {port :: port(),
         size = 0 :: non_neg_integer(),
         acc = [] :: [],
         cb_state :: any(),
         finish_before :: non_neg_integer()}).



-spec cmd([string() | binary(), ...], iodata(), playpen_opts(), io_opts()) ->
                 {ok, {Code, Output, CallbackState}}
                 | {error, {output_too_large | timeout, Output, CallbackState}}
                     when
      Code :: non_neg_integer(),
      Output :: iodata(),
      CallbackState :: term().
cmd([_Callable | _Argv] = Cmd, Stdin, PPOpts0, IOOpts) ->
    {Id, PPOpts} = case PPOpts0 of
                       #{uniq_id := Id_} ->
                           {Id_, PPOpts0};
                       _ ->
                           Id_ = "playpen_" ++ integer_to_list(erlang:unique_integer([positive])),
                           {Id_, PPOpts0#{uniq_id => Id_}}
                   end,
    [Callable1 | Argv1] = build_playpen_argv(Cmd, PPOpts),
    ?log(info, "<~p> | ~p ~p", [iolist_size(Stdin), Callable1, Argv1]),
    Port = erlang:open_port(
             {spawn_executable, Callable1},
             [{args, Argv1},
              binary, exit_status, use_stdio, stderr_to_stdout, hide]),
    case Stdin of
        [] -> ok;
        _ ->
            Port ! {self(), {command, Stdin}}
    end,
    StartedAt = erlang:monotonic_time(second),
    FinishBefore = StartedAt + maps:get(timeout, PPOpts0, ?MAX_TIME_S),
    Response = port_loop(Port, IOOpts, FinishBefore),
    case Response of
        {error, ErrReason} ->
            true = erlang:port_close(Port),
            StopRes = os:cmd("docker stop -t 2 " ++ Id),
            ?log(debug, "Force-close container: reason=~p stop=~p",
                 [ErrReason, StopRes]);
        _ ->
            ok
    end,

    %% This flush should loop to read all extra messages
    receive {Port, Payload} ->
            ?log(warning, "Extra port data flushed ~p", [Payload])
    after 0 -> ok
    end,
    Response.

build_playpen_argv(Cmd, #{image := Image} = Opts) ->
    Executable = case maps:find(executable, Opts) of
                     {ok, P} -> P;
                     error ->
                         %% may return false!
                         os:find_executable("docker")
                 end,
    Cmd1 = ["-i",
            "--rm",
            "--pids-limit", "32",
            "--net", "none",
            "--cap-drop=ALL",
            "--security-opt=no-new-privileges",
            Image | Cmd],
    PPOpts = maps:without([executable, sudo, image, timeout], Opts),
    Cmd2 = [Executable, "run" | add_playpen_opts(Cmd1, lists:sort(maps:to_list(PPOpts)))],
    Cmd3 = case maps:find(timeout, Opts) of
               {ok, Timeout} ->
                   TimeoutUtil = os:find_executable("timeout"),
                   [TimeoutUtil, "-k", integer_to_list(Timeout + 5),
                    integer_to_list(Timeout) | Cmd2];
               error -> Cmd2
           end,
    case maps:find(sudo, Opts) of
        {ok, true} ->
            Sudo = os:find_executable("sudo"),
            [Sudo | Cmd3];
        _ ->
            Cmd3
    end.
    %% Cmd.

add_playpen_opts(Cmd, []) ->
    Cmd;
add_playpen_opts(Cmd, [{mount, Mounts} | Opts]) ->
    MountsArg =
        lists:foldl(
          fun({From ,To, RW}, Acc) ->
                  RWs = case RW of
                            ro -> "ro";
                            rw -> "rw"
                        end,
                  V = lists:join(":", [From, To, RWs]),
                  ["--volume", unicode:characters_to_binary(V) | Acc]
          end, [], Mounts),
    MountsArg ++  add_playpen_opts(Cmd, Opts);
add_playpen_opts(Cmd, [{memory_limit, V} | Opts]) ->
    %% Swap is not allowed!
    StrV = integer_to_list(V) ++ "m",
    ["--memory-swap", StrV,
     "--memory", StrV | add_playpen_opts(Cmd, Opts)];
add_playpen_opts(Cmd, [{cpu_limit, N} | Opts]) ->
    ["--cpus", integer_to_list(N) | add_playpen_opts(Cmd, Opts)];
add_playpen_opts(Cmd, [{uniq_id, Id} | Opts]) ->
    ["--name", Id | add_playpen_opts(Cmd, Opts)].

port_loop(Port, Opts, FinishBefore) ->
    CallbackState = maps:get(output_callback_state, Opts, undefined),
    port_loop(Opts, #loop_st{port = Port,
                             cb_state = CallbackState,
                             finish_before = FinishBefore}).

port_loop(#{max_output_size := Max}, #loop_st{size = Size,
                                              acc = Acc,
                                              cb_state = CbState}) when Size > Max ->
    ?log(info, "Too large output. max=~p; size=~p", [Max, Size]),
    {error, {output_too_large, lists:reverse(Acc), CbState}};
port_loop(Opts, #loop_st{acc = Acc, cb_state = CbState, finish_before = FinishBefore} = St) ->
    IoTimeout = maps:get(timeout, Opts, ?MAX_TIME_S * 1000),
    Now = erlang:monotonic_time(second),
    case FinishBefore - Now of
        Remaining when Remaining =< 0 ->
            ?log(info, "Execution timeout; finish_before=~p; now=~p",
                 [FinishBefore, Now]),
            {error, {timeout, lists:reverse(Acc), CbState}};
        Remaining ->
            Timeout = erlang:min(Remaining * 1000, IoTimeout),
            port_recv_loop(Opts, Timeout, St)
    end.

port_recv_loop(Opts, Timeout, #loop_st{port = Port, size = Size, acc = Acc, cb_state = CbState} = St) ->
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
            port_loop(Opts, St#loop_st{size = Size + byte_size(Data),
                                       acc = Acc1,
                                       cb_state = CbState1});
        {Port, {exit_status, Status}} ->
            {ok, {Status, lists:reverse(Acc), CbState}}
    after Timeout ->
            ?log(info, "Data read timeout=~p", [Timeout]),
            {error, {timeout, lists:reverse(Acc), CbState}}
    end.


-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

build_argv_simple_test() ->
    Opts = #{image => "ubuntu",
             executable => "docker"},
    ?assertEqual(["docker", "run",
                  "-i",
                  "--rm",
                  "--pids-limit", "32",
                  "--net", "none",
                  "--cap-drop=ALL",
                  "--security-opt=no-new-privileges",
                  "ubuntu", "cat"],
                 build_playpen_argv(["cat"], Opts)).

build_argv_full_test() ->
    Opts = #{image => "ubuntu",
             executable => "docker",
             uniq_id => "my-uniq-id",
             timeout => 10,
             memory_limit => 100,
             cpu_limit => 2,
             mount => [{"/tmp", "/mnt", rw}],
             sudo => true
            },
    ?assertEqual(["/usr/bin/sudo",
                  "/usr/bin/timeout", "-k", "15", "10",
                  "docker", "run",
                  "--cpus", "2",
                  "--memory-swap", "100m", "--memory", "100m",
                  "--volume", <<"/tmp:/mnt:rw">>,
                  "--name", "my-uniq-id",
                  "-i",
                  "--rm",
                  "--pids-limit", "32",
                  "--net", "none",
                  "--cap-drop=ALL",
                  "--security-opt=no-new-privileges",
                  "ubuntu", "cat"],
                 build_playpen_argv(["cat"], Opts)).

-endif.
