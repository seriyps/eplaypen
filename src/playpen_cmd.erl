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

-type playpen_opts() ::
        #{executable => file:filename(),
          sudo => boolean(),
          image => unicode:chardata(),
          mount => [{From :: file:filename(), To :: file:filename(), Mode :: ro | rw}],
          timeout => non_neg_integer(),
          memory_limit => pos_integer(),
          cpu_limit => pos_integer()}.

-type io_opts() ::
        #{max_output_size => pos_integer(),
          timeout => non_neg_integer() | infinity,
          collect_output => boolean(),
          output_callback_state => term(),
          output_callback =>
              fun( (Chunk :: iodata(), State :: term()) -> State :: term() )}.



-spec cmd([string() | binary(), ...], iodata(), playpen_opts(), io_opts()) ->
                 {ok, {Code, Output, CallbackState}}
                 | {error, {output_too_large | timeout, Output, CallbackState}}
                     when
      Code :: non_neg_integer(),
      Output :: iodata(),
      CallbackState :: term().
cmd([_Callable | _Argv] = Cmd, Stdin, PPOpts, IOOpts) ->
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
    Response = port_loop(Port, IOOpts),
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
    Cmd1 = ["-i", Image | Cmd],
    PPOpts = maps:without([executable, sudo, image, timeout], Opts),
    Cmd2 = [Executable, "run" | add_playpen_opts(Cmd1, lists:sort(maps:to_list(PPOpts)))],
    Cmd3 = case maps:find(timeout, Opts) of
               {ok, Timeout} ->
                   TimeoutUtil = os:find_executable("timeout"),
                   [TimeoutUtil, "-k", integer_to_list(Timeout + 2),
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
    ["--cpus", integer_to_list(N) | add_playpen_opts(Cmd, Opts)].

port_loop(Port, Opts) ->
    CallbackState = maps:get(output_callback_state, Opts, undefined),
    port_loop(Port, Opts, {0, [], CallbackState}).

port_loop(Port, #{max_output_size := Max}, {Size, Acc, CbState}) when Size > Max ->
    ?log(info, "Too large output. Max: ~p, Size: ~p", [Max, Size]),
    erlang:port_close(Port),
    {error, {output_too_large, lists:reverse(Acc), CbState}};
port_loop(Port, Opts, {Size, Acc, CbState}) ->
    Timeout = maps:get(timeout, Opts, infinity),
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
            ?log(info, "Port timeout. Timeout: ~p, Size: ~p", [Timeout, Size]),
            erlang:port_close(Port),
            {error, {timeout, lists:reverse(Acc), CbState}}
    end.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

build_argv_simple_test() ->
    Opts = #{image => "ubuntu",
             executable => "docker"},
    ?assertEqual(["docker", "run", "-i", "ubuntu", "cat"],
                 build_playpen_argv(["cat"], Opts)).

build_argv_full_test() ->
    Opts = #{image => "ubuntu",
             executable => "docker",
             timeout => 10,
             memory_limit => 100,
             cpu_limit => 2,
             mount => [{"/tmp", "/mnt", rw}],
             sudo => true
            },
    ?assertEqual(["/usr/bin/sudo",
                  "/usr/bin/timeout", "-k", "12", "10",
                  "docker", "run",
                  "--cpus", "2",
                  "--memory-swap", "100m", "--memory", "100m",
                  "--volume", <<"/tmp:/mnt:rw">>,
                  "-i", "ubuntu", "cat"],
                 build_playpen_argv(["cat"], Opts)).

-endif.
