%%% @author Sergey Prokhorov <me@seriyps.ru>
%%% @copyright (C) 2015, Sergey Prokhorov
%%% @doc
%%%
%%% @end
%%% Created : 26 Jan 2015 by Sergey Prokhorov <me@seriyps.ru>

-module(playpen_web_pastebin).
-behaviour(cowboy_rest).

-export([init/2]).
-export([allowed_methods/2, content_types_accepted/2, content_types_provided/2,
        resource_exists/2]).
-export([to_qs/2, to_json/2,
         from_qs/2, from_json/2]).
-export([encrypt/2, hex/1]).

-type paste() ::
        #{pk => non_neg_integer(),
          code => binary(),                     % required
          release => binary(),                  % required
          emit => binary(),                     % required
          expires => calendar:datetime() | null,
          created => calendar:datetime()}.

-define(KEY, begin
                 {ok, K} = application:get_env(eplaypen, pastebin_id_key),
                 K
             end).
-define(BODY_LIMIT, 102400).
-define(DB_POOL, playpen_db).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.


allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"HEAD">>, <<"OPTIONS">>], Req, State}.

content_types_accepted(Req, State) ->
    CT = [{{<<"application">>, <<"x-www-form-urlencoded">>, '*'}, from_qs},
          {{<<"application">>, <<"json">>, '*'}, from_json}],
    {CT, Req, State}.

content_types_provided(Req, State) ->
    CT = [{{<<"application">>, <<"x-www-form-urlencoded">>, []}, to_qs},
          {{<<"application">>, <<"json">>, []}, to_json},
          {{<<"text">>, <<"plain">>, '*'}, to_text}],
    {CT, Req, State}.

resource_exists(Req, State) ->
    case cowboy_req:binding(paste_id, Req) of
		undefined ->
			{false, Req, State};
		PasteID ->
            Decrypted = decrypt(unhex(PasteID), ?KEY),
			case get_paste(Decrypted) of
				{ok, Paste} ->
                    {true, Req, Paste};
				{error, not_found} ->
                    {false, Req, PasteID};
                {error, _} ->
                    {halt, Req, State}
			end
	end.


to_qs(Req, #{code := Code, release := Release, emit := Emit, pk := Pk} = State) ->
    {cow_qs:qs([{<<"code">>, Code},
                {<<"release">>, Release},
                {<<"emit">>, Emit},
                {<<"pk">>, hex(encrypt(Pk, ?KEY))}]),
     Req, State}.

to_json(Req, #{code := Code, release := Release, emit := Emit, pk := Pk} = State) ->
    {jiffy:encode(#{<<"code">> => Code,
                    <<"release">> => Release,
                    <<"emit">> => Emit,
                    <<"pk">> => hex(encrypt(Pk, ?KEY))}),
     Req, State}.


from_qs(Req, State) ->
    {ok, KV, Req1} = cowboy_req:read_urlencoded_body(Req, #{length => ?BODY_LIMIT}),
    handle_kv(KV, Req1, State).

from_json(Req, State) ->
    case cowboy_req:read_body(Req, #{length => ?BODY_LIMIT}) of
        {ok, Data, Req1} ->
            try jiffy:decode(Data) of
                {KV} ->
                    handle_kv(KV, Req1, State)
            catch throw:_ ->
                    Req2 = cowboy_req:reply(400, #{}, "Invalid JSON", Req1),
                    {ok, Req2, State}
            end;
        {more, _, Req1} ->
            Req2 = cowboy_req:reply(400, #{}, "Request body too long", Req1),
            {ok, Req2, State}
    end.

handle_kv(KV, Req, State) ->
    Release = proplists:get_value(<<"release">>, KV),
    Format = proplists:get_value(<<"emit">>, KV),
    case {lists:member(Format, playpen:available_outputs()),
          maps:is_key(Release, playpen:available_releases())} of
        {true, true} ->
            {ok, Pk} = create_paste(
                         #{code => proplists:get_value(<<"code">>, KV),
                           release => Release,
                           emit => Format,
                           expires => null}),
            {{true, <<"/api/pastebin/", (hex(encrypt(Pk, ?KEY)))/binary>>}, Req, State};
        {_, _} ->
            Req2 = cowboy_req:reply(
                     400, #{},
                     io_lib:format("Invalid emit ~p or release ~p",
                                   [Format, Release]), Req),
            {halt, Req2, State}
    end.


%% internals

-spec get_paste(non_neg_integer()) -> {ok, paste()} | {error, not_found | any()}.
get_paste(Id) ->
    %% {ok, #{pk => Id, release => <<"17.4">>, emit => <<"beam">>, code => <<"Hello world">>}}.
    case poolboy:transaction(
           ?DB_POOL,
           fun(C) ->
                   Q = "SELECT release, emit, code, created FROM pastebin WHERE id=$1",
                   epgsql:equery(C, Q, [Id])
           end) of
        {ok, _, [{Release, Emit, Code, Created}]} ->
            {ok, #{pk => Id,
                   release => Release,
                   emit => Emit,
                   code => Code,
                   created => Created}};
        {ok, _, []} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

-spec create_paste(paste()) -> {ok, non_neg_integer()} | {error, any()}.
create_paste(#{code := Code, release := Release, emit := Emit, expires := Expires}) ->
    %% {ok, erlang:phash2(Code)}.
    case poolboy:transaction(
           ?DB_POOL,
           fun(C) ->
                   Q = "INSERT INTO pastebin "
                       "(release, emit, code, expires, created) "
                       "VALUES ($1, $2, $3, $4, now()) RETURNING id",
                   epgsql:equery(C, Q, [Release, Emit, Code, Expires])
           end) of
        {ok, 1, _, [{Id}]} ->
            {ok, Id};
        {error, Reason} ->
            {error, Reason}
    end.


encrypt(Pk, Key) ->
    St = crypto:stream_init(rc4, Key),
    PkBin = case Pk of
                _ when Pk =< 16#FFFF ->   <<Pk:16/little>>; % 65535
                _ when Pk =< 16#FFFFFF -> <<Pk:24/little>>; % 16777215
                _ ->                      <<Pk:32/little>>
            end,
    {_, Encrypted} = crypto:stream_encrypt(St, PkBin),
    %% lager:info("Orig: ~w, Encrypted: ~w, ~w, ~w",
    %%            [PkBin, Encrypted, Pk, begin <<D:16/little>> = Encrypted, D end]),
    Encrypted.

decrypt(Encrypted, Key) ->
    St = crypto:stream_init(rc4, Key),
    {_, Decrypted} = crypto:stream_decrypt(St, Encrypted),
    case Decrypted of
        <<Pk:32/little>> -> Pk;
        <<Pk:24/little>> -> Pk;
        <<Pk:16/little>> -> Pk
    end.


hex(Bin) ->
    HChar = fun(N) when N < 10 -> $0 + N;
               (N) when N < 16 -> $W + N
            end,
    <<<<(HChar(H)), (HChar(L))>> || <<H:4, L:4>> <= Bin>>.

unhex(Chars) ->
    UnHChar = fun(C) when C < $W -> C - $0;
                 (C) when C > $W -> C - $W
              end,
    << <<(UnHChar(C)):4>> || <<C>> <= Chars>>.
