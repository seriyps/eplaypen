#!/usr/bin/env escript

%% https://blog.stenmans.org/theBeamBook/#BEAM_files
%% $OTP$/lib/compiler/src/beam_dict.erl
%% $OTP$/lib/compiler/src/beam_asm.erl
%% $OTP$/lib/compiler/src/beam_disasm.erl

-module(beam_chunks).
-export([main/1]).
-mode(compile).

main([ModStr]) ->
    Mod = list_to_atom(ModStr),
    c:m(Mod),
    io:format("~n~n===~s.beam chunks===~n~n", [Mod]),
    DebugInfo = case erlang:system_info(otp_release) > "19" of
                      true -> debug_info;
                      _ -> "Dbgi"
                end,
    ChunkNames = [attributes, compile_info, exports, labeled_exports,
                  imports, indexed_imports , locals, labeled_locals,
                  abstract_code, DebugInfo,
                  atoms, "LitT", "StrT", "FunT", "Line", "Code"],
    {ok, {_, Chunks0}} =
        beam_lib:chunks(code:which(Mod), ChunkNames, [allow_missing_chunks]),
    Chunks = lists:map(fun decode_chunk/1, Chunks0),
    lists:foreach(fun({Name, Val}) ->
                          io:format("~s:~n~120p~n~n", [Name, Val])
                  end, Chunks).
%% lines: {file, seq_no(), name_index()}
%%        {line, fname_index(), line()}

decode_chunk({"LitT", <<_UncompressedSize:32, Compressed/binary>>}) ->
    <<_NumLiterals:32, Table/binary>> = zlib:uncompress(Compressed),
    Literals = parse_literals(Table),
    {"LitT", Literals};
decode_chunk({"Line", <<Ver:32, Bits:32, NumLineInstrs:32,
                        NumLines:32, NumFnames:32,
                        LinesFnames/binary>>}) ->
    {Lines, Fnames} = decode_lineinfo(LinesFnames, 0, [], NumLines),
    FnamesL = [Name || <<Size:16, Name:Size/binary>> <= Fnames],
    (NumFnames == length(FnamesL)) orelse error({fnames, NumFnames, FnamesL}),
    {"Line", [{version, Ver},
              {bits, Bits},
              {num_line_instructions, NumLineInstrs},
              {lines, Lines},
              {file_names, FnamesL}]};
decode_chunk({"Code", <<HdrSize:32/integer, Hdr:HdrSize/binary, OpCodes/binary>>}) ->
    <<Instructionset:32/integer,
      OpcodeMax:32/integer,
      NumberOfLabels:32/integer,
      NumberOfFunctions:32/integer>> = Hdr,
    {"Code",
     [{instructionset, Instructionset},
      {opcodemax, OpcodeMax},
      {numberoflabels, NumberOfLabels},
      {numberofFunctions, NumberOfFunctions},
      {op_codes_bin, OpCodes},
      {op_codes_disasm, disasm(OpCodes)}]
     };
decode_chunk({"StrT", ConcatenatedStrings}) ->
    {"StrT", binary_to_list(ConcatenatedStrings)};
decode_chunk({"FunT", <<NumLambdas:32/integer, Lambdas/binary>>}) ->
    %% beam_dict:lambda_table/1
    LambdasL =
        [[{f, F},
          {a, A},
          {lbl, Lbl},
          {idx, Index},
          {num_free, NumFree},
          {old_uniq, OldUniq}]
         || <<F:32,A:32,Lbl:32,Index:32,NumFree:32,OldUniq:32>> <= Lambdas],
    (NumLambdas == length(LambdasL)) orelse error({lambdas, NumLambdas, LambdasL}),
    {"FunT", LambdasL};
decode_chunk(Chunk) ->
    Chunk.


%% LitT
parse_literals(<<Size:32, Literal:Size/binary, Tail/binary>>) ->
    [binary_to_term(Literal) | parse_literals(Tail)];
parse_literals(<<>>) -> [].


%% Line
decode_lineinfo(Tail, _F, Acc, 0) ->
    {lists:reverse(Acc), Tail};
decode_lineinfo(<<B, Bs/binary>>, F, Acc, LNum) ->
    Tag = decode_tag(B band 2#111),
    {{Tag, Num}, RemBs} = decode_int(Tag, B, Bs),
    case Tag of
        i ->
            decode_lineinfo(RemBs, F, [{line, F, Num} | Acc], LNum - 1);
        a ->
            <<B2, Bs2/binary>> = RemBs,
            Tag2 = decode_tag(B2 band 2#111),
            {{Tag2, Num2}, RemBs2} = decode_int(Tag2, B2, Bs2),
            decode_lineinfo(RemBs2, Num2, [{file, Num, Num2} | Acc], LNum - 1)
    end.


%% OpCodes
disasm(Code) ->
    case disasm_one(Code) of
        {SymbolOp, Args, <<>>} ->
            [{SymbolOp, Args}];
        {SymbolOp, Args, Code1} ->
            [{SymbolOp, Args} | disasm(Code1)]
    end.

-spec disasm_one(binary()) -> {atom(), [{arg(), any()}], binary()}.
disasm_one(<<B, Bs0/binary>>) ->
    {SymbolOp, Arity} = beam_opcodes:opname(B),
    case SymbolOp of
        Sel when Sel == select_val;
                 Sel == select_tuple_arity ->
            disasm_select_inst(Sel, Bs0);
        Map when Map == put_map_assoc;
                 Map == put_map_exact;
                 Map == get_map_elements;
                 Map == has_map_fields ->
            disasm_map_inst(Map, Arity, Bs0);
        put_tuple2 ->
            disasm_put_tuple2(Bs0);
        _ ->
            {Args, Bs} = decode_n_args(Arity, Bs0),
            {SymbolOp, Args, Bs}
    end.


disasm_select_inst(Inst, Bs) ->
    {X, Bs1} = decode_arg(Bs),
    {F, Bs2} = decode_arg(Bs1),
    {Z, Bs3} = decode_arg(Bs2),
    {U, Bs4} = decode_arg(Bs3),
    {u, Len} = U,
    {List, RestBs} = decode_n_args(Len, Bs4),
    {Inst, [X, F, {Z, U, List}], RestBs}.


disasm_map_inst(Inst, Arity, Bs0) ->
    {Args0, Bs1} = decode_n_args(Arity, Bs0),
    [Z | Args1]  = lists:reverse(Args0),
    Args = lists:reverse(Args1),
    {U, Bs2} = decode_arg(Bs1),
    {u, Len} = U,
    {List, RestBs} = decode_n_args(Len, Bs2),
    {Inst, Args ++ [{Z, U, List}], RestBs}.


disasm_put_tuple2(Bs) ->
    {X, Bs1} = decode_arg(Bs),
    {Z, Bs2} = decode_arg(Bs1),
    {U, Bs3} = decode_arg(Bs2),
    {u, Len} = U,
    {List, RestBs} = decode_n_args(Len, Bs3),
    {put_tuple2, [X,{Z,U,List}], RestBs}.


-spec decode_n_args(non_neg_integer(), binary()) -> {[{arg(), any()}], binary()}.
decode_n_args(N, Bs) when N >= 0 ->
    decode_n_args(N, [], Bs).

decode_n_args(N, Acc, Bs0) when N > 0 ->
    {A1, Bs} = decode_arg(Bs0),
    decode_n_args(N - 1, [A1 | Acc], Bs);
decode_n_args(0, Acc, Bs) ->
    {lists:reverse(Acc), Bs}.

%% https://blog.stenmans.org/theBeamBook/#SEC-BeamModulesCTE
-spec decode_arg(binary()) -> {{arg(), any()}, binary()}.
decode_arg(<<B, Bs/binary>>) ->
    Tag = decode_tag(B band 2#111),
    case Tag of
        z ->
            decode_z_tagged(Tag, B, Bs);
        _ ->
            %% if Tag == 'a', can do atom table lookup
            decode_int(Tag, B, Bs)
    end.

-spec decode_z_tagged(tag(), byte(), binary()) -> {{arg(), any()}, binary()}.
decode_z_tagged(Tag, B, Bs) when (B band 16#08) =:= 0 ->
    N = B bsr 4,
    case N of
        0 -> % float
            decode_float(Bs);
        1 -> % list
            {{Tag, N}, Bs};
        2 -> % fr - floating point register
            decode_fr(Bs);
        3 -> % allocation list
            decode_alloc_list(Bs);
        4 -> % literal
            %% Can do literal tab lookup here
            {{u, LitIndex}, RestBs} = decode_arg(Bs),
            {{literal, LitIndex}, RestBs};
        _ ->
            exit({decode_z_tagged, {invalid_extended_tag, N}})
    end.

%% Utils
-define(tag_u, 0).                  % small unsigned literal
-define(tag_i, 1).                  % integer / bigint
-define(tag_a, 2).                  % atom
-define(tag_x, 3).                  % x register
-define(tag_y, 4).                  % stack slot
-define(tag_f, 5).                  % pointer to (local?) function / label
-define(tag_h, 6).                  % seems unused; was 'char'
-define(tag_z, 7).                  % flag that this instr is extended

-type tag() :: u | i | a | x | y | f | h | z.
-type arg() :: tag() | float | fr | literal.

-spec decode_tag(byte()) -> tag().
decode_tag(?tag_u) -> u;
decode_tag(?tag_i) -> i;
decode_tag(?tag_a) -> a;
decode_tag(?tag_x) -> x;
decode_tag(?tag_y) -> y;
decode_tag(?tag_f) -> f;
decode_tag(?tag_h) -> h;
decode_tag(?tag_z) -> z.


-spec decode_float(binary()) -> {{float, float()}, binary()}.
decode_float(Bs) ->
    {<<Float:64/float>>, RestBs} = take_bytes(8, Bs),
    {{float, Float}, RestBs}.


-spec decode_fr(binary()) -> {{fr, non_neg_integer()}, binary()}.
decode_fr(Bs) ->
    {{u, Fr}, RestBs} = decode_arg(Bs),
    {{fr, Fr}, RestBs}.


-spec decode_alloc_list(binary()) -> {{u, {alloc, List}}, binary()} when
      List :: [{words, non_neg_integer()} |
               {floats, non_neg_integer()} |
               {literal, non_neg_integer()}].
decode_alloc_list(Bs) ->
    {{u, N}, RestBs} = decode_arg(Bs),
    decode_alloc_list_1(N, RestBs, []).

decode_alloc_list_1(0, RestBs, Acc) ->
    {{u, {alloc, lists:reverse(Acc)}}, RestBs};
decode_alloc_list_1(N, Bs0, Acc) ->
    {{u, Type}, Bs1} = decode_arg(Bs0),
    {{u, Val}, Bs} = decode_arg(Bs1),
    Res = case Type of
              0 -> {words, Val};
              1 -> {floats, Val};
              2 -> {literal, Val}               %can do literal tab lookup here
          end,
    decode_alloc_list_1(N - 1, Bs, [Res | Acc]).


-spec decode_int(tag(), byte(), binary()) -> {{tag(), integer()}, binary()}.
decode_int(Tag, B, Bs) when (B band 16#08) =:= 0 ->
    %% N < 16 = 4 bits, NNNN:0:TTT
    N = B bsr 4,
    {{Tag, N}, Bs};
decode_int(Tag, B, <<>>) when (B band 16#10) =:= 0 ->
    %% N < 2048 = 11 bits = 3:8 bits, NNN:01:TTT, NNNNNNNN
    Val0 = B band 2#11100000,
    N = (Val0 bsl 3),
    {{Tag, N}, <<>>};
decode_int(Tag, B, <<B1, Bs1/binary>>) when (B band 16#10) =:= 0 ->
    %% N < 2048 = 11 bits = 3:8 bits, NNN:01:TTT, NNNNNNNN
    Val0 = B band 2#11100000,
    N = (Val0 bsl 3) bor B1,
    {{Tag, N}, Bs1};
decode_int(Tag, B, Bs) ->
    {Len, Bs1} = decode_int_length(B, Bs),
    {<<F, _/binary>> = IntBs, RemBs} = take_bytes(Len, Bs1),
    N = build_arg(IntBs),
    Num = if F > 127, Tag =:= i ->
                  decode_negative(N,Len);
             true -> N
          end,
    {{Tag, Num}, RemBs}.


-spec decode_int_length(byte(), binary()) -> {non_neg_integer(), binary()}.
decode_int_length(B, Bs) ->
    case B bsr 5 of
        7 ->
            {{u, L}, ArgBs} = decode_arg(Bs),
            {L + 9,ArgBs};
        L ->
            {L + 2, Bs}
    end.


-spec decode_negative(non_neg_integer(), non_neg_integer()) -> neg_integer().
decode_negative(N, Len) ->
    N - (1 bsl (Len * 8)).


-spec take_bytes(non_neg_integer(), binary()) -> {binary(), binary()}.
take_bytes(N, Bs) ->
    <<Bts:N/binary, Bs1/binary>>  = Bs,
    {Bts, Bs1}.

-spec build_arg(binary()) -> integer().
build_arg(Bs) ->
    build_arg(Bs, 0).

build_arg(<<B, Bs/binary>>, N) ->
    build_arg(Bs, (N bsl 8) bor B);
build_arg(<<>>, N) ->
    N.
