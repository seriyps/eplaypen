#!/usr/bin/env escript

%% https://blog.stenmans.org/theBeamBook/#BEAM_files
%% $OTP$/lib/compiler/src/beam_dict.erl
%% $OTP$/lib/compiler/src/beam_asm.erl

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
                  atoms, abstract_code,
                  DebugInfo, "LitT", "StrT", "FunT", "Line", "Code"],
    {ok, {_, Chunks0}} =
        beam_lib:chunks(code:which(Mod), ChunkNames, [allow_missing_chunks]),
    Chunks = lists:map(fun decode_chunk/1, Chunks0),
    lists:foreach(fun({Name, Val}) ->
                          io:format("~s:~n~p~n~n", [Name, Val])
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
    {Lines, Fnames} = decode_lineinfo(binary_to_list(LinesFnames), 0, [], NumLines),
    FnamesL = [Name || <<Size:16, Name:Size/binary>> <= list_to_binary(Fnames)],
    (NumFnames == length(FnamesL)) orelse error({fnames, NumFnames, FnamesL}),
    {"Line", [{version, Ver},
              {bits, Bits},
              {num_line_instrunctions, NumLineInstrs},
              {lines, Lines},
              {file_names, FnamesL}]};
decode_chunk({"Code", <<HdrSize:32/integer, Hdr:HdrSize/binary, Tail/binary>> = All}) ->
    <<Instructionset:32/integer,
      OpcodeMax:32/integer,
      NumberOfLabels:32/integer,
      NumberOfFunctions:32/integer,
      Rest/binary>> = Hdr,
    OpcodeSize = size(All) - HdrSize - 8, %% 8 is size of CunkSize & SubSize
    <<OpCodes:OpcodeSize/binary, _Pad/binary>> = Tail,
    {"Code",
     [{instructionset, Instructionset},
      {opcodemax, OpcodeMax},
      {numberoflabels, NumberOfLabels},
      {numberofFunctions, NumberOfFunctions},
      {extra, Rest},
      {op_codes, OpCodes}]
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


parse_literals(<<Size:32, Literal:Size/binary, Tail/binary>>) ->
    [binary_to_term(Literal) | parse_literals(Tail)];
parse_literals(<<>>) -> [].


decode_lineinfo(Tail, _F, Acc, 0) ->
    {lists:reverse(Acc), Tail};
decode_lineinfo([B | Bs], F, Acc, LNum) ->
    Tag = decode_tag(B band 2#111),
    {{Tag, Num}, RemBs} = decode_int(Tag,B,Bs),
    case Tag of
        i ->
            decode_lineinfo(RemBs, F, [{line, F, Num} | Acc], LNum - 1);
        a ->
            [B2 | Bs2] = RemBs,
            Tag2 = decode_tag(B2 band 2#111),
            {{Tag2, Num2}, RemBs2} = decode_int(Tag2,B2,Bs2),
            decode_lineinfo(RemBs2, Num2, [{file, Num, Num2} | Acc], LNum - 1)
    end.

decode_int_length(B, Bs) ->
    {B bsr 5 + 2, Bs}.

-define(tag_i, 1).
-define(tag_a, 2).

decode_tag(?tag_i) -> i;
decode_tag(?tag_a) -> a.

decode_int(Tag,B,Bs) when (B band 16#08) =:= 0 ->
    %% N < 16 = 4 bits, NNNN:0:TTT
    N = B bsr 4,
    {{Tag,N},Bs};
decode_int(Tag,B,[]) when (B band 16#10) =:= 0 ->
    %% N < 2048 = 11 bits = 3:8 bits, NNN:01:TTT, NNNNNNNN
    Val0 = B band 2#11100000,
    N = (Val0 bsl 3),
    {{Tag,N},[]};
decode_int(Tag,B,Bs) when (B band 16#10) =:= 0 ->
    %% N < 2048 = 11 bits = 3:8 bits, NNN:01:TTT, NNNNNNNN
    [B1|Bs1] = Bs,
    Val0 = B band 2#11100000,
    N = (Val0 bsl 3) bor B1,
    {{Tag,N},Bs1};
decode_int(Tag,B,Bs) ->
    {Len,Bs1} = decode_int_length(B,Bs),
    {IntBs,RemBs} = take_bytes(Len,Bs1),
    N = build_arg(IntBs),
    {{Tag,N},RemBs}.


take_bytes(N, Bs) ->
    take_bytes(N, Bs, []).

take_bytes(N, [B|Bs], Acc) when N > 0 ->
    take_bytes(N-1, Bs, [B|Acc]);
take_bytes(0, Bs, Acc) ->
    {lists:reverse(Acc), Bs}.


build_arg(Bs) ->
    build_arg(Bs, 0).

build_arg([B|Bs], N) ->
    build_arg(Bs, (N bsl 8) bor B);
build_arg([], N) ->
    N.
