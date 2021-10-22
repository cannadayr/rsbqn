-module(gen_test).
-mode(compile).
-export([main/1]).

% usage:
% escript gen_test.erl path/to/bqn

% https://stackoverflow.com/a/2249387
cmd(Cmd, Args) ->
    Tag = make_ref(),
    {Pid, Ref} = erlang:spawn_monitor(fun() ->
            Rv = cmd_sync(Cmd, Args),
            exit({Tag, Rv})
        end),
    receive
        {'DOWN', Ref, process, Pid, {Tag, Data}} -> Data;
        {'DOWN', Ref, process, Pid, Reason} -> exit(Reason)
    end.

cmd_sync(Cmd, Args) ->
    P = open_port({spawn_executable, os:find_executable(Cmd)}, [
            binary, use_stdio, stream, eof, {args, Args}]),
    cmd_receive(P, []).

cmd_receive(Port, Acc) ->
    receive
        {Port, {data, Data}} -> cmd_receive(Port, [Data|Acc]);
        {Port, eof}          -> {ok, lists:reverse(Acc)}
    end.

utf8(Str) ->
    unicode:characters_to_binary(erlang:binary_to_list(Str)).
gen_line(assert,Code,undefined) ->
    [<<"\tassert_panic(">>,utf8(Code),<<");\n">>];
gen_line(assert,Code,Comment) ->
    [<<"\tdebug!(\"test: ">>,unicode:characters_to_binary(erlang:binary_to_list(Comment)),<<"\");">>,<<"assert_panic(">>,utf8(Code),<<"); // ">>,unicode:characters_to_binary(erlang:binary_to_list(Comment)),<<"\n">>];
gen_line(Expected,Code,undefined) ->
    [<<"\tassert_eq!(">>,erlang:float_to_binary(Expected,[{decimals, 1}]),<<",run(">>,utf8(Code),<<").to_f64());\n">>];
gen_line(Expected,Code,Comment) ->
    [<<"\tdebug!(\"test: ">>,unicode:characters_to_binary(erlang:binary_to_list(Comment)),<<"\");">>,<<"assert_eq!(">>,erlang:float_to_binary(Expected,[{decimals, 1}]),<<",run(">>,utf8(Code),<<").to_f64()); // ">>,unicode:characters_to_binary(erlang:binary_to_list(Comment)),<<"\n">>].
gen_code([],Accm) ->
    lists:reverse(Accm);
gen_code(Todo,Accm) ->
    {Expected,Code,Comment} = hd(Todo),
    Line = gen_line(Expected,Code,Comment),
    gen_code(tl(Todo),[Line] ++ Accm).
gen_tests(_Repo,[],Accm) ->
    lists:reverse(Accm);
gen_tests(Repo,Args,Accm) ->
   {Expected,Code,Comment} = hd(Args),
    {ok,CurDir} = file:get_cwd(),
    {Cmd,CmdArgs} = { erlang:binary_to_list(filename:join([CurDir, <<"crs.bqn">>])),[Repo,io_lib:format("~ts",[Code])] },
    {ok,Result} = cmd(Cmd,CmdArgs),
    gen_tests(Repo,tl(Args),[{Expected,string:trim(Result),Comment}]++Accm).
parse_code([Code]) ->
    {Code,undefined};
parse_code([Code,Comment]) ->
    {Code,Comment}.
has_pct(Line) ->
    case binary:match(Line,<<"%">>) of
        nomatch -> false;
        _ -> true
    end.
cast(L) ->
    case lists:member($.,L) of
        true ->
            erlang:list_to_float(L);
        false ->
            float(erlang:list_to_integer(L))
    end.
% # comment
parse_line(Line,Accm,_HasPct)
    when binary_part(Line,0,1) =:= <<"#">>;
         Line =:= <<>> ->
    Accm;
%% ! % expression
parse_line(Line,Accm,_HasPct)
    when binary_part(Line,0,1) =:= <<"!">> ->
    [_Head,Tail] = binary:split(Line, <<"%">>, [global]),
    {Code,Comment} = parse_code(binary:split(Tail, <<"#">>, [global])),
    [{assert,Code,Comment}] ++ Accm;
% result % expression
parse_line(Line,Accm,true) ->
    [Head,Tail] = binary:split(Line, <<"%">>, [global]),
    Expected = cast(string:trim(erlang:binary_to_list(Head))),
    {Code,Comment} = parse_code(binary:split(Tail, <<"#">>, [global])),
    [{Expected,Code,Comment}] ++ Accm;
% expression
parse_line(Line,Accm,false) ->
    {Code,Comment} = parse_code(binary:split(Line, <<"#">>, [global])),
    [{1.0,Code,Comment}] ++ Accm.
parse([],Accm) ->
    lists:reverse(Accm);
parse(Lines,Accm) ->
    Line = hd(Lines),
    parse(tl(Lines),parse_line(Line,Accm,has_pct(Line))).
suite(Repo,Name) ->
    Path = filename:join([Repo,<<"test/cases/">>,Name]),
    {ok, Data} = file:read_file(Path),
    Args = parse(binary:split(Data, [<<"\n">>], [global]),[]),
    Tests = gen_tests(Repo,Args,[]),
    gen_code(Tests,[]).
main([Repo]) ->
    ByteCode = suite(Repo,<<"bytecode.bqn">>),
    %Simple = suite(Repo,<<"simple.bqn">>),
    Prim = suite(Repo,<<"prim.bqn">>),
    io:format("~ts~n",[erlang:iolist_to_binary([
        <<"use log::{debug};\n">>,
        <<"use core::f64::{INFINITY,NEG_INFINITY};\n">>,
        %<<"use std::{panic};\n">>,
        <<"use crate::ebqn::{run,assert_panic};\n">>,
        <<"use crate::schema::{Code,new_scalar,new_char,new_string,Body,A,Decoder};\n">>,
        <<"pub fn bytecode() {\n">>,ByteCode,<<"}\n\n">>,
        <<"pub fn prim(runtime: A) {\n">>,Prim,<<"}\n\n">>
        %<<"pub fn simple(runtime: A) {\n">>,Simple,<<"\n}\n">>
    ])]);
main(_Args) ->
    io:format("bad arguments~n"),
    halt(1).
