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

gen_line(assert,ByteCode,Code,undefined) ->
    [<<"\t{let desc = r#\"test: ">>,Code,<<"\"#;info!(\"{}\",desc);">>,<<"let code = ">>,ByteCode,<<";let wrapper = AssertUnwindSafe(code);panic::catch_unwind( move || { run(wrapper.clone()) }).unwrap_err();}\n">>];
gen_line(assert,ByteCode,Code,Comment) ->
    [<<"\t{let desc = r#\"test: ">>,Comment,<<"\"#;info!(\"{}\",desc);">>,<<"let code = ">>,ByteCode,<<";let wrapper = AssertUnwindSafe(code);panic::catch_unwind( move || { run(wrapper.clone()) }).unwrap_err();} // ">>,Code,<<"\n">>];
gen_line(Expected,ByteCode,Code,undefined) ->
    [<<"\t{let desc = r#\"test: ">>,Code,<<"\"#;info!(\"{}\",desc);">>,<<"assert_eq!(">>,erlang:float_to_binary(Expected,[{decimals, 4},compact]),<<",run(">>,ByteCode,<<").to_f64());}\n">>];
gen_line(Expected,ByteCode,Code,Comment) ->
    [<<"\t{let desc = r#\"test: ">>,Comment,<<"\"#;info!(\"{}\",desc);">>,<<"assert_eq!(">>,erlang:float_to_binary(Expected,[{decimals, 4},compact]),<<",run(">>,ByteCode,<<").to_f64());} // ">>,Code,<<"\n">>].
gen_code([],Accm) ->
    lists:reverse(Accm);
gen_code(Todo,Accm) ->
    {Expected,ByteCode,Code,Comment} = hd(Todo),
    Line = gen_line(Expected,ByteCode,Code,Comment),
    gen_code(tl(Todo),[Line] ++ Accm).
gen_tests(_Repo,[],Accm) ->
    lists:reverse(Accm);
gen_tests(Repo,Args,Accm) ->
   {Expected,Code,Comment} = hd(Args),
    {ok,CurDir} = file:get_cwd(),
    {Cmd,CmdArgs} = { erlang:binary_to_list(filename:join([CurDir, <<"crs.bqn">>])),[Repo,io_lib:format("~ts",[Code])] },
    {ok,Result} = cmd(Cmd,CmdArgs),
    gen_tests(Repo,tl(Args),[{Expected,string:trim(Result),Code,Comment}]++Accm).
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
    Simple = suite(Repo,<<"simple.bqn">>),
    Prim = suite(Repo,<<"prim.bqn">>),
    Under = suite(Repo,<<"under.bqn">>),
    file:write_file("rs_src/test.rs",erlang:iolist_to_binary([
        <<"use log::{info};\n">>,
        <<"use core::f64::{INFINITY,NEG_INFINITY};\n">>,
        %<<"use std::{panic};\n">>,
        <<"use crate::ebqn::{run};\n">>,
        <<"use std::panic::{self, AssertUnwindSafe};\n">>,
        <<"use crate::schema::{Code,new_scalar,new_char,new_string,Body,A,Decoder};\n\n">>,
        <<"pub fn bytecode() {\n">>,ByteCode,<<"}\n\n">>,
        <<"pub fn simple(runtime: &A) {\n">>,Simple,<<"\n}\n">>,
        <<"pub fn prim(runtime: &A) {\n">>,Prim,<<"}\n\n">>,
        <<"pub fn under(runtime: &A) {\n">>,Under,<<"}\n\n">>
    ]));
main(_Args) ->
    io:format("bad arguments~n"),
    halt(1).
