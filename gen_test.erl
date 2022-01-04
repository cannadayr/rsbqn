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

prefix(Name,N,false) ->
    [<<"#[test]\n">>,<<"pub fn ">>,Name,<<"_">>,integer_to_list(N),<<"() {\n    ">>];
prefix(Name,N,true) ->
    prefix(Name,N,false) ++ [<<"let runtime = runtime();">>].
suffix() ->
    [<<"}\n">>].
gen_line(Name,assert,ByteCode,Code,undefined,N,NeedsRuntime,rust) ->
    [<<"#[should_panic]\n">>,prefix(Name,N,NeedsRuntime),<<"{let desc = r#\"test: ">>,Code,<<"\"#;info!(\"{}\",desc);">>,<<"run(">>,ByteCode,<<")};\n">>,suffix()];
gen_line(Name,assert,ByteCode,Code,Comment,N,NeedsRuntime,rust) ->
    [<<"#[should_panic]\n">>,prefix(Name,N,NeedsRuntime),<<"{let desc = r#\"test: ">>,Comment,<<"\"#;info!(\"{}\",desc);">>,<<"run(">>,ByteCode,<<")}; // ">>,string:trim(erlang:binary_to_list(Code)),<<"\n">>,suffix()];
gen_line(_Name,assert,_ByteCode,Code,_Comment,_N,_NeedsRuntime,erlang) ->
    [<<"\t?_assertException(error,function_clause,test(R,C,<<\"">>,re:replace(Code, [$"], [$\\, $\\, $"], [{return, list}, global]),<<"\"/utf8>>))\n">>];
gen_line(Name,Expected,ByteCode,Code,undefined,N,NeedsRuntime,rust) ->
    [prefix(Name,N,NeedsRuntime),<<"{let desc = r#\"test: ">>,Code,<<"\"#;info!(\"{}\",desc);">>,<<"assert_eq!(new_scalar(">>,erlang:float_to_binary(Expected,[{decimals, 4},compact]),<<"),run(">>,ByteCode,<<"));}\n">>,suffix()];
gen_line(Name,Expected,ByteCode,Code,Comment,N,NeedsRuntime,rust) ->
    [prefix(Name,N,NeedsRuntime),<<"{let desc = r#\"test: ">>,Comment,<<"\"#;info!(\"{}\",desc);">>,<<"assert_eq!(new_scalar(">>,erlang:float_to_binary(Expected,[{decimals, 4},compact]),<<"),run(">>,ByteCode,<<"));} // ">>,string:trim(erlang:binary_to_list(Code)),<<"\n">>,suffix()];
gen_line(_Name,Expected,_ByteCode,Code,_Comment,_N,_NeedsRuntime,erlang) ->
    [<<"\t?_assert(test(R,C,<<\"">>,re:replace(Code, [$"], [$\\, $\\, $"], [{return, list}, global]),<<"\"/utf8>>) =:= {ok,">>,erlang:float_to_binary(Expected,[{decimals, 4},compact]),<<"})\n">>].
gen_code(_Name,[],Accm,_N,_NeedsRuntime,_Lang) ->
    lists:reverse(Accm);
gen_code(Name,Todo,Accm,N,NeedsRuntime,Lang) ->
    {Expected,ByteCode,Code,Comment} = hd(Todo),
    Line = gen_line(Name,Expected,ByteCode,Code,Comment,N,NeedsRuntime,Lang),
    gen_code(Name,tl(Todo),[Line] ++ Accm,N+1,NeedsRuntime,Lang).
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
suite(Repo,Name,ShortName,NeedsRuntime,Lang) ->
    Path = filename:join([Repo,<<"test/cases/">>,Name]),
    {ok, Data} = file:read_file(Path),
    Args = parse(binary:split(Data, [<<"\n">>], [global]),[]),
    Tests = gen_tests(Repo,Args,[]),
    gen_code(ShortName,Tests,[],0,NeedsRuntime,Lang).
template(Content,rust,_Module) ->
    erlang:iolist_to_binary([
        <<"use log::{info};\n">>,
        <<"use core::f64::{INFINITY,NEG_INFINITY};\n">>,
        <<"use ebqn::init_log;\n">>,
        <<"use ebqn::ebqn::{run,call,runtime,prog};\n">>,
        <<"use ebqn::schema::{Code,new_scalar,new_char,new_string,Body,A,Decoder,V};\n\n">>,
        <<"\n\n">>,Content,<<"\n\n">>
    ]);
template(Content,erlang,Module) ->
    erlang:iolist_to_binary([
        <<"-module(">>,Module,<<").\n">>,
        <<"-include_lib(\"eunit/include/eunit.hrl\").\n">>,
        <<"-import(ebqn,[init_r/0,init_c/1,compile/3,callp/2]).\n">>,
        <<"test(R,C,Src) ->\n">>,
        <<"    {ok,P} = compile(R,C,Src),\n">>,
        <<"    callp(P,0.0).\n">>,
        <<"bytecode_test_() ->\n">>,
        <<"    {ok,R} = init_r(),\n">>,
        <<"    {ok,C} = init_c(R),\n">>,
        <<"{timeout,600,\n">>,
        <<"[\n">>,
            lists:join(<<",">>,Content),
        <<"]}.\n">>
    ]).
rust_tests(Repo) ->
    ByteCode = suite(Repo,<<"bytecode.bqn">>,<<"bytecode">>,false,rust),
    file:write_file("tests/bytecode.rs",template(ByteCode,rust,undefined)),
    Simple = suite(Repo,<<"simple.bqn">>,<<"simple">>,true,rust),
    file:write_file("tests/simple.rs",template(Simple,rust,undefined)),
    Prim = suite(Repo,<<"prim.bqn">>,<<"prim">>,true,rust),
    file:write_file("tests/prim.rs",template(Prim,rust,undefined)),
    Undo = suite(Repo,<<"undo.bqn">>,<<"prim">>,true,rust),
    file:write_file("tests/undo.rs",template(Undo,rust,undefined)),
    Under = suite(Repo,<<"under.bqn">>,<<"under">>,true,rust),
    file:write_file("tests/under.rs",template(Under,rust,undefined)),
    Identity = suite(Repo,<<"identity.bqn">>,<<"identity">>,true,rust),
    file:write_file("tests/identity.rs",template(Identity,rust,undefined)).
    %Literal = suite(Repo,<<"literal.bqn">>,<<"literal">>,true),
    %file:write_file("tests/literal.rs",template(Literal));
erl_tests(Repo) ->
    ByteCode = suite(Repo,<<"bytecode.bqn">>,<<"bytecode">>,false,erlang),
    file:write_file("etests/bytecode.erl",template(ByteCode,erlang,<<"bytecode">>)),
    Simple = suite(Repo,<<"simple.bqn">>,<<"simple">>,true,erlang),
    file:write_file("etests/simple.erl",template(Simple,erlang,<<"simple">>)),
    Prim = suite(Repo,<<"prim.bqn">>,<<"prim">>,true,erlang),
    file:write_file("etests/prim.erl",template(Prim,erlang,<<"prim">>)),
    Undo = suite(Repo,<<"undo.bqn">>,<<"undo">>,true,erlang),
    file:write_file("etests/undo.erl",template(Undo,erlang,<<"undo">>)),
    Under = suite(Repo,<<"under.bqn">>,<<"under">>,true,erlang),
    file:write_file("etests/under.erl",template(Under,erlang,<<"under">>)),
    Identity = suite(Repo,<<"identity.bqn">>,<<"identity">>,true,erlang),
    file:write_file("etests/identity.erl",template(Identity,erlang,<<"identity">>)).
    %Literal = suite(Repo,<<"literal.bqn">>,<<"literal">>,true),
    %file:write_file("etests/literal.erl",template(Literal,erlang,<<"literal">>)).
main([Repo]) ->
    rust_tests(Repo),
    erl_tests(Repo);
main(_Args) ->
    io:format("bad arguments~n"),
    halt(1).
