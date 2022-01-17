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

prefix(Name,N,undefined) ->
    [<<"#[test]\n">>,<<"pub fn ">>,Name,<<"_">>,integer_to_list(N),<<"() {\n    ">>];
prefix(Name,N,runtime) ->
    prefix(Name,N,undefined) ++ [<<"let runtimev = runtime(); let runtime = runtimev.as_a().unwrap();">>];
prefix(Name,N,compiler) ->
    [<<"#[test]\n">>,<<"pub fn ">>,Name,<<"_compiler_">>,integer_to_list(N),<<"() {\n    ">>] ++ [<<"let runtimev = runtime(); let runtime = runtimev.as_a().unwrap();">>] ++ [<<"let compiler = c(&runtimev);">>].
suffix() ->
    [<<"}\n">>].
gen_line(Name,assert,_ByteCode,Code,_Comment,N,compiler) ->
    [<<"#[should_panic]\n">>,prefix(Name,N,compiler),<<"let desc = r#\"test: ">>,Code,<<"\"#;info!(\"{}\",desc);">>,<<"let src = new_string(\"">>,re:replace(Code, [$"], [$\\, $\\, $"], [{return, list}, global]),<<"\"); let prog = prog(&compiler,src,&runtimev); call(0,Some(&run(prog)),None,None);\n">>,suffix()];
gen_line(Name,assert,ByteCode,Code,undefined,N,Dependency) ->
    [<<"#[should_panic]\n">>,prefix(Name,N,Dependency),<<"{let desc = r#\"test: ">>,Code,<<"\"#;info!(\"{}\",desc);">>,<<"run(">>,ByteCode,<<")};\n">>,suffix()];
gen_line(Name,assert,ByteCode,Code,Comment,N,Dependency) ->
    [<<"#[should_panic]\n">>,prefix(Name,N,Dependency),<<"{let desc = r#\"test: ">>,Comment,<<"\"#;info!(\"{}\",desc);">>,<<"run(">>,ByteCode,<<")}; // ">>,string:trim(erlang:binary_to_list(Code)),<<"\n">>,suffix()];
gen_line(Name,Expected,_ByteCode,Code,_Comment,N,compiler) ->
    [prefix(Name,N,compiler),<<"let desc = r#\"test: ">>,Code,<<"\"#;info!(\"{}\",desc);">>,<<"let src = new_string(\"">>,re:replace(Code, [$"], [$\\, $\\, $"], [{return, list}, global]),<<"\"); ">>,<<"let prog = prog(&compiler,src,&runtimev); assert_eq!(new_scalar(">>,erlang:float_to_binary(Expected,[{decimals, 4},compact]),<<"),call(0,Some(&run(prog)),None,None).into_v().unwrap());\n">>,suffix()];
gen_line(Name,Expected,ByteCode,Code,undefined,N,Dependency) ->
    [prefix(Name,N,Dependency),<<"{let desc = r#\"test: ">>,Code,<<"\"#;info!(\"{}\",desc);">>,<<"assert_eq!(new_scalar(">>,erlang:float_to_binary(Expected,[{decimals, 4},compact]),<<"),run(">>,ByteCode,<<"));}\n">>,suffix()];
gen_line(Name,Expected,ByteCode,Code,Comment,N,Dependency) ->
    [prefix(Name,N,Dependency),<<"{let desc = r#\"test: ">>,Comment,<<"\"#;info!(\"{}\",desc);">>,<<"assert_eq!(new_scalar(">>,erlang:float_to_binary(Expected,[{decimals, 4},compact]),<<"),run(">>,ByteCode,<<"));} // ">>,string:trim(erlang:binary_to_list(Code)),<<"\n">>,suffix()].
gen_code(_Name,[],Accm,_N,_Dependency) ->
    lists:reverse(Accm);
gen_code(Name,Todo,Accm,N,Dependency) ->
    {Expected,ByteCode,Code,Comment} = hd(Todo),
    Line = gen_line(Name,Expected,ByteCode,Code,Comment,N,Dependency),
    gen_code(Name,tl(Todo),[Line] ++ Accm,N+1,Dependency).
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
suite(Repo,Name,ShortName,Dependency) ->
    Path = filename:join([Repo,<<"test/cases/">>,Name]),
    {ok, Data} = file:read_file(Path),
    Args = parse(binary:split(Data, [<<"\n">>], [global]),[]),
    Tests = gen_tests(Repo,Args,[]),
    gen_code(ShortName,Tests,[],0,Dependency).
template(Content,core) ->
    erlang:iolist_to_binary([
        <<"use log::{info};\n">>,
        <<"use core::f64::{INFINITY,NEG_INFINITY};\n">>,
        <<"use ebqn::init_log;\n">>,
        <<"use ebqn::ebqn::{run,call,runtime,prog};\n">>,
        <<"use ebqn::schema::{Code,new_scalar,new_char,new_string,Body,A,Decoder,V};\n\n">>,
        <<"\n\n">>,Content,<<"\n\n">>
    ]);
template(Content,compiler) ->
    erlang:iolist_to_binary([
        <<"use log::{info};\n">>,
        <<"use core::f64::{INFINITY,NEG_INFINITY};\n">>,
        <<"use ebqn::init_log;\n">>,
        <<"use ebqn::ebqn::{run,call,runtime,prog};\n">>,
        <<"use ebqn::schema::{Code,new_scalar,new_char,new_string,Body,A,Decoder,V};\n">>,
        <<"use ebqn::code::c;\n\n">>,
        Content,
        <<"\n\n">>
    ]).
runtime_tests(Repo) ->
    ByteCode = suite(Repo,<<"bytecode.bqn">>,<<"bytecode">>,undefined),
    file:write_file("tests/bytecode.rs",template(ByteCode,core)),
    Simple = suite(Repo,<<"simple.bqn">>,<<"simple">>,runtime),
    file:write_file("tests/simple.rs",template(Simple,core)),
    Prim = suite(Repo,<<"prim.bqn">>,<<"prim">>,runtime),
    file:write_file("tests/prim.rs",template(Prim,core)),
    Undo = suite(Repo,<<"undo.bqn">>,<<"undo">>,runtime),
    file:write_file("tests/undo.rs",template(Undo,core)),
    Under = suite(Repo,<<"under.bqn">>,<<"under">>,runtime),
    file:write_file("tests/under.rs",template(Under,core)),
    Identity = suite(Repo,<<"identity.bqn">>,<<"identity">>,runtime),
    file:write_file("tests/identity.rs",template(Identity,core)).
    %Literal = suite(Repo,<<"literal.bqn">>,<<"literal">>,runtime),
    %file:write_file("tests/literal.rs",template(Literal));
compiler_tests(Repo) ->
    ByteCode = suite(Repo,<<"bytecode.bqn">>,<<"bytecode">>,compiler),
    file:write_file("tests/bytecode_compiler.rs",template(ByteCode,compiler)),
    Simple = suite(Repo,<<"simple.bqn">>,<<"simple">>,compiler),
    file:write_file("tests/simple_compiler.rs",template(Simple,compiler)),
    Prim = suite(Repo,<<"prim.bqn">>,<<"prim">>,compiler),
    file:write_file("tests/prim_compiler.rs",template(Prim,compiler)),
    Undo = suite(Repo,<<"undo.bqn">>,<<"undo">>,compiler),
    file:write_file("tests/undo_compiler.rs",template(Undo,compiler)),
    Under = suite(Repo,<<"under.bqn">>,<<"under">>,compiler),
    file:write_file("tests/under_compiler.rs",template(Under,compiler)),
    Identity = suite(Repo,<<"identity.bqn">>,<<"identity">>,compiler),
    file:write_file("tests/identity_compiler.rs",template(Identity,compiler)).
    %Literal = suite(Repo,<<"literal.bqn">>,<<"literal">>,compiler),
    %file:write_file("tests/literal.rs",template(Literal));
main([Repo]) ->
    runtime_tests(Repo),
    compiler_tests(Repo);
main(_Args) ->
    io:format("bad arguments~n"),
    halt(1).
