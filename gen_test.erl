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
    [<<"#[test]\n">>,<<"pub fn ">>,Name,<<"_">>,integer_to_list(N),<<"() {\n    ">>,<<"let mut stack = Stack::new();">>,<<"let root = Env::new_root();">>];
prefix(Name,N,runtime) ->
    prefix(Name,N,undefined) ++ [<<"let runtime = runtime(Some(&root),&mut stack).expect(">>,$",<<"runtime failed">>,$",<<").into_a().unwrap();">>];
prefix(Name,N,compiler) ->
    [<<"#[test]\n">>,<<"pub fn ">>,Name,<<"_">>,integer_to_list(N),<<"() {\n    ">>,<<"let root = Env::new_root();">>,<<"let mut stack = Stack::new();">>,<<"let runtime = runtime(Some(&root),&mut stack).expect(">>,$",<<"runtime failed">>,$",<<");">>,<<"let compiler = run(Some(&root),&mut stack,c(&runtime)).expect(">>,$",<<"compiler failed">>,$",<<");">>,<<"let names = V::A(Cc::new(A::new(vec![],vec![0])));">>,<<"let redef = V::A(Cc::new(A::new(vec![],vec![0])));">>].
suffix() ->
    [<<"}\n">>].
gen_line(Name,assert,_ByteCode,Code,_Comment,N,compiler) ->
    [<<"#[should_panic]\n">>,prefix(Name,N,compiler),<<"info!(\"test: {}\",r##\"">>,Code,<<"\"##);">>,<<"let src = new_string(r##\"">>,Code,<<"\"##); let prog = prog(&mut stack,&compiler,src,&runtime,&root,&names,&redef,0.0).expect(">>,$",<<"program compilation failed">>,$",<<");run(Some(&root),&mut stack,prog.0).unwrap();\n">>,suffix()];
gen_line(Name,assert,ByteCode,Code,undefined,N,Dependency) ->
    [<<"#[should_panic]\n">>,prefix(Name,N,Dependency),<<"{info!(\"test: {}\",r##\"">>,Code,<<"\"##);">>,<<"run(Some(&root),&mut stack,">>,ByteCode,<<").unwrap()};\n">>,suffix()];
gen_line(Name,assert,ByteCode,Code,Comment,N,Dependency) ->
    [<<"#[should_panic]\n">>,prefix(Name,N,Dependency),<<"{info!(\"test: {}\",r##\"">>,Comment,<<"\"##);">>,<<"run(Some(&root),&mut stack,">>,ByteCode,<<").unwrap()}; // ">>,string:trim(erlang:binary_to_list(Code)),<<"\n">>,suffix()];
gen_line(Name,Expected,_ByteCode,Code,_Comment,N,compiler) ->
    [prefix(Name,N,compiler),<<"info!(\"test: {}\",r##\"">>,Code,<<"\"##);">>,<<"let src = new_string(r##\"">>,Code,<<"\"##); ">>,<<"let prog = prog(&mut stack,&compiler,src,&runtime,&root,&names,&redef,0.0).expect(">>,$",<<"program compilation failed">>,$",<<"); let exec = run(Some(&root),&mut stack,prog.0).unwrap(); assert_eq!(new_scalar(">>,erlang:float_to_binary(Expected,[{decimals, 4},compact]),<<"),exec);\n">>,suffix()];
gen_line(Name,Expected,ByteCode,Code,undefined,N,Dependency) ->
    [prefix(Name,N,Dependency),<<"{info!(\"test: {}\",r##\"">>,Code,<<"\"##);">>,<<"assert_eq!(new_scalar(">>,erlang:float_to_binary(Expected,[{decimals, 4},compact]),<<"),run(Some(&root),&mut stack,">>,ByteCode,<<").unwrap());}\n">>,suffix()];
gen_line(Name,Expected,ByteCode,Code,Comment,N,Dependency) ->
    [prefix(Name,N,Dependency),<<"{info!(\"test: {}\",r##\"">>,Comment,<<"\"##);">>,<<"assert_eq!(new_scalar(">>,erlang:float_to_binary(Expected,[{decimals, 4},compact]),<<"),run(Some(&root),&mut stack,">>,ByteCode,<<").unwrap());} // ">>,string:trim(erlang:binary_to_list(Code)),<<"\n">>,suffix()].
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
split_comment(Tail,true) ->
    binary:split(Tail, <<"#">>, [global]);
split_comment(Tail,false) ->
    [Tail].
% # comment
parse_line(Line,Accm,_HasPct,_CommentMode)
    when binary_part(Line,0,1) =:= <<"#">>;
         Line =:= <<>> ->
    Accm;
%% ! % expression
parse_line(Line,Accm,_HasPct,CommentMode)
    when binary_part(Line,0,1) =:= <<"!">> ->
    [_Head,Tail] = binary:split(Line, <<"%">>, [global]),
    {Code,Comment} = parse_code(split_comment(Tail,CommentMode)),
    [{assert,Code,Comment}] ++ Accm;
% result % expression
parse_line(Line,Accm,true,CommentMode) ->
    [Head,Tail] = binary:split(Line, <<"%">>, [global]),
    Expected = cast(string:trim(erlang:binary_to_list(Head))),
    Src = split_comment(Tail,CommentMode),
    {Code,Comment} = parse_code(Src),
    [{Expected,Code,Comment}] ++ Accm;
% expression
parse_line(Line,Accm,false,CommentMode) ->
    Src = split_comment(Line,CommentMode),
    {Code,Comment} = parse_code(Src),
    [{1.0,Code,Comment}] ++ Accm.
parse([],Accm,_CommentMode) ->
    lists:reverse(Accm);
parse(Lines,Accm,CommentMode) ->
    Line = hd(Lines),
    parse(tl(Lines),parse_line(Line,Accm,has_pct(Line),CommentMode),CommentMode).
suite(Repo,Name,ShortName,Dependency,CommentMode) ->
    Path = filename:join([Repo,<<"test/cases/">>,Name]),
    {ok, Data} = file:read_file(Path),
    Args = parse(binary:split(Data, [<<"\n">>], [global]),[],CommentMode),
    Tests = gen_tests(Repo,Args,[]),
    gen_code(ShortName,Tests,[],0,Dependency).
template(Content,core) ->
    erlang:iolist_to_binary([
        <<"use log::{info};\n">>,
        <<"use core::f64::{INFINITY,NEG_INFINITY};\n">>,
        <<"use rsbqn::init_log;\n">>,
        <<"use rsbqn::vm::{run,call,runtime,prog};\n">>,
        <<"use rsbqn::schema::{Code,Env,new_scalar,new_char,new_string,Bodies,Exp,A,Decoder,V,Stack};\n\n">>,
        <<"\n\n">>,Content,<<"\n\n">>
    ]);
template(Content,compiler) ->
    erlang:iolist_to_binary([
        <<"use log::{info};\n">>,
        <<"use core::f64::{INFINITY,NEG_INFINITY};\n">>,
        <<"use rsbqn::init_log;\n">>,
        <<"use rsbqn::vm::{run,call,runtime,prog};\n">>,
        <<"use rsbqn::schema::{Code,Env,new_scalar,new_char,new_string,Bodies,Exp,A,Decoder,V,Stack};\n">>,
        <<"use rsbqn::gen::code::c;\n">>,
        <<"use bacon_rajan_cc::Cc;\n\n">>,
        Content,
        <<"\n\n">>
    ]).
runtime_tests(Repo) ->
    ByteCode = suite(Repo,<<"bytecode.bqn">>,<<"bytecode">>,undefined,true),
    file:write_file("tests/bytecode.rs",template(ByteCode,core)),
    Simple = suite(Repo,<<"simple.bqn">>,<<"simple">>,runtime,true),
    file:write_file("tests/simple.rs",template(Simple,core)),
    Prim = suite(Repo,<<"prim.bqn">>,<<"prim">>,runtime,true),
    file:write_file("tests/prim.rs",template(Prim,core)),
    Undo = suite(Repo,<<"undo.bqn">>,<<"undo">>,runtime,true),
    file:write_file("tests/undo.rs",template(Undo,core)),
    Under = suite(Repo,<<"under.bqn">>,<<"under">>,runtime,true),
    file:write_file("tests/under.rs",template(Under,core)),
    Identity = suite(Repo,<<"identity.bqn">>,<<"identity">>,runtime,true),
    file:write_file("tests/identity.rs",template(Identity,core)),
    Fill = suite(Repo,<<"fill.bqn">>,<<"fill">>,runtime,true),
    file:write_file("tests/fill.rs",template(Fill,core)).
% The following tests can't be precompiled and embedded as bytecode,
% as errors might happen during compilation.
compiler_tests(Repo) ->
    Literal = suite(Repo,<<"literal.bqn">>,<<"literal">>,compiler,true),
    file:write_file("tests/literal.rs",template(Literal,compiler)),
    Syntax = suite(Repo,<<"syntax.bqn">>,<<"syntax">>,compiler,true),
    file:write_file("tests/syntax.rs",template(Syntax,compiler)),
    % token level tests require CommentMode =:= false
    Token = suite(Repo,<<"token.bqn">>,<<"token">>,compiler,false),
    file:write_file("tests/token.rs",template(Token,compiler)),
    Header = suite(Repo,<<"header.bqn">>,<<"header">>,compiler,true),
    file:write_file("tests/header.rs",template(Header,compiler)),
    Namespace = suite(Repo,<<"namespace.bqn">>,<<"namespace">>,compiler,true),
    file:write_file("tests/namespace.rs",template(Namespace,compiler)),
    Unhead = suite(Repo,<<"unhead.bqn">>,<<"unhead">>,compiler,true),
    file:write_file("tests/unhead.rs",template(Unhead,compiler)).
main([Repo]) ->
    runtime_tests(Repo),
    compiler_tests(Repo);
main(_Args) ->
    io:format("bad arguments~n"),
    halt(1).
