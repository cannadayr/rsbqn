-module(gen_test).
-mode(compile).
-export([main/1]).

% usage:
% escript gen_code.erl path/to/bqn

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
    unicode:characters_to_binary(Str).

test(Repo,Test) ->
    {ok,Cwd} = file:get_cwd(),
    {Cmd,Args} = { erlang:binary_to_list(filename:join([Cwd, <<"crs.bqn">>])),[Repo,Test] },
    cmd(Cmd,Args).
    
main([Repo]) ->
    {ok,R0} = test(Repo,<<"r0">>),
    {ok,R1} = test(Repo,<<"r1">>),
    {ok,C}  = test(Repo,<<"c">>),
    {ok,F}  = test(Repo,<<"f">>),
    %{ok,F}  = test(Repo,<<"f">>),
    file:write_file("src/gen/code.rs",erlang:iolist_to_binary([
        <<"use log::{debug};\n">>,
        <<"use core::f64::{INFINITY,NEG_INFINITY};\n">>,
        <<"use crate::vm::run;\n">>,
        <<"use crate::schema::{Code,new_scalar,new_char,new_string,Bodies,A,Decoder,V};\n">>,
        <<"use bacon_rajan_cc::Cc;\n">>,
        <<"pub fn r0(provide: &A) -> Cc<Code> {\nCode::new(">>,utf8(R0),<<")\n}\n\n">>,
        <<"pub fn r1(provide: &A,runtime_0v: &V) -> Cc<Code> {let runtime_0 = runtime_0v.as_a().unwrap();\nCode::new(">>,utf8(R1),<<")\n}\n\n">>,
        <<"pub fn c(runtimev: &V) -> Cc<Code> {\nlet runtime = runtimev.as_a().unwrap();Code::new(">>,utf8(C),<<")\n}\n\n">>,
        <<"pub fn f(runtimev: &V) -> Cc<Code> {\nlet runtime = runtimev.as_a().unwrap();Code::new(">>,utf8(F),<<")\n}\n\n">>
    ]));
main(_Args) ->
    io:format("bad arguments~n"),
    halt(1).
