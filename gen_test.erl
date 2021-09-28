-module(gen_test).
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

gen_code([],Accm) ->
    lists:reverse(Accm);
gen_code(Todo,Accm) ->
    {Expected,Code,Comment} = hd(Todo),
    Line =
        case Comment of
            undefined -> [<<"\tpanic::catch_unwind(|| { assert_eq!(">>,Expected,<<",run(">>,Code,<<")); });\n">>];
            C         -> [<<"\tdebug!(\"test: ">>,unicode:characters_to_binary(erlang:binary_to_list(C)),<<"\");">>,<<"panic::catch_unwind(|| { assert_eq!(">>,Expected,<<",run(">>,Code,<<")); }); // ">>,unicode:characters_to_binary(erlang:binary_to_list(C)),<<"\n">>]
        end,
    gen_code(tl(Todo),[Line] ++ Accm).
gen_tests(_Repo,[],Accm) ->
    lists:reverse(Accm);
gen_tests(Repo,Args,Accm) ->
   {Expected,Code,Comment} = hd(Args),
    {ok,CurDir} = file:get_cwd(),
    {Cmd,CmdArgs} = { erlang:binary_to_list(filename:join([CurDir, <<"crs.bqn">>])),[Repo,io_lib:format("~ts",[Code])] },
    {ok,Result} = cmd(Cmd,CmdArgs),
    gen_tests(Repo,tl(Args),[{erlang:float_to_binary(Expected,[{decimals, 1}]),string:trim(Result),Comment}]++Accm).
parse_code([Code]) ->
    {Code,undefined};
parse_code([Code,Comment]) ->
    {Code,Comment}.
parse_line(Line,Accm)
    when binary_part(Line,0,1) =:= <<"#">>;
         Line =:= <<>> ->
    Accm;
parse_line(Line,Accm) ->
    [Head,Tail] = binary:split(Line, <<"%">>, [global]),
    Expected = float(erlang:list_to_integer(string:trim(erlang:binary_to_list(Head)))),
    {Code,Comment} = parse_code(binary:split(Tail, <<"#">>, [global])),
    [{Expected,Code,Comment}] ++ Accm.
parse([],Accm) ->
    lists:reverse(Accm);
parse(Lines,Accm) ->
    Line = hd(Lines),
    parse(tl(Lines),parse_line(Line,Accm)).
main([Repo]) ->
    Path = filename:join([Repo,<<"test/cases/bytecode.bqn">>]),
    {ok, Data} = file:read_file(Path),
    Args = parse(binary:split(Data, [<<"\n">>], [global]),[]),
    Tests = gen_tests(Repo,Args,[]),
    Code = gen_code(Tests,[]),
    io:format("~ts~n",[erlang:iolist_to_binary([
        <<"use std::panic;\n">>,
        <<"use log::{debug};\n">>,
        <<"use crate::ebqn::run;\n">>,
        <<"use crate::schema::{Code,new_scalar,Body};\n">>,
        <<"pub fn bytecode() {\n">>,Code,<<"\n}">>
    ])]);
main(_Args) ->
    io:format("bad arguments~n"),
    halt(1).
