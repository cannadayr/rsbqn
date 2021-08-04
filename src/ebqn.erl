-module(ebqn).

-include("crates.hrl").
-on_load(init/0).

-export([init_st/0,run/4]).
-export([st/1,incr_st/1]).
-export([test0/0]).

init() ->
    ?load_nif_from_crate(ebqn, ?crate_ebqn, 0).

init_st() ->
    exit(nif_not_loaded).

st(_State) ->
    exit(nif_not_loaded).

incr_st(_State) ->
    exit(nif_not_loaded).

run(_State,_B,_O,_S) ->
    exit(nif_not_loaded).

test0() ->
    {ok,St} = ebqn:init_st(),
    {ok,5} = ebqn:run(St,[0,0,25],[5],[[0,1,0,0]]),
    {ok,3} = ebqn:run(St,[0,0,14,0,1,25],[4,3],[[0,1,0,0]]),
    {ok,5} = ebqn:run(St,[0,0,22,0,0,11,25],[5],[[0,1,0,1]]),
    {ok,4} = ebqn:run(St,[0,0,22,0,0,11,14,0,1,22,0,0,12,25],[5,4],[[0,1,0,1]]),
    {ok,2} = ebqn:run(St,[0,0,22,0,0,11,14,0,1,22,0,1,11,14,21,0,0,25],[2,3],[[0,1,0,2]]),
    {ok,1} = ebqn:run(St,[0,0,22,0,0,11,14,0,1,21,0,0,16,25],[1,4],[[0,1,0,1]]).
