-module(ebqn).

-include("crates.hrl").
-on_load(init/0).

-export([init_st/0,run/4]).
-export([st/1,incr_st/1]).

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
