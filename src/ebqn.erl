-module(ebqn).

-include("cargo.hrl").
-on_load(init/0).

-export([init_st/0,test/0]).

init() ->
    ?load_nif_from_crate(ebqn, 0).

init_st() ->
    exit(nif_not_loaded).
test() ->
    exit(nif_not_loaded).
