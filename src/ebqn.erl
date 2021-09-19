-module(ebqn).

-include("cargo.hrl").
-on_load(init/0).

-export([init_st/0,tests/0]).

init() ->
    ?load_nif_from_crate(ebqn, 0).

init_st() ->
    exit(nif_not_loaded).
tests() ->
    exit(nif_not_loaded).
