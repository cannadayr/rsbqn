-module(ebqn).

-include("crates.hrl").
-on_load(init/0).

-export([init_st/0]).

init() ->
    ?load_nif_from_crate(ebqn, ?crate_ebqn, 0).

init_st() ->
    exit(nif_not_loaded).
