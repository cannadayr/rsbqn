-module(ebqn).

-include("crates.hrl").
-on_load(init/0).

-export([add/2]).

init() ->
    ?load_nif_from_crate(ebqn, ?crate_ebqn, 0).

add(_X,_Y) ->
    exit(nif_not_loaded).
