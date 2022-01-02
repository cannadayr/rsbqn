-module(ebqn).

-include("cargo.hrl").
-on_load(init/0).

-export([init_r/0,init_c/1,compile/3,callp/2,tests/0]).

init() ->
    ?load_nif_from_crate(ebqn, 0).

init_r() ->
    exit(nif_not_loaded).
init_c(_Runtime) ->
    exit(nif_not_loaded).
compile(_Runtime,_Compiler,_Src) ->
    exit(nif_not_loaded).
callp(_Prog,_X) ->
    exit(nif_not_loaded).
tests() ->
    exit(nif_not_loaded).
