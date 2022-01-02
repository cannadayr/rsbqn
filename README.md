ebqn
=====

EBQN is a Rust implementation of a BQN virtual machine, embedded in Erlang as a NIF.

Status
------

|TEST|STATUS|DETAIL
|---|---|---|
|bytecode|FAILED|36 passed; 1 failed; 0 ignored;|
|identity|ok|14 passed; 0 failed; 0 ignored;|
|prim|ok|535 passed; 0 failed; 0 ignored;|
|simple|ok|20 passed; 0 failed; 0 ignored;|
|under|FAILED|40 passed; 1 failed; 0 ignored;|
|prim|ok|68 passed; 0 failed; 0 ignored;|
|fill|N/A|N/A|
|header|N/A|N/A|
|literal|N/A|N/A|
|namespace|N/A|N/A|
|syntax|N/A|N/A|
|token|N/A|N/A|

Test
-----

    escript gen_test.erl /path/to/mlochbaum/bqn
    escript gen_code.erl /path/to/mlochbaum/bqn

    cargo test
