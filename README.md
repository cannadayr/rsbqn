ebqn
=====

EBQN is a Rust implementation of a BQN virtual machine, embedded in Erlang as a NIF.

Its purpose is to act as an auxiliary BQN interpreter.

For a general purpose BQN interpreter, it is recommended to use either the [reference implementation](https://github.com/mlochbaum/BQN/blob/master/bqn.js) or [CBQN](https://github.com/dzaima/CBQN).

Its priorities, in-order are:
1. Correctness
2. Embeddability
3. Simplicity
4. Performance

Due to this, some features are either low priority, or not a priority.
Features that are low priority, but in-scope:
1. System functions
2. r0 and r1 runtimes
3. Namespaces
4. Alternate compilation targets (WASM)
5. SIMD
6. JIT

Features that are currently out-of-scope:
1. System functions with IO

Status
------

|TEST|STATUS|
|---|---|
|bytecode|tests passing|
|fill|not working|
|header|not working|
|identity|tests passing|
|literal|not working|
|namespace|not working|
|prim|tests passing|
|simple|tests passing|
|syntax|not working|
|token|not working|
|under|tests passing|
|undo|tests passing|


Build
-----

    escript gen_test.erl /path/to/mlochbaum/bqn
    escript gen_code.erl /path/to/mlochbaum/bqn

    RUST_LOG=info cargo test -- --nocapture

