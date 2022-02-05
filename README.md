rsbqn
=====

RSBQN is a Rust implementation of a BQN virtual machine.

Build
------

    git restore --source origin/BYTECODE -- src/gen/code.rs
    cargo build

Status
------

|TEST|STATUS|DETAIL
|---|---|---|
|bytecode|FAILED|36 passed; 1 failed; 0 ignored;|
|identity|ok|14 passed; 0 failed; 0 ignored;|
|prim|ok|535 passed; 0 failed; 0 ignored;|
|simple|ok|20 passed; 0 failed; 0 ignored;|
|under|FAILED|40 passed; 1 failed; 0 ignored;|
|undo|ok|68 passed; 0 failed; 0 ignored;|
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

Causal Profiling
-----

    cargo build --profile bench --features coz-loop,coz-ops,coz-fns
    coz run --- ./target/release/rsbqn

Heap Analysis
-----

    cargo build --profile bench --features dhat
    ./target/release/rsbqn

Perf
-----

    cargo build --profile bench
    perf stat -e instructions ./target/release/rsbqn

Profile Guided Optimiziation
-----

    RUSTFLAGS="-Cprofile-generate=/tmp/pgo-data" cargo build --release --target=x86_64-unknown-linux-gnu
    for i in {1..10}; do ./target/x86_64-unknown-linux-gnu/release/rsbqn ; done
    ${HOME}/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/x86_64-unknown-linux-gnu/bin/llvm-profdata merge -o /tmp/pgo-data/merged.profdata /tmp/pgo-data/
    RUSTFLAGS="-Cprofile-use=/tmp/pgo-data/merged.profdata" cargo build --release --target=x86_64-unknown-linux-gnu
