
This repository contains a rust implementation of the [grug](https://github.com/grug-lang/grug) language.
It contains rust bindings, c bindings, and a bytecode vm based backend
compatible with grug.h.

# Building

run `cargo build` from within the repository. 

# Testing and Benchmarks

This repository contains [grug-tests](https://github.com/grug-lang/grug-tests)
as a submodule to allow for easy testing and
[grug-bench](https://github.com/grug-lang/grug-bench) for benchmarking.

when you want to run the tests, clone the submodule with

`git submodule update --init --force`

Build the tests and benchmark libraries by following their instructions.
Trying to run the tests without building the test and bench will result in a linker error.

The tests are located in `./gruggers/src/grug_tests`, and the benchmarks are
located in `./gruggers/src/grug_bench`.

Run `cargo test -- grug_tests` to run the test suite
Run `cargo test -- grug_bench` to run the benchmarks

adding `--release` between `test` and `--` will compile gruggers in release mode.
