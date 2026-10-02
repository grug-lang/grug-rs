
This repository contains a rust implementation of the [grug](https://github.com/grug-lang/grug) language.
It contains rust bindings, c bindings, and a bytecode vm based backend
compatible with grug.h.

# Building

run `cargo build` from within the repository. 

# Host Functions in Other Languages

Host functions are defined in C. If your implementation is in another language,
converting it to C is your responsibility, using whatever tooling already exists
for that language. This repository and [grug-ir](https://github.com/grug-lang/grug-ir)
deliberately do not ship per-language frontends.

The reason is that the valuable property is the one the ahead-of-time pipeline
provides: host functions compiled to LLVM IR become inlinable intrinsics in the
compiled grug program, rather than FFI calls. Anything that can produce C can
take part in that, so a language-specific frontend would be a second thing to
maintain for the same result. See grug-ir for the C to IR step (`c2grir.py`) and
the rest of the pipeline.

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

Run `cargo test --test grug_tests` to run the test suite
Run `cargo test -- grug_bench` to run the benchmarks

adding `--release` between `test` and `--` will compile gruggers in release mode.
