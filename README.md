# grug-rs

This repository contains a rust implementation of the [grug](https://github.com/grug-lang/grug) language.
It will include rust bindings, a frontend, and possibly multiple backends for
grug

# Building

run `cargo build` from within the repository. 

# Testing

This repository contains [grug-tests](https://github.com/grug-lang/grug-tests)
as a submodule to allow for easy testing. 

when you want to run the tests, clone the submodule with

`git submodule update --init --force`

build the tests with 

`cd src/grug-tests/`
`./build.sh`

then run the tests with 

`cargo test`

