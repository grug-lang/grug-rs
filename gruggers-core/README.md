# Gruggers Core
Defines the core types and interfaces needed to implement the grug language in
rust. 

It is currently used by the [gruggers](https://crates.io/crates/gruggers) crate
to define its interface. Grug backend implementations should use the
interfaces defined here so it is compatible with any grug bindings made for grug.h

It also provides the `ntstring` module which defines the `NTStr` and `NTStrPtr`
types which are null terminated strings which are compatible with extern "C" apis.
