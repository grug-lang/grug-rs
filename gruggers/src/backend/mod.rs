//! Contains three different backends by default
//!
//! The interpreter backend is intended to be the reference backend. It's extremely slow.
//!
//! The Bytecode backend is a good, usable backend that comes bundled. It's
//! reasonably performant and can actually be used in an actual application.
mod interpreter;
pub use interpreter::Interpreter;

mod bytecode;
pub use bytecode::BytecodeBackend;

pub use gruggers_core::backend::*;

mod stub;
pub use stub::StubBackend;

// TODO: JIT backend
