//! Contains the core types used by all grug implementations to ensure compatibility with a c api.
//!
//! This can be used both to implement grug bindings and grug backends.
//!
//! [`mod@state`] contains the interface that bindings must follow to ensure
//! for backends. Following this interface also lets backends implemented in
//! other languages to interface with rust bindings seamlessly
//!
//! [`mod@backend`] contains the interface that backends must follow to ensure
//! they can be used by bindings. Following this interface lets backends
//! written in rust to seamlessly operate with bindings in other languages.
//!
//! [`mod@error`] contains the interface of compile time and initialization
//! errors. 
//!
//! [`mod@runtime_error`] contains the interface of runtime errors.
//!
//! [`mod@ntstring`] defines null terminated string types for interop with C. A
//! lot of types in this crate use there string types instead of the default
//! rust string types to ensure layout compatibility with other langauges. The
//! strings in this module can often be cheaply converted into rust string types.
//!
//! [`mod@ast`] defines all the types that are used by the [`GrugAst`](ast::GrugAst) struct.
//! [`GrugAst`](ast::GrugAst) contains ast nodes for various parts of the grug language. It
//! contains both structs and enums and is fully compatible with equivalent c
//! structs and tagged unions.
//! 
//! [`mod@types`] defines types that are likely to be used by lots of bindings'
//! implementations. 
#![deny(warnings)]
#![warn(missing_docs)]
pub mod types;
pub mod ast;
pub mod backend;
pub mod state;
pub mod error;

pub mod ntstring;
pub mod runtime_error;
mod utils;

// MIT LICENSE
// 
// Copyright (c) 2026 NikhilNathanael

// Permission is hereby granted, free of charge, to any person obtaining a copy of
// this software and associated documentation files (the “Software”), to deal in
// the Software without restriction, including without limitation the rights to
// use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
// of the Software, and to permit persons to whom the Software is furnished to do
// so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
