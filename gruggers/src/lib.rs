#![doc = include_str!("../README.md")]
// #![deny(warnings)]
// #![deny(missing_docs)]
#![allow(clippy::single_char_add_str, clippy::bool_comparison)]

mod frontend;
pub mod backend;

mod mod_api;
pub mod serde;
pub mod state;

pub mod capi;
mod xar;
pub mod arena;

mod cachemap;
mod watcher;
mod async_fs;
pub mod error;

mod pal;

pub use gruggers_core::types;
pub use gruggers_core::ast;

pub use gruggers_core::ntstring;
pub use gruggers_core::nt;


// TODO: Add #[track_caller] to error functions for tracing
// TODO: Standard Library
// TODO: (WIP) ModsDir struct 
// TODO: (WIP) README and docs for gruggers
// TODO: (WIP) Debug info to backends
// TODO: (WIP) Better Error handling
//
// TODO: (finished) Resource management
// TODO: (finished) Hot reloading grug_files
// TODO: (finished) Non-utf8 version of NTStrPtr and NTStr

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
