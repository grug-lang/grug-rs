#![deny(warnings)]
#![allow(clippy::single_char_add_str, clippy::bool_comparison)]

pub mod frontend;
pub mod backend;

pub mod mod_api;
pub mod types;
pub mod serde;
pub mod error;
pub mod state;

// pub mod capi;
pub mod xar;
pub mod ast;

pub mod ntstring;

mod cachemap;


// TODO: update AST to be like new grug-for-c ast
// TODO: Debug info to backends
// TODO: Better Error handling
// TODO: ModsDir struct 
// TODO: Standard Library

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
