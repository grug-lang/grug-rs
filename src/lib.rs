// #![deny(warnings)]

pub mod frontend;
pub mod backend;

pub mod mod_api;
pub mod types;
pub mod serde;
pub mod error;
pub mod state;

pub mod ntstring;

// TODO: implementation errors like too many statements in block should not be grug errors
// TODO: Better Error handling
