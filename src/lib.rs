#![feature(min_type_alias_impl_trait)]
#![feature(destructuring_assignment)]
#![feature(generators, generator_trait)]
#![feature(in_band_lifetimes)]
#![feature(concat_idents)]
#![feature(stmt_expr_attributes)]
#![feature(proc_macro_hygiene)]

#![allow(mixed_script_confusables)]

#[macro_use(c)]
extern crate cute;
#[macro_use]
extern crate maplit;

extern crate proc_macros; /* to avoid a cargo bug when cross-compiling (e.g. wasm) */

pub use proc_macros::{
    // make_regex_node
};

pub mod utils;
pub mod regex;
pub mod parser;

pub mod regex_demo;
pub mod parser_demo;


////////////////////////////////////////////////////////////////////////////////
/// Unit Test

mod test {}
