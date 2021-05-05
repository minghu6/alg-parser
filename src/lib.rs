#![feature(min_type_alias_impl_trait)]
#![feature(destructuring_assignment)]
#![feature(generators, generator_trait)]
#![feature(in_band_lifetimes)]
#![feature(concat_idents)]
#![feature(stmt_expr_attributes)]
#![feature(proc_macro_hygiene)]

#[macro_use(c)]
extern crate cute;
#[macro_use]
extern crate maplit;

extern crate proc_macros; /* to avoid a cargo bug when cross-compiling (e.g. wasm) */

// #[::proc_macro_hack::proc_macro_hack] /* if function-like proc-macros expanding to exprs */
pub use proc_macros::{
    // make_regex_node
};

pub mod utils;
pub mod regex;
pub mod parser;

////////////////////////////////////////////////////////////////////////////////
/// Unit Test

mod test {}
