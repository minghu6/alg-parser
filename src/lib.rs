#![feature(min_type_alias_impl_trait)]
#![feature(destructuring_assignment)]
#![feature(generators, generator_trait)]
#![feature(in_band_lifetimes)]
#![feature(concat_idents)]
#![feature(stmt_expr_attributes)]
#![feature(proc_macro_hygiene)]
#![feature(extend_one)]
#![feature(associated_type_bounds)]
#![feature(trait_alias)]

#![allow(mixed_script_confusables)]

#[macro_use(c)]
extern crate cute;
#[macro_use]
extern crate maplit;

extern crate m6_key_set as key_set;

extern crate proc_macros; /* to avoid a cargo bug when cross-compiling (e.g. wasm) */

pub use proc_macros::{
    make_vec_macro_rules
};

make_vec_macro_rules!(vecdeq , std::collections::VecDeque, push_back);


pub mod utils;
pub mod algs;
pub mod regex;
pub mod dsl;
pub mod lexer;
pub mod parser;
pub mod regex_demo;
pub mod parser_demo;


pub use crate::parser_demo::{
    parse_algb_ratio
};

pub use crate::utils:: {
    Stack
};


////////////////////////////////////////////////////////////////////////////////
/// Unit Test

mod test {}
