#![allow(mixed_script_confusables)]

#![feature(coroutines)]
#![feature(concat_idents)]
#![feature(stmt_expr_attributes)]
#![feature(proc_macro_hygiene)]
#![feature(extend_one)]
#![feature(trait_alias)]
#![feature(type_alias_impl_trait)]


#[macro_use(c)]
extern crate cute;
#[macro_use]
extern crate maplit;

extern crate m6_key_set as key_set;

extern crate proc_macros; /* to avoid a cargo bug when cross-compiling (e.g. wasm) */

pub use proc_macros::make_vec_macro_rules;

make_vec_macro_rules!(vecdeq , std::collections::VecDeque, push_back);


pub mod utils;
pub mod algs;
pub mod regex;
pub mod dsl;
pub mod lexer;
pub mod parser;


pub use crate::utils::Stack;


////////////////////////////////////////////////////////////////////////////////
/// Unit Test

mod test {}
