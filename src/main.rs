#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(mixed_script_confusables)]

use alg_parser::parser_demo::*;
use alg_parser::regex_demo::*;


fn demo_ll_parser() {
    demo_grammar_ll_expression();
}

fn main() {
    //demo_regex_nfa_dfa();
    demo_ll_parser();
}
