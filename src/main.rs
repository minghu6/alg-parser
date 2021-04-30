use std::rc::Rc;

use alg_parser::data::*;
use alg_parser::utils::*;
use alg_parser::*;

///! Regex: int | [a-zA-z][a-zA-Z0-9]* | [0-9]*
fn gen_sample_grammar_1() -> GrammarNode {
    let mut root = GrammarNode::create_or_node();

    // int
    let mut int_node = GrammarNode::create_and_node();
    int_node.add_child(GrammarNode::from_charset(charset! {i-i}));
    int_node.add_child(GrammarNode::from_charset(charset! {n-n}));
    int_node.add_child(GrammarNode::from_charset(charset! {t-t}));

    // identifier
    let mut id_node = GrammarNode::create_and_node();
    id_node.add_child(GrammarNode::from_charset(charset! { a-z | A-Z }));
    id_node.add_child(GrammarNode::from_charset_repeat_times(
        charset! { a-z | A-Z | 0-9 },
        (0, usize::MAX),
    ));

    // number literals
    let literal_node = GrammarNode::from_charset_repeat_times(charset! { 0-9 }, (1, usize::MAX));

    root.add_child(int_node);
    root.add_child(id_node);
    root.add_child(literal_node);

    root
}

fn main() {
    // Regex
    let grammar_sample1 = gen_sample_grammar_1();
    println!("grammr1:");
    println!("{}", grammar_sample1);
    println!();

    // NFA
    let mut nfa_counter = gen_counter();
    let (begin_state, _end_state) = regex2nfa(&mut nfa_counter, &grammar_sample1);

    let begin_state_1 = Rc::clone(&begin_state);
    let sample1_begin_state_ref = (*begin_state_1).borrow();
    println!("NFA states (grammar1):");
    println!("{:?}", sample1_begin_state_ref);

    // match_with_nfa(&sample1_begin_state_ref, "int");
    // match_with_nfa(&sample1_begin_state_ref, "intA");
    // match_with_nfa(&sample1_begin_state_ref, "23");
    // match_with_nfa(&sample1_begin_state_ref, "0A");

    // DFA
    let mut dfa_counter = gen_counter();
    let alphabet = charset! { a-z | A-Z | 0-9 };

    let begin_state_2 = Rc::clone(&begin_state);
    let dfa_states = nfa2dfa(&mut dfa_counter, begin_state_2, &alphabet);
    let begin_dfa_state_1 = dfa_states.get(0).unwrap();

    let begin_dfa_state_1_ref = (*begin_dfa_state_1).borrow();
    println!("{:?}", begin_dfa_state_1_ref);
}
