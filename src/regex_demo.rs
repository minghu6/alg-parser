use std::rc::Rc;

use crate::utils::*;
use crate::regex::*;
use crate::{ charset, simple_regex };

/// writen in verbose format
fn demo_regex_nfa_dfa() {
    ///! Regex: int | [a-zA-z][a-zA-Z0-9]* | [0-9]*
    fn gen_sample_grammar_1() -> RegexNode {
        let mut root = RegexNode::create_or_node();

        // int
        let mut int_node = RegexNode::create_and_node();
        int_node.add_child(RegexNode::from_charset(charset! {i-i}));
        int_node.add_child(RegexNode::from_charset(charset! {n-n}));
        int_node.add_child(RegexNode::from_charset(charset! {t-t}));

        // identifier
        let mut id_node = RegexNode::create_and_node();
        id_node.add_child(RegexNode::from_charset(charset! { a-z | A-Z }));
        id_node.add_child(RegexNode::from_charset_repeat_times(
            charset! { a-z | A-Z | 0-9 },
            (0, usize::MAX),
        ));

        // number literals
        let literal_node =
            RegexNode::from_charset_repeat_times(charset! { 0-9 }, (1, usize::MAX));

        root.add_child(int_node);
        root.add_child(id_node);
        root.add_child(literal_node);

        root
    }

    ///! Regex: a[a-zA-Z0-9]*bc
    fn gen_sample_grammar_2() -> RegexNode {
        let mut root = RegexNode::create_and_node();

        // a
        let a_node = RegexNode::from_charset(charset! {a-a});

        // letterdigits
        let letterdigits_node =
            RegexNode::from_charset_repeat_times(charset! { a-z | A-Z | 0-9 }, (0, usize::MAX));

        // b
        let b_node = RegexNode::from_charset(charset! {b-b});

        // c
        let c_node = RegexNode::from_charset(charset! {c-c});

        root.add_child(a_node);
        root.add_child(letterdigits_node);
        root.add_child(b_node);
        root.add_child(c_node);

        root
    }

    fn display_sample_1() {
        // Regex
        let grammar_sample1 = gen_sample_grammar_1();
        println!("grammr1:");
        println!("{}", grammar_sample1);
        println!();

        // NFA
        let mut nfa_counter = gen_counter();
        let begin_state = regex2nfa(&mut nfa_counter, &grammar_sample1);

        let begin_state_1 = Rc::clone(&begin_state);
        let sample1_begin_state_ref = (*begin_state_1).borrow();
        println!("NFA states (grammar1):");
        println!("{:?}", sample1_begin_state_ref);

        match_with_nfa(&sample1_begin_state_ref, "int");
        match_with_nfa(&sample1_begin_state_ref, "intA");
        match_with_nfa(&sample1_begin_state_ref, "23");
        match_with_nfa(&sample1_begin_state_ref, "0A");

        // DFA
        let mut dfa_counter = gen_counter();
        let alphabet = charset! { a-z | A-Z | 0-9 };

        let begin_state_2 = Rc::clone(&begin_state);
        let dfa_states = nfa2dfa(&mut dfa_counter, begin_state_2, &alphabet);
        let begin_dfa_state_1 = dfa_states;

        let begin_dfa_state_1_ref = (*begin_dfa_state_1).borrow();
        println!("{:?}", begin_dfa_state_1_ref);

        match_with_dfa(&begin_dfa_state_1_ref, "int");
        match_with_dfa(&begin_dfa_state_1_ref, "intA");
        match_with_dfa(&begin_dfa_state_1_ref, "23");
        match_with_dfa(&begin_dfa_state_1_ref, "0A");
    }

    fn display_sample_2() {
        // Regex
        let grammar_sample2 = gen_sample_grammar_2();
        println!("grammr2:");
        println!("{}", grammar_sample2);
        println!();

        // NFA
        let mut nfa_counter = gen_counter();
        let begin_state = regex2nfa(&mut nfa_counter, &grammar_sample2);

        let begin_state_2 = Rc::clone(&begin_state);
        let sample2_begin_state_ref = (*begin_state_2).borrow();
        println!("NFA states (grammar2):");
        println!("{:?}", sample2_begin_state_ref);

        match_with_nfa(&sample2_begin_state_ref, "abc");
        match_with_nfa(&sample2_begin_state_ref, "abcbbbcbc");
        match_with_nfa(&sample2_begin_state_ref, "abde");

        // DFA
        let mut dfa_counter = gen_counter();
        let alphabet = charset! { a-z | A-Z | 0-9 };

        let begin_state_2 = Rc::clone(&begin_state);
        let dfa_states = nfa2dfa(&mut dfa_counter, begin_state_2, &alphabet);
        let begin_dfa_state_2 = dfa_states;

        let begin_dfa_state_2_ref = (*begin_dfa_state_2).borrow();
        println!("{:?}", begin_dfa_state_2_ref);

        match_with_dfa(&begin_dfa_state_2_ref, "abc");
        match_with_dfa(&begin_dfa_state_2_ref, "abcbbbcbc");
        match_with_dfa(&begin_dfa_state_2_ref, "abde");
    }

    display_sample_1();
    display_sample_2();
}

fn basic_regex() {

    simple_regex!{ ["a-zb-xc"](+) ["z"]() ["z"]() };

}