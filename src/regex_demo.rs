#![allow(dead_code)]

use crate::utils::*;
use crate::state::*;
use crate::regex::*;
use crate::{ charset };

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
        let nfa_g = regex2nfa(&mut nfa_counter, &grammar_sample1);

        println!("NFA states (grammar1):");
        println!("{}", nfa_g);

        str_full_match_with_nfa(&nfa_g, "int");   // t
        str_full_match_with_nfa(&nfa_g, "intA");  // t
        str_full_match_with_nfa(&nfa_g, "23");    // t
        str_full_match_with_nfa(&nfa_g, "0A");    // f

        // DFA
        let dfa_g = DFAStateGraph::from_nfa_graph(
            nfa_g.clone(),
            Box::new(ascii_charset()),
            CharSet::new_getter
        );

        println!("{}", dfa_g);

        str_full_match_with_dfa(&dfa_g, "int");
        str_full_match_with_dfa(&dfa_g, "intA");
        str_full_match_with_dfa(&dfa_g, "23");
        str_full_match_with_dfa(&dfa_g, "0A");
    }

    fn display_sample_2() {
        // Regex
        let grammar_sample2 = gen_sample_grammar_2();
        println!("grammr2:");
        println!("{}", grammar_sample2);
        println!();

        // NFA
        let mut nfa_counter = gen_counter();
        let nfa_g = regex2nfa(&mut nfa_counter, &grammar_sample2);

        println!("NFA states (grammar2):");
        println!("{}", nfa_g);

        str_full_match_with_nfa(&nfa_g, "abc");        // t
        str_full_match_with_nfa(&nfa_g, "abcbbbcbc");  // t
        str_full_match_with_nfa(&nfa_g, "abde");       // f

        // DFA
        let dfa_g = DFAStateGraph::from_nfa_graph(
            nfa_g.clone(),
            Box::new(ascii_charset()),
            CharSet::new_getter
        );

        println!("{}", dfa_g);

        str_full_match_with_dfa(&dfa_g, "abc");
        str_full_match_with_dfa(&dfa_g, "abcbbbcbc");
        str_full_match_with_dfa(&dfa_g, "abde");
    }

    display_sample_1();
    display_sample_2();
}



#[cfg(test)]
mod test {
    #[test]
    fn test_regex2nfa2dfa() {
        use super::demo_regex_nfa_dfa;

        demo_regex_nfa_dfa();
    }
}