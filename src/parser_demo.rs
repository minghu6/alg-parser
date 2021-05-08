#![allow(non_snake_case)]
#![allow(unused_variables)]

use super::parser::*;
use super::*; // require macro

/// 一个简单四则运算的语法，为了支持LL(1)的文法，消除了左递归
/// ```antlr
/// expression:
///   | sum;
///
/// sum:
///   | product addend;
///
/// product:
///   | pri multiplier;
///
/// pri:
///   | Id;
///   | IntLit;
///   | LPAREN sum RPAREN;
///
/// multiplier:
///   | (* | /) pri multiplier;
///   | ε;
///
/// addend:
///   | (+ | -) sum;
///   | ε;
/// ```
///
pub fn demo_grammar_ll_expression() {
    declare_nonterminal! {expression, sum, product, addend, pri, multiplier};
    declare_terminal! {Id, IntLit, LPAREN, RPAREN, sub, add, mul, div};
    use_epsilon!(ε);

    let expression = grammar![expression|
        expression:
        | sum;

        sum:
        | product addend;

        product:
        | pri multiplier;

        pri:
        | Id;
        | IntLit;
        | LPAREN sum RPAREN;

        multiplier:
        | mul pri multiplier;
        | div pri multiplier;
        | ε;

        addend:
        | add sum;
        | div sum;
        | ε;
    |];

    let fst_sets = expression.first_sets();
    let foll_sets = expression.follow_sets(&fst_sets);

    println!("{:#?}", fst_sets);
    println!("{:#?}", foll_sets);
}


pub fn grammar_demo1() -> Gram {
    declare_nonterminal!(E);
    declare_nonterminal!(T);
    declare_nonterminal!(E1);
    declare_nonterminal!(T1);
    declare_nonterminal!(F);
    declare_terminal!(add);
    declare_terminal!(mul);
    declare_terminal!(lparen);
    declare_terminal!(rparen);
    declare_terminal!(id);
    use_epsilon!(ε);

    // Yes, it just looks like a Haskell EDSL!
    grammar![G1|
        E:
        | T E1;

        E1:
        | add T E1;
        | ε;

        T:
        | F T1;

        T1:
        | mul F T1;
        | ε;

        F:
        | lparen E rparen;
        | id;
    |]
}

pub fn grammar_demo2() -> Gram {
    declare_nonterminal! {S, B, C, D, E, F};
    declare_terminal! {a, b, c, h, f, g};
    use_epsilon!(ε);

    grammar![G2|
        S:
        | a B D h;

        B:
        | c C;

        C:
        | b C;
        | ε;

        D:
        | E F;

        E:
        | g;
        | ε;

        F:
        | f;
        | ε;
    |]
}

pub fn grammar_demo3() -> Gram {
    declare_nonterminal! {S, A, B, C};
    declare_terminal! {a, b, d, g, h};
    use_epsilon!(ε);

    grammar![G3|
        S:
        | A C B;
        | C b b;
        | B a;

        A:
        | d a;
        | B C;

        B:
        | g;
        | ε;

        C:
        | h;
        | ε;
    |]
}

#[cfg(test)]
mod test {
    use crate::{First, Follow};

    #[ignore]
    #[test]
    fn ll_expression_parser_works() {
        // use super::ll_expression_parser;

        // ll_expression_parser();
    }

    #[test]
    fn test_grammar_first_follow_set() {
        use super::{grammar_demo1, grammar_demo2, grammar_demo3};

        ///////////////////////////////////////////////////////////////////////
        /////// G1 Grammar
        let g1 = grammar_demo1();
        // println!("{:#?}", g1);

        let g1_first_sets = g1.first_sets();
        // println!("{:#?}", g1_first_sets);

        let g1_fsts_expected = vec![
            First! {E| lparen id },
            First! {T| lparen id },
            First! {F| lparen id },
            First! {E1| add ε },
            First! {T1| mul ε },
        ];

        for (sym, firstset) in g1_fsts_expected.iter() {
            debug_assert_eq!(g1_first_sets.get(sym).unwrap(), firstset);
        }

        let g1_follow_sets = g1.follow_sets(&g1_first_sets);
        let g1_folls_expected = vec![
            Follow! {E| rparen NUL },
            Follow! {E1| rparen NUL },
            Follow! {T| add rparen NUL },
            Follow! {T1| add rparen NUL },
            Follow! {F| add mul rparen NUL }
        ];

        for (sym, follset) in g1_folls_expected.iter() {
            debug_assert_eq!(g1_follow_sets.get(sym).unwrap(), follset);
        }

        // println!("{:#?}", g1_follow_sets);

        ///////////////////////////////////////////////////////////////////////
        /////// G2 Grammar
        let g2 = grammar_demo2();
        let g2_fsts_expected = vec![
            First! {S| a },
            First! {B| c },
            First! {C| b ε },
            First! {D| g f ε},
            First! {E| g ε },
            First! {F| f ε },
        ];
        let g2_first_sets = g2.first_sets();
        for (sym, firstset) in g2_fsts_expected.iter() {
            debug_assert_eq!(g2_first_sets.get(sym).unwrap(), firstset);
        }
        let g2_follow_sets = g2.follow_sets(&g2_first_sets);
        let g2_folls_expected = vec![
            Follow! {S| NUL },
            Follow! {B| g f h },
            Follow! {C| g f h },
            Follow! {D| h },
            Follow! {E| f h },
            Follow! {F| h }
        ];

        for (sym, follset) in g2_folls_expected.iter() {
            debug_assert_eq!(g2_follow_sets.get(sym).unwrap(), follset, "{:?}", sym);
        }

        ///////////////////////////////////////////////////////////////////////
        /////// G3 Grammar
        let g3 = grammar_demo3();
        let g3_fsts_expected = vec![
            First! {S| d g h ε b a },
            First! {A| d g h ε },
            First! {B| g ε },
            First! {C| h ε },
        ];
        let g3_first_sets = g3.first_sets();
        for (sym, firstset) in g3_fsts_expected.iter() {
            debug_assert_eq!(g3_first_sets.get(sym).unwrap(), firstset);
        }
        let g3_follow_sets = g3.follow_sets(&g3_first_sets);
        let g3_folls_expected = vec![
            Follow! {S| NUL },
            Follow! {A| h g NUL },
            Follow! {B| a NUL h g },
            Follow! {C| b g NUL h }
        ];

        for (sym, follset) in g3_folls_expected.iter() {
            debug_assert_eq!(g3_follow_sets.get(sym).unwrap(), follset);
        }

    }
}
