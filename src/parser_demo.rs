#![allow(non_snake_case)]
#![allow(unused_variables)]

use std::{cell::RefCell, str::FromStr};
use std::rc::Rc;

use num::{ BigRational, BigInt };

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
    declare_terminal! {id, intlit, lparen, rparen, sub, add, mul, div};
    use_epsilon!(ε);

    let expression = grammar![expression|
        expression:
        | sum;

        sum:
        | product addend;

        product:
        | pri multiplier;

        pri:
        | id;
        | intlit;
        | lparen sum rparen;

        multiplier:
        | mul pri multiplier;
        | div pri multiplier;
        | ε;

        addend:
        | add sum;
        | sub sum;
        | ε;
    |];

    let fst_sets = expression.first_sets();
    let foll_sets = expression.follow_sets(&fst_sets);
    let pred_sets
        = expression.prediction_ll1_sets(&fst_sets, &foll_sets);

    // println!("{:#?}", fst_sets);
    // println!("{:#?}", foll_sets);
    // println!("{:#?}", pred_sets);

    let parser = LL1Parser::new("expression", expression, basic_lexer());

    match parser.parse(" (1+3 * 56) -4 /2 -3") {
        Err(err) => println!("{}", err),
        //_ => ()
        Ok(ast) => println!("{}", ast.as_ref().borrow())
    }
}

#[cfg(yes)]
pub mod algebraic_expression {
    use std::{cell::RefCell, str::FromStr};
    use std::rc::Rc;

    use num::{ BigRational, BigInt };

    use crate::utils::Stack;
    use super::*; // require macro


    fn bigf0() -> BigRational {
        BigRational::from_integer(bigint0())
    }

    fn bigf1() -> BigRational {
        BigRational::from_integer(bigint1())
    }

    fn bigint1() -> BigInt {
        BigInt::from(1)
    }

    fn bigint0() -> BigInt {
        BigInt::from(0)
    }

    fn str2bf (s: &str) -> BigRational {
        BigRational::new(BigInt::from_str(s).unwrap(), bigint1())
    }

    // sum:
    // | product addend;
    fn eval_sum (root: &Rc<RefCell<AST>>) -> BigRational {
        declare_nonterminal! {expression, sum, product, addend, pri, multiplier};
        declare_terminal! {id, intlit, lparen, rparen, sub, add, mul, div};

        let root_ref = root.as_ref().borrow();

        let prod_value;
        if let Some(prod_node) = root_ref.get_elem(&product) {
            prod_value = eval_product(&prod_node.get_ast().unwrap())
        } else {
            prod_value = bigf0();
        }

        if let Some(addend_node) = root_ref.get_elem(&addend) {
            prod_value + eval_addend(&addend_node)
        } else {
            prod_value
        }
    }

    // product:
    // | pri multiplier;
    fn eval_product (root: &Rc<RefCell<AST>>) -> BigRational {
        declare_nonterminal! {expression, sum, product, addend, pri, multiplier};
        declare_terminal! {id, intlit, lparen, rparen, sub, add, mul, div};

        let root_ref = root.as_ref().borrow();

        let pri_value;
        if let Some(pri_node) = root_ref.get_elem(&pri) {
            pri_value = eval_pri(&pri_node.get_ast().unwrap())
        } else {
            pri_value = bigf0();
        }

        if let Some(multiplier_node) = root_ref.get_elem(&multiplier) {
            pri_value + eval_multiplier(&multiplier_node)
        } else {
            pri_value
        }
    }

    // pri:
    // | id;
    // | intlit;
    // | lparen sum rparen;
    fn eval_pri (root: &Rc<RefCell<AST>>) -> BigRational {
        declare_nonterminal! {expression, sum, product, addend, pri, multiplier};
        declare_terminal! {id, intlit, lparen, rparen, sub, add, mul, div};

        let root_ref = root.as_ref().borrow();

        if let Some(id_leaf) = root_ref.get_elem(&id) {
            return str2bf(id_leaf.get_token().unwrap().as_ref().value())
        } else if let Some(sum_node) = root_ref.get_elem(&sum) {
            return eval_sum(&sum_node.get_ast().unwrap());
        } else {
            // handle id
            unimplemented!()
        }
    }

    // multiplier:
    //   | (* | /) pri multiplier;
    //   | ε;
    // fn eval_multiplier(root: &Rc<RefCell<AST>>) -> Stack<(GramSym, BigRational)> {
    //     declare_nonterminal! {expression, sum, product, addend, pri, multiplier};
    //     declare_terminal! {id, intlit, lparen, rparen, sub, add, mul, div};

    //     let root_ref = root.as_ref().borrow();

    //     let pri_node
    //         = root_ref.get_elem(&pri).unwrap().get_ast().unwrap();
    //     let pri_value = eval_pri(pri_node);

    //     let op;
    //     if let Some(_) = root_ref.get_elem(&mul) {
    //         op = mul.clone();
    //     } else {
    //         op = div.clone();
    //     }

    //     let mut multiplier_stack
    //         = stack![(op, pri_value)];

    //     if let Some(v) = root_ref.get_elem(&multiplier) {
    //         multiplier_stack = eval_multiplier(v.get_ast().unwrap());
    //     }



    //     if let Some(_) = root_ref.get_elem(&mul) {
    //         return ;
    //     } else if let Some()
    // }

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
    use std::str::FromStr;

    use crate::{First, Follow};

    #[test]
    fn ll_expression_parser_works() {
        use super::{demo_grammar_ll_expression};

        demo_grammar_ll_expression()
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
        // let g1_pred_sets
        //      = g1.prediction_ll1_sets(&g1_first_sets, &g1_follow_sets);
        // println!("{:#?}", g1_pred_sets);

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

    #[test]
    fn test_bigint_rational_basic_op() {
        use num::{ BigRational, BigInt };

        let a = BigInt::from_str("100000000000000000000000000002").unwrap();
        let b = BigInt::from_str("20000000000000000000000000000").unwrap();
        let c: BigRational = BigRational::new(a.clone(), b.clone());
        let d: BigRational = BigRational::new(a, BigInt::from(1));

        println!("{}", (c + d).round());
    }
}
