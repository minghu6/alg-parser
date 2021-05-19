#![allow(non_snake_case)]
#![allow(unused_variables)]

use std::{cell::RefCell, collections::VecDeque, rc::Rc, str::FromStr};

use num::{BigInt, BigRational};

use super::algs::{
    gram::*,
};
use super::lexer::*;
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
pub fn demo_grammar_ll_algb_sum() -> (impl Parser, impl Evaluator<BigRational>) {
    declare_nonterminal! {expression, sum, product, addend, pri, multiplier};
    declare_terminal! {id, intlit, lparen, rparen, sub, add, mul, div};
    use_epsilon!(ε);

    let sum = grammar![sum|
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
        | add product addend;
        | sub product addend;
        | ε;
    |];

    let fst_sets = sum.first_sets();
    let foll_sets = sum.follow_sets(&fst_sets);
    let pred_sets = sum.prediction_sets(&fst_sets, &foll_sets);

    // println!("{:#?}", fst_sets);
    // println!("{:#?}", foll_sets);
    // println!("{:#?}", pred_sets);

    let parser = LL1Parser::new("sum", sum, basic_lexer());

    fn bigf0() -> BigRational {
        BigRational::from_integer(bigint0())
    }

    fn bigint1() -> BigInt {
        BigInt::from(1)
    }

    fn bigint0() -> BigInt {
        BigInt::from(0)
    }

    fn str2bf(s: &str) -> BigRational {
        BigRational::new(BigInt::from_str(s).unwrap(), bigint1())
    }

    pub struct AlgSumEval {}

    impl Evaluator<BigRational> for AlgSumEval {
        fn eval(&self, ast: &Rc<RefCell<AST>>) -> Result<BigRational, String> {
            Ok(eval_sum(ast))
        }
    }

    // sum:
    // | product addend;
    fn eval_sum(root: &Rc<RefCell<AST>>) -> BigRational {
        declare_nonterminal! {expression, sum, product, addend, pri, multiplier};
        declare_terminal! {id, intlit, lparen, rparen, sub, add, mul, div};

        let root_ref = root.as_ref().borrow();

        let mut prod_value;
        if let Some(prod_node) = root_ref.get_elem(&product) {
            prod_value = eval_product(&prod_node.get_ast().unwrap())
        } else {
            prod_value = bigf0();
        }

        if let Some(addend_node) = root_ref.get_elem(&addend) {
            for (sym, ratio) in eval_addend(&addend_node.get_ast().unwrap()).into_iter() {
                if sym == add {
                    prod_value += ratio
                } else {
                    prod_value -= ratio
                }
            }
        }

        prod_value
    }

    // product:
    // | pri multiplier;
    fn eval_product(root: &Rc<RefCell<AST>>) -> BigRational {
        declare_nonterminal! {expression, sum, product, addend, pri, multiplier};
        declare_terminal! {id, intlit, lparen, rparen, sub, add, mul, div};

        let root_ref = root.as_ref().borrow();

        let pri_node = root_ref.get_elem(&pri).unwrap();
        let pri_value = eval_pri(&pri_node.get_ast().unwrap());

        if let Some(multiplier_node) = root_ref.get_elem(&multiplier) {
            let multiplier_deq = eval_multiplier(&multiplier_node.get_ast().unwrap());

            let mut acc = pri_value;
            for (sym, ratio) in multiplier_deq.into_iter() {
                if sym == mul {
                    acc *= ratio;
                } else {
                    acc /= ratio;
                }
            }

            acc
        } else {
            pri_value
        }
    }

    // pri:
    // | id;
    // | intlit;
    // | lparen sum rparen;
    fn eval_pri(root: &Rc<RefCell<AST>>) -> BigRational {
        declare_nonterminal! {expression, sum, product, addend, pri, multiplier};
        declare_terminal! {id, intlit, lparen, rparen, sub, add, mul, div};

        let root_ref = root.as_ref().borrow();

        if let Some(int_leaf) = root_ref.get_elem(&intlit) {
            return str2bf(int_leaf.get_token().unwrap().as_ref().value());
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
    fn eval_multiplier(root: &Rc<RefCell<AST>>) -> VecDeque<(GramSym, BigRational)> {
        // 将来可以在一个单独有GramSym实现的Proc Macro里面实现MIXIN，现在只能每次都引用这些符号
        declare_nonterminal! {expression, sum, product, addend, pri, multiplier};
        declare_terminal! {id, intlit, lparen, rparen, sub, add, mul, div};

        let root_ref = root.as_ref().borrow();

        let pri_node = root_ref.get_elem(&pri).unwrap().get_ast().unwrap();
        let pri_value = eval_pri(pri_node);

        let op;
        if let Some(_) = root_ref.get_elem(&mul) {
            op = mul.clone();
        } else {
            op = div.clone();
        }

        let mut multiplier_queue = vecdeq![(op, pri_value)];

        if let Some(v) = root_ref.get_elem(&multiplier) {
            multiplier_queue.extend(eval_multiplier(v.get_ast().unwrap()));
        }

        multiplier_queue
    }

    /*
      addend:
        | add product addend;
        | sub product addend;
        | ε;
    */
    fn eval_addend(root: &Rc<RefCell<AST>>) -> VecDeque<(GramSym, BigRational)> {
        declare_nonterminal! {expression, sum, product, addend, pri, multiplier};
        declare_terminal! {id, intlit, lparen, rparen, sub, add, mul, div};

        let root_ref = root.as_ref().borrow();

        let op;
        if let Some(_) = root_ref.get_elem(&add) {
            op = add.clone();
        } else {
            op = sub.clone();
        }

        let prod_node = root_ref.get_elem(&product).unwrap();
        let prod_value = eval_product(&prod_node.get_ast().unwrap());

        match root_ref.get_elem(&addend) {
            Some(addend_node) => {
                let mut acc_deq = eval_addend(addend_node.get_ast().unwrap());
                acc_deq.push_front((op, prod_value));
                acc_deq
            }
            None => {
                vecdeq![(op, prod_value)]
            }
        }
    }

    (parser, AlgSumEval {})
}

// An Interestiong Function export to other project
pub fn parse_algb_ratio(strlit: &str) -> Result<BigRational, String> {
    let (parser, evalor) = demo_grammar_ll_algb_sum();

    match parser.parse(strlit) {
        Err(err) => Err(err),
        Ok(ast) => evalor.eval(&ast),
    }
}

pub fn grammar_demo1() -> Gram {
    declare_nonterminal! {
        E,
        T,
        T1,
        E1,
        F
    }

    declare_terminal! {
        add, mul, lparen, rparen, id
    };

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

/// parse java
/// ``` java
/// class Mammal{
///     boolean canSpeak(){
///         return true;
///     }

///     void speak(){
///         println("mammal speaking...");
///     }
/// }

/// class Cow extends Mammal{
///     void speak(){
///         println("moo~~ moo~~");
///     }
/// }

/// class Sheep extends Mammal{
///     void speak(){
///         println("mee~~ mee~~");
///     }
/// }

/// // this is a line comment
/// Mammal a = Cow();
/// Mammal b = Sheep();

// // canSpeak() inherit from super class
// println("a.canSpeak() : " + a.canSpeak());
// println("b.canSpeak() : " + b.canSpeak());
/// ```
/// Antlr4语法的转化式：
/// A* => As ; As: A | A As | ε
/// B -> A+ => B -> A As
/// A? =>  ; A1: A | ε
///
pub fn demo_full() -> LL1Parser {
    declare_nonterminal! {
        prog,
        block,
        block_statements,
        expression,
        sum,
        product,
        addend,
        pri,
        multiplier,
        statement,
        block_statement,

        function_declaration,
        class_declaration,

        extend_class,

        type_type_or_void,
        type_type,
        pri_type,
        formal_parameters,
        function_body,
        expression_list,
        function_call,
        algb_pri,
        lit,
        class_or_interface_type,
        class_body,
        class_body_declarations,
        class_body_declaration,
        member_declaration,
        fieldDeclaration,
        variable_initializer,
        variable_declarator,
        variable_declarators,
        expression_1,
        bop
    };
    declare_terminal! {
        -: lexer :-
        // comment
        slash_block_comment,
        slash_line_comment,

        // key
        r#return,
        boolean,
        void,
        extends,
        class,

        id,
        intlit,
        strlit,
        lparen,
        rparen,
        sub,
        add,
        mul,
        div,
        semi,
        lbrace,
        rbrace,
        comma,
        dot,
        assign
    };
    use_epsilon!(ε);

    let mut mini_java = grammar![mini_java|
        prog:
        | block;
        | block_statements;

        block:
        | lbrace block_statements rbrace;

        block_statements:
        | block_statement block_statements;
        | ε;

        block_statement:
        | statement;
        | function_declaration;
        | class_declaration;

        class_declaration:
        | class id extend_class class_body;

        class_body:
        | lbrace rbrace;
        | lbrace class_body_declarations rbrace;

        class_body_declarations:
        | class_body_declaration class_body_declarations;
        | ε;

        class_body_declaration:
        | member_declaration;
        | semi;

        member_declaration:
        | function_declaration;
        | fieldDeclaration;

        fieldDeclaration:
        | variable_declarators semi;

        variable_declarators:
        | type_type variable_declarator;
        | type_type variable_declarator comma variable_declarators;

        variable_declarator:
        | id;
        | id assign variable_initializer;

        variable_initializer:
        | expression;

        extend_class:
        | extends type_type;
        | ε;

        function_declaration:
        | type_type_or_void id formal_parameters
          function_body;
        | id formal_parameters
          function_body;

        formal_parameters:
        | lparen rparen;

        function_body:
        | block;
        | semi;

        type_type_or_void:
        | type_type;
        | void;

        type_type:
        | pri_type;
        | class_or_interface_type;

        class_or_interface_type:
        | id;
        | id dot class_or_interface_type;

        pri_type:
        | boolean;

        statement:
        | semi;
        | expression semi;
        | fieldDeclaration;
        | r#return expression semi;

        expression:
        | sum;
        | pri expression_1;
        | function_call;

        expression_1:
        | dot function_call expression_1;
        | bop pri expression_1;
        | ε;

        bop:
        | add;
        | sub;

        pri:
        | lit;
        | id;

        lit:
        | intlit;
        | strlit;

        function_call:
        | id lparen expression_list rparen;

        expression_list:
        | expression;
        | expression comma expression_list;
        | ε;
    |];

    let sum_rule = grammar![sum|
        sum:
        | product addend;

        product:
        | algb_pri multiplier;

        algb_pri:
        | id;
        | intlit;
        | lparen sum rparen;

        multiplier:
        | mul algb_pri multiplier;
        | div algb_pri multiplier;
        | ε;

        addend:
        | add product addend;
        | sub product addend;
        | ε;
    |];

    mini_java.extend_gram(sum_rule);

    let fst_sets = mini_java.first_sets();
    let foll_sets = mini_java.follow_sets(&fst_sets);
    let pred_sets = mini_java.prediction_sets(&fst_sets, &foll_sets);

    // println!("{:#?}", mini_java);
    // println!("{:#?}", fst_sets);
    // println!("{:#?}", foll_sets);
    display_predsets(&pred_sets);

    LL1Parser::new("mini java", mini_java, lexer)
}

#[cfg(test)]
mod test {
    use num::{BigInt, BigRational, FromPrimitive};

    use crate::parser::Parser;

    #[test]
    fn test_demo_full() {
        use super::demo_full;

        let parser = demo_full();

        // define method
        let exp1 = r#"
        /**
        mammal.play 演示面向对象编程：继承和多态。
        */

        class Mammal{
            boolean canSpeak(){
                return true;
            }

            void speak(){
                println("mammal speaking...");
            }
        }

        class Cow extends Mammal{
            void speak(){
                println("moo~~ moo~~");
            }
        }

        class Sheep extends Mammal{
            void speak(){
                println("mee~~ mee~~");
            }
        }

        //将子类的实例赋给父类的变量
        Mammal a = Cow();
        Mammal b = Sheep();

        //canSpeak()方法是继承的
        println("a.canSpeak() : " + a.canSpeak());
        println("b.canSpeak() : " + b.canSpeak());

        //下面两个的叫声会不同。在运行期动态绑定方法。
        a.speak();  //会打印牛叫
        b.speak();  //会打印羊叫
        "#;

        match parser.parse(exp1) {
            Err(err) => panic!("{}", err),
            Ok(ast) => {
                println!("{}", ast.as_ref().borrow());
            }
        }
    }

    #[test]
    fn ll_expression_parser_works() {
        use super::parser::{Evaluator, Parser};
        use crate::parser_demo::{demo_grammar_ll_algb_sum, parse_algb_ratio};

        let (parser, evalor) = demo_grammar_ll_algb_sum();

        match parser.parse(" (1+3 * 56) - 4/ 2 +3") {
            Err(err) => println!("{}", err),
            //_ => ()
            Ok(ast) => {
                println!("{}", ast.as_ref().borrow());

                println!("eval result: {}", evalor.eval(&ast).unwrap());
            }
        }

        assert_eq!(
            parse_algb_ratio(" 7 * 3 -2").unwrap(),
            BigRational::from_integer(BigInt::from_i32(19).unwrap())
        );
    }

    #[test]
    fn test_bigint_rational_basic_op() {
        use num::{BigInt, BigRational};
        use std::str::FromStr;

        let a = BigInt::from_str("100000000000000000000000000002").unwrap();
        let b = BigInt::from_str("20000000000000000000000000000").unwrap();
        let c: BigRational = BigRational::new(a.clone(), b.clone());
        let d: BigRational = BigRational::new(a, BigInt::from(1));

        println!("{}", (c + d).round());
    }
}
