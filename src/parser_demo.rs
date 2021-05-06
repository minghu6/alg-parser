
use super::parser::*;
use super::*;  // require macro

/// 一个简单四则运算的语法，为了支持LL(1)的文法，消除了左递归
/// ```antlr
/// expression:
/// | sum;
///
/// sum:
/// | product addend;
///
/// product:
/// | pri multiplier;
///
/// pri:
/// | Id;
/// | IntLit;
/// | LPAREN sum RPAREN;
///
/// multiplier:
/// | (* | /) pri multiplier;
/// | ε;
///
/// addend:
/// | (+ | -) sum;
/// | ε;
/// ```
///
pub fn grammar_ll_expression() -> LLGrammarGeneral {
    sign_llrule!(sum);
    create_llrule!(
        expression:
          | sum;
    );

    sign_llrule!(product);
    sign_llrule!(addend);
    impl_llrule!(
        sum:
          | product addend;
    );

    sign_llrule!(multiplier);
    sign_llrule!(pri);
    impl_llrule!(
        product:
          | pri multiplier;
    );

    use_llterminal!(id);
    use_llterminal!(int);
    use_llterminal!(lparen);
    use_llterminal!(rparen);
    impl_llrule!(
        pri:
          | id;
          | int;
          | lparen sum rparen;
    );

    use_llterminal!(mul);
    use_llterminal!(div);
    use_llepsilon!(ε);
    impl_llrule!(
        multiplier:
          | mul pri multiplier;
          | div pri multiplier;
          | ε;
    );

    use_llterminal!(add);
    use_llterminal!(sub);
    impl_llrule!(
        addend:
          | add sum;
          | sub sum;
          | ε;
    );

    expression
}

pub fn ll_expression_parser() {
    let expression_genl = grammar_ll_expression();
    // println!("{:?}", expression_genl);

    let expression_first_sets = first_sets(expression_genl.get_rule().unwrap());

    println!("{:?}", expression_first_sets);
}

#[cfg(test)]
mod test {
    #[test]
    fn ll_expression_parser_works() {
        use super::ll_expression_parser;

        ll_expression_parser();
    }
}