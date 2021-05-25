////////////////////////////////////////////////////////////////////////////////
/////// Grammar DSL
/////// 效果差强人意，代价大概是
/////// ```
/////// #![allow(non_snake_case)]
/////// #![allow(unused_variables)]
///////```


pub struct Sym {}

#[macro_export]
macro_rules! declare_terminal {
    ($name:ident) => {
        let $name = $crate::algs::gram::GramSym::Terminal(
            stringify!($name).to_string()
        );
    };

    ( $($name:ident),* ) => {
        $(
            declare_terminal!{$name};
        )+
    };

    ( -: $lexer_name:ident :-  $($name:ident),* ) => {
        use $crate::regex::*;  // export all matcher sym, workaround solution

        let mut matcher_vec = vec![];
        let mut comment_m_vec = vec![];
        $(
            declare_terminal!{$name};

            let matcher_gen = concat_idents!($name, _m);

            let matcher = matcher_gen();

            if stringify!($name).ends_with("comment") {
                comment_m_vec.push(matcher);
            } else {
                matcher_vec.push(matcher);
            }
        )+

        let preprocor
        = $crate::lexer::CommentPreProcessor::new(
            "comment preprocor",
            comment_m_vec
        );

        let $lexer_name
        = $crate::lexer::Lexer::new(
            "generated by code",
            matcher_vec,
            vec![
                Box::new(preprocor)
            ]
        );

    }

}


/// 暂时什么都不做
#[macro_export]
macro_rules! declare_nonterminal {
    ($name:ident) => {
        let $name = $crate::algs::gram::GramSym::NonTerminal(
            stringify!($name).to_string()
        );
    };

    ( $($name:ident),* ) => {
        $(
            declare_nonterminal!($name);
        )+
    }
}

//  限制只能引入已存在的符号
#[macro_export]
macro_rules! use_epsilon {
    ($name:ident) => {
        debug_assert!(stringify!($name) == "ε", "epsilon sym just should be `ε`");
        declare_nonterminal!($name);
    };
}

/// 创建一个规则
/// 第一个产生式默认是入口的根语法
#[macro_export]
macro_rules! grammar {
    [$gram_name:ident|
         $($name:ident : $(| $($gramsym:ident)+ ;)+ )+
    |] =>
    {
        {
            let mut _grammar = $crate::algs::gram::Gram::new(stringify!($gram_name));
            $(
                $(
                    let mut gram_str_vec = Vec::<$crate::algs::gram::GramSym>::new();
                    let mut has_epsilon = false;

                    $(
                        if stringify!($gramsym) == "ε" {
                            has_epsilon = true;
                        } else {
                            gram_str_vec.push($gramsym.clone());
                        }
                    )+

                    let gramsymstr = if has_epsilon {
                        $crate::algs::gram::GramSymStr::Epsilon
                     } else {
                        $crate::algs::gram::GramSymStr::Str(gram_str_vec)
                     };

                    _grammar.insert_prod(
                        ($name.clone(), gramsymstr)
                    );
                )+
            )+

            _grammar
        }
    }
}

// #[macro_export]
// macro_rules! use_sym {
//     ($name:ident) => {
//         let $name = $crate::parser::Sym{};
//     };

//     ( $($name:ident),* ) => {  // 这个中间`，`的写法儿看起来有点儿tricky
//         $(
//             use_sym!($name);
//         )+
//     }
// }

#[macro_export]
macro_rules! First {
    ($name:ident | $($sym:ident)+) => {
        {
            let mut _set = indexset!{};
            $(
                let sym_s = stringify!($sym);

                let fstsym = if sym_s == "ε" {
                    $crate::algs::gram::FstSetSym::Epsilon
                } else {
                    $crate::algs::gram::FstSetSym::Sym(sym_s.to_string())
                };

                _set.insert(fstsym);
            )+

            ($crate::algs::gram::GramSym::NonTerminal(stringify!($name).to_string()), _set)
        }
    }
}

#[macro_export]
macro_rules! Follow {
    ($name:ident | $($sym:ident)+) => {
        {
            let mut _set = indexset!{};
            $(
                let sym_s = stringify!($sym);

                let fstsym = if sym_s == "NUL" {
                    $crate::algs::gram::FollSetSym::EndMarker
                } else {
                    $crate::algs::gram::FollSetSym::Sym(sym_s.to_string())
                };

                _set.insert(fstsym);
            )+

            ($crate::algs::gram::GramSym::NonTerminal(stringify!($name).to_string()), _set)
        }
    }
}
