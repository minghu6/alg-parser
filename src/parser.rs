use std::{
    collections::{HashMap, HashSet},
};

use indexmap::{indexset, IndexSet};

use super::regex::PriRegexMatcher;

////////////////////////////////////////////////////////////////////////////////
/////// Grammar Symbol

/// 语法符号
#[derive(Hash, PartialEq, Eq, Clone, Debug)]
pub enum GramSym {
    Terminal(String),
    NonTerminal(String),
}

impl GramSym {
    pub fn name(&self) -> &str {
        match self {
            Self::Terminal(name) => name,
            Self::NonTerminal(name) => name,
        }
    }

    pub fn is_terminal(&self) -> bool {
        match self {
            Self::Terminal(_) => true,
            Self::NonTerminal(_) => false,
        }
    }

    pub fn is_nonterminal(&self) -> bool {
        !self.is_terminal()
    }

    pub fn to_fst(&self) -> FstSetSym {
        FstSetSym::Sym(self.name().to_string())
    }
}

/// 语法符号串
#[derive(Hash, PartialEq, Eq, Clone, Debug)]
pub enum GramSymStr {
    Normal(Vec<GramSym>),
    Epsilon,
}

impl GramSymStr {
    pub fn is_normal(&self) -> bool {
        match self {
            Self::Normal(_) => true,
            _ => false,
        }
    }

    pub fn get_normal(&self) -> Option<&Vec<GramSym>> {
        match self {
            Self::Normal(normal) => Some(normal),
            _ => None,
        }
    }
}

/// Grammar Production Type
type GramProd = (GramSym, GramSymStr);

#[derive(Debug)]
pub struct Gram {
    name: String,
    productions: IndexSet<GramProd>,
}

type FirstSets = HashMap<GramSym, HashSet<FstSetSym>>;
#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum FstSetSym {
    Sym(String),
    Epsilon,
}

impl FstSetSym {
    pub fn is_epsilon(&self) -> bool {
        match self {
            Self::Epsilon => true,
            _ => false,
        }
    }
}

type FollowSets = HashMap<GramSym, HashSet<FollSetSym>>;
#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum FollSetSym {
    Sym(String),
    EndMarker,
}

impl Gram {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            productions: indexset! {},
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn insert_prod(&mut self, prod: GramProd) {
        self.productions.insert(prod);
    }

    pub fn nonterm_syms(&self) -> Vec<GramSym> {
        self.productions
            .clone()
            .into_iter()
            .map(|(k, _v)| k)
            .collect::<Vec<GramSym>>()
    }

    pub fn syms(&self) -> Vec<GramSym> {
        let mut all_syms = HashSet::<GramSym>::new();

        for (sym, str) in self.productions.iter() {
            all_syms.insert(sym.clone());

            if let GramSymStr::Normal(normal_str) = str {
                all_syms.extend(normal_str.clone().into_iter());
            }
        }

        all_syms.into_iter().collect()
    }

    /// get start symbol， return None if prods is empty.
    pub fn start_sym(&self) -> Option<&GramSym> {
        match self.productions.get_index(0) {
            Some((k, _v)) => Some(k),
            None => None,
        }
    }

    pub fn first_sets(&self) -> FirstSets {
        let mut first_sets = self
            .syms()
            .into_iter()
            .map(|sym| match &sym {
                GramSym::NonTerminal(_) => (sym.clone(), hashset! {}),
                GramSym::Terminal(_) => (
                    sym.clone(),
                    hashset! { FstSetSym::Sym(sym.name().to_string()) },
                ),
            })
            .collect();

        let mut round = 0usize;
        loop {
            round += 1;
            if _calc_first_sets_turn(&self.productions, &mut first_sets) {
                break;
            }
        }

        if round > 2 {
            println!(
                "{}: calc firstsets additional rounds: {}",
                self.name(),
                round - 1
            );
        }

        // filter terminal entry
        // first_sets.into_iter()
        //     .filter(|(k, _v)| !k.is_terminal())
        //     .collect()
        first_sets
    }

    pub fn follow_sets(&self, first_sets: &FirstSets) -> FollowSets {
        let mut foll_sets = self
            .nonterm_syms()
            .into_iter()
            .map(|sym| (sym.clone(), hashset! {}))
            .collect::<FollowSets>();

        foll_sets
            .get_mut(self.start_sym().unwrap())
            .unwrap()
            .insert(FollSetSym::EndMarker);

        let mut round = 0usize;
        loop {
            round += 1;
            if _calc_follow_sets_turn(&self.productions, &mut foll_sets, first_sets) {
                break;
            }
        }
        if round > 2 {
            println!(
                "{}: calc followsets additional rounds: {}",
                self.name(),
                round - 1
            );
        }
        foll_sets
    }
}

fn _calc_first_sets_turn(
    productions: &IndexSet<GramProd>,
    first_sets: &mut HashMap<GramSym, HashSet<FstSetSym>>,
) -> bool
{
    let mut stable = true;

    for prod in productions.iter() {
        let x = &prod.0;
        let str = &prod.1;
        let mut x_first_set = first_sets.get(&x).unwrap().clone();
        let x_first_set_old_size = x_first_set.len();

        match str {
            GramSymStr::Epsilon => {
                x_first_set.insert(FstSetSym::Epsilon);
            }

            GramSymStr::Normal(normal_str) => {
                match x {
                    GramSym::Terminal(_) => {
                        x_first_set.insert(FstSetSym::Sym(x.name().to_string()));
                    }

                    GramSym::NonTerminal(_) => {
                        let mut str_iter = normal_str.iter();

                        loop {
                            let cur_sym;
                            if let Some(_sym) = str_iter.next() {
                                cur_sym = _sym;
                            } else {
                                x_first_set.insert(FstSetSym::Epsilon);
                                break;
                            }

                            let cur_sym_first_set
                                = first_sets.get(&cur_sym).unwrap();
                            x_first_set.extend(cur_sym_first_set.clone().into_iter());

                            if cur_sym_first_set.contains(&FstSetSym::Epsilon) {
                                // continue
                            } else {
                                break;
                            }
                        }
                    }
                }
            }
        }

        if x_first_set_old_size < x_first_set.len() {
            stable = false;
        }

        // 更新first_sets
        first_sets
            .get_mut(x)
            .unwrap()
            .extend(x_first_set.into_iter());
    }

    stable
}

fn _calc_follow_sets_turn(
    productions: &IndexSet<GramProd>,
    follow_sets: &mut FollowSets,
    first_sets: &FirstSets,
) -> bool
{
    let mut stable = true;

    for prod in productions.iter() {
        let x = &prod.0;
        let str = &prod.1;
        let x_follow_set = follow_sets.get(&x).unwrap().clone();

        if str.is_normal() && x.is_nonterminal() {
            let normal_str = str.get_normal().unwrap();
            let lastpos = normal_str.len() - 1;
            for (i, str_x) in normal_str.iter().enumerate().rev() {
                if str_x.is_terminal() {
                    continue;
                }

                let mut here_set = follow_sets.get(str_x).unwrap().clone();
                let here_set_old_size = here_set.len();

                // apply follow rule 2+
                let mut j = i;
                while j < lastpos {
                    let next_first = first_sets.get(&normal_str[j + 1]).unwrap();
                    here_set.extend(
                        next_first
                            .clone()
                            .into_iter()
                            .filter(|x| !x.is_epsilon())
                            .map(|x| match x {
                                FstSetSym::Sym(value) => FollSetSym::Sym(value),
                                _ => unreachable!(),
                            }),
                    );

                    // 没有epsilon转换就停止穿透
                    if !next_first.iter().any(|x| x.is_epsilon()) { break }
                    j += 1;
                }
                // apply follow rule 3
                if (i + 1..normal_str.len())
                    .map(|j| &normal_str[j])
                    .all(|x| first_sets.get(x).unwrap().contains(&FstSetSym::Epsilon))
                {
                    here_set.extend(x_follow_set.clone().into_iter());

                    if stable && here_set_old_size < here_set.len() {
                        stable = false;
                    }
                }

                // rewrite
                follow_sets
                    .get_mut(str_x)
                    .unwrap()
                    .extend(here_set.into_iter());
            }
        }
    }

    stable
}

////////////////////////////////////////////////////////////////////////////////
/////// Grammar DSL
/// Grammar DSL
/// 效果差强人意，代价大概是
/// ```
/// #![allow(non_snake_case)]
/// #![allow(unused_variables)]
///```
///////

pub struct Sym {}

#[macro_export]
macro_rules! declare_terminal {
    ($name:ident) => {
        let $name = $crate::parser::GramSym::Terminal(
            stringify!($name).to_string()
        );
    };

    ( $($name:ident),* ) => {
        $(
            declare_terminal!($name);
        )+
    }
}

#[macro_export]
macro_rules! declare_nonterminal {
    ($name:ident) => {
        let $name = $crate::parser::GramSym::NonTerminal(
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
#[macro_export]
macro_rules! grammar {
    [$gram_name:ident|
         $($name:ident : $(| $($gramsym:ident)+ ;)+ )+
    |] =>
    {
        {
            let mut _grammar = $crate::parser::Gram::new(stringify!($gram_name));
            $(
                $(
                    let mut gram_str_vec = Vec::<$crate::parser::GramSym>::new();
                    let mut has_epsilon = false;

                    $(
                        if stringify!($gramsym) == "ε" {
                            has_epsilon = true;
                        } else {
                            gram_str_vec.push($gramsym.clone());
                        }
                    )+

                    let gramsymstr = if has_epsilon {
                        $crate::parser::GramSymStr::Epsilon
                     } else {
                        $crate::parser::GramSymStr::Normal(gram_str_vec)
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

#[macro_export]
macro_rules! use_sym {
    ($name:ident) => {
        let $name = $crate::parser::Sym{};
    };

    ( $($name:ident),* ) => {  // 这个中间`，`的写法儿看起来有点儿tricky
        $(
            use_sym!($name);
        )+
    }
}

#[macro_export]
macro_rules! First {
    ($name:ident | $($sym:ident)+) => {
        {
            let mut _set = hashset!{};
            $(
                let sym_s = stringify!($sym);

                let fstsym = if sym_s == "ε" {
                    $crate::parser::FstSetSym::Epsilon
                } else {
                    $crate::parser::FstSetSym::Sym(sym_s.to_string())
                };

                _set.insert(fstsym);
            )+

            ($crate::parser::GramSym::NonTerminal(stringify!($name).to_string()), _set)
        }
    }
}

#[macro_export]
macro_rules! Follow {
    ($name:ident | $($sym:ident)+) => {
        {
            let mut _set = hashset!{};
            $(
                let sym_s = stringify!($sym);

                let fstsym = if sym_s == "NUL" {
                    $crate::parser::FollSetSym::EndMarker
                } else {
                    $crate::parser::FollSetSym::Sym(sym_s.to_string())
                };

                _set.insert(fstsym);
            )+

            ($crate::parser::GramSym::NonTerminal(stringify!($name).to_string()), _set)
        }
    }
}

////////////////////////////////////////////////////////////////////////////////
/////// Lexer
pub struct Lexer {
    pub name: String,
    pub token_type_map: HashMap<String, PriRegexMatcher>, // epsilon 不参与
}


////////////////////////////////////////////////////////////////////////////////
/////// Unit Test

#[cfg(test)]
mod test {}
