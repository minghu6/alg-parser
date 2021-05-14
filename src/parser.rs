use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    fmt,
    rc::{Rc},
    vec,
    iter
};

use indexmap::{indexmap, indexset, IndexMap, IndexSet};
use itertools::Itertools;

use super::utils::Stack;
use super::regex::*;


////////////////////////////////////////////////////////////////////////////////
/////// Grammar Symbol

/// GramSym: 语法符号
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

    pub fn to_fst_set_sym(&self) -> FstSetSym {
        FstSetSym::Sym(self.name().to_string())
    }

    pub fn to_pred_set_sym(&self) -> PredSetSym {
        PredSetSym::Sym(self.name().to_string())
    }
}

impl fmt::Display for GramSym {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let sym_id
        = if self.is_terminal() { format!("<{}>", self.name()) }
          else { format!("[{}]", self.name()) };

        write!(f, "{}", sym_id)
    }
}

impl fmt::Display for Stack<GramSym> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, item) in self.stack_iter().enumerate() {
            if i < self.len() - 1 {
                write!(f, "{} ", item)?
            } else {
                write!(f, "{}", item)?
            }
        }

        Ok(())
    }
}

/// GramSymStr: 语法符号串
#[derive(Hash, PartialEq, Eq, Clone, Debug)]
pub enum GramSymStr {
    Str(Vec<GramSym>),
    Epsilon,
}

impl GramSymStr {
    pub fn is_normal(&self) -> bool {
        match self {
            Self::Str(_) => true,
            _ => false,
        }
    }

    pub fn is_epsilon(&self) -> bool {
        match self {
            Self::Epsilon => true,
            _ => false,
        }
    }

    pub fn get_normal(&self) -> Option<&Vec<GramSym>> {
        match self {
            Self::Str(normal) => Some(normal),
            _ => None,
        }
    }
}

impl fmt::Display for GramSymStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Epsilon => write!(f, "epsilon"),
            Self::Str(str_vec) => {
                for (i, sym) in str_vec.iter().enumerate() {
                    let sym_id
                    = if sym.is_terminal() { format!("<{}>", sym.name()) }
                      else { format!("[{}]", sym.name()) };

                    if i < str_vec.len() - 1 {
                        write!(f, "{} ", sym_id)?
                    } else {
                        write!(f, "{}", sym_id)?
                    }
                }
                Ok(())
            }
        }
    }
}

/// GramProd: 语法产生式的类型
type GramProd = (GramSym, GramSymStr);


/// FstSets: FirstSets 类型
type FirstSets = HashMap<GramSym, HashSet<FstSetSym>>;
#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum FstSetSym {
    Sym(String),
    Epsilon,
}

// FstSetSym: First Sets 符号类型
impl FstSetSym {
    pub fn is_epsilon(&self) -> bool {
        match self {
            Self::Epsilon => true,
            _ => false,
        }
    }

    pub fn to_pred_set_sym(&self) -> PredSetSym {
        match self {
            Self::Epsilon => PredSetSym::Epsilon,
            Self::Sym(value) => PredSetSym::Sym(value.clone()),
        }
    }
}

/// FollSets: Follow Sets 符号类型
type FollowSets = HashMap<GramSym, HashSet<FollSetSym>>;
#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum FollSetSym {
    Sym(String),
    EndMarker,
}

impl FollSetSym {
    pub fn to_fst_set_sym(&self) -> Option<FstSetSym> {
        match self {
            Self::EndMarker => None,
            Self::Sym(value) => Some(FstSetSym::Sym(value.clone())),
        }
    }

    pub fn to_pred_set_sym(&self) -> PredSetSym {
        match self {
            Self::EndMarker => PredSetSym::EndMarker,
            Self::Sym(value) => PredSetSym::Sym(value.clone()),
        }
    }
}

/// PredSetSym: 预测集符号
#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum PredSetSym {
    Sym(String),
    Epsilon,
    EndMarker
}

impl PredSetSym {
    pub fn is_sym(&self) -> bool {
        match self {
            Self::Sym(_) => true,
            _ => false
        }
    }
}

impl fmt::Display for PredSetSym {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Sym(value) => write!(f, "{}", value),
            Self::EndMarker => write!(f, "$"),
            Self::Epsilon => write!(f, "ε")
        }
    }
}

type PredLL1Sets = HashMap<PredSetSym, IndexSet<GramProd>>;

pub fn display_predsets(predsets: &PredLL1Sets) {
    for (predsym, prodset) in predsets.iter() {
        for (leftsym, symstr) in prodset.iter() {
            println!("{}: {} => {}", predsym, leftsym, symstr)
        }
        println!();
    }
}

/// Gram: 语法类型
#[derive(Debug, Clone)]
pub struct Gram {
    name: String,
    productions: IndexSet<GramProd>,
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

    // move method
    pub fn extend_gram(&mut self, income_gram: Gram) {
        self.extend(income_gram.into_iter());
    }

    pub fn insert_prod(&mut self, prod: GramProd) {
        self.productions.insert(prod);
    }

    pub fn get_prod(&self, sym: &GramSym) -> Vec<GramProd> {
        self.productions
            .iter()
            .filter(|(left_sym, _)| left_sym == sym)
            .cloned()
            .collect()
    }

    pub fn nonterm_syms(&self) -> Vec<GramSym> {
        self.productions
            .clone()
            .into_iter()
            .map(|(k, _v)| k)
            .collect::<Vec<GramSym>>()
    }

    pub fn term_syms(&self) -> Vec<GramSym> {
        let mut term_syms = HashSet::<GramSym>::new();

        for (_sym, str) in self.productions.iter() {
            if let GramSymStr::Str(normal_str) = str {
                for str_sym in normal_str.iter() {
                    if str_sym.is_terminal() {
                        term_syms.insert(str_sym.clone());
                    }
                }
            }
        }

        term_syms.into_iter().collect()
    }

    pub fn syms(&self) -> Vec<GramSym> {
        let mut all_syms = HashSet::<GramSym>::new();

        for (sym, str) in self.productions.iter() {
            all_syms.insert(sym.clone());

            if let GramSymStr::Str(normal_str) = str {
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

    pub fn sym_has_epsilon(&self, sym: &GramSym) -> bool {
        self.productions
            .iter()
            .any(|(x, str)| x == sym && *str == GramSymStr::Epsilon)
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

    pub fn prediction_sets(
        &self,
        first_sets: &FirstSets,
        follow_sets: &FollowSets,
    ) -> PredLL1Sets {
        // 计算每个产生式的第一个Token的集合
        let pred_table_vec: Vec<(GramProd, HashSet<PredSetSym>)>
        = self
            .productions
            .iter()
            .map(|(sym, symstr)| {
                let term_set = match symstr {
                    GramSymStr::Epsilon => follow_sets
                        .get(&sym)
                        .unwrap()
                        .iter()
                        .map(|x| x.to_pred_set_sym())
                        .collect(),
                    GramSymStr::Str(normal_str_vec) => match normal_str_vec.get(0).unwrap() {
                        GramSym::Terminal(value) => {
                            hashset! { PredSetSym::Sym(value.clone()) }
                        }
                        GramSym::NonTerminal(_) => {
                            let mut sub_term_set = hashset! {};
                            let mut str_iter = normal_str_vec.iter();

                            loop {
                                let sub_sym;
                                match str_iter.next() {
                                    Some(_sym) => sub_sym = _sym,
                                    None => {
                                        break;
                                    }
                                }

                                if sub_sym.is_nonterminal() {
                                    sub_term_set.extend(
                                        first_sets
                                            .get(sub_sym)
                                            .unwrap()
                                            .iter()
                                            .map(|fstsym| fstsym.to_pred_set_sym())
                                    );

                                    if !self.sym_has_epsilon(&sym) {
                                        break;
                                    }
                                }

                                sub_term_set.insert(sub_sym.to_pred_set_sym());
                            }

                            sub_term_set
                        }
                    },
                };
                ((sym.clone(), symstr.clone()), term_set)
            })
            .collect();

        let epsilon_set = pred_table_vec
            .iter()
            .filter(
                |((_, symstr), _term_set)|
                symstr.is_epsilon()
            )
            .map(|(prod, _)| prod.clone())
            .collect::<IndexSet<GramProd>>();

        let endmark_set = pred_table_vec
            .iter()
            .filter(
                |(_prod, term_set)|
                term_set.contains(&PredSetSym::EndMarker)
            )
            .map(|(prod, _)| prod.clone())
            .collect::<IndexSet<GramProd>>();

        let mut res= self.term_syms()
            .iter()
            .map(|term_sym| {
                let pred_sym = term_sym.to_pred_set_sym();

                let symstr_set = pred_table_vec
                    .iter()
                    .filter(
                        |((_, symstr), term_set)|
                        symstr.is_normal()
                        && term_set.contains(&pred_sym)
                    )
                    .map(|(prod, _)| prod.clone())
                    .collect::<IndexSet<GramProd>>();

                (pred_sym, symstr_set)
            })
            .collect::<PredLL1Sets>();

            res.extend_one((PredSetSym::Epsilon, epsilon_set));
            res.extend_one((PredSetSym::EndMarker, endmark_set));

            res
    }
}

///```none
///     1.If X is terminal, then FIRST(X) is {X}.
///     2.If X → ε is a production, then add ε to FIRST(X).
///     3.If X is nonterminal and X →Y1 Y2 ... Yk.
///       从求取First(Y1)开始,如果Y1的某个产生式有ε,就继续求取First(Y2)......,
///       如果整个串都已经耗尽了,就把ε也加入.
///```
fn _calc_first_sets_turn(
    productions: &IndexSet<GramProd>,
    first_sets: &mut HashMap<GramSym, HashSet<FstSetSym>>,
) -> bool {
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

            GramSymStr::Str(normal_str) => {
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

                            let cur_sym_first_set = first_sets.get(&cur_sym).unwrap();
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

/// ```none
/// 1. Place $ in FOLLOW(S), where S is the start symbol and $ is the input right endmarker.
/// 2. If there is a production A -> αΒβ, then everything in FIRST(β), except for ε, is placed in FOLLOW(B).
/// 3. If there is a production A -> αΒ, or a production A -> αΒβ where FIRST(β) contains ε(i.e., β -> ε),
///    then everything in FOLLOW(A) is in FOLLOW(B).
///    如同计算First一样迭代，如果整个串都已经耗尽了,就把ε也加入Follow(B).
/// ```
fn _calc_follow_sets_turn(
    productions: &IndexSet<GramProd>,
    follow_sets: &mut FollowSets,
    first_sets: &FirstSets,
) -> bool {
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
                    if !next_first.iter().any(|x| x.is_epsilon()) {
                        break;
                    }
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

impl Extend<GramProd> for Gram {
    fn extend<I: IntoIterator<Item = GramProd>>(&mut self, iter: I) {
        for item in iter {
            self.productions.insert(item);
        }
    }
}

impl iter::IntoIterator for Gram {
    type Item = GramProd;
    type IntoIter = indexmap::set::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.productions.into_iter()
    }
}

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
        let $name = $crate::parser::GramSym::Terminal(
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

        $(
            declare_terminal!{$name};

            let matcher_gen = concat_idents!($name, _m);

            let matcher = matcher_gen();
            matcher_vec.push(matcher);
        )+

        let $lexer_name = $crate::parser::Lexer::new("generated by code", matcher_vec);

    }

}


/// 暂时什么都不做
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
/// 第一个产生式默认是入口的根语法
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
                        $crate::parser::GramSymStr::Str(gram_str_vec)
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

/// Token
#[derive(Debug, Clone)]
pub struct Token {
    name: String,
    value: String,
}

impl Token {
    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn value(&self) -> &str {
        &self.value
    }

    pub fn to_fst_set_sym(&self) -> FstSetSym {
        FstSetSym::Sym(self.name.clone())
    }

    /// To GramSym::Terminal
    pub fn to_gram_sym(&self) -> GramSym {
        GramSym::Terminal(self.name.clone())
    }

    pub fn to_pred_set_sym(&self) -> PredSetSym {
        PredSetSym::Sym(self.name.clone())
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.to_gram_sym(), self.value())
    }
}

/// Lexer
#[derive(Debug)]
pub struct Lexer {
    pub name: String,
    pub tokens_map: IndexMap<String, PriRegexMatcher>,
    // epsilon 不参与
    // IndexMap: 顺序决定优先级
}

impl Lexer {
    pub fn new(name: &str, tokens: Vec<PriRegexMatcher>) -> Self {
        let tokens_map = tokens
            .into_iter()
            .map(|x| (x.name().to_string(), x))
            .collect();

        let name = name.to_string();

        Self { name, tokens_map }
    }

    pub fn tokenize(&self, source: &str) -> Vec<Token> {
        if source.len() == 0 {
            return vec![];
        }

        let mut tokens = vec![];
        let mut cache = String::new();
        let source_chars: Vec<char> = source.chars().collect();
        let mut i = 0;

        while i < source_chars.len() {
            cache.push(source_chars[i]);

            // turn round matcher to try match
            for (order, (token_type, matcher)) in self.tokens_map.iter().enumerate() {
                let mut j = i;
                while j < source_chars.len() - 1 && matcher.is_match(cache.trim()) {
                    j += 1;
                    cache.push(source_chars[j]);
                }

                // 找到一个token匹配
                if j > i {
                    // 退回一个贪心多吞噬的字符
                    // case 1: token stream exausted: token match or unmatch
                    // case 2: token stream unexauted: token unmatch
                    // for token unmatch
                    if !matcher.is_match(cache.trim()) {
                        j -= 1;
                        cache.pop();
                    }

                    // 查看是否有更高优先级的matcher可以匹配到token, 如果有就放弃这个匹配
                    // 这主要是为了让关键字： `if`，`return` 匹配优先于标识符的匹配
                    if self.tokens_map.iter()
                        .enumerate()
                        .filter(|(m_order, _)| *m_order < order)
                        .any(|(_, (_, m, ))| m.is_match(cache.trim()))
                    {
                        // restore cache
                        (i..j).into_iter().for_each(|_| { cache.pop(); });
                        continue;
                    }

                    tokens.push(Token {
                        name: token_type.clone(),
                        value: cache.trim().to_string(),
                    });
                    cache = String::new();
                    i = j;

                    break;
                }
            }

            i += 1;
        }

        // tail recycle
        for (token_type, matcher) in self.tokens_map.iter() {
            if matcher.is_match(cache.trim()) {
                tokens.push(Token {
                    name: token_type.clone(),
                    value: cache.trim().to_string(),
                });
            }
        }

        tokens
    }
}

pub fn basic_lexer() -> Lexer {
    let matchers = vec![
        intlit_m(),
        id_m(),
        lparen_m(),
        rparen_m(),
        add_m(),
        sub_m(),
        mul_m(),
        div_m(),
    ];

    Lexer::new("basic lexer", matchers)
}

////////////////////////////////////////////////////////////////////////////////
/////// AST


/// AST Node
#[derive(Debug)]
pub enum ASTNode {
    Tree(Rc<RefCell<AST>>),
    Leaf(Rc<Token>),
}

impl ASTNode {
    pub fn dump(&self, f: &mut fmt::Formatter, padlevel: usize) -> fmt::Result {
        let padding = "  ".repeat(padlevel);

        match self {
            Self::Leaf(token) =>
                writeln!(f, "{}({}){}", padding, padlevel, *token),
            Self::Tree(ast) => {
                let ast_ref = ast.as_ref().borrow();
                ast_ref.dump(f, padlevel)
            }
        }
    }

    pub fn get_token(&self) -> Option<&Rc<Token>> {
        match self {
            Self::Leaf(token) => Some(token),
            _ => None
        }
    }

    pub fn get_ast(&self) -> Option<&Rc<RefCell<AST>>> {
        match self {
            Self::Tree(ast) => Some(ast),
            _ => None
        }
    }
}

/// AST
#[derive(Debug)]
pub struct AST {
    /// AST's grammar type
    sym: GramSym,
    elems: IndexMap<GramSym, ASTNode>,
}

impl AST {
    pub fn new(sym: &GramSym) -> Self {
        Self {
            sym: sym.clone(),
            elems: indexmap! {},
        }
    }

    pub fn sym(&self) -> &GramSym {
        &self.sym
    }

    pub fn elem_syms(&self) -> Vec<GramSym> {
        self.elems.keys().cloned().collect_vec()
    }

    pub fn get_elem(&self, sym: &GramSym) -> Option<&ASTNode> {
        self.elems.get(sym)
    }

    pub fn insert_leaf(&mut self, token: Token) {
        let leaf_name = token.to_gram_sym();
        let leaf = ASTNode::Leaf(Rc::new(token));

        self.elems.insert(leaf_name, leaf);
    }

    pub fn insert_tree(&mut self, tree: Rc<RefCell<AST>>) {
        let tree_name = tree.as_ref().borrow().sym().clone();
        let tree = ASTNode::Tree(tree);

        self.elems.insert(tree_name, tree);
    }

    fn dump(&self, f: &mut fmt::Formatter, padlevel: usize) -> fmt::Result {
        let padding = "  ".repeat(padlevel);

        writeln!(f, "{}({}){}: ", padding, padlevel, self.sym())?;

        for (_elem_sym, elem_node) in self.elems.iter() {
            elem_node.dump(f, padlevel + 1)?;
        }

        Ok(())
    }

    /// There are no circle dependency on Tree
    fn copy_tree(&self) -> Rc<RefCell<Self>> {
        let mut new_tree = Self::new(self.sym());

        for (sym, node) in self.elems.iter() {
            match node {
                ASTNode::Leaf(token) => {
                    new_tree.elems.insert(
                        sym.clone(),
                        ASTNode::Leaf(token.clone())
                    );
                },
                ASTNode::Tree(subtree) => {
                    new_tree.insert_tree(
                        subtree.as_ref().borrow().copy_tree()
                    );
                }
            }
        }

        Rc::new(RefCell::new(new_tree))
    }
}


impl fmt::Display for AST {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.dump(f, 0)
    }
}


////////////////////////////////////////////////////////////////////////////////
/////// Parser Trait
pub trait Parser {
    fn parse(&self, source: &str) -> Result<Rc<RefCell<AST>>, String>;
}

////////////////////////////////////////////////////////////////////////////////
/////// Evaluator Trait
pub trait Evaluator<RES> {
    fn eval(&self, ast: &Rc<RefCell<AST>>) -> Result<RES, String>;
}


////////////////////////////////////////////////////////////////////////////////
/////// 带回溯的 变形 LL(1) Parser
pub struct LL1Parser {
    name: String,
    gram: Gram,
    lexer: Lexer,
    prediction_sets: PredLL1Sets,
}

type LL1ParseStatesStack = Vec<(Rc<RefCell<AST>>, Stack<GramSym>)>;

impl LL1Parser {
    pub fn new(name: &str, gram: Gram, lexer: Lexer) -> Self {
        let first_sets = gram.first_sets();
        let follow_sets = gram.follow_sets(&first_sets);
        let prediction_sets = gram.prediction_sets(&first_sets, &follow_sets);

        Self {
            name: name.to_string(),
            gram,
            lexer,
            prediction_sets,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn get_pred_str(&self, predsym: &PredSetSym, leftsym_expcted: &GramSym) -> Option<Vec<&GramSymStr>> {
        let prods_set;
        match self.prediction_sets.get(predsym) {
            Some(value) => prods_set = value,
            None => {
                return None;
            }
        };

        let matched_symstr_vec = prods_set
            .into_iter()
            .filter(|(leftsym, _symstr)| leftsym == leftsym_expcted)
            .map(|(_leftsym, symstr)| symstr)
            .collect_vec();

        if matched_symstr_vec.is_empty() {
            None
        } else {
            Some(matched_symstr_vec)
        }
    }
}

// Impl for Parser
impl Parser for LL1Parser {
    fn parse(&self, source: &str) -> Result<Rc<RefCell<AST>>, String> {
        let tokens = self.lexer.tokenize(source);
        if tokens.is_empty() {
            return Err("empty tokens".to_string());
        }

        println!("tokens: {:#?}\n", tokens);
        println!("LL(1): ");

        let start_sym = self.gram.start_sym().unwrap();
        let root = Rc::new(RefCell::new(AST::new(&start_sym)));

        let mut res
        = Err(format!(
            "Unexpected token: `{}` for root grammar",
            tokens[0]
        ));

        // Check root， 分支预测
        if let Some(poss_brs)
        = self.get_pred_str(
            &tokens[0].to_pred_set_sym(),
            start_sym
        )
        {

            let mut poss_brs_iter = poss_brs.into_iter();

            while let Some(symstr) = poss_brs_iter.next() {

                if let GramSymStr::Str(gramsym_vec) = symstr {
                    // gramsym_vec rev for stack
                    let states_stack = vec![
                        (root.clone(), Stack::from(gramsym_vec.clone()))
                    ];

                    if let Ok(_res) = _ll1_parse(&self, &tokens[..], states_stack) {
                        res = Ok(_res);
                        break;
                    }
                }
            }
        }

        res
    }
}


/// Result: <ASTRoot, UnsupportedTokenType>
fn _ll1_parse(
    parser: &LL1Parser,
    tokens: &[Token],
    mut states_stack: LL1ParseStatesStack
) -> Result<Rc<RefCell<AST>>, String> {
    if tokens.is_empty() {
        return Err("empty tokens".to_string());
    }

    debug_assert!(
        !states_stack.is_empty(),
        "states stack should keep filled by invoker"
    );

    let root = states_stack[0].0.clone();
    let tokenslen = tokens.len();
    let tokenslastpos = tokenslen - 1;
    let mut i = 0;

    while let Some((cur_ast, mut symstr_stack)) = states_stack.pop() {
        println!(">>> `{} => ...{}`", cur_ast.as_ref().borrow().sym(), symstr_stack);

        // 分支匹配，遇到终结符直接匹配，遇到非终结符就入栈回到起点
        while let Some(right_sym) = symstr_stack.pop() {
            if i > tokenslastpos {
                if let Some(_)  // 检查当前产生式是否允许结束
                = parser.get_pred_str(&PredSetSym::EndMarker, &right_sym)
                {
                    return Ok(root);
                } else {
                    return Err(format!(
                        "Unfinished production: {:?}",
                        (
                            cur_ast.as_ref().borrow().sym(),
                            symstr_stack
                        )
                    ));
                }
            }

            if right_sym.is_terminal() {
                println!("? eat terminal: `{}`", right_sym);

                if right_sym == tokens[i].to_gram_sym() {
                    cur_ast.as_ref().borrow_mut().insert_leaf(tokens[i].clone());

                    // cosume a token
                    println!("! eaten token: {:?}", tokens[i]);
                    i += 1;
                } else {
                    return Err(format!(
                        "Unmatched token{}, a {} expected",
                        tokens[i],
                        right_sym
                    ));
                }
            }
            else {  // handle nonterminal
                // LL(1)的带回溯的分支预测
                // case-1 (just goon)
                // case-2 (push, loop continue)
                // case-3 (return res<ok/error>)
                // case-4 (error)

                if let Some(poss_brs)
                = parser.get_pred_str(&tokens[i].to_pred_set_sym(), &right_sym)
                {
                    let sub_sym_tree = Rc::new(RefCell::new(AST::new(&right_sym)));
                    cur_ast
                        .as_ref()
                        .borrow_mut()
                        .insert_tree(sub_sym_tree.clone());
                    states_stack.push((cur_ast.clone(), symstr_stack.clone()));

                    // case-1
                    // 正经的LL(1) 匹配
                    if poss_brs.len() == 1 {
                        let pred_symstr = poss_brs[0];
                        // 在计算predsets时已经把epsilon str的情况单独提出来了
                        let norm_sym_vec = pred_symstr.get_normal().unwrap();

                        println!(
                            "?>! `{}`: `{}`",
                            right_sym, GramSymStr::Str(norm_sym_vec.clone())
                        );

                        states_stack.push(
                                (sub_sym_tree, Stack::from(norm_sym_vec.clone()))
                        );

                        break;
                    }

                    // case-2, 需要实现树的复制
                    // 回溯的LL(1) 匹配
                    // else， 前面必定返回，所以不必加额外的else block来消耗宝贵的缩进资源
                    let mut poss_brs_iter = poss_brs.into_iter();

                    while let Some(pred_symstr) = poss_brs_iter.next() {
                        let norm_sym_vec = pred_symstr.get_normal().unwrap();

                        println!(
                            "?>? `{}`: `{}`",
                            right_sym, GramSymStr::Str(norm_sym_vec.clone())
                        );

                        states_stack.push(
                            (sub_sym_tree.clone(), Stack::from(norm_sym_vec.clone()))
                        );
                        // 实际上是拷贝了整个树
                        let new_states_stack
                            = _copy_ll1_states_stack(&states_stack);

                        let (_, new_tokens) = tokens.split_at(i);

                        // right_sym 完成匹配
                        if let Ok(_res)
                            = _ll1_parse(parser, new_tokens, new_states_stack) {

                            return Ok(_res);
                        } else {
                            println!(
                                "*<x `{}`",
                                right_sym
                            );
                        }

                        // clean env
                        states_stack.pop();
                    }

                    return Err(format!(
                        "All possible branch has been failed for grammar: `{}`",
                        right_sym
                    ));
                }

                // case-3
                // 如果找不到符合条件的分支，就试一下right_sym是否存在epsilon转换
                else if let Some(_)
                = parser.get_pred_str(&PredSetSym::Epsilon, &right_sym) {
                    println!(
                        " ... skipp epsilon: `{}` (token: {})",
                        right_sym,
                        tokens[i]
                    );
                    // just skip epsilon
                }

                // case-4
                else {
                    return Err(format!(
                        "Unexpected token: `{}` for grammar: `{}`",
                        tokens[i], right_sym
                    ));
                }


            }
        }  // end while

        println!();
    }

    if i < tokenslastpos {
        return Err(format!(
            "Tokens remains: `{:?}`",
            tokens.get(i..tokenslen).unwrap()
        ));
    }

    Ok(root)
}


fn _copy_ll1_states_stack(states_stack: &LL1ParseStatesStack) -> LL1ParseStatesStack
{
    if states_stack.is_empty() { return vec![] }

    let (root, root_stack) = &states_stack[0];
    let new_root = root.as_ref().borrow().copy_tree();
    let mut new_states_stack
        = vec![(new_root.clone(), root_stack.clone())];

    let mut parent = new_root;

    for (tree, sym_stack) in states_stack.iter().skip(1) {
        // 根据旧的AST的sym，查找新的AST
        // 原理是statesstack每个元素的AST是层层相连的
        // (ith AST 是 i+1th AST的直接父母)
        let new_tree = parent.as_ref().borrow().get_elem(
            tree.as_ref().borrow().sym()
        ).unwrap().get_ast().unwrap().clone();

        new_states_stack.push(
            (new_tree.clone(), sym_stack.clone())
        );

        parent = new_tree;
    }

    new_states_stack
}

////////////////////////////////////////////////////////////////////////////////
/////// Unit Test

#[cfg(test)]
mod test {
    #[test]
    fn test_basic_lexer() {
        use super::basic_lexer;

        let lexer = basic_lexer();
        //println!("{:#?}", lexer);

        //println!("{:#?}", lexer.tokenize(" 010+32*(52+6) "));
        println!("{:#?}", lexer.tokenize(" \"I am\" in string\" "));
    }
}
