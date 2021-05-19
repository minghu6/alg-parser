use std::{cell::RefCell, collections::{HashMap, HashSet}, fmt, rc::Rc, vec, iter};

use indexmap::{indexmap, indexset, IndexMap, IndexSet};
use itertools::Itertools;

use crate::utils::Stack;

use super::state::TransData;

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
        let sym_id = if self.is_terminal() {
            format!("<{}>", self.name())
        } else {
            format!("[{}]", self.name())
        };

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
                    let sym_id = if sym.is_terminal() {
                        format!("<{}>", sym.name())
                    } else {
                        format!("[{}]", sym.name())
                    };

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
    EndMarker,
}

impl PredSetSym {
    pub fn is_sym(&self) -> bool {
        match self {
            Self::Sym(_) => true,
            _ => false,
        }
    }
}

impl fmt::Display for PredSetSym {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Sym(value) => write!(f, "{}", value),
            Self::EndMarker => write!(f, "$"),
            Self::Epsilon => write!(f, "ε"),
        }
    }
}

pub type PredLL1Sets = HashMap<PredSetSym, IndexSet<GramProd>>;

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

    pub fn start_prod(&self) -> Option<&GramProd> {
        match self.productions.get_index(0) {
            Some(prod) => Some(prod),
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

    pub fn prediction_sets(&self, first_sets: &FirstSets, follow_sets: &FollowSets) -> PredLL1Sets {
        // 计算每个产生式的第一个Token的集合
        let pred_table_vec: Vec<(GramProd, HashSet<PredSetSym>)> = self
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
                                            .map(|fstsym| fstsym.to_pred_set_sym()),
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
            .filter(|((_, symstr), _term_set)| symstr.is_epsilon())
            .map(|(prod, _)| prod.clone())
            .collect::<IndexSet<GramProd>>();

        let endmark_set = pred_table_vec
            .iter()
            .filter(|(_prod, term_set)| term_set.contains(&PredSetSym::EndMarker))
            .map(|(prod, _)| prod.clone())
            .collect::<IndexSet<GramProd>>();

        let mut res = self
            .term_syms()
            .iter()
            .map(|term_sym| {
                let pred_sym = term_sym.to_pred_set_sym();

                let symstr_set = pred_table_vec
                    .iter()
                    .filter(|((_, symstr), term_set)| {
                        symstr.is_normal() && term_set.contains(&pred_sym)
                    })
                    .map(|(prod, _)| prod.clone())
                    .collect::<IndexSet<GramProd>>();

                (pred_sym, symstr_set)
            })
            .collect::<PredLL1Sets>();

        res.extend_one((PredSetSym::Epsilon, epsilon_set));
        res.extend_one((PredSetSym::EndMarker, endmark_set));

        res
    }

    /// 性能上可能应该需要一个Iterator Wrapper
    pub fn find_prod(&self, lhs: &GramSym)
    -> Vec<GramProd>
    {
        self.productions.iter().filter( |prod: &&GramProd| {
            &prod.0 == lhs
        }).cloned().collect_vec()
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



/// LR0Item: LR(0) Item
#[derive(Hash, PartialEq, Eq)]
pub struct LR0Item {
    prod: GramProd,
    pos: usize,  // dot pos in right side gram str
}

impl LR0Item {
    pub fn from_prod(prod: GramProd) -> Self {
        Self {
            prod,
            pos: 0
        }
    }

    /// the right hand side symbol immediately follows dot
    /// None indicated that dot points to the end
    pub fn rhs_sym(&self) -> Option<&GramSym> {
        let symstr = &self.prod.1;

        if let GramSymStr::Str(normal_vec) = symstr {
            if self.pos < normal_vec.len() {
                Some(&normal_vec[self.pos])
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn inc(&mut self) {
        if let Some(_) = self.rhs_sym() {
            self.pos += 1;
        }
    }

    pub fn lhs_sym(&self) -> &GramSym {
        &self.prod.0
    }

    pub fn dump(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (lhs, rhs) = &self.prod;
        write!(f, "{}: ", lhs)?;
        if let GramSymStr::Str(symstr) = rhs {
            let mut str_vec = symstr.iter().map(|sym| format!("{}", sym)).collect_vec();
            str_vec.insert(self.pos, "\u{00B7}".to_string());
            write!(f, "{}", str_vec.join(" "))
        } else {
            write!(f, "ε")
        }
    }
}

impl fmt::Debug for LR0Item {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.dump(f)
    }
}

impl fmt::Display for LR0Item {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.dump(f)
    }
}


/// _LR0Closure
type _LR0Closure = IndexSet<LR0Item>;

/// LR0Closure
pub struct LR0Closure {
    _closure: Rc<RefCell<_LR0Closure>>
}

impl From<_LR0Closure> for  LR0Closure {
    fn from(set: _LR0Closure) -> Self {
        Self {
            _closure: Rc::new(RefCell::new(set))
        }
    }
}


impl fmt::Display for LR0Closure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let set = self._closure.as_ref().borrow();

        for item in set.iter() {
            write!(f, "{}", item)?
        }

        Ok(())
    }
}

// impl TransData<LR0Item, GramSym> for LR0Closure {
//     fn insert(&mut self, pat: LR0Item) -> bool {
//         self._closure.as_ref().borrow_mut().insert(pat)
//     }

//     fn is_match(&self, pat: &GramSym) -> bool {
//         self
//     }

//     fn item2pat(&self, e: E) -> P {

//     }
// }

/*
* Impl Gram for LR0
*/
impl Gram {
    pub fn lr0closure(&self, items: impl Iterator<Item=LR0Item>) -> LR0Closure {
        let mut closure = indexset! {};

        //closure.extend(items);

        let mut new_item_vec = vec![];
        new_item_vec.extend(items);

        while !new_item_vec.is_empty() {
            let wait_for_calc = new_item_vec;
            new_item_vec = vec![];

            for closure_item in wait_for_calc.into_iter() {
                if let Some(rhs) = closure_item.rhs_sym() {
                    if rhs.is_nonterminal() {
                        new_item_vec.extend(
                            self.find_prod(&rhs)
                            .into_iter()
                            .map(|prod| LR0Item::from_prod(prod))
                            .filter(|item| !closure.contains(item))
                        )
                    }
                }

                closure.insert(closure_item);
            }
        }

        LR0Closure::from(closure)
    }

    pub fn start_item(&self) -> Option<LR0Item> {
        if let Some(prod) = self.start_prod() {
            return Some(LR0Item::from_prod(prod.clone()))
        }

        None
    }
}

#[cfg(test)]
#[allow(non_snake_case)]
mod test {
    use super::*;
    use crate::*;

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

    #[test]
    fn test_grammar_first_follow_set() {
        use crate::{First, Follow};

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
            Follow! {F| add mul rparen NUL },
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
            Follow! {F| h },
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
            Follow! {C| b g NUL h },
        ];

        for (sym, follset) in g3_folls_expected.iter() {
            debug_assert_eq!(g3_follow_sets.get(sym).unwrap(), follset);
        }
    }

    #[test]
    fn test_lr0() {
        declare_nonterminal! {E, E1, T, F};
        declare_terminal! {mul, add, n, lparen, rparen};

        let gram = grammar![G|
            E1:
            | E;

            E:
            | T;
            | E add T;

            T:
            | F;
            | T mul F;

            F:
            | n;
            | lparen E rparen;
        |];

        let start_set = vec![gram.start_item().unwrap()];
        let closure = gram.lr0closure(start_set.into_iter());

        println!("{}", closure);
    }

}