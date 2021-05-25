use std::{cell::RefCell, fmt, iter, rc::{Rc}};

use indexmap::{indexmap, indexset, IndexSet, IndexMap};
use itertools::Itertools;

use crate::{stack};

use super::
{super:: {
        utils::{
            gen_counter
        }
    }, state::{DFAState, DFAStateGraph, DFATransition, TransData, TryNxtRes}};

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

    pub fn to_foll_set_sym(&self) -> FollSetSym {
        FollSetSym::Sym(self.name().to_string())
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
pub type GramProd = (GramSym, GramSymStr);

pub fn format_gramprod(prod: &GramProd) -> String {
    format!(
        "{} => {}",
        format!("{}", prod.0),
        format!("{}", prod.1),
    )
}

/// FstSets: FirstSets 类型
pub type FstSets = IndexMap<GramSym, IndexSet<FstSetSym>>;
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

impl fmt::Display for FstSetSym {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Sym(value) => write!(f, "{}", value),
            Self::Epsilon => write!(f, "ε"),
        }
    }
}

/// FollSets: Follow Sets 符号类型
pub type FollSets = IndexMap<GramSym, IndexSet<FollSetSym>>;
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

impl fmt::Display for FollSetSym {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Sym(value) => write!(f, "{}", value),
            Self::EndMarker => write!(f, "$"),
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

pub type PredLL1Sets = IndexMap<PredSetSym, IndexSet<GramProd>>;

pub fn display_predsets(predsets: &PredLL1Sets) {
    for (predsym, prodset) in predsets.iter() {
        for (leftsym, symstr) in prodset.iter() {
            println!("{}: {} => {}", predsym, leftsym, symstr)
        }

        println!();
    }
}

pub fn display_follsets(foll_sets: &FollSets) {
    for (lhs_sym, prodset) in foll_sets.iter() {
        print!("{}: \n", lhs_sym);

        print!("{}\n",
            prodset.iter().map(|x| format!("| {}", x)).collect_vec().join("\n")
        );

        println!();
    }
}

pub fn display_fstsets(fst_sets: &FstSets) {
    for (lhs_sym, prodset) in fst_sets.iter() {
        print!("{}: \n", lhs_sym);

        print!("{}\n",
            prodset.iter().map(|x| format!("| {}", x)).collect_vec().join("\n")
        );

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

    pub fn get_prod_index(&self, prod_ind: usize) -> Option<&GramProd> {
        self.productions.get_index(prod_ind)
    }

    pub fn nonterm_syms(&self) -> Vec<GramSym> {
        self.productions
            .clone()
            .into_iter()
            .map(|(k, _v)| k)
            .collect::<Vec<GramSym>>()
    }

    pub fn term_syms(&self) -> Vec<GramSym> {
        let mut term_syms = IndexSet::<GramSym>::new();

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
        let mut all_syms = IndexSet::<GramSym>::new();

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

    pub fn first_sets(&self) -> FstSets {
        let mut first_sets = self
            .syms()
            .into_iter()
            .map(|sym| match &sym {
                GramSym::NonTerminal(_) => (sym.clone(), indexset! {}),
                GramSym::Terminal(_) => (
                    sym.clone(),
                    indexset! { FstSetSym::Sym(sym.name().to_string()) },
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

    pub fn follow_sets(&self, first_sets: &FstSets) -> FollSets {
        let mut foll_sets = self
            .nonterm_syms()
            .into_iter()
            .map(|sym| (sym.clone(), indexset! {}))
            .collect::<FollSets>();

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

    pub fn prediction_sets(&self, first_sets: &FstSets, follow_sets: &FollSets) -> PredLL1Sets {
        // 计算每个产生式的第一个Token的集合
        let pred_table_vec: Vec<(GramProd, IndexSet<PredSetSym>)> = self
            .productions
            .iter()
            .map(|(sym, symstr)| {
                let term_set = match symstr {
                    GramSymStr::Epsilon => follow_sets
                        .get(sym)
                        .unwrap()
                        .iter()
                        .map(|x| x.to_pred_set_sym())
                        .collect(),
                    GramSymStr::Str(normal_str_vec) => match normal_str_vec.get(0).unwrap() {
                        GramSym::Terminal(value) => {
                            indexset! { PredSetSym::Sym(value.clone()) }
                        }
                        GramSym::NonTerminal(_) => {
                            let mut sub_term_set = indexset! {};
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
    first_sets: &mut IndexMap<GramSym, IndexSet<FstSetSym>>,
) -> bool {
    let mut stable = true;

    for prod in productions.iter() {
        let x = &prod.0;
        let str = &prod.1;
        let mut x_first_set = first_sets.get(x).unwrap().clone();
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

                            let cur_sym_first_set = first_sets.get(cur_sym).unwrap();
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
    follow_sets: &mut FollSets,
    first_sets: &FstSets,
) -> bool {
    let mut stable = true;

    for prod in productions.iter() {
        let x = &prod.0;
        let str = &prod.1;
        let x_follow_set = follow_sets.get(x).unwrap().clone();

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
/////// LR(0) FSM && SLR(1)

/// LR0Item: LR(0) Item
#[derive(Hash, PartialEq, Eq, Clone)]
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

    pub fn symstr(&self) -> &GramSymStr {
        &self.prod.1
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

    pub fn do_inc(&mut self) {
        if let Some(_) = self.rhs_sym() {
            self.pos += 1;
        }
    }

    pub fn inc(&self) -> Self {
        let mut other = self.clone();
        other.do_inc();
        other
    }

    pub fn lhs_sym(&self) -> &GramSym {
        &self.prod.0
    }

    pub fn prod_len(&self) -> usize {
        match &self.prod.1 {
            GramSymStr::Str(symstr) => symstr.len(),
            _ => 0
        }
    }

    pub fn is_end(&self) -> bool {
        self.pos >= self.prod_len()
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

    pub fn to_gram_prod(&self) -> GramProd {
        self.prod.to_owned()
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
#[derive(Clone)]
pub struct LR0Closure {
    _closure: Rc<RefCell<_LR0Closure>>
}

impl LR0Closure {
    pub fn new() -> Self {
        Self {
            _closure: Rc::new(RefCell::new(
                indexset! {}
            ))
        }
    }

    pub fn inner_closure(&self) -> Rc<RefCell<_LR0Closure>> {
        self._closure.clone()
    }

    /// collect all sym pointed by dot
    /// including terminal and nonterminal, end will be filt out
    pub fn dot_syms(&self) -> Vec<GramSym> {
        let closure = self._closure.as_ref().borrow();

        closure.iter()
        .filter_map(|item| item.rhs_sym())
        .cloned()
        .collect_vec()
    }

    pub fn dot_nonterm_syms(&self) -> Vec<GramSym> {
        self.dot_syms().into_iter()
        .filter(|sym| sym.is_nonterminal())
        .collect_vec()
    }

    pub fn insert_item(&mut self, item: LR0Item) -> bool {
        self._closure.as_ref().borrow_mut().insert(item)
    }

    pub fn items(&self) -> Vec<LR0Item> {
        self._closure.as_ref().borrow().iter().cloned().collect_vec()
    }

    pub fn end_items(&self) -> Vec<LR0Item> {
        self._closure.as_ref().borrow().iter()
        .filter(|item| item.is_end())
        .cloned()
        .collect_vec()
    }

    pub fn next_sym_closures(&self) -> Vec<(GramSym, Vec<LR0Item>)> {
        let mut sym_closures = indexmap! {};

        for item in self._closure.as_ref().borrow().iter() {
            if let Some(sym) = item.rhs_sym() {
                if !sym_closures.contains_key(sym) {
                    sym_closures.insert(sym.clone(), vec![]);
                }

                let closure = sym_closures.get_mut(sym).unwrap();

                closure.push(item.inc());
            }
        }

        sym_closures
        .into_iter()
        .collect_vec()
    }
}


impl From<_LR0Closure> for LR0Closure {
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
            writeln!(f, "{}", item)?
        }

        Ok(())
    }
}

impl PartialEq for LR0Closure {
    fn eq(&self, other: &Self) -> bool {
        *self._closure.as_ref().borrow() == *other.inner_closure().as_ref().borrow()
    }
}

/*
* For LR(0) Finite State Machine (FSM)
*/

impl TransData<LR0Closure, GramSym, GramSym> for GramSym {
    fn insert(&mut self, _pat: GramSym) -> bool {
        unreachable!();
    }

    fn is_match(&self, pat: &GramSym) -> bool {
        self == pat
    }

    fn item2pat(&self, _e: &GramSym) -> GramSym {
        unreachable!()
    }
}

/*
* For SLR(1) Parsing Table
*/

/// SLR(1) PT Action
#[derive(Debug, Clone)]
pub enum SLR1PTAct {
    /// Shift (state id)
    /// 用weak ref也不赖，但是和R(usize)形式上统一也用标识符表示法
    /// 故此states使用IndexMap保存
    S(usize),

    /// Reduce (production index)
    R(usize),

    /// Goto (state id)
    G(usize),

    /// Acceptable State
    Accept,

    /// Empty Action
    None
}

impl fmt::Display for SLR1PTAct {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            Self::None => { write!(f, "{}", "   ") },
            _ => { write!(f, "{:?}", self) }
        }
    }
}

/// SLR(1) PT
pub struct SLR1PT {
    /// 相比Array2 or Vectror2D， 考虑到action实际上只会在同一列(labeled by follsym)上跳转
    /// IndexMap可以更方便地by key和by index索引, 而不需要使用额外的{ key => index }的map做转换
    /// 一次创建可以保证每列的高度相同，唯一需要注意一下得大概就是按行打印的时候。
    data: IndexMap<FollSetSym, IndexMap<usize, SLR1PTAct>>
}

impl SLR1PT {
    pub fn new(ordered_states: &Vec<usize>, termsyms: Vec<GramSym>, nontermsyms: Vec<GramSym>) -> Self {
        let mut data = indexmap! {};

        data.extend(
            termsyms
            .into_iter()
            .map(|x| x.to_foll_set_sym())
            .chain(vec![FollSetSym::EndMarker].into_iter())
            .chain(
                nontermsyms
                .into_iter()
                .map(|x| x.to_foll_set_sym())
            )
            .map(|sym| {
                let mut col = indexmap! {};
                col.extend(
                    ordered_states
                        .iter()
                        .map(|id| (id.clone(), SLR1PTAct::None))
                );

                (sym, col)
            })
        );

        Self {
            data
        }
    }

    /// -> (width, height)
    pub fn size(&self) -> (usize, usize) {
        let width = self.data.keys().len();

        let height = self.data.values().nth(0).unwrap().len();

        (width, height)
    }


    /// (id, tokensym)
    pub fn get(&self, pos: &(usize, FollSetSym)) -> Option<&SLR1PTAct> {
        let (i, sym) = pos;

        if let Some(col) = self.data.get(sym) {
            col.get(i)
        } else {
            None
        }
    }

    /// (id, tokensym)
    pub fn get_mut(&mut self, pos: &(usize, FollSetSym)) -> Option<&mut SLR1PTAct> {
        let (i, sym) = pos;

        if let Some(col) = self.data.get_mut(sym) {
            col.get_mut(i)
        } else {
            None
        }
    }

    /// (Ln, Col)
    pub fn get_index(&self, pos: (usize, usize)) -> Option<&SLR1PTAct> {
        let (w, _h) = self.size();
        let (i, j) = pos;

        // 0 <= i for type guarantee
        if i < w {
            self.data.get_index(i).unwrap().1.get(&j)
        } else {
            None
        }
    }

    pub fn get_index_mut(&mut self, pos: (usize, usize)) -> Option<&mut SLR1PTAct> {
        let (w, _h) = self.size();
        let (i, j) = pos;

        // 0 <= i for type guarantee
        if i < w {
            self.data.get_index_mut(i).unwrap().1.get_mut(&j)
        } else {
            None
        }
    }
}


impl fmt::Display for SLR1PT {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let cell_width = 6;

        // 适用于符号不超过20个 (超过的应该分部分显示，但考虑到这个打印是为了readable demo
        // 所以兼容那种情况也没有意义，就这样实现了)
        writeln!(f, "{}", format!(
            "   {}",
            self.data.keys().map(|k|
                format!("{:^cell_width$}", format!("{}", k), cell_width = cell_width)
            )
            .join(" | ")
        ))?;

        let (width, height) = self.size();

        writeln!(f, "   {}",
        (0..width)
        .map(|_j| {
            format!(
                "{}",
                "-".repeat(cell_width)
            )
        })
        .collect_vec()
        .join("-|-")
        ).ok();

        (0..height).for_each(|i| {
            write!(f, "{:<3}", i).ok();

            write!(f, "{}",
                (0..width)
                .map(|j| {
                    format!(
                        "{:^cell_width$}",
                        format!("{}", self.get_index((j, i)).unwrap()),
                        cell_width = cell_width
                    )
                })
                .collect_vec()
                .join(" | ")
            ).ok();

            write!(f, "\n").ok();

            write!(f, "   {}",
            (0..width)
            .map(|_j| {
                format!(
                    "{}",
                    "-".repeat(cell_width)
                )
            })
            .collect_vec()
            .join("-|-")
            ).ok();

            write!(f, "\n").ok();
        });

        Ok(())
    }
}

pub type LR0DFAG = DFAStateGraph<LR0Closure, GramSym, GramSym>;
pub type LR0DFA = DFAState<LR0Closure, GramSym, GramSym>;

/*
* Impl Gram for LR0
*/
impl Gram {
    pub fn lr0closure(&self, items: impl Iterator<Item=LR0Item>) -> LR0Closure {
        let mut closure = indexset! {};

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

    pub fn start_lr0_item(&self) -> Option<LR0Item> {
        if let Some(prod) = self.start_prod() {
            return Some(LR0Item::from_prod(prod.clone()))
        }

        None
    }

    pub fn start_lr0_item_closure(&self) -> Option<LR0Closure> {
        if let Some(start_item) = self.start_lr0_item() {
            let start_set = vec![start_item];
            return Some(self.lr0closure(start_set.into_iter()))
        }

        None
    }

    ///
    ///     要支持 LR(0) parsing, 需要保证每一个没有transition(也就是end状态)的state
    ///
    ///     有且只有一个LR(0)-Item
    pub fn lr0_dfa(&self) -> DFAStateGraph<LR0Closure, GramSym, GramSym> {
        if self.productions.is_empty() {
            return DFAStateGraph::empty()
        }

        let start_closure = self.start_lr0_item_closure().unwrap();
        let mut counter = gen_counter();
        let start_state = Rc::new(RefCell::new(
            DFAState::<LR0Closure, GramSym, GramSym>::from_counter_data(
                &mut counter, start_closure
            )
        ));

        let mut state_coll
        = indexmap!{
            start_state.as_ref().borrow().id.clone() => start_state.clone()
        };
        let mut state_stack
        = stack![start_state];

        while let Some(cur_state) = state_stack.pop() {
            let state_rc = cur_state.as_ref().borrow();
            let state_closure = state_rc.data.data().unwrap().to_owned();
            drop(state_rc);

            for (sym, items_vec)
            in state_closure.next_sym_closures().into_iter() {
                let closure = self.lr0closure(items_vec.into_iter());
                let found_state;

                if let Some(state)
                = state_coll.values().find(|state: &&Rc<RefCell<DFAState<LR0Closure, GramSym, GramSym>>>| {
                    state.as_ref().borrow().data.data().unwrap() == &closure
                }) {
                    found_state = state.to_owned();
                } else {
                    found_state = Rc::new(RefCell::new(
                        DFAState::<LR0Closure, GramSym, GramSym>::from_counter_data(
                            &mut counter, closure
                        )
                    ));

                    state_stack.push(found_state.clone());
                    state_coll.insert(
                        found_state.as_ref().borrow().id.clone() ,
                        found_state.clone()
                    );
                }

                cur_state.as_ref().borrow_mut().insert_transition(
                    DFATransition::new(
                        Rc::new(RefCell::new(sym)),
                        Rc::downgrade(&found_state)
                    )
                );
            }
        }

        DFAStateGraph::from(state_coll)
    }

    pub fn slr0_pt(&self, dfa_g: &LR0DFAG, follow_sets: &FollSets) -> SLR1PT {
        let states= dfa_g.ordered_states();
        let state_ids = states.iter()
        .map(|s| s.as_ref().borrow().id.clone())
        .collect_vec();

        let mut pt = SLR1PT::new(
            &state_ids,
            self.term_syms(),
            self.nonterm_syms()
        );

        // Install Accept
        *pt.get_mut(&(0, FollSetSym::EndMarker)).unwrap() = SLR1PTAct::Accept;

        // Install Shift
        // For each column labeled by a token t,
        // the table contains shift n if there is a transition from state q to state n
        // that is labeled by token t.
        // token t: terminal sym t
        self.term_syms()
        .into_iter()
        .for_each(|sym| {  // for each t
            states
            .iter()
            .for_each(|state| {  // for each state q
                if let TryNxtRes::Ok(to_state)
                = state.as_ref().borrow().try_next(&sym) {
                    let this_state_id = state.as_ref().borrow().id;
                    let to_state_id = to_state.as_ref().borrow().id;

                    *pt.get_mut(&(this_state_id, sym.to_foll_set_sym())).unwrap()
                    = SLR1PTAct::S(to_state_id)
                }
            })
        });

        // Install Reduce
        // For each column labeled by a token t, the table contains action reduce n if
        //  a) state q contains LR(0) item N → α ⋅,
        //  b) production N → α is the nth production, and
        //  c) t is in FOLLOW(N).
        self.term_syms()
        .into_iter()
        .map(|sym| sym.to_foll_set_sym())
        .chain(vec![FollSetSym::EndMarker].into_iter())
        .for_each(|sym| {  // for each t
            states
            .iter()
            .for_each(|state| {  // for each state q
                let mut end_items = state.as_ref().borrow().data.data().unwrap().end_items();

                if let Some(end_item) = end_items.pop() {
                    if follow_sets.get(end_item.lhs_sym()).unwrap().contains(&sym) {
                        let this_state_id = state.as_ref().borrow().id;

                        let prod = end_item.to_gram_prod();
                        let (prod_ind, _value) = self.productions.get_full(&prod).unwrap();

                        let ptcell
                        = pt.get_mut(&(this_state_id, sym.clone())).unwrap();

                        if let SLR1PTAct::None = *ptcell {
                            *ptcell = SLR1PTAct::R(prod_ind);
                        } else {
                            eprintln!("\nConflics({}, {}): {}/{}\n",
                            this_state_id, sym,
                            *ptcell, SLR1PTAct::R(prod_ind)
                            );
                        }
                    }
                }
            });
        });

        // Install Nonterminal Goto
        self.nonterm_syms()
        .into_iter()
        .for_each(|sym| {  // for each t
            states
            .iter()
            .for_each(|state| {  // for each state q
                if let TryNxtRes::Ok(to_state)
                = state.as_ref().borrow().try_next(&sym) {
                    let this_state_id = state.as_ref().borrow().id;
                    let to_state_id = to_state.as_ref().borrow().id;

                    *pt.get_mut(&(this_state_id, sym.to_foll_set_sym())).unwrap()
                    = SLR1PTAct::G(to_state_id)
                }
            })
        });

        pt
    }
}


////////////////////////////////////////////////////////////////////////////////
/////// LR(1) FSM && LALR(1)


/// LR1Item: LR(1) Item
#[derive(Hash, PartialEq, Eq, Clone)]
pub struct LR1Item {
    item: LR0Item,
    lookahead: FollSetSym  // terminal
}


impl LR1Item {
    pub fn from_lr0item_tokensym(item: LR0Item, lookahead: FollSetSym) -> Self {
        Self {
            item,
            lookahead
        }
    }

    pub fn symstr(&self) -> &GramSymStr {
        self.item.symstr()
    }

    pub fn rhs_sym(&self) -> Option<&GramSym> {
        self.item.rhs_sym()
    }

    pub fn do_inc(&mut self) {
        self.item.do_inc()
    }

    pub fn inc(&self) -> Self {
        Self {
            item: self.item.inc(),
            lookahead: self.lookahead.clone()
        }
    }

    pub fn next_rhs_sym(&self) -> Option<GramSym> {
        if let Some(sym) = self.item.inc().rhs_sym() {
            Some(sym.clone())
        } else {
            None
        }
    }

    pub fn lhs_sym(&self) -> &GramSym {
        self.item.lhs_sym()
    }

    pub fn prod_len(&self) -> usize {
        self.item.prod_len()
    }

    pub fn is_end(&self) -> bool {
        self.item.is_end()
    }

    pub fn dump(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.item.dump(f)?;
        write!(f, " | {}", self.lookahead)
    }

    pub fn to_gram_prod(&self) -> GramProd {
        self.item.to_gram_prod()
    }
}

impl fmt::Debug for LR1Item {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.dump(f)
    }
}

impl fmt::Display for LR1Item {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.dump(f)
    }
}


/// _LR1Closure
type _LR1Closure = IndexSet<LR1Item>;

/// LR1Closure
#[derive(Clone)]
pub struct LR1Closure {
    _closure: Rc<RefCell<_LR1Closure>>
}

impl LR1Closure {
    pub fn new() -> Self {
        Self {
            _closure: Rc::new(RefCell::new(
                indexset! {}
            ))
        }
    }

    pub fn inner_closure(&self) -> Rc<RefCell<_LR1Closure>> {
        self._closure.clone()
    }

    /// collect all sym pointed by dot
    /// including terminal and nonterminal, end will be filt out
    pub fn dot_syms(&self) -> Vec<GramSym> {
        let closure = self._closure.as_ref().borrow();

        closure.iter()
        .filter_map(|item| item.rhs_sym())
        .cloned()
        .collect_vec()
    }

    pub fn dot_nonterm_syms(&self) -> Vec<GramSym> {
        self.dot_syms().into_iter()
        .filter(|sym| sym.is_nonterminal())
        .collect_vec()
    }

    pub fn insert_item(&mut self, item: LR1Item) -> bool {
        self._closure.as_ref().borrow_mut().insert(item)
    }

    pub fn items(&self) -> Vec<LR1Item> {
        self._closure.as_ref().borrow().iter().cloned().collect_vec()
    }

    pub fn end_items(&self) -> Vec<LR1Item> {
        self._closure.as_ref().borrow().iter()
        .filter(|item| item.is_end())
        .cloned()
        .collect_vec()
    }

    pub fn next_sym_closures(&self) -> Vec<(GramSym, Vec<LR1Item>)> {
        let mut sym_closures = indexmap! {};

        for item in self._closure.as_ref().borrow().iter() {
            if let Some(sym) = item.rhs_sym() {
                if !sym_closures.contains_key(sym) {
                    sym_closures.insert(sym.clone(), vec![]);
                }

                let closure = sym_closures.get_mut(sym).unwrap();

                closure.push(item.inc());
            }
        }

        sym_closures
        .into_iter()
        .collect_vec()
    }
}


impl From<_LR1Closure> for LR1Closure {
    fn from(set: _LR1Closure) -> Self {
        Self {
            _closure: Rc::new(RefCell::new(set))
        }
    }
}

impl fmt::Display for LR1Closure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let set = self._closure.as_ref().borrow();

        for item in set.iter() {
            writeln!(f, "{}", item)?
        }

        Ok(())
    }
}

impl PartialEq for LR1Closure {
    fn eq(&self, other: &Self) -> bool {
        *self._closure.as_ref().borrow() == *other.inner_closure().as_ref().borrow()
    }
}

/// Impl LR(1) for Gram
impl Gram {
    pub fn lr1closure(
        &self,
        items: impl Iterator<Item=LR1Item>,
        first_sets: &FstSets
    ) -> LR1Closure
    {
        let mut closure = indexset! {};

        let mut new_item_vec = vec![];
        new_item_vec.extend(items);

        while !new_item_vec.is_empty() {
            let wait_for_calc = new_item_vec;
            new_item_vec = vec![];

            for closure_item in wait_for_calc.into_iter() {
                if let Some(rhs) = closure_item.rhs_sym() {
                    if rhs.is_terminal() {
                        closure.insert(closure_item);
                        continue;
                    }

                    if let Some(nxt_rhs_sym) = closure_item.next_rhs_sym() {
                        self.term_syms()
                        .into_iter()
                        .filter(|tokensym| {
                            let fstset
                            = first_sets
                            .get(&nxt_rhs_sym)
                            .unwrap();

                            fstset.contains(&tokensym.to_fst_set_sym())

                        })
                        .for_each(|tokensym| {
                            new_item_vec.extend(
                                self.find_prod(rhs)
                                .into_iter()
                                .map(|prod|
                                    LR1Item::from_lr0item_tokensym(
                                        LR0Item::from_prod(prod),
                                        tokensym.to_foll_set_sym()
                                    )
                                )
                            )
                        })
                    } else {
                        new_item_vec.extend(
                            self.find_prod(rhs)
                            .into_iter()
                            .map(|prod|

                                LR1Item::from_lr0item_tokensym(
                                    LR0Item::from_prod(prod),
                                    closure_item.lookahead.clone()
                                )
                            )
                        )
                    }
                }

                closure.insert(closure_item);
            }
        }

        LR1Closure::from(closure)
    }

    pub fn start_lr1_item(&self) -> Option<LR1Item> {
        if let Some(prod) = self.start_prod() {
            Some(
                LR1Item::from_lr0item_tokensym(
                    LR0Item::from_prod(prod.clone()),
                    FollSetSym::EndMarker
                )
            )
        } else {
            None
        }
    }

    pub fn start_lr1_item_closure(&self, first_sets: &FstSets) -> Option<LR1Closure> {
        if let Some(start_item) = self.start_lr1_item() {
            return Some(
                self.lr1closure(
                    vec![start_item].into_iter(),
                     first_sets
                    )
                )
        }

        None
    }

    /// used for LALR(1) parser (Look Ahead LR(1))
    pub fn lr1_dfa(&self, first_sets: &FstSets) -> LR1DFAG {
        if self.productions.is_empty() {
            return LR1DFAG::empty()
        }

        let start_closure = self.start_lr1_item_closure(first_sets).unwrap();
        let mut counter = gen_counter();
        let start_state = Rc::new(RefCell::new(
            LR1DFA::from_counter_data(
                &mut counter, start_closure
            )
        ));

        let mut state_coll
        = indexmap!{
            start_state.as_ref().borrow().id.clone() => start_state.clone()
        };
        let mut state_stack
        = stack![start_state];

        while let Some(cur_state) = state_stack.pop() {
            let state_rc = cur_state.as_ref().borrow();
            let state_closure = state_rc.data.data().unwrap().to_owned();
            drop(state_rc);

            for (sym, items_vec)
            in state_closure.next_sym_closures().into_iter() {
                let closure = self.lr1closure(items_vec.into_iter(), first_sets);
                let found_state;

                if let Some(state)
                = state_coll.values().find(|state: &&Rc<RefCell<DFAState<LR1Closure, GramSym, GramSym>>>| {
                    state.as_ref().borrow().data.data().unwrap() == &closure
                }) {
                    found_state = state.to_owned();
                } else {
                    found_state = Rc::new(RefCell::new(
                        LR1DFA::from_counter_data(
                            &mut counter, closure
                        )
                    ));

                    state_stack.push(found_state.clone());
                    state_coll.insert(
                        found_state.as_ref().borrow().id.clone() ,
                        found_state.clone()
                    );
                }

                cur_state.as_ref().borrow_mut().insert_transition(
                    DFATransition::new(
                        Rc::new(RefCell::new(sym)),
                        Rc::downgrade(&found_state)
                    )
                );
            }
        }

        DFAStateGraph::from(state_coll)
    }

    pub fn slr1_pt(&self, dfa_g: &LR1DFAG) -> SLR1PT {
        let states = dfa_g.ordered_states();
        let state_ids = states.iter()
        .map(|s| s.as_ref().borrow().id.clone())
        .collect_vec();

        let mut pt = SLR1PT::new(
            &state_ids,
            self.term_syms(),
            self.nonterm_syms()
        );

        // Install Accept
        *pt.get_mut(&(0, FollSetSym::EndMarker)).unwrap() = SLR1PTAct::Accept;

        // Install Shift
        // For each column labeled by a token t,
        // the table contains shift n if there is a transition from state q to state n
        // that is labeled by token t.
        // token t: terminal sym t
        self.term_syms()
        .into_iter()
        .for_each(|sym| {  // for each t
            states
            .iter()
            .for_each(|state| {  // for each state q
                if let TryNxtRes::Ok(to_state)
                = state.as_ref().borrow().try_next(&sym) {
                    let this_state_id = state.as_ref().borrow().id;
                    let to_state_id = to_state.as_ref().borrow().id;

                    *pt.get_mut(&(this_state_id, sym.to_foll_set_sym())).unwrap()
                    = SLR1PTAct::S(to_state_id)
                }
            })
        });

        // Install Reduce
        // For each column labeled by a token t, the table contains action reduce n if
        //  a) state q contains LR(0) item N → α ⋅,
        //  b) production N → α is the nth production, and
        //  c) t is in FOLLOW(N).
        self.term_syms()
        .into_iter()
        .map(|sym| sym.to_foll_set_sym())
        .chain(vec![FollSetSym::EndMarker].into_iter())
        .for_each(|sym| {  // for each t
            states
            .iter()
            .for_each(|state| {  // for each state q
                let mut end_items = state.as_ref().borrow().data.data().unwrap().end_items();

                if let Some(end_item) = end_items.pop() {
                    if sym == end_item.lookahead {
                        let this_state_id = state.as_ref().borrow().id;

                        let prod = end_item.to_gram_prod();
                        let (prod_ind, _value) = self.productions.get_full(&prod).unwrap();

                        let ptcell
                        = pt.get_mut(&(this_state_id, sym.clone())).unwrap();

                        if let SLR1PTAct::None = *ptcell {
                            *ptcell = SLR1PTAct::R(prod_ind);
                        } else {

                            eprintln!("\nConflics({}, {}): {}/{}\n",
                            this_state_id, sym,
                            *ptcell, SLR1PTAct::R(prod_ind)
                            );
                        }
                    }
                }
            });
        });

        // Install Nonterminal Goto
        self.nonterm_syms()
        .into_iter()
        .for_each(|sym| {  // for each t
            states
            .iter()
            .for_each(|state| {  // for each state q
                if let TryNxtRes::Ok(to_state)
                = state.as_ref().borrow().try_next(&sym) {
                    let this_state_id = state.as_ref().borrow().id;
                    let to_state_id = to_state.as_ref().borrow().id;

                    *pt.get_mut(&(this_state_id, sym.to_foll_set_sym())).unwrap()
                    = SLR1PTAct::G(to_state_id)
                }
            })
        });

        pt
    }
}


pub type LR1DFAG = DFAStateGraph<LR1Closure, GramSym, GramSym>;
pub type LR1DFA = DFAState<LR1Closure, GramSym, GramSym>;

impl TransData<LR1Closure, GramSym, GramSym> for GramSym {
    fn insert(&mut self, _pat: GramSym) -> bool {
        unreachable!();
    }

    fn is_match(&self, pat: &GramSym) -> bool {
        self == pat
    }

    fn item2pat(&self, _e: &GramSym) -> GramSym {
        unreachable!()
    }
}

#[cfg(test)]
#[allow(non_snake_case)]
#[allow(unused_imports)]
#[allow(unused_variables)]
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
        use crate::parser::{
            Parser,
            SLR1Parser,
            SLR1vTParser
        };

        declare_nonterminal! {E, E1, T, F};
        declare_terminal! {
            -: lexer :-
            mul, add, n, lparen, rparen
        };

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

        // let start_set = vec![gram.start_item().unwrap()];
        // let closure = gram.lr0closure(start_set.into_iter());
        // println!("{}", closure);

        let lr0dfa = gram.lr0_dfa();
        println!("{}", lr0dfa);

        // Test LR(0) FSM
        // let channels = lexer.tokenize("n*n");
        // let main_channel = channels.get("default").unwrap();
        // let main_tokens = &main_channel.tokens;

        // println!("tokens: {:#?}", main_tokens);

        let follow_sets = gram.follow_sets(&gram.first_sets());
        // println!("follow_sets: {:#?}", follow_sets);

        // let slrparser = SLR1Parser::new("example", gram.clone(), lexer);

        // match slrparser.parse("n+n*n") {
        //     Err(err) => panic!("{}", err),
        //     Ok(ast) => {
        //         println!("{}", ast.as_ref().borrow());
        //     }
        // }

        let pt = gram.slr0_pt(&lr0dfa, &follow_sets);

        println!("{}", pt);
    }

    #[test]
    fn lr_handle_conflics() {
        use crate::parser::{
            Parser,
            SLR1Parser,
            SLR1vTParser
        };

        declare_nonterminal! {S, A, B, D};
        declare_terminal! {
            -: lexer :-
            e, a, b
        };

        use_epsilon!(ε);

        make_regex_and_matcher!(e_r, e_m);
        make_regex_and_matcher!(a_r, a_m);
        make_regex_and_matcher!(b_r, b_m);

        let gram = grammar![G|
            S:
            | e B b A;
            | e A;

            A:
            | a;
            | ε;

            B:
            | a;
            | a D;

            D:
            | b;
        |];

        let lr0dfa = gram.lr0_dfa();
        // println!("{}", lr0dfa);

        let first_sets = gram.first_sets();
        let follow_sets = gram.follow_sets(&first_sets);

        println!("LR(0) Parsing Table:");
        let lr0pt = gram.slr0_pt(&lr0dfa, &follow_sets);
        println!("{}", lr0pt);

        let lr1dfa = gram.lr1_dfa(&first_sets);

        println!("LR(1) Parsing Table:");
        let lr1pt = gram.slr1_pt(&lr1dfa);
        println!("{}", lr1pt);

    }

    #[test]
    fn check_lr0_ambiguous_grammar() {
        use crate::parser::{
            Parser,
            SLR1Parser,
            SLR1vTParser
        };

        declare_nonterminal! {E};
        declare_terminal! {
            -: lexer :-
            n, lparen, rparen, mul, add
        };

        let gram = grammar![G|
            E:
            | n;

            E:
            | E add E;
            | E mul E;
            | lparen E rparen;
        |];

        let lr0dfa = gram.lr0_dfa();
        println!("{}", lr0dfa);

        let follow_sets = gram.follow_sets(&gram.first_sets());

        let pt = gram.slr0_pt(&lr0dfa, &follow_sets);

        println!("{}", pt);
    }

    #[test]
    fn test_lalr1_0() {
        use crate::parser::{
            Parser,
            SLR1Parser,
            SLR1vTParser
        };

        declare_nonterminal! {S1, S, C};
        declare_terminal! {
            c, d
        };

        let gram = grammar![G|
            S1:
            | S;

            S:
            | C C;

            C:
            | c C;
            | d;
        |];

        let lr0dfa = gram.lr0_dfa();
        println!("{}", lr0dfa);

        // let follow_sets = gram.follow_sets(&gram.first_sets());

        // let pt = gram.slr0_pt(&lr0dfa, &follow_sets);

        // println!("{}", pt);

        let first_sets = gram.first_sets();
        // display_fstsets(&first_sets);
        let lr0dfa = gram.lr1_dfa(&first_sets);
        println!("{}", lr0dfa);
    }

    #[test]
    fn test_lalr1_1() {
        use crate::parser::{
            Parser,
            SLR1Parser,
            SLR1vTParser
        };

        declare_nonterminal! {S1, S, L, R};
        declare_terminal! {
            -: lexer :-
            assign,  mul, id
        };

        let gram = grammar![G|
            S1:
            | S;

            S:
            | L assign R;
            | R;

            L:
            | mul R;
            | id;

            R:
            | L;
        |];

        let lr0dfa = gram.lr0_dfa();
        println!("{}", lr0dfa);

        let first_sets = gram.first_sets();
        // display_fstsets(&first_sets);
        println!("{}", lr0dfa);

        let first_sets = gram.first_sets();
        let follow_sets = gram.follow_sets(&first_sets);

        println!("LR(0) Parsing Table:");
        let lr0pt = gram.slr0_pt(&lr0dfa, &follow_sets);
        println!("{}", lr0pt);

        let lr1dfa = gram.lr1_dfa(&first_sets);
        println!("LR(1) Parsing Table:");
        let lr1pt = gram.slr1_pt(&lr1dfa);
        println!("{}", lr1pt);
    }
}