use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    fmt,
    rc::Rc,
};

use super::regex::PriRegexMatcher;

////////////////////////////////////////////////////////////////////////////////
/////// TokenType

////////////////////////////////////////////////////////////////////////////////
/////// GrammarRule Common trait
// pub trait GrammarRule{
//     fn name(&self) -> &str;

//     fn generals(&self) -> &[Vec<& Self>] where Self: Sized;
// }

// impl PartialEq for dyn GrammarRule{
//     fn eq(&self, other: &Self) -> bool {
//         self.name() == other.name()
//     }
// }

// impl Hash for dyn GrammarRule{
//     fn hash<H: Hasher>(&self, rule: &mut H) {
//         rule.write(self.name().as_bytes())
//     }
// }

// impl fmt::Display for dyn GrammarRule{
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         write!(f, "{}", self.name())
//     }
// }

////////////////////////////////////////////////////////////////////////////////
/////// Lexer
pub struct Lexer {
    pub name: String,
    pub token_type_map: HashMap<String, PriRegexMatcher>, // epsilon 不参与
}

////////////////////////////////////////////////////////////////////////////////
/////// LL Grammar Rules

pub type LLBranchType = Vec<LLGrammarGeneral>;

pub struct LLGrammarRule {
    name: String,
    branches: Vec<LLBranchType>,
}

impl LLGrammarRule {
    pub fn with_name(name: &str) -> Self {
        Self {
            name: name.to_string(),
            branches: vec![],
        }
    }

    pub fn add_branch(&mut self, branch: Vec<LLGrammarGeneral>) {
        self.branches.push(branch)
    }

    pub fn branches(&self) -> &[Vec<LLGrammarGeneral>] {
        &self.branches[..]
    }

    pub fn name(&self) -> String {
        self.name.to_string()
    }
}

/// print graph form of LLGrammarRule
fn dump_llrule(
    llrule: &LLGrammarRule,
    f: &mut fmt::Formatter,
    visited_nodes: &mut HashSet<String>,
) -> fmt::Result {
    if visited_nodes.contains(&llrule.name) {
        return Ok(());
    }

    write!(f, "{}\n", llrule)?;
    visited_nodes.insert(llrule.name.clone());

    for branch in llrule.branches() {
        for frag in branch.iter() {
            if let LLGrammarGeneral::Rule(rule) = frag {
                dump_llrule(&rule.as_ref().borrow(), f, visited_nodes)?
            }
        }
    }

    Ok(())
}

impl fmt::Display for LLGrammarRule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}:", self.name)?;
        for branch in self.branches() {
            write!(f, "  |").ok();
            for frag in branch.iter() {
                match frag {
                    LLGrammarGeneral::Rule(rule) => write!(f, " {}", rule.as_ref().borrow().name)?,
                    LLGrammarGeneral::Terminal(terminal) => write!(f, " {}", terminal.name())?,
                    LLGrammarGeneral::Epsilon => write!(f, " ε")?,
                }
            }
            writeln!(f, "").ok();
        }
        Ok(())
    }
}

impl fmt::Debug for LLGrammarRule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        dump_llrule(self, f, &mut hashset! {})
    }
}

////////////////////////////////////////////////////////////////////////////////
/////// LL Grammar Generals (生成式)

#[derive(Clone)]
pub enum LLGrammarGeneral {
    Rule(Rc<RefCell<LLGrammarRule>>),
    Terminal(PriRegexMatcher),
    Epsilon,
}

impl LLGrammarGeneral {
    pub fn name(&self) -> String {
        match self {
            Self::Rule(rule)
                => rule.as_ref().borrow().name().to_owned(),
            Self::Terminal(term)
                => term.name().to_string(),
            Self::Epsilon
                => "ε".to_string()
        }
    }

    pub fn get_rule(&self) -> Option<&Rc<RefCell<LLGrammarRule>>> {
        match self {
            Self::Rule(rule) => Some(rule),
            _ => None,
        }
    }

    pub fn is_rule(&self) -> bool {
        match self {
            LLGrammarGeneral::Rule(_) => true,
            _ => false,
        }
    }

    pub fn is_epsilon(&self) -> bool {
        match self {
            LLGrammarGeneral::Epsilon => true,
            _ => false,
        }
    }
}

impl fmt::Display for LLGrammarGeneral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Rule(rule) => write!(f, "{}", rule.as_ref().borrow()),
            Self::Terminal(terminal) => write!(f, "{}", terminal.name()),
            Self::Epsilon => write!(f, "ε"),
        }
    }
}

impl fmt::Debug for LLGrammarGeneral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Rule(rule) => write!(f, "{:?}", rule.as_ref().borrow()),
            _ => write!(f, "{}", self),
        }
    }
}


////////////////////////////////////////////////////////////////////////////////
/////// LL Grammar DSL

#[macro_export]
macro_rules! llr {
    ($name:ident) => {
        Rc::new(RefCell::new($crate::parser::LLGrammarGeneral::Rule(&$name)))
    };
}

/// 使用同名regex matcher创建一个 `LLGrammarGeneral::Terminal`
/// 并进行绑定
#[macro_export]
macro_rules! use_llterminal {
    ($name:ident) => {
        let $name = $crate::parser::LLGrammarGeneral::Terminal(
            paste::paste! { $crate::regex::[<$name _m>] }(),
        );
    };
}

/// 创建一个规则
#[macro_export]
macro_rules! create_llrule {
    ($name:ident : $(| $($general:ident)+ ;)+ ) => {
        let $name =
        {  // 单独生命周期避免冲突
            let rule = std::rc::Rc::new(std::cell::RefCell::new(
                $crate::parser::LLGrammarRule::with_name(stringify!($name))
            ));

            $(
                let mut branch = vec![];

                $(
                    branch.push($general.clone());
                )+

                rule.as_ref().borrow_mut().add_branch(branch);
            )+

            $crate::parser::LLGrammarGeneral::Rule(rule)
        };
    }
}

/// 实现一个已经声明过的规则
#[macro_export]
macro_rules! impl_llrule {
    ($name:ident : $(| $($general:ident)+ ;)+ ) => {
        {  // 单独生命周期避免冲突
            let rule = $name.clone();

            $(
                let mut branch = vec![];

                $(
                    branch.push($general.clone());
                )+

                if let $crate::parser::LLGrammarGeneral::Rule(ref _rule) = rule {
                    _rule.as_ref().borrow_mut().add_branch(branch);
                }
            )+
        }
    }
}

/// 声明一个规则，实际上是创建了一个
/// `LLGrammarGeneral::Rule` 绑定
#[macro_export]
macro_rules! sign_llrule {
    ($name:ident) => {
        let $name = LLGrammarGeneral::Rule(
            std::rc::Rc::new(
                std::cell::RefCell::new(
                    LLGrammarRule::with_name(
                        stringify!($name),
        ))));
    };
}

/// 引入 `LLGrammarGeneral::Epsilon`
//  限制只能引入已存在的符号
#[macro_export]
macro_rules! use_llepsilon {
    ($name:ident) => {
        let $name = $crate::parser::LLGrammarGeneral::Epsilon.clone();
    };
}

////////////////////////////////////////////////////////////////////////////////
/////// LL Grammar (First/Follow) Set

/// rule_name => set<String as Token Name>
type FirstSetType = HashMap<String, Rc<RefCell<HashSet<String>>>>;

/// LL(1)
pub fn first_sets(rule: &Rc<RefCell<LLGrammarRule>>) -> FirstSetType {
    let mut res = hashmap! {};
    // let rule_fun
    //      = |genl: Rc<RefCell<LLGrammarGeneral>>| genl.as_ref().borrow().is_rule();
    // for rule in
    //     entry_general.dfs_walk(&rule_fun)
    // {
    //     _first_sets(&rule, &mut res, &mut hashset! {});

    // }

    let mut i = 1usize;
    loop {
        if i > 2 {
            println!("another round: {}", i - 1);
        }
        if _first_sets(rule, &mut res, &mut hashset! {}) {
            break;
        }
        i += 1;
    }

    res
}

fn _first_sets(
    rule: &Rc<RefCell<LLGrammarRule>>,
    acc: &mut FirstSetType,
    visited_rule: &mut HashSet<String>,
) -> bool {
    let rule_ref = rule.as_ref().borrow();
    let this_id = rule_ref.name.to_string();
    visited_rule.insert(this_id.clone());

    let this_first_set;
    match acc.get(&this_id) {
        Some(_inner_first_set) => {
            this_first_set = _inner_first_set.clone();
        }
        None => {
            this_first_set = Rc::new(RefCell::new(hashset! {}));
            acc.insert(this_id, this_first_set.clone());
        }
    }

    // 第一遍计算，要计算所有rule节点，包括FirstSet所不需要的
    // 从上到下
    for branch in rule_ref.branches() {
        for genl in branch.into_iter() {
            match genl {
                LLGrammarGeneral::Rule(genl_rule) => {
                    if !visited_rule.contains(&genl_rule.as_ref().borrow().name) {
                        // 之后取出来， 放入this_first_set
                        _first_sets(&genl_rule, acc, visited_rule);
                    }
                },
                _ => (),
            }
        }
    }

    // 第二遍加入
    let mut stable = true;
    for branch in rule_ref.branches() {
        let mut brg = branch.into_iter();

        loop {
            let genl;
            match brg.next() {
                Some(_genl) => {
                    genl = _genl.clone();
                }
                None => {
                    break;
                }
            }

            match genl {
                LLGrammarGeneral::Rule(genl_rule) => {
                    let genl_rule_ref = genl_rule.as_ref().borrow();
                    let child_set = acc.get(&genl_rule_ref.name()).unwrap();
                    let child_set_ref = child_set.as_ref().borrow();
                    let mut this_first_set_ref = this_first_set.as_ref().borrow_mut();

                    if !this_first_set_ref.is_superset(&child_set_ref) {
                        this_first_set_ref.extend(child_set_ref.clone().into_iter());
                        stable = false;
                    }

                    if any_branch_maybe_epsilon(&genl_rule) {
                        continue;
                    }
                },
                LLGrammarGeneral::Terminal(matcher) => {
                    this_first_set
                        .as_ref()
                        .borrow_mut()
                        .insert(matcher.name().to_string());
                },
                _ => (),
            }

            break;
        }

        if any_branch_maybe_epsilon(rule) {
            this_first_set
                .as_ref()
                .borrow_mut()
                .insert("epsilon".to_string());
        }
    }

    stable
}

fn is_epsilon_branch(branch: &LLBranchType) -> bool {
    branch[0].is_epsilon()
}

fn any_branch_maybe_epsilon(rule: &Rc<RefCell<LLGrammarRule>>) -> bool {
    let rule_ref = rule.as_ref().borrow();

    for br in rule_ref.branches.iter() {
        if is_epsilon_branch(br) {
            return true;
        };

        if !br.iter().all(|genl| genl.is_rule()) {
            continue;
        }

        // 滤得只含grammar rule的分支
        // 然后检查这些分支的每一个rule是否都有epsilon分支(递归)
        // LL 没有左递归，可以放心递归
        if br
            .iter()
            .all(|genl| any_branch_maybe_epsilon(genl.get_rule().unwrap()))
        {
            return true;
        }
    }

    false
}

////////////////////////////////////////////////////////////////////////////////
/////// Unit Test

#[cfg(test)]
mod test {}
