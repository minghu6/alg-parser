#![allow(dead_code)]

use std::{
    cell::RefCell, collections::VecDeque, fmt, hash::Hash, iter::IntoIterator, rc::Rc,
    vec::IntoIter,
};

use itertools::Itertools;

use super::algs::state::*;
use super::utils::{char_dec, char_inc, char_range, gen_counter, CounterType};

///! 因为只有一套实现(起码我开始写的时候是这么认为的，汗)，所以不需要定义接口

////////////////////////////////////////////////////////////////////////////////
//// Charset

///! charset!{a-z | 0-9 | 啊-吧}
#[macro_export]
macro_rules! charset {
    ( $($lower:tt-$upper:tt)|* ) => {
        {
            let mut _charset = $crate::regex::CharSet::new();

            $(
                let lowers = stringify!($lower);
                let uppers = stringify!($upper);

                let lowerc = lowers.chars().next().unwrap();
                let upperc = uppers.chars().next().unwrap();

                _charset.insert_scope((lowerc, upperc));
            )*

            _charset
        }
    }
}

/// 我对这个结构不太满意，先凑和用
#[derive(Debug, Clone, Eq, Hash)]
pub struct CharSet {
    pub char_scopes: Vec<(char, char)>,
}

impl CharSet {
    pub fn new() -> Self {
        Self {
            char_scopes: Vec::new(),
        }
    }

    pub fn new_getter() -> Rc<RefCell<dyn TransData<char, char, char>>> {
        Rc::new(RefCell::new(Self::new()))
    }

    pub fn with_char_scope(char_scope: (char, char)) -> Self {
        let char_scopes = vec![char_scope];

        Self { char_scopes }
    }

    pub fn contains(&self, inputc: &char) -> bool {
        for (lower, upper) in self.char_scopes.iter() {
            if lower <= inputc && inputc <= upper {
                return true;
            }
        }

        false
    }

    pub fn insert_scope(&mut self, char_scope: (char, char)) {
        self.char_scopes.push(char_scope); // push return ()
        self.compress(); // 每次都压缩可能不太合适
    }

    pub fn is_empty(&self) -> bool {
        self.char_scopes.len() == 0
    }

    pub fn to_vec(&self) -> Vec<char> {
        self.char_scopes
            .clone()
            .into_iter()
            .map(|(lower, upper)| char_range(lower, upper))
            .flatten()
            .collect::<Vec<char>>()
    }

    // merge char_scopes
    pub fn compress(&mut self) {
        if self.is_empty() {
            return;
        }

        self.char_scopes
            .sort_by_cached_key(|(lower, _)| lower.clone());
        let mut compressed_vecdeq = VecDeque::new();
        compressed_vecdeq.push_back(self.char_scopes[0]);

        for (lower, upper) in self.char_scopes.iter() {
            let here = compressed_vecdeq.back_mut().unwrap();

            let here_upper_inc = match char_inc(&here.1) {
                Some(upper_inc) => upper_inc,
                None => here.1,
            };

            if (here.1 >= *lower || here_upper_inc == *lower)
                && (here.1 <= *upper || here_upper_inc == *upper)
            {
                here.1 = upper.clone();
            } else if here_upper_inc < *lower {
                compressed_vecdeq.push_back((lower.clone(), upper.clone()));
            }
        }

        self.char_scopes = compressed_vecdeq.into_iter().collect();
    }

    ///
    /// ```
    /// use alg_parser::regex::CharSet;
    /// use alg_parser::charset;
    ///
    /// debug_assert_eq!(CharSet::parse("a-c"), CharSet::with_char_scope(('a', 'c')));
    /// debug_assert_eq!(CharSet::parse("A-Z0-9"), charset!{A-Z | 0-9});
    /// debug_assert_eq!(CharSet::parse("_a-z"), charset!{_-_ | a-z});
    /// debug_assert_eq!(CharSet::parse("abc"), charset!{a-a | b-b | c-c});
    ///
    /// ```
    pub fn parse(input: &str) -> Self {
        let mut char_scopes = vec![];
        enum Status {
            End,
            Fst,
            Mid,
        }

        let mut status = Status::End;
        let mut scope = vec![];
        for c in input.chars() {
            match status {
                Status::End => {
                    scope.push(c);
                    status = Status::Fst;
                }

                Status::Fst => match c {
                    '-' => status = Status::Mid,
                    _ => {
                        scope.push(scope[0]);
                        char_scopes.push((scope[0], scope[1]));
                        scope = vec![c];
                        status = Status::Fst;
                    }
                },

                Status::Mid => {
                    scope.push(c);
                    char_scopes.push((scope[0], scope[1]));
                    scope = vec![];
                    status = Status::End;
                }
            }
        }

        // recycle trail
        if scope.len() == 1 {
            char_scopes.push((scope[0], scope[0]));
        }

        Self { char_scopes }
    }

    pub fn exclude_one(&mut self, c: char) {
        self.compress();

        enum Strategy {
            RemoveSingle(char),
            DoNothing,
            PushNewScope((char, char)),
        }

        let mut strategy = Strategy::DoNothing;
        for (lower, upper) in self.char_scopes.iter_mut() {
            if *lower == *upper {
                if *lower == c {
                    strategy = Strategy::RemoveSingle(c);
                    break;
                }
            } else {
                if *lower == c {
                    *lower = char_inc(&c).unwrap();
                    break;
                } else if *upper == c {
                    *upper = char_dec(&c).unwrap();
                } else if *lower < c && c < *upper {
                    strategy = Strategy::PushNewScope((char_inc(&c).unwrap(), upper.clone()));
                    *upper = char_dec(&c).unwrap();
                }
            }
        }

        match strategy {
            Strategy::RemoveSingle(c) => {
                self.char_scopes = self
                    .char_scopes
                    .iter()
                    .cloned()
                    .filter(|(lower, upper)| lower == upper && *lower == c)
                    .collect();
            }

            Strategy::PushNewScope(scope) => {
                self.char_scopes.push(scope);
            }

            _ => (),
        }
    }

    ///
    /// ```
    /// use alg_parser::regex::{ CharSet, all_char_set };
    /// use alg_parser::charset;
    /// let mut charset1 = all_char_set();
    /// charset1.exclude_chars("\"\\\n\t");
    /// debug_assert!(!charset1.contains(&'"'))
    /// ```
    pub fn exclude_chars(&mut self, chars: &str) {
        for c in chars.chars() {
            self.exclude_one(c);
        }
    }

    fn _test(&self) {
        // 用来看一下宏展开
        //charset!(a b);

        // let mycharset = charset!( a2-a3 | 0-9 | b-b );
        // println!("{}", mycharset);
    }
}

impl fmt::Display for CharSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let res = self
            .char_scopes
            .iter()
            .map(|(lower, upper)| {
                if lower == upper {
                    format!("{}", lower)
                } else {
                    format!("{}-{}", lower, upper)
                }
            })
            .collect::<Vec<String>>()
            .join("");

        write!(f, "{}", res)
    }
}

impl IntoIterator for CharSet {
    type Item = char;
    type IntoIter = IntoIter<char>;

    fn into_iter(self) -> Self::IntoIter {
        self.char_scopes
            .into_iter()
            .map(|(lower, upper)| char_range(lower, upper))
            .flatten()
            .collect::<Vec<char>>()
            .into_iter()
    }
}

impl PartialEq for CharSet {
    fn eq(&self, other: &Self) -> bool {
        let mut self_copy = self.clone();
        self_copy.compress();

        let mut other_copy = other.clone();
        other_copy.compress();

        self_copy.char_scopes == other_copy.char_scopes
    }
}

pub fn all_char_set() -> CharSet {
    CharSet::with_char_scope(('\u{0}', '\u{FFFF}'))
}

impl AlphabetSet<char> for CharSet {
    fn iter(&self) -> IntoIter<char> {
        self.to_vec().into_iter()
    }
}

impl TransData<char, char, char> for CharSet {
    fn is_match(&self, pat: &char) -> bool {
        self.contains(pat)
    }

    fn insert(&mut self, pat: char) -> bool {
        if self.contains(&pat) {
            return false;
        }

        self.insert_scope((pat, pat));
        true
    }

    fn item2pat(&self, e: &char) -> char {
        e.to_owned()
    }
}

////////////////////////////////////////////////////////////////////////////////
/// GrammarNode: Simple Regex Node

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum RegexNodeType {
    And,
    Or,
    Node,
    Epsilon, // 空集
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct RegexNode {
    pub childen: Vec<Box<RegexNode>>,
    pub nodetype: RegexNodeType,
    pub repeat_times: (usize, usize),
    pub chars: CharSet,
}

impl RegexNode {
    ///
    /// Static Create Method
    ///
    pub fn new() -> Self {
        Self {
            childen: Vec::new(),
            nodetype: RegexNodeType::Epsilon,
            repeat_times: (1, 1),
            chars: CharSet::new(),
        }
    }

    pub fn from_charset(chars: CharSet) -> Self {
        let mut node = Self::new();
        node.nodetype = RegexNodeType::Node;
        node.chars = chars;

        node
    }

    pub fn from_charset_repeat_times(chars: CharSet, repeat_times: (usize, usize)) -> Self {
        let mut node = Self::from_charset(chars);
        node.repeat_times = repeat_times;

        node
    }

    pub fn create_or_node() -> Self {
        let mut node = Self::new();
        node.nodetype = RegexNodeType::Or;

        node
    }

    pub fn create_and_node() -> Self {
        let mut node = Self::new();
        node.nodetype = RegexNodeType::And;

        node
    }

    pub fn wrap_at_most_one(child: Self) -> RegexNode {
        let mut wrapper = Self::create_and_node();
        wrapper.add_child(child);
        wrapper.repeat_times = (0, 1);
        wrapper
    }

    ///
    /// Instance Update Method
    ///
    pub fn add_child(&mut self, child: Self) {
        self.childen.push(Box::new(child))
    }

    ///
    /// Instance Show Method
    ///
    pub fn in_short_text(&self) -> String {
        let mut text = String::new();
        if self.childen.len() == 0 {
            text.push_str(format!("[{}]", self.chars).as_str());
            text.push_str(match self.repeat_times {
                (1, usize::MAX) => "+",
                (0, usize::MAX) => "*",
                _ => "",
            });
        } else {
            let delim = if self.nodetype == RegexNodeType::And {
                ""
            } else {
                " | "
            };

            text.push_str(
                c![(*child).in_short_text(), for child in self.childen.iter()]
                    .join(delim)
                    .as_str(),
            );
        }

        text
    }
}

impl fmt::Display for RegexNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.in_short_text())
    }
}

#[macro_export]
macro_rules! simple_regex {
    ( $(
         $([$charstr:literal]$($repeat:tt)?)+
        ) |+
    ) => {
        {
            let mut _root = $crate::regex::RegexNode::create_or_node();

            $(
                let mut _sub_and_node = $crate::regex::RegexNode::create_and_node();

                $(
                    let mut repeat_times = (1, 1);
                    $(
                        let repeat_s = stringify!($repeat);

                        if repeat_s.len() == 3 {
                            match repeat_s.chars().nth(1).unwrap() {
                                '+' => repeat_times = (1, usize::MAX),
                                '*' => repeat_times = (0, usize::MAX),
                                _ => ()
                            }
                        }
                    )?

                    let charset = $crate::regex::CharSet::parse($charstr);

                    _sub_and_node.add_child(
                        $crate::regex::RegexNode::from_charset_repeat_times(
                            charset,
                            repeat_times
                        )
                    );
                )+

                _root.add_child(_sub_and_node);
            )+

            _root
        }

    };
}

////////////////////////////////////////////////////////////////////////////////
//// Converter

pub fn regex2nfa(counter: &mut dyn CounterType, node: &RegexNode) -> StateGraph<char, char, char> {
    _regex2nfa(counter, node)
}

fn _regex2nfa(counter: &mut dyn CounterType, node: &RegexNode) -> StateGraph<char, char, char> {
    let mut states_vec = vec![];
    let (mut begin_state, mut end_state);

    match node.nodetype {
        RegexNodeType::Or => {
            // 构建新的开始和结束状态，用它们连每一个转移状态
            begin_state = Rc::new(RefCell::new(State::with_counter(counter)));
            end_state = Rc::new(RefCell::new(State::with_counter_accept(counter)));

            states_vec.push(begin_state.clone());

            for child in node.childen.iter() {
                let sub_g = _regex2nfa(counter, child);
                let (sub_begin_state, sub_end_state) =
                    (sub_g.top().unwrap(), sub_g.tail().unwrap());

                // 新的开始状态通过 ε 连接到子图的开始状态
                // 子图的结束状态通过 ε 连接到新的结束状态
                (*begin_state)
                    .borrow_mut()
                    .insert_epsilon_transition(Rc::downgrade(&sub_begin_state));
                (**sub_end_state)
                    .borrow_mut()
                    .insert_epsilon_transition(Rc::downgrade(&end_state));
                (**sub_end_state).borrow_mut().acceptable = false;

                states_vec.extend(sub_g.states);
            }

            states_vec.push(end_state.clone());
        }

        RegexNodeType::And => {
            // 各个转移状态前后相连
            let sub_graphs: Vec<StateGraph<char, char, char>> = node
                .childen
                .iter()
                .map(|child| _regex2nfa(counter, child))
                .collect();

            if sub_graphs.len() > 0 {
                let top_g = sub_graphs.first().unwrap();
                let tail_g = sub_graphs.last().unwrap();

                begin_state = top_g.top().unwrap().clone();
                end_state = tail_g.tail().unwrap().clone();

                for i in (1..sub_graphs.len()).rev() {
                    sub_graphs[i - 1].clone().just_concat(&sub_graphs[i]);
                }

                sub_graphs
                    .into_iter()
                    .for_each(|subg| states_vec.extend(subg.states));
            } else {
                unreachable!()
            }
        }

        RegexNodeType::Node => {
            // 根据node的condition创建两个状态
            begin_state = Rc::new(RefCell::new(State::with_counter(counter)));
            end_state = Rc::new(RefCell::new(State::with_counter_accept(counter)));

            let trans = Transition {
                data: Some(Rc::new(node.chars.clone())),
                to_state: Rc::downgrade(&end_state),
            };

            (*begin_state).borrow_mut().insert_transition(trans);

            states_vec.push(begin_state.clone());
            states_vec.push(end_state.clone());
        }

        _ => unreachable!(),
    }

    // 处理重复的情况
    // (1, 1)
    // (0, 1)          ?
    // (1, usize::MAX) +
    // (0, usize::MAX) *
    if node.repeat_times.0 != 1 || node.repeat_times.1 != 1 {
        if node.repeat_times.1 > 1 {
            // 构造一个循环， to_state 反指 from_state
            (*end_state).borrow_mut().insert_transition(Transition {
                data: None,
                to_state: Rc::downgrade(&begin_state),
            })
        }

        // 0次再加两个节点
        if node.repeat_times.0 == 0 {
            let old_begin_state = begin_state;
            let old_end_state = end_state;
            begin_state = Rc::new(RefCell::new(State::with_counter(counter)));
            end_state = Rc::new(RefCell::new(State::with_counter_accept(counter)));

            (*begin_state)
                .borrow_mut()
                .insert_transition(Transition::epsilon(Rc::downgrade(&old_begin_state)));

            (*old_end_state)
                .borrow_mut()
                .insert_transition(Transition::epsilon(Rc::downgrade(&end_state)));
            (*old_end_state).borrow_mut().acceptable = false;

            (*begin_state)
                .borrow_mut()
                .insert_transition(Transition::epsilon(Rc::downgrade(&end_state)));

            states_vec.insert(0, begin_state.clone());
            states_vec.push(end_state.clone());
        }
    }

    // println!(
    //     "sub graph:\n {}",
    //     StateGraph {
    //         name: "".to_owned(),
    //         states: states_vec.clone(),
    //     }
    // );

    StateGraph {
        name: "".to_owned(),
        states: states_vec,
    }
}

////////////////////////////////////////////////////////////////////////////////
/// Runner

// Do NFA match
pub fn str_full_match_with_nfa(nfa_g: &StateGraph<char, char, char>, input: &str) -> bool {
    // #[cfg(debug_assertions)]
    // println!("NFA matching: {} ", input);

    let matched;
    let chars_input = input.chars().collect_vec();

    matched = match match_with_nfa(nfa_g, &chars_input[..]) {
        MatchResult::FullMatched => true,
        _ => false,
    };

    // #[cfg(debug_assertions)]
    // println!("full matched? : {}\n", matched);

    matched
}

/// Do DFA match
pub fn str_full_match_with_dfa(dfa_g: &DFAStateGraph<char, char, char>, input: &str) -> bool {
    #[cfg(debug_assertions)]
    println!("DFA matching: {} ", input);

    // println!("DFA G:\n{}", dfa_g);

    let matched;
    let chars_input = input.chars().collect_vec();

    matched = match match_with_dfa(dfa_g, &chars_input[..]) {
        MatchResult::FullMatched => true,
        _ => false,
    };

    #[cfg(debug_assertions)]
    println!("matched? : {}\n", matched);

    matched
}

////////////////////////////////////////////////////////////////////////////////
//// Token Matcher
pub trait TokenMatcher {
    fn is_match(&self, value: &str) -> bool;
    fn name(&self) -> &str;
}

impl fmt::Display for dyn TokenMatcher {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name())
    }
}

impl fmt::Debug for dyn TokenMatcher {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name())
    }
}

impl PartialEq for dyn TokenMatcher {
    fn eq(&self, other: &Self) -> bool {
        self.name() == other.name()
    }
}

pub fn ascii_charset() -> CharSet {
    CharSet::with_char_scope(('\u{0}', '\u{127}'))
}

////////////////////////////////////////////////////////////////////////////////
/// Primitive Regex Matcher using NFA State<char, char, char>
#[derive(Clone, Debug)]
pub struct NFAMatcher {
    name: String,
    state_g: StateGraph<char, char, char>,
}

impl NFAMatcher {
    pub fn from_regex_node(name: &str, regex_node: &RegexNode) -> Self {
        let nfa_g = regex2nfa(&mut gen_counter(), regex_node);

        Self {
            name: name.to_string(),
            state_g: nfa_g,
        }
    }
}

impl TokenMatcher for NFAMatcher {
    fn name(&self) -> &str {
        &self.name
    }

    fn is_match(&self, value: &str) -> bool {
        str_full_match_with_nfa(&self.state_g, value)
    }
}

////////////////////////////////////////////////////////////////////////////////
/// Primitive Regex Matcher using DFA State<char, char, char>
#[derive(Clone, Debug)]
pub struct DFAMatcher {
    name: String,
    state_g: DFAStateGraph<char, char, char>,
}

impl DFAMatcher {
    pub fn from_regex_node(name: &str, regex_node: &RegexNode) -> Self {
        let nfa_g = regex2nfa(&mut gen_counter(), regex_node);
        let dfa_g =
            DFAStateGraph::from_nfa_graph(nfa_g, Box::new(ascii_charset()), CharSet::new_getter);

        Self {
            name: name.to_string(),
            state_g: dfa_g,
        }
    }
}

impl TokenMatcher for DFAMatcher {
    fn name(&self) -> &str {
        &self.name
    }

    fn is_match(&self, value: &str) -> bool {
        str_full_match_with_dfa(&self.state_g, value)
    }
}

////////////////////////////////////////////////////////////////////////////////
/// Common Lex Grammar Node

/// Generate Literal Regex Node
/// "abc" => a `and` b `and` c (differ from Charset:parse which is `or`)
pub fn lit_regex_node(lit: &str) -> RegexNode {
    let mut node = RegexNode::create_and_node();

    for c in lit.chars() {
        node.add_child(RegexNode::from_charset(CharSet::with_char_scope((c, c))));
    }

    node
}

// declaretive macro is weak,
// it's just a workaround impl,
// using proc macro for better
// key embarassing thing is declare macro can't support make symbol
#[macro_export]
macro_rules! make_matcher {
    ($name_r:ident => $name_m:ident) => {
        pub fn $name_m() -> Box<dyn $crate::regex::TokenMatcher> {
            Box::new($crate::regex::DFAMatcher::from_regex_node(
                $crate::utils::but_last_n_str(stringify!($name_r), 2),
                &$name_r(),
            ))
        }
    };

    ($name_r:ident => $name_m:ident | nfa) => {
        pub fn $name_m() -> Box<dyn $crate::regex::TokenMatcher> {
            Box::new($crate::regex::NFAMatcher::from_regex_node(
                $crate::utils::but_last_n_str(stringify!($name_r), 2),
                &$name_r(),
            ))
        }
    };
}

#[macro_export]
macro_rules! make_regex_and_matcher {
    ($name_r:ident, $name_m:ident, $value:literal, $name:literal) => {
        pub fn $name_r() -> $crate::regex::RegexNode {
            $crate::regex::lit_regex_node($value)
        }

        pub fn $name_m() -> Box<dyn $crate::regex::TokenMatcher> {
            Box::new(
                //$crate::regex::DFAMatcher::from_regex_node($name, &$name_r())
                // NFA2DFA 好像有点儿问题，暂时用NFA
                $crate::regex::NFAMatcher::from_regex_node($name, &$name_r())
            )
        }
    };

    ($name_r:ident , $name_m:ident , $value:literal) => {
        make_regex_and_matcher!($name_r, $name_m, $value, $value);
    };

    ($name_r:ident , $name_m:ident) => {
        pub fn $name_r() -> $crate::regex::RegexNode {
            $crate::regex::lit_regex_node($crate::utils::but_last_n_str(stringify!($name_r), 2))
        }

        make_matcher!($name_r => $name_m | nfa);  // NFA2DFA 好像有点儿问题，暂时用NFA
    };
}

//
// Regex Node And Matcher
//
make_regex_and_matcher!(int_r, int_m);
make_regex_and_matcher!(if_r, if_m);
make_regex_and_matcher!(else_r, else_m);
make_regex_and_matcher!(add_r, add_m, "+", "add");
make_regex_and_matcher!(sub_r, sub_m, "-", "sub");
make_regex_and_matcher!(mul_r, mul_m, "*", "mul");
make_regex_and_matcher!(div_r, div_m, "/", "div");
make_regex_and_matcher!(lparen_r, lparen_m, "(", "lparen");
make_regex_and_matcher!(rparen_r, rparen_m, ")", "rparen");
make_regex_and_matcher!(singlequote_r, singlequote_m, r#"'"#, "singlequote");
make_regex_and_matcher!(doublequote_r, doublequote_m, r#"""#, "doublequote");
make_regex_and_matcher!(underscope_r, underscope_m, r#"_"#, "underscope");
make_regex_and_matcher!(dot_r, dot_m, ".", "dot");
make_regex_and_matcher!(semi_r, semi_m, ";", "semi");
make_regex_and_matcher!(boolean_r, boolean_m);
make_regex_and_matcher!(return_r, return_m, "return", "r#return");
make_regex_and_matcher!(lbrace_r, lbrace_m, "{", "lbrace");
make_regex_and_matcher!(rbrace_r, rbrace_m, "}", "rbrace");
make_regex_and_matcher!(lbrack_r, lbrack_m, "[", "lbrack");
make_regex_and_matcher!(rbrack_r, rbrack_m, "]", "rbrack");
make_regex_and_matcher!(void_r, void_m);
make_regex_and_matcher!(comma_r, comma_m, ",", "comma");
make_regex_and_matcher!(extends_r, extends_m);
make_regex_and_matcher!(class_r, class_m);
make_regex_and_matcher!(assign_r, assign_m, "=", "assign");
make_regex_and_matcher!(n_r, n_m);

/// Identity Regex
/// [_a-zA-Z][_a-zA-Z0-9]+
pub fn id_r() -> RegexNode {
    let mut root = RegexNode::create_and_node();
    let capital_node = RegexNode::from_charset(charset!(_ - _ | a - z | A - Z));
    let trails_node = RegexNode::from_charset_repeat_times(
        charset!(_ - _ | a - z | A - Z | 0 - 9),
        (0, usize::MAX),
    );

    root.add_child(capital_node);
    root.add_child(trails_node);

    root
}

/// 匹配以转义符`\`开头的长度为2的串
pub fn escape_seq_r() -> RegexNode {
    simple_regex! {
        [r#"\"#](1)[r#"tnbfr"\"#](1)
    }
}

/// 匹配单个字符，排除`\`, `"`, `\n`, `\t`
pub fn normal_seq_r() -> RegexNode {
    let mut normal_seq_charset = all_char_set();
    normal_seq_charset.exclude_chars("\"\\\n\t");

    RegexNode::from_charset(normal_seq_charset)
}

pub fn strlit_r() -> RegexNode {
    let mut root = RegexNode::create_and_node();

    let mut seq = RegexNode::create_or_node();
    seq.add_child(escape_seq_r());
    seq.add_child(normal_seq_r());
    seq.repeat_times = (0, usize::MAX);

    root.add_child(doublequote_r());
    root.add_child(seq);
    root.add_child(doublequote_r());

    root
}

pub fn no_zero_digit_r() -> RegexNode {
    simple_regex! { ["1-9"](1) }
}

pub fn digit_r() -> RegexNode {
    simple_regex! { ["0-9"](1) }
}

pub fn underscopes_r() -> RegexNode {
    let mut node = underscope_r();

    node.repeat_times = (1, usize::MAX);

    node
}

/// ```antlr
/// fragment
/// Digits  // 0_1, 22, 3, 4__5_
///     : Digit (((Digit | UNDERSCORE)+)? Digit)?
///     ;
/// ```
pub fn digits_r() -> RegexNode {
    let mut root = RegexNode::create_and_node();

    let captial_part = digit_r();

    let mut trail_part_1 = RegexNode::create_and_node();

    let mut mid_part_1 = RegexNode::create_or_node();
    mid_part_1.add_child(digit_r());
    mid_part_1.add_child(underscope_r());
    mid_part_1.repeat_times = (1, usize::MAX);

    let mid_part = RegexNode::wrap_at_most_one(mid_part_1);

    trail_part_1.add_child(mid_part);
    trail_part_1.add_child(digit_r());

    let trail_part = RegexNode::wrap_at_most_one(trail_part_1);

    root.add_child(captial_part);
    root.add_child(trail_part);

    root
}

/// ```antlr
/// DecimalFloatingPointLiteral
/// : Digits DOT Digits?
/// | DOT Digits
/// ```
pub fn floatlit_r() -> RegexNode {
    let mut root = RegexNode::create_or_node();

    let mut br1 = RegexNode::create_and_node();
    br1.add_child(digits_r());
    br1.add_child(dot_r());
    br1.add_child(RegexNode::wrap_at_most_one(digit_r()));

    let mut br2 = RegexNode::create_and_node();
    br2.add_child(dot_r());
    br2.add_child(digits_r());

    root.add_child(br1);
    root.add_child(br2);

    root
}

/// line comment: `; xxxxx`
pub fn semi_line_comment_r() -> RegexNode {
    let mut escape_charset = all_char_set();
    escape_charset.exclude_chars("\n\r");
    let anybut_node = RegexNode::from_charset_repeat_times(escape_charset, (0, usize::MAX));

    let mut root = RegexNode::create_and_node();
    root.add_child(semi_r());
    root.add_child(anybut_node);

    root
}

/// line comment: `; xxxxx`
pub fn slash_line_comment_r() -> RegexNode {
    let mut escape_charset = all_char_set();
    escape_charset.exclude_chars("\n\r");
    let anybut_node = RegexNode::from_charset_repeat_times(escape_charset, (0, usize::MAX));

    let mut root = RegexNode::create_and_node();
    root.add_child(lit_regex_node("//"));
    root.add_child(anybut_node);

    root
}

/// 就远匹配而不是就近匹配 FIXME: 仿照strlit_r实现
pub fn slash_block_comment_r() -> RegexNode {
    let escape_charset = all_char_set();
    let anybut_node = RegexNode::from_charset_repeat_times(escape_charset, (0, usize::MAX));

    let mut root = RegexNode::create_and_node();
    root.add_child(lit_regex_node("/*"));
    root.add_child(anybut_node);
    root.add_child(lit_regex_node("*/"));

    root
}

pub fn intlit_r() -> RegexNode {
    digits_r()
}

/// Int Regex
/// [0-9]+
pub fn number_r() -> RegexNode {
    RegexNode::from_charset_repeat_times(charset!(0 - 9), (1, usize::MAX))
}

make_matcher!(id_r => id_m | nfa);
make_matcher!(intlit_r => intlit_m | nfa);
make_matcher!(strlit_r => strlit_m | nfa);
make_matcher!(digits_r => digits_m | nfa);
make_matcher!(floatlit_r => floatlit_m | nfa);
make_matcher!(semi_line_comment_r => semi_line_comment_m | nfa);
make_matcher!(slash_line_comment_r => slash_line_comment_m | nfa);
make_matcher!(slash_block_comment_r => slash_block_comment_m | nfa);

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_struct_charset() {
        use super::CharSet;

        let mut mycharset = CharSet::new();
        mycharset.insert_scope(('a', 'z'));
        mycharset.insert_scope(('2', '2'));

        assert_eq!(format!("{}", mycharset), "2a-z");
        assert!(mycharset.contains(&'b'));
        assert!(!mycharset.contains(&'3'));
        assert!(mycharset.contains(&'2'));
    }

    #[test]
    fn test_macro_charset() {
        assert_eq!(
            format!("{}", charset! { a-a | 0-9 | a-z | 你-好}),
            "0-9a-z你-好"
        );
    }

    #[test]
    fn test_charset_intoiter() {
        use std::collections::HashSet;

        assert_eq!(
            charset![a - a | 0 - 3]
                .into_iter()
                .collect::<HashSet<char>>(),
            hashset!['a', '0', '1', '2', '3']
        );
    }

    #[test]
    fn test_simple_regex() {
        use crate::simple_regex;

        let regex1 = simple_regex! { ["a-zb-xc"](+) ["z"]() ["z"]() };
        println!("{}", regex1);
    }

    #[test]
    fn nfa_dfa_match_works() {
        use super::{
            ascii_charset, gen_counter, int_r, lparen_r, regex2nfa, str_full_match_with_dfa,
            str_full_match_with_nfa, CharSet,
        };

        let mut nfa_g = regex2nfa(&mut gen_counter(), &int_r());
        let mut dfa_root = DFAStateGraph::from_nfa_graph(
            nfa_g.clone(),
            Box::new(ascii_charset()),
            CharSet::new_getter,
        );

        // println!("{}", int_r());
        // println!("{}", nfa_g);
        // Pattern "int"
        assert!(str_full_match_with_nfa(&nfa_g, "int"));
        assert!(!str_full_match_with_nfa(&nfa_g, "Int"));
        assert!(!str_full_match_with_nfa(&nfa_g, "i7t"));

        assert!(str_full_match_with_dfa(&dfa_root, "int"));
        assert!(!str_full_match_with_dfa(&dfa_root, "Int"));
        assert!(!str_full_match_with_dfa(&dfa_root, "i7t"));

        // pattern "("
        nfa_g = regex2nfa(&mut gen_counter(), &lparen_r());
        dfa_root = DFAStateGraph::from_nfa_graph(
            nfa_g.clone(),
            Box::new(ascii_charset()),
            CharSet::new_getter,
        );
        assert!(!str_full_match_with_nfa(&nfa_g, "int"));
        assert!(!str_full_match_with_nfa(&nfa_g, "I"));
        assert!(str_full_match_with_nfa(&nfa_g, "("));

        assert!(!str_full_match_with_dfa(&dfa_root, "int"));
        assert!(!str_full_match_with_dfa(&dfa_root, "I"));
        assert!(str_full_match_with_dfa(&dfa_root, "("));
    }

    #[test]
    fn pri_regx_matcher_did_works() {
        use super::{int_m, rparen_m};

        let int_m = int_m();

        assert!(int_m.is_match("int"));
        assert!(!int_m.is_match("Int"));
        assert!(!int_m.is_match("i7t"));

        let rparen_m = rparen_m();

        debug_assert!(rparen_m.is_match(")"));

        assert!(!rparen_m.is_match("("));
    }

    #[test]
    fn test_strlit() {
        let nfa_g = regex2nfa(&mut gen_counter(), &strlit_r());
        println!("strlit graph: \n{}", nfa_g);

        // Test NFA
        assert!(str_full_match_with_nfa(&nfa_g, r#"  "abcdef"   "#.trim()));
        assert!(str_full_match_with_nfa(&nfa_g, r#"  "abc\\def"   "#.trim()));
        assert!(str_full_match_with_nfa(
            &nfa_g,
            r#"  "abc\\\"def"   "#.trim()
        ));
        assert!(!str_full_match_with_nfa(&nfa_g, r#"  "abc"def"   "#.trim()));

        // Test DFA
        // strlit_m
        let strlitm = strlit_m();

        assert!(strlitm.is_match(r#"  "abcdef"   "#.trim()));

        assert!(strlitm.is_match(r#"  "abc\\def"   "#.trim()));

        assert!(strlitm.is_match(r#"  "abc\\\"def"   "#.trim()));

        assert!(!strlitm.is_match(r#"  "abc"def"   "#.trim()));
    }

    #[test]
    fn test_some_defined_matcher() {
        let nfa_digits = regex2nfa(&mut gen_counter(), &digits_r());
        assert!(str_full_match_with_nfa(&nfa_digits, "41_12"));
        // digits_m
        let digitsm = digits_m();

        assert!(!digitsm.is_match(""));
        assert!(digitsm.is_match("5"));
        assert!(digitsm.is_match("02"));
        assert!(digitsm.is_match("1_24"));
        assert!(digitsm.is_match("1__2"));
        assert!(digitsm.is_match("0_2"));
        assert!(!digitsm.is_match("123__"));

        let floatm = floatlit_m();

        assert!(floatm.is_match("12.0"));
        assert!(floatm.is_match(".1"));
        assert!(floatm.is_match(".0"));
        assert!(floatm.is_match("0.0"));
        assert!(floatm.is_match("0."));
        assert!(!floatm.is_match("12"));
        assert!(!floatm.is_match("."));

        let line_commentm = semi_line_comment_m();
        assert!(line_commentm.is_match(";abxsxjsb/\"xs/\'"));
        assert!(!line_commentm.is_match(
            ";abxsxjsb/\"xs/\'

        "
        ));
        assert!(line_commentm.is_match(";aaa."));

        let slash_commentm = slash_line_comment_m();
        assert!(
            slash_commentm.is_match(r#" //下面两个的叫声会不同。在运行期动态绑定方法。"#.trim())
        );

        let block_slashm = slash_block_comment_m();
        assert!(block_slashm.is_match(
            r#"
        /**
        abcdde*
        */
        "#
            .trim()
        ));

        assert!(block_slashm.is_match(
            r#"
        /**
        abcdde* /
        */
        "#
            .trim()
        ));

        assert!(block_slashm.is_match(
            r#"
        /**
        mammal.play 演示面向对象编程：继承和多态。
        */
        "#
            .trim()
        ));

        assert!(!block_slashm.is_match(
            r#"
        /**/
        mammal.play 演示面向对象编程：继承和多态。

        "#
            .trim()
        ));

        assert!(void_m().is_match("void"));

        let idm = id_m();

        assert!(idm.is_match("a"));
    }

    #[test]
    fn test_all_sets() {
        use super::{all_char_set, gen_counter, regex2nfa, slash_line_comment_r};

        let mut all = all_char_set();
        all.exclude_chars("\n\r");

        assert!(all.contains(&'。'));

        let state = regex2nfa(&mut gen_counter(), &slash_line_comment_r());
        println!("{:?}", state);

        str_full_match_with_nfa(&state, "//a。a");
    }
}
