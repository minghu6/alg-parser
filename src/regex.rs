#![allow(dead_code)]

use std::{
    borrow::Borrow,
    cell::RefCell,
    collections::{HashMap, HashSet, VecDeque},
    fmt,
    hash::{Hash, Hasher},
    iter::IntoIterator,
    mem::drop,
    rc::{Rc},
    vec::IntoIter,
};

use key_set::{KeyHashSet, KeySet};

use super::utils::{char_inc, char_range, gen_counter, CounterType};

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

                _charset.add((lowerc, upperc));
            )*

            _charset
        }
    }
}

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

    pub fn with_char_scope(char_scope: (char, char)) -> Self {
        let char_scopes = vec![char_scope];

        Self { char_scopes }
    }

    pub fn add(&mut self, char_scope: (char, char)) {
        self.char_scopes.push(char_scope); // push return ()
        self.compress(); // 每次都压缩可能不太合适
    }

    pub fn contains(&self, inputc: &char) -> bool {
        for (lower, upper) in self.char_scopes.iter() {
            if lower <= inputc && inputc <= upper {
                return true;
            }
        }

        false
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

    pub fn iter(&self) -> IntoIter<char> {
        self.to_vec().into_iter()
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
            End, Fst, Mid
        }

        let mut status = Status::End;
        let mut scope = vec![];
        for c in input.chars() {
            match status {
                Status::End => {
                    scope.push(c);
                    status = Status::Fst;
                },

                Status::Fst => {
                    match c {
                        '-' => status = Status::Mid,
                        _ => {
                            scope.push(scope[0]);
                            char_scopes.push((scope[0], scope[1]));
                            scope = vec![c];
                            status = Status::Fst;
                        }
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

        Self {
            char_scopes
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


////////////////////////////////////////////////////////////////////////////////
/// GrammarNode: Simple Regex Node

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum GrammarNodeType {
    And,
    Or,
    Node,
    Epsilon, // 空集
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct RegexNode {
    pub childen: Vec<Box<RegexNode>>,
    pub nodetype: GrammarNodeType,
    pub repeat_times: (usize, usize),
    pub chars: CharSet,
    pub name: Option<String>,
}

impl RegexNode {
    ///
    /// Static Create Method
    ///
    pub fn new() -> Self {
        Self {
            childen: Vec::new(),
            nodetype: GrammarNodeType::Epsilon,
            repeat_times: (1, 1),
            chars: CharSet::new(),
            name: None,
        }
    }

    pub fn from_charset(chars: CharSet) -> Self {
        let mut node = Self::new();
        node.nodetype = GrammarNodeType::Node;
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
        node.nodetype = GrammarNodeType::Or;

        node
    }

    pub fn create_and_node() -> Self {
        let mut node = Self::new();
        node.nodetype = GrammarNodeType::And;

        node
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
            let delim = if self.nodetype == GrammarNodeType::And {
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
/// State： (AKA NFA State)

#[derive(Clone)]
pub struct State {
    pub id: usize,
    pub acceptable: bool,
    pub transitions: Vec<Transition>,
    pub grammar_node: RegexNode,
}

impl State {
    ///
    /// Static Create Method
    ///
    pub fn with_counter(counter: &mut CounterType) -> Self {
        Self {
            id: counter(),
            acceptable: false,
            transitions: Vec::new(),
            grammar_node: RegexNode::new(),
        }
    }

    /// with acceptable is true
    pub fn with_counter_accept(counter: &mut CounterType) -> Self {
        let mut state = Self::with_counter(counter);
        state.acceptable = true;
        state
    }

    ///
    /// Instance Update Method
    ///

    pub fn add_transition(&mut self, transition: Transition) {
        self.transitions.push(transition)
    }

    pub fn copy_transitions(&mut self, state: &State) {
        self.transitions = state.transitions.clone();
    }

    pub fn add_epsilon_transition(&mut self, state: Rc<RefCell<State>>) {
        self.add_transition(Transition::epsilon(state))
    }

    fn dump(
        state: &Self,
        f: &mut fmt::Formatter<'_>,
        visited_states: &mut HashSet<usize>,
    ) -> fmt::Result {
        let this_state = state;
        match writeln!(f, "{}", this_state) {
            // 打印单状态
            Err(err) => return Err(err),
            _ => (),
        }

        visited_states.insert(this_state.id);

        for transition in this_state.transitions.iter() {
            let to_state = (*transition.to_state).borrow();
            if !visited_states.contains(&to_state.id) {
                match Self::dump(&to_state, f, visited_states) {
                    Err(err) => {
                        return Err(err);
                    }
                    _ => (),
                }
            }
        }

        Ok(())
    }
}

/// Hash
impl Hash for State {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_usize(self.id)
    }
}

impl PartialEq<State> for State {
    fn eq(&self, other: &State) -> bool {
        self.id == other.id
    }
}

impl Eq for State {}

/// Show one state
impl fmt::Display for State {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut show_str = String::from(format!("{}", self.id));

        if self.transitions.len() > 0 {
            for transition in self.transitions.iter() {
                let to_state = &transition.to_state;
                let to_state_id = (*(*to_state)).borrow().id;

                show_str.push_str(
                    format!(
                        "\t{} -> {}\n",
                        format!("{}", transition),
                        format!("{}", to_state_id)
                    )
                    .as_str(),
                );
            }
        } else {
            show_str.push_str("\t(end)");
            show_str.push_str("\n");
        }

        if self.acceptable {
            show_str.push_str("\tacceptable\n");
        }

        write!(f, "{}", show_str.as_str())
    }
}

/// Show full state
impl fmt::Debug for State {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut visited_states = hashset!();

        match State::dump(self, f, &mut visited_states) {
            Err(err) => Err(err),
            Ok(()) => Ok(()),
        }
    }
}


////////////////////////////////////////////////////////////////////////////////
/////// Transition

#[derive(Debug, Clone)]
pub struct Transition {
    pub chars: CharSet,
    pub max_times: usize,
    pub to_state: Rc<RefCell<State>>,
}

impl Transition {
    pub fn from_grammar_node(node: &RegexNode, to_state: Rc<RefCell<State>>) -> Self {
        Self {
            chars: node.chars.clone(),
            max_times: 1,
            to_state: to_state,
        }
    }

    pub fn from_max_times(max_times: usize, to_state: Rc<RefCell<State>>) -> Self {
        Self {
            chars: CharSet::new(),
            max_times,
            to_state: to_state,
        }
    }

    pub fn epsilon(to_state: Rc<RefCell<State>>) -> Self {
        Self {
            chars: CharSet::new(),
            max_times: 0,
            to_state: to_state,
        }
    }

    pub fn is_match(&self, inputc: &char) -> bool {
        self.chars.contains(inputc)
    }

    pub fn is_epsilon(&self) -> bool {
        self.chars.is_empty()
    }
}

impl fmt::Display for Transition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_epsilon() {
            write!(f, "{}", "ε")
        } else {
            write!(f, "{}", self.chars)
        }
    }
}

////////////////////////////////////////////////////////////////////////////////
/////// DFA State: 建立在普通的State(NFA State)的集合的基础之上

pub type StatesSet = KeyHashSet<Rc<RefCell<State>>, usize>;

pub fn display_states_set(states_set: &StatesSet) {
    let states_set_ref = (*states_set).borrow();

    for state in states_set_ref.iter() {
        let state_ref = (**state).borrow();
        println!("{}", state_ref);
    }
}

pub struct DFAState {
    pub id: usize,
    pub states: Rc<RefCell<StatesSet>>,
    pub transitions: Vec<DFATransition>,
}

impl DFAState {
    pub fn dfa_states_get_key(x: &Rc<RefCell<DFAState>>) -> usize {
        (**x).borrow().id
    }

    pub fn states_set_getkey(x: &Rc<RefCell<State>>) -> usize {
        (**x).borrow().id
    }

    pub fn new_key_set() -> Rc<RefCell<StatesSet>> {
        Rc::new(RefCell::new(KeyHashSet::new(DFAState::states_set_getkey)))
    }

    pub fn with_counter_states(counter: &mut CounterType, states: Rc<RefCell<StatesSet>>) -> Self {
        Self {
            id: counter(),
            states,
            transitions: vec![],
        }
    }

    pub fn is_acceptable(&self) -> bool {
        (*self.states)
            .borrow()
            .iter()
            .any(|state| (*(*state)).borrow().acceptable)
    }

    pub fn add_transition(&mut self, transition: DFATransition) {
        self.transitions.push(transition)
    }

    pub fn has_transition(&self, to_state: Rc<RefCell<DFAState>>) -> bool {
        self.transitions
            .iter()
            .any(|trans| trans.to_state == to_state)
    }

    /// Print
    fn dump(
        state: &Self,
        f: &mut fmt::Formatter<'_>,
        visited_states: &mut HashSet<usize>,
    ) -> fmt::Result {
        let this_state = state;
        match writeln!(f, "{}", this_state) {
            // 打印单状态
            Err(err) => return Err(err),
            _ => (),
        }

        visited_states.insert(this_state.id);

        for transition in this_state.transitions.iter() {
            let to_state_rc = transition.to_state.clone();
            let to_state = to_state_rc.as_ref().borrow();

            if !visited_states.contains(&to_state.id) {
                match Self::dump(&to_state, f, visited_states) {
                    Err(err) => {
                        return Err(err);
                    }
                    _ => (),
                }
            }
        }

        Ok(())
    }
}

/// 根据to_state查找transition, 没有就创建;
/// 在这个transition的charset上插入这个字符
pub fn insert_char_on_transition(
    from_state: Rc<RefCell<DFAState>>,
    to_state: Rc<RefCell<DFAState>>,
    c: char,
) {
    let from_state_ref = from_state.as_ref().borrow();
    let trans_ind = from_state_ref
        .transitions
        .iter()
        .position(|trans| trans.to_state == to_state)
        .unwrap();
    let mut new_trans = from_state_ref.transitions[trans_ind].clone();
    drop(from_state_ref);

    new_trans.chars.add((c, c));
    from_state
        .as_ref()
        .borrow_mut()
        .transitions
        .remove(trans_ind);
    from_state.as_ref().borrow_mut().transitions.push(new_trans);
}

/// DFAState 比较相等根据它所包含的State
/// 而State 比较相等根据它的id
impl PartialEq for DFAState {
    fn eq(&self, other: &DFAState) -> bool {
        let state_keyset = (*self.states).borrow();
        let other_keyset = (*other.states).borrow();

        *state_keyset == *other_keyset
    }
}

/// Show one state
impl fmt::Display for DFAState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut show_str = String::from(format!("({})", self.id));

        if self.transitions.len() > 0 {
            for transition in self.transitions.iter() {
                let to_state = &transition.to_state;
                let to_state_id = (*(*to_state)).borrow().id;

                show_str.push_str(
                    format!(
                        "\t{} -> {}",
                        format!("{}", transition),
                        format!("({})", to_state_id)
                    )
                    .as_str(),
                );

                show_str.push('\n');
            }
        }

        write!(f, "{}", show_str.as_str()).ok();

        write!(f, "\tNFA states: ",).ok();
        write!(
            f,
            "{}\n",
            (*(self.states))
                .borrow()
                .iter()
                .map(|state| format!("_{}", (**state).borrow().id))
                .collect::<Vec<String>>()
                .join(", ")
        )
        .ok();

        if self.is_acceptable() {
            writeln!(f, "\t(acceptable)").ok();
        }

        if self.transitions.is_empty() {
            writeln!(f, "\t(end)").ok();
        }

        Ok(())
    }
}

/// Show full state
impl fmt::Debug for DFAState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut visited_states = hashset!();

        match Self::dump(self, f, &mut visited_states) {
            Err(err) => Err(err),
            Ok(()) => Ok(()),
        }
    }
}

////////////////////////////////////////////////////////////////////////////////
/////// DFATransition: 因为有to_state这个字段， 所以无法复用, 而且实际上性质也不同
#[derive(Clone)]
pub struct DFATransition {
    pub chars: CharSet,
    pub to_state: Rc<RefCell<DFAState>>,
}

impl DFATransition {
    pub fn new(to_state: Rc<RefCell<DFAState>>) -> Self {
        Self {
            chars: CharSet::new(),
            to_state: to_state,
        }
    }

    pub fn with_chars(to_state: Rc<RefCell<DFAState>>, chars: CharSet) -> Self {
        let mut this = Self::new(to_state);
        this.chars = chars;
        this
    }

    pub fn is_match(&self, inputc: &char) -> bool {
        self.chars.contains(inputc)
    }
}

impl fmt::Display for DFATransition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.chars)
    }
}

////////////////////////////////////////////////////////////////////////////////
/// Converter

/// Concatenate two subgraph
/// 也许subgraph应该是一个子图结构
pub fn concat_state(
    from_graph: &(Rc<RefCell<State>>, Rc<RefCell<State>>),
    to_graph: &(Rc<RefCell<State>>, Rc<RefCell<State>>),
) {
    let from_end_state = Rc::clone(&from_graph.1);
    let to_begin_state = Rc::clone(&to_graph.0);

    (*from_end_state)
        .borrow_mut()
        .copy_transitions(&(*to_begin_state).borrow());
    (*from_end_state).borrow_mut().acceptable = false;
}

/// Regex to NFA

fn add_repetition(
    from_state: Rc<RefCell<State>>,
    to_state: Rc<RefCell<State>>,
    node: &RegexNode,
    counter: &mut CounterType,
) -> (Rc<RefCell<State>>, Rc<RefCell<State>>) {
    let (begin_state, end_state);

    if node.repeat_times.1 > 1 {
        // 构造一个循环， to_state 反指 from_state
        (*to_state)
            .borrow_mut()
            .add_transition(Transition::from_max_times(
                node.repeat_times.1,
                Rc::clone(&from_state),
            ))
    }

    // 0次再加两个节点
    if node.repeat_times.0 == 0 {
        begin_state = Rc::new(RefCell::new(State::with_counter(counter)));
        end_state = Rc::new(RefCell::new(State::with_counter_accept(counter)));

        (*begin_state)
            .borrow_mut()
            .add_transition(Transition::epsilon(Rc::clone(&to_state)));

        (*to_state)
            .borrow_mut()
            .add_transition(Transition::epsilon(Rc::clone(&end_state)));
        (*to_state).borrow_mut().acceptable = false;

        (*begin_state)
            .borrow_mut()
            .add_transition(Transition::epsilon(Rc::clone(&end_state)));
    } else {
        begin_state = from_state;
        end_state = to_state;
    }

    (begin_state, end_state)
}

pub fn regex2nfa(counter: &mut CounterType, node: &RegexNode) -> Rc<RefCell<State>> {
    _regex2nfa(counter, node).0
}

fn _regex2nfa(
    counter: &mut CounterType,
    node: &RegexNode,
) -> (Rc<RefCell<State>>, Rc<RefCell<State>>) {
    let (mut begin_state, mut end_state);

    match node.nodetype {
        GrammarNodeType::Or => {
            // 构建新的开始和结束状态，用它们连每一个转移状态
            begin_state = Rc::new(RefCell::new(State::with_counter(counter)));
            end_state = Rc::new(RefCell::new(State::with_counter_accept(counter)));

            for child in node.childen.iter() {
                let (sub_begin_state, sub_end_state) = _regex2nfa(counter, child);

                // 新的开始状态通过 ε 连接到子图的开始状态
                // 子图的结束状态通过 ε 连接到新的结束状态
                (*begin_state)
                    .borrow_mut()
                    .add_epsilon_transition(Rc::clone(&sub_begin_state));
                (*sub_end_state)
                    .borrow_mut()
                    .add_epsilon_transition(Rc::clone(&end_state));
                (*sub_end_state).borrow_mut().acceptable = false;
            }
        }

        GrammarNodeType::And => {
            // 各个转移状态前后相连
            let sub_graphs: Vec<(Rc<RefCell<State>>, Rc<RefCell<State>>)> = node
                .childen
                .iter()
                .map(|child| _regex2nfa(counter, child))
                .collect();

            if sub_graphs.len() > 0 {
                begin_state = Rc::clone(&sub_graphs.first().unwrap().0);
                end_state = Rc::clone(&sub_graphs.last().unwrap().1);

                for i in 1..sub_graphs.len() {
                    concat_state(&mut (sub_graphs[i - 1].clone()), &(sub_graphs[i].clone()))
                }
            } else {
                unreachable!()
            }
        }

        GrammarNodeType::Node => {
            // 根据node的condition创建两个状态
            begin_state = Rc::new(RefCell::new(State::with_counter(counter)));
            end_state = Rc::new(RefCell::new(State::with_counter_accept(counter)));

            (*begin_state)
                .borrow_mut()
                .add_transition(Transition::from_grammar_node(node, Rc::clone(&end_state)))
        }

        _ => unreachable!(),
    }

    // 处理重复的情况
    if node.repeat_times.0 != 1 || node.repeat_times.1 != 1 {
        (begin_state, end_state) = add_repetition(begin_state, end_state, node, counter);
    }

    (begin_state, end_state)
}

/// NFA to DFA
/// return begin NFA state
/// 特别返回Vec是因为DFATransition使用了Weak而不是Rc，所有权转移避免被回收
pub fn nfa2dfa(
    counter: &mut CounterType,
    begin_state: Rc<RefCell<State>>,
    alphabet: &CharSet,
) -> Rc<RefCell<DFAState>>
{
    // state的epsilon闭包缓存
    let mut state_closure_cache = HashMap::new();

    let cur_states_set =
        get_state_epsilon_states_set(Rc::clone(&begin_state), &mut state_closure_cache);

    // 从开始状态的epsilon闭包开始
    let cur_dfa_state = Rc::new(RefCell::new(DFAState::with_counter_states(
        counter,
        Rc::clone(&cur_states_set),
    ))); // state set => dfa state

    // 保存每次计算出的新的states_set
    let mut new_dfa_state_vec = Vec::<Rc<RefCell<DFAState>>>::new();
    // 保存所有已计算的dfa state
    let mut dfa_states_vec = Vec::<Rc<RefCell<DFAState>>>::new();
    dfa_states_vec.push(Rc::clone(&cur_dfa_state));
    new_dfa_state_vec.push(Rc::clone(&cur_dfa_state));

    while new_dfa_state_vec.len() > 0 {
        let wait_for_calc = new_dfa_state_vec;
        new_dfa_state_vec = vec![];

        for taken_state in wait_for_calc.into_iter() {
            for c in alphabet.iter() {
                let taken_states_set = Rc::clone(&(*taken_state).borrow().states);
                // move(s, c)
                let next_states_set = r#move(Rc::clone(&taken_states_set), &c);

                if (*next_states_set).borrow().is_empty() {
                    continue;
                } // 在一个有一定规模的字符集里大部分都是这样的情况

                //  ε-closure
                let next_states_set_closure = states_set_epsilon_closure(
                    Rc::clone(&next_states_set),
                    &mut state_closure_cache,
                );

                let next_dfa_state;
                match find_dfa_state_by_states_set(
                    &dfa_states_vec,
                    Rc::clone(&next_states_set_closure),
                ) {
                    Some(old_dfa_state) => {
                        next_dfa_state = old_dfa_state;

                        if !taken_state
                            .as_ref()
                            .borrow()
                            .has_transition(Rc::clone(&next_dfa_state))
                        {
                            let trans = DFATransition::new(Rc::clone(&next_dfa_state));
                            taken_state.as_ref().borrow_mut().add_transition(trans);
                        }
                    }

                    None => {
                        let new_dfa_state = Rc::new(RefCell::new(DFAState::with_counter_states(
                            counter,
                            Rc::clone(&next_states_set_closure),
                        )));

                        dfa_states_vec.push(Rc::clone(&new_dfa_state));
                        new_dfa_state_vec.push(Rc::clone(&new_dfa_state));

                        (*taken_state)
                            .borrow_mut()
                            .add_transition(DFATransition::new(Rc::clone(&new_dfa_state)));

                        next_dfa_state = new_dfa_state;
                    }
                }

                insert_char_on_transition(Rc::clone(&taken_state), next_dfa_state, c);
            }
        }
    }

    //display_states_set(&(*cur_states_set).borrow());

    //cur_dfa_state
    dfa_states_vec[0].clone()
}

/// 计算一个State通过 ε 转换所能到达的状态集合，目的是把这个计算的集合加入到缓存集中。
pub fn get_state_epsilon_states_set(
    state: Rc<RefCell<State>>,
    cache: &mut HashMap<usize, Rc<RefCell<StatesSet>>>,
) -> Rc<RefCell<StatesSet>> {
    let state_ref = (*state).borrow();
    // 缓存命中
    if let Some(res) = cache.get(&state_ref.id) {
        return Rc::clone(res);
    }

    let closure = DFAState::new_key_set();
    let mut clousure_ref = (*closure).borrow_mut();
    clousure_ref.insert(Rc::clone(&state)); // 包含自身

    for transition in state_ref.transitions.iter() {
        if transition.is_epsilon() {
            let next_state = Rc::clone(&transition.to_state);

            let other = get_state_epsilon_states_set(next_state, cache);
            clousure_ref.extend((*other).borrow().clone());
        }
    }

    // 加入缓存中
    cache.insert(state_ref.id, Rc::clone(&closure));

    Rc::clone(&closure)
}

/// 得到一个状态集合的闭包
fn states_set_epsilon_closure(
    states_set: Rc<RefCell<StatesSet>>,
    cache: &mut HashMap<usize, Rc<RefCell<StatesSet>>>,
) -> Rc<RefCell<StatesSet>> {
    let states_set_ref = (*states_set).borrow();
    let res = DFAState::new_key_set();
    let res_here = Rc::clone(&res);
    let mut res_ref = (*res_here).borrow_mut();

    for state in states_set_ref.iter() {
        let state_here = Rc::clone(state);
        let state_closure = get_state_epsilon_states_set(state_here, cache);
        res_ref.extend((*state_closure).borrow().clone());
    }

    // 包括起始状态
    res_ref.extend((*states_set_ref).borrow().clone());

    res
}

/// 从StatesSet到StatesSet， 不直接用DFAState是因为counter有id分配的问题
fn r#move(s0: Rc<RefCell<StatesSet>>, c: &char) -> Rc<RefCell<StatesSet>> {
    let s0_states_set_ref = (*s0).borrow();

    let mut s1 = KeyHashSet::new(DFAState::states_set_getkey);

    for state in s0_states_set_ref.iter() {
        for trans in state.as_ref().borrow().transitions.iter() {
            if trans.is_match(c) {
                s1.insert(Rc::clone(&trans.to_state));
                break;
            }
        }
    }

    Rc::new(RefCell::new(s1))
}

/// find if states_set exists
fn find_dfa_state_by_states_set(
    states_set_vec: &Vec<Rc<RefCell<DFAState>>>,
    target: Rc<RefCell<StatesSet>>,
) -> Option<Rc<RefCell<DFAState>>> {
    match states_set_vec.iter().find(|&states_set| {
        let dfa_state = (**states_set).borrow();
        let matched = *(*dfa_state.states).borrow() == *(*target).borrow();

        matched
    }) {
        Some(res) => Some(Rc::clone(res)),
        None => None,
    }
}

////////////////////////////////////////////////////////////////////////////////
/// Runner

// Do NFA match
pub fn match_with_nfa(state: &State, input: &str) -> bool {
    #[cfg(debug_assertions)]
    println!("NFA matching: {} ", input);

    let chars_input: Vec<char> = input.chars().map(|x| x).collect();

    let matched_index = _match_with_nfa(state, &chars_input[..], 0);
    let matched = matched_index == input.len();

    #[cfg(debug_assertions)]
    println!("matched? : {}\n", matched);

    matched
}

fn _match_with_nfa(state: &State, chars_input: &[char], init_index: usize) -> usize {
    #[cfg(debug_assertions)]
    println!("trying state : {}, index ={}", state.id, init_index);

    let mut next_index = init_index;

    for transition in state.transitions.iter() {
        let next_state = (*transition.to_state).borrow();
        if transition.is_epsilon() {
            // 自动跳过epsilon转移，到达下一个状态
            // epsilon 自动转换状态
            next_index = _match_with_nfa(&next_state, chars_input, init_index);

            // 检查是不是可以退出了
            if next_index == chars_input.len() {
                break;
            }
        } else if transition.is_match(&chars_input[init_index]) {
            next_index += 1;

            if next_index == chars_input.len() {
                if reach_acceptable_state(&next_state) {
                    break; // 成功匹配
                } else {
                    // 如果最终状态不可接受，意味着分支匹配失败
                    next_index -= 1;
                }
            } else {
                next_index = _match_with_nfa(&next_state, chars_input, next_index);
            }
        }
    }

    next_index
}

/// 当前状态、以及该状态通过 epsilon 连接的状态是否有可接受的(acceptable = True)
fn reach_acceptable_state(state: &State) -> bool {
    if state.acceptable {
        return true;
    }

    state
        .transitions
        .iter()
        .filter(|trans| trans.is_epsilon())
        .any(|trnas| {
            let next_state = trnas.to_state.clone();
            if next_state.as_ref().borrow().acceptable {
                true
            } else {
                reach_acceptable_state(&next_state.as_ref().borrow())
            }
        })
}

// Do DFA match
pub fn match_with_dfa(state: &DFAState, input: &str) -> bool {
    #[cfg(debug_assertions)]
    println!("DFA matching: {} ", input);

    let chars_input: Vec<char> = input.chars().map(|x| x).collect();

    let matched = match _match_with_dfa(state, &chars_input[..], 0) {
        Ok((_, nxt_st)) => nxt_st.as_ref().borrow().is_acceptable(),
        _ => false,
    };

    #[cfg(debug_assertions)]
    println!("matched? : {}\n", matched);

    matched
}

fn _match_with_dfa(
    state: &DFAState,
    chars_input: &[char],
    init_index: usize,
) -> Result<(usize, Rc<RefCell<DFAState>>), ()> {
    #[cfg(debug_assertions)]
    println!("trying DFA state : {}, index ={}", state.id, init_index);

    if let Some(next_state) = match state
        .transitions
        .iter()
        .find(|trans| trans.is_match(&chars_input[init_index]))
    {
        Some(trans) => Some(Rc::clone(&trans.to_state)),
        None => None,
    } {
        let next_index = init_index + 1;

        // 递归结束条件
        if next_index >= chars_input.len() {
            return Ok((next_index, next_state));
        }

        _match_with_dfa(
            &next_state.as_ref().borrow(),
            chars_input,
            next_index,
        )
    } else {
        Err(())
    }
}

////////////////////////////////////////////////////////////////////////////////
/// Primitive Regex Matcher
pub trait TokenMatcher {
    fn is_match(&self, value: &str) -> bool;
}

pub fn ascii_charset() -> CharSet {
    CharSet::with_char_scope(('\u{0}', '\u{127}'))
}

/// A Native Raw Primitive Regex Matcher
#[derive(Clone)]
pub struct PriRegexMatcher {
    name: String,
    state: Rc<RefCell<DFAState>>,
}

impl PriRegexMatcher {
    pub fn with_regex_node(name: &str, regex_node: &RegexNode) -> Self {
        let nfa_root = regex2nfa(&mut gen_counter(), regex_node);
        let dfa_root = nfa2dfa(&mut gen_counter(), nfa_root, &ascii_charset());

        Self {
            name: name.to_string(),
            state: dfa_root,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn new_key_set() -> Rc<RefCell<KeyHashSet<Self, String>>> {
        Rc::new(RefCell::new(
            KeyHashSet::new(|x| x.name.clone())
        ))
    }

    pub fn is_match(&self, value: &str) -> bool {
        match_with_dfa(&self.state.as_ref().borrow(), value)
    }
}

impl fmt::Display for PriRegexMatcher {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl PartialEq for PriRegexMatcher {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

////////////////////////////////////////////////////////////////////////////////
/// Common Lex Grammar Node

/// Generate Literal Regex Node
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
        pub fn $name_m() -> $crate::regex::PriRegexMatcher {
            $crate::regex::PriRegexMatcher::with_regex_node(
                $crate::utils::but_last_n_str(stringify!($name_r), 2),
                &$name_r(),
            )
        }
    }
}

#[macro_export]
macro_rules! make_regex_and_matcher {
    ($name_r:ident , $name_m:ident , $value:literal, $name:literal) => {
        pub fn $name_r() -> $crate::regex::RegexNode {
            $crate::regex::lit_regex_node($value)
        }

        pub fn $name_m() -> $crate::regex::PriRegexMatcher {
            $crate::regex::PriRegexMatcher::with_regex_node($name, &$name_r())
        }
    };

    ($name_r:ident , $name_m:ident , $value:literal) => {
        make_regex_and_matcher!($name_r, $name_m, $value, $value);
    };

    ($name_r:ident , $name_m:ident) => {
        pub fn $name_r() -> $crate::regex::RegexNode {
            $crate::regex::lit_regex_node($crate::utils::but_last_n_str(stringify!($name_r), 2))
        }

        make_matcher!($name_r => $name_m);
    };
}

//
// Regex Node And Matcher
//
make_regex_and_matcher!(int_r, int_m);
make_regex_and_matcher!(if_r, if_m);
make_regex_and_matcher!(else_r, else_m);
make_regex_and_matcher!(add_r, add_m, "+", "'+'");
make_regex_and_matcher!(sub_r, sub_m, "-", "'-'");
make_regex_and_matcher!(mul_r, mul_m, "*", "'*'");
make_regex_and_matcher!(div_r, div_m, "/", "'/'");
make_regex_and_matcher!(lparen_r, lparen_m, "(", "'('");
make_regex_and_matcher!(rparen_r, rparen_m, ")", "')'");


/// Identity Regex
/// [_a-zA-Z][_a-zA-Z0-9]+
pub fn id_r() -> RegexNode {
    let mut root = RegexNode::create_and_node();
    let capital_node = RegexNode::from_charset( charset!(_-_ | a-z | A-Z));
    let trails_node = RegexNode::from_charset_repeat_times(
        charset!(_-_| a-z | A-Z | 0-9),
         (1, usize::MAX)
    );

    root.add_child(capital_node);
    root.add_child(trails_node);

    root
}

pub fn intlit_r() -> RegexNode {
    simple_regex!{ ["_a-zA-Z"]()["0-9a-zA-Z"]() }
}

pub fn strlit_r() -> RegexNode {
    simple_regex!{ [r#"""#](1)[" \t\n"](*)[r#"""#](1) }
}

make_matcher!(id_r => id_m);
make_matcher!(intlit_r => intlit_m);
make_matcher!(strlit_r => strlit_m);


/// Int Regex
/// [0-9]+
pub fn number_r() -> RegexNode {
    RegexNode::from_charset_repeat_times( charset!(0-9), (1, usize::MAX))
}

make_matcher!(number_r => number_m);





#[cfg(test)]
mod test {
    #[test]
    fn test_struct_charset() {
        use super::CharSet;

        let mut mycharset = CharSet::new();
        mycharset.add(('a', 'z'));
        mycharset.add(('2', '2'));

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
            charset![a - a | 0 - 3].into_iter().collect::<HashSet<char>>(),
            hashset!['a', '0', '1', '2', '3']
        );
    }

    #[test]
    fn test_simple_regex() {
        use crate::simple_regex;

        let regex1 = simple_regex!{ ["a-zb-xc"](+) ["z"]() ["z"]() };
        println!("{}", regex1);
    }

    #[test]
    fn nfa_dfa_match_works() {
        use super::{
            match_with_dfa, gen_counter, int_r, lparen_r, match_with_nfa, nfa2dfa, regex2nfa, ascii_charset
        };

        let mut nfa_root = regex2nfa(&mut gen_counter(), &int_r());
        let mut dfa_root = nfa2dfa(&mut gen_counter(), nfa_root.clone(), &ascii_charset());

        // Pattern "int"
        assert!(match_with_nfa(&nfa_root.as_ref().borrow(), "int"));
        assert!(!match_with_nfa(&nfa_root.as_ref().borrow(), "Int"));
        assert!(!match_with_nfa(&nfa_root.as_ref().borrow(), "i7t"));

        assert!(match_with_dfa(&dfa_root.as_ref().borrow(), "int"));
        assert!(!match_with_dfa(&dfa_root.as_ref().borrow(), "Int"));
        assert!(!match_with_dfa(&dfa_root.as_ref().borrow(), "i7t"));

        // pattern "("
        nfa_root = regex2nfa(&mut gen_counter(), &lparen_r());
        dfa_root = nfa2dfa(&mut gen_counter(), nfa_root.clone(), &ascii_charset());
        assert!(!match_with_nfa(&nfa_root.as_ref().borrow(), "int"));
        assert!(!match_with_nfa(&nfa_root.as_ref().borrow(), "I"));
        assert!(match_with_nfa(&nfa_root.as_ref().borrow(), "("));

        assert!(!match_with_dfa(&dfa_root.as_ref().borrow(), "int"));
        assert!(!match_with_dfa(&dfa_root.as_ref().borrow(), "I"));
        assert!(match_with_dfa(&dfa_root.as_ref().borrow(), "("));
    }

    #[test]
    fn pri_regx_matcher_did_works() {
        use super::{ int_m, rparen_m };

        let int_m = int_m();

        assert!(int_m.is_match("int"));
        assert!(!int_m.is_match("Int"));
        assert!(!int_m.is_match("i7t"));

        let rparen_m = rparen_m();

        debug_assert!(rparen_m.is_match(")"));

        assert!(!rparen_m.is_match("("));
    }
}
