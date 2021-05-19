use std::{borrow::Borrow, cell::RefCell, collections::{
        HashMap,
        HashSet
    }, fmt, hash::{
        self,
        Hasher
    }, rc::{Rc, Weak},
    vec

};

use key_set::{KeyHashSet, KeySet};

use crate::utils::{CounterType, gen_counter};


/*
*
* Traits
*
*/


pub trait TransData<E, P> : fmt::Display {
    fn is_match(&self, pat: &P) -> bool;

    /// if pat doesn't exist return true else false
    fn insert(&mut self, pat: E) -> bool;

    fn item2pat(&self, e: &E) -> P;
}


pub trait AlphabetSet<T> {
    fn iter(&self) -> vec::IntoIter<T>;
}


/*
*
* Struct && Implement
*
*/

////////////////////////////////////////////////////////////////////////////////
/// State Graph：
/// 一般图的存储结构应该是一个Table包含所有图的节点的strong ref，节点之间通过weak ref相连。
/// 这么做有一个机制上的原因是Rust没有Java那种可以处理循环引用的GC（Trace对象，检查从GC根路径开始能否访问到）。
/// `Rc`如其名所说，是一个ref counter的机制（Rc提供strong ref），需要换成`Weak`来避免循环引用导致的内存无法释放（从而造成内存泄漏）。
/// 但使用weak ref需要在一个地方保存所有图节点的一个strong ref使其不被释放，从这个意义上State Graph struct 具有必要的作用。
///
/// P: Pattern type of matcher of transition
#[derive(Clone, Debug)]
pub struct StateGraph<E, P> {
    pub name: String,  // just for identity, temporarily.
    pub states: Vec<Rc<RefCell<State<E, P>>>>
}

impl <E, P> StateGraph<E, P> {
    pub fn top(&self) -> Option<&Rc<RefCell<State<E, P>>>> {
        self.states.first()
    }

    pub fn tail(&self) -> Option<&Rc<RefCell<State<E, P>>>> {
        self.states.last()
    }

    /// Concatenate two graph
    /// (don't extend states)
    pub fn just_concat(
        self,
        to_graph: &StateGraph<E, P>,
    )
    {
        let from_end_state = self.tail().unwrap();
        let to_begin_state = to_graph.top().unwrap();

        (*from_end_state)
            .borrow_mut()
            .copy_transitions(to_begin_state);

        (*from_end_state).borrow_mut().acceptable = false;
    }
}

impl <E, P> fmt::Display for StateGraph<E, P> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.top().unwrap().as_ref().borrow())
    }
}

////////////////////////////////////////////////////////////////////////////////
/// State： (AKA NFA State)

#[derive(Clone)]
pub struct State<E, P> {
    pub id: usize,
    pub acceptable: bool,
    pub transitions: Vec<Transition<E, P>>,
}

impl <E, P> State<E, P> {
    ///
    /// Static Create Method
    ///
    pub fn with_counter(counter: &mut CounterType) -> Self {
        Self {
            id: counter(),
            acceptable: false,
            transitions: Vec::new(),
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
    pub fn insert_transition(&mut self, transition: Transition<E, P>) {
        self.transitions.push(transition)
    }

    pub fn copy_transitions(&mut self, state: &Rc<RefCell<Self>>) {
        self.transitions = state.as_ref().borrow().transitions.clone();
    }

    pub fn insert_epsilon_transition(&mut self, to_state: Weak<RefCell<Self>>) {
        self.insert_transition(Transition::epsilon(to_state))
    }

    pub fn has_epsilon_transition(&self) -> bool {
        self.transitions.iter().any(|trans| trans.is_epsilon())
    }

    /// 当前状态、以及该状态通过 epsilon 连接的状态是否有可接受的(acceptable = True)
    pub fn can_reach_acceptable_state(&self) -> bool {
        if self.acceptable {
            return true;
        }

        self
            .transitions
            .iter()
            .filter(|trans| trans.is_epsilon())
            .any(|trnas| {
                let next_state = trnas.to_state.clone();
                if next_state.upgrade().unwrap().as_ref().borrow().can_reach_acceptable_state()
                {
                    true
                } else {
                    false
                }
            })
    }

    pub fn is_end(&self) -> bool {
        self.transitions.is_empty()
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
            let to_state_rc = transition.to_state.upgrade().unwrap();
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

/// Hash
impl <E, P> hash::Hash for State<E, P> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_usize(self.id)
    }
}

impl <E, P> PartialEq<State<E, P>> for State<E, P> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl <E, P> Eq for State<E, P> {}

/// Show one state
impl <E, P> fmt::Display for State<E, P> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut show_str = String::from(format!("{}", self.id));

        if self.transitions.len() > 0 {
            for transition in self.transitions.iter() {
                let to_state = &transition.to_state;
                let to_state_id = to_state.upgrade().unwrap().as_ref().borrow().id;

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
impl <E, P> fmt::Debug for State<E, P> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut visited_states = hashset!();

        match State::dump(self, f, &mut visited_states) {
            Err(err) => Err(err),
            Ok(()) => Ok(()),
        }
    }
}


////////////////////////////////////////////////////////////////////////////////
//// State Transition
pub struct Transition<E, P> {
    pub data: Option<Rc<dyn TransData<E, P>>>,
    pub to_state: Weak<RefCell<State<E, P>>>,
}

impl <E, P> Transition<E, P> {
    pub fn epsilon(to_state: Weak<RefCell<State<E, P>>>) -> Self
    {
        Self {
            data: None,
            to_state,
        }
    }

    pub fn is_match(&self, inputc: &P) -> bool {
        if let Some(matcher) = &self.data {
            matcher.as_ref().is_match(inputc)
        } else {
            // epsilon transition always match
            true
        }
    }

    pub fn is_epsilon(&self) -> bool {
        match self.data {
            Some(_) => false,
            None => true
        }
    }
}

impl <E, P> Clone for Transition<E, P> {
    fn clone(&self) -> Self {
        Self {
            data: self.data.clone(),
            to_state: self.to_state.clone()
        }
    }
}

impl <E, P> fmt::Display for Transition<E, P> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.data {
            None => {
                write!(f, "{}", "ε")
            },
            Some(data) => {
                write!(f, "{}", data.as_ref())
            }
        }
    }
}


////////////////////////////////////////////////////////////////////////////////
/////// DFA State Graph: 建立在普通的State(NFA State)的集合的基础之上
#[derive(Clone, Debug)]
pub struct DFAStateGraph<E, P> {
    name: String,  // just for identity, temporarily.
    _nfa_states: Vec<Rc<RefCell<State<E, P>>>>,
    states: Vec<Rc<RefCell<DFAState<E, P>>>>
}

impl <E, P> DFAStateGraph<E, P> {
    pub fn top(&self) -> Option<&Rc<RefCell<DFAState<E, P>>>> {
        self.states.first()
    }

    pub fn tail(&self) -> Option<&Rc<RefCell<DFAState<E, P>>>> {
        self.states.last()
    }
}

impl <E, P> DFAStateGraph<E, P> {
    /// NFA to DFA
    pub fn from_nfa_graph(
        nfa_g: StateGraph<E, P>,
        alphabet: Box<dyn AlphabetSet<E>>,
        empty_set_getter: impl Fn() -> Rc<RefCell<dyn TransData<E, P>>>
    ) -> Self
    {
        let begin_state = nfa_g.top().unwrap();
        let mut counter = gen_counter();

        // state的epsilon闭包缓存
        let mut state_closure_cache = HashMap::new();

        let cur_states_set =
            single_state_epsilon_states_set( Rc::downgrade(begin_state), &mut state_closure_cache);

        // 从开始状态的epsilon闭包开始
        let cur_dfa_state = Rc::new(RefCell::new(DFAState::from_counter_states(
            &mut counter,
            Rc::clone(&cur_states_set),
        ))); // state set => dfa state

        // 保存每次计算出的新的states_set
        let mut new_dfa_state_vec = Vec::<Rc<RefCell<DFAState<E, P>>>>::new();
        // 保存所有已计算的dfa state
        let mut dfa_states_vec = Vec::<Rc<RefCell<DFAState<E, P>>>>::new();
        dfa_states_vec.push(Rc::clone(&cur_dfa_state));
        new_dfa_state_vec.push(Rc::clone(&cur_dfa_state));

        while new_dfa_state_vec.len() > 0 {
            let wait_for_calc = new_dfa_state_vec;
            new_dfa_state_vec = vec![];

            for taken_state in wait_for_calc.into_iter() {
                for c in alphabet.iter() {
                    let taken_states_set
                    = Rc::clone(&(*taken_state).borrow().states);

                    let statedata = empty_set_getter();
                    let pat = statedata.as_ref().borrow().item2pat(&c);

                    // move(s, c)
                    let next_states_set
                    = r#move(Rc::clone(&taken_states_set), &pat);

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
                                .has_transition(Rc::downgrade(&next_dfa_state))
                            {
                                let trans = DFATransition::new(
                                    statedata,
                                    Rc::downgrade(&next_dfa_state)
                                );
                                taken_state.as_ref().borrow_mut().insert_transition(trans);
                            }
                        }

                        None => {
                            let new_dfa_state = Rc::new(RefCell::new(DFAState::from_counter_states(
                                &mut counter,
                                Rc::clone(&next_states_set_closure),
                            )));

                            dfa_states_vec.push(Rc::clone(&new_dfa_state));
                            new_dfa_state_vec.push(Rc::clone(&new_dfa_state));

                            (*taken_state)
                                .borrow_mut()
                                .insert_transition(DFATransition::new(
                                    statedata,
                                    Rc::downgrade(&new_dfa_state))
                                );

                            next_dfa_state = new_dfa_state;
                        }
                    }

                    insert_pat_on_transition(taken_state.clone(), next_dfa_state, c);
                }
            }
        }

        Self {
            name: nfa_g.name,
            _nfa_states: nfa_g.states,
            states: dfa_states_vec
        }
    }
}

impl <E, P> fmt::Display for DFAStateGraph<E, P> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.top().unwrap().as_ref().borrow())
    }
}

////////////////////////////////////////////////////////////////////////////////
/////// DFA State: 建立在普通的State(NFA State)的集合的基础之上

pub type StatesSet<E, P> = KeyHashSet<Weak<RefCell<State<E, P>>>, usize>;


pub struct DFAState<E, P> {
    pub id: usize,
    pub states: Rc<RefCell<StatesSet<E, P>>>,
    pub transitions: Vec<DFATransition<E, P>>,
}

impl <E, P> DFAState<E, P> {
    pub fn dfa_states_get_key(x: &Weak<RefCell<DFAState<E, P>>>) -> usize {
        (*x.upgrade().unwrap()).borrow().id
    }

    pub fn states_set_getkey(x: &Weak<RefCell<State<E, P>>>) -> usize {
        (*x.upgrade().unwrap()).borrow().id
    }

    pub fn new_key_set() -> Rc<RefCell<StatesSet<E, P>>> {
        Rc::new(RefCell::new(KeyHashSet::new(DFAState::states_set_getkey)))
    }

    pub fn from_counter_states(counter: &mut CounterType, states: Rc<RefCell<StatesSet<E, P>>>) -> Self {
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
            .any(|state| (*state.upgrade().unwrap()).borrow().can_reach_acceptable_state())
    }

    pub fn insert_transition(&mut self, transition: DFATransition<E, P>) {
        self.transitions.push(transition)
    }

    pub fn has_transition(&self, to_state: Weak<RefCell<DFAState<E, P>>>) -> bool {
        self.transitions
            .iter()
            .any(|trans|
                 trans.to_state.upgrade().unwrap() == to_state.upgrade().unwrap())
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
            let to_state_rc = transition.to_state.upgrade().unwrap();
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

/// Hash
impl <E, P> hash::Hash for DFAState<E, P> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_usize(self.id)
    }
}

impl <E, P> Eq for DFAState<E, P> {}

/// DFAState 比较相等根据它所包含的State
/// 而State 比较相等根据它的id
impl <E, P> PartialEq for DFAState<E, P> {
    fn eq(&self, other: &Self) -> bool {
        let state_keyset = (*self.states).borrow();
        let other_keyset = (*other.states).borrow();

        *state_keyset == *other_keyset
    }
}

/// Show one state
impl <E, P> fmt::Display for DFAState<E, P> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut show_str = String::from(format!("({})", self.id));

        if self.transitions.len() > 0 {
            for transition in self.transitions.iter() {
                let to_state = &transition.to_state;
                let to_state_id = (*to_state.upgrade().unwrap()).borrow().id;

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
                .map(|state| format!("_{}",
                    state.upgrade().unwrap().as_ref().borrow().id))
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
impl <E, P> fmt::Debug for DFAState<E, P> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut visited_states = hashset!();

        match Self::dump(self, f, &mut visited_states) {
            Err(err) => Err(err),
            Ok(()) => Ok(()),
        }
    }
}


////////////////////////////////////////////////////////////////////////////////
/////// DFATransition

pub struct DFATransition<E, P> {
    pub data: Rc<RefCell<dyn TransData<E, P>>>,
    pub to_state: Weak<RefCell<DFAState<E, P>>>,
}

impl <E, P> DFATransition<E, P> {
    pub fn new(
        data: Rc<RefCell<dyn TransData<E, P>>>,
        to_state: Weak<RefCell<DFAState<E, P>>>
    ) -> Self
    {
        Self {
            data,
            to_state
        }
    }

    pub fn is_match(&self, inputc: &P) -> bool {
        self.data.as_ref().borrow().is_match(inputc)
    }
}


impl <E, P> Clone for DFATransition<E, P> {
    fn clone(&self) -> Self {
        Self {
            data: self.data.clone(),
            to_state: self.to_state.clone()
        }
    }
}

impl <E, P> fmt::Display for DFATransition<E, P> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.data.as_ref().borrow())
    }
}


/*
*
* Common Function Tools
*
*/


/// 根据to_state查找transition, 没有就创建;
/// 在这个transition的charset上插入这个字符
fn insert_pat_on_transition<E, P>(
    from_state: Rc<RefCell<DFAState<E, P>>>,
    to_state: Rc<RefCell<DFAState<E, P>>>,
    c: E,
)
{
    let from_state_ref = from_state.as_ref().borrow();
    let trans_ind = from_state_ref
        .transitions
        .iter()
        .position(|trans|
            trans.to_state.upgrade().unwrap() == to_state
        )
        .unwrap();
    let new_trans = from_state_ref.transitions[trans_ind].clone();
    drop(from_state_ref);

    new_trans.data.as_ref().borrow_mut().insert(c);

    from_state
        .as_ref()
        .borrow_mut()
        .transitions
        .remove(trans_ind);
    from_state.as_ref().borrow_mut().transitions.push(new_trans);
}


////////////////////////////////////////////////////////////////////////////////
//// Converter


/// 计算一个State通过 ε 转换所能到达的状态集合，目的是把这个计算的集合加入到缓存集中。
pub fn single_state_epsilon_states_set<E, P>(
    state: Weak<RefCell<State<E, P>>>,
    cache: &mut HashMap<usize, Rc<RefCell<StatesSet<E, P>>>>,
) -> Rc<RefCell<StatesSet<E, P>>>
{
    let state_rc = state.upgrade().unwrap();
    let state_ref = (*state_rc).borrow();
    // 缓存命中
    if let Some(res) = cache.get(&state_ref.id) {
        return Rc::clone(res);
    }

    let closure = DFAState::new_key_set();
    let mut clousure_ref = (*closure).borrow_mut();
    clousure_ref.insert(state.clone()); // 包含自身

    for transition in state_ref.transitions.iter() {
        if transition.is_epsilon() {
            let next_state = transition.to_state.clone();

            let other = single_state_epsilon_states_set(next_state, cache);
            clousure_ref.extend((*other).borrow().clone());
        }
    }

    // 加入缓存中
    cache.insert(state_ref.id, Rc::clone(&closure));

    Rc::clone(&closure)
}

/// 得到一个状态集合的闭包
fn states_set_epsilon_closure<E, P>(
    states_set: Rc<RefCell<StatesSet<E, P>>>,
    cache: &mut HashMap<usize, Rc<RefCell<StatesSet<E, P>>>>,
) -> Rc<RefCell<StatesSet<E, P>>> {
    let states_set_ref = (*states_set).borrow();
    let res = DFAState::new_key_set();
    let res_here = Rc::clone(&res);
    let mut res_ref = (*res_here).borrow_mut();

    for state in states_set_ref.iter() {
        let state_closure = single_state_epsilon_states_set(state.clone(), cache);
        res_ref.extend((*state_closure).borrow().clone());
    }

    // 包括起始状态
    res_ref.extend((*states_set_ref).borrow().clone());

    res
}

/// 从StatesSet到StatesSet， 不直接用DFAState是因为counter有id分配的问题
fn r#move<E, P>(s0: Rc<RefCell<StatesSet<E, P>>>, c: &P) -> Rc<RefCell<StatesSet<E, P>>> {
    let s0_states_set_ref = (*s0).borrow();

    let mut s1 = KeyHashSet::new(DFAState::states_set_getkey);

    for state in s0_states_set_ref.iter() {
        for trans in state.upgrade().unwrap().as_ref().borrow().transitions.iter() {
            if trans.is_match(c) {
                s1.insert(trans.to_state.clone());
            }
        }
    }

    Rc::new(RefCell::new(s1))
}

/// find if states_set exists
fn find_dfa_state_by_states_set<E, P>(
    states_set_vec: &Vec<Rc<RefCell<DFAState<E, P>>>>,
    target: Rc<RefCell<StatesSet<E, P>>>,
) -> Option<Rc<RefCell<DFAState<E, P>>>> {
    match states_set_vec.iter().find(|&states_set| {
        let dfa_state = (**states_set).borrow();
        let matched = *(*dfa_state.states).borrow() == *(*target).borrow();

        matched
    }) {
        Some(res) => Some(Rc::clone(res)),
        None => None,
    }
}

///// Debug tool in NFA2DFA
// fn display_states_set<E, P>(states_set: &Rc<RefCell<StatesSet<E, P>>>) {
//     let states_set = states_set.as_ref().borrow();
//     let states_set_ref = (*states_set).borrow();

//     for state in states_set_ref.iter() {
//         let state_rc = state.upgrade().unwrap();
//         let state_ref = state_rc.as_ref().borrow();
//         println!("{}", *state_ref);
//     }
// }