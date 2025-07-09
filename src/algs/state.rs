use std::{
    borrow::Borrow,
    cell::RefCell,
    collections::{HashMap, HashSet},
    fmt,
    hash::{self, Hasher},
    rc::{Rc, Weak},
    vec,
};

use indexmap::{indexmap, IndexMap};
use itertools::Itertools;
use key_set::{KeyHashSet, KeySet};
use maplit::hashset;

use crate::utils::{gen_counter, CounterType, ObjId};


////////////////////////////////////////////////////////////////////////////////
//// Traits

pub trait TransData<D, TranE, TranP>: fmt::Display {
    fn is_match(&self, pat: &TranP) -> bool;

    /// if pat doesn't exist return true else false
    fn insert(&mut self, pat: TranE) -> bool;

    fn item2pat(&self, e: &TranE) -> TranP;
}


pub trait AlphabetSet<T> {
    fn iter(&self) -> vec::IntoIter<T>;
}


pub trait Dtrait = PartialEq + fmt::Display;

////////////////////////////////////////////////////////////////////////////////
//// Structures

/// State Graph：
/// 一般图的存储结构应该是一个Table包含所有图的节点的strong ref，节点之间通过weak ref相连。
/// 这么做有一个机制上的原因是Rust没有Java那种可以处理循环引用的GC（Trace对象，检查从GC根路径开始能否访问到）。
/// `Rc`如其名所说，是一个ref counter的机制（Rc提供strong ref），需要换成`Weak`来避免循环引用导致的内存无法释放（从而造成内存泄漏）。
/// 但使用weak ref需要在一个地方保存所有图节点的一个strong ref使其不被释放，从这个意义上State Graph struct 具有必要的作用。
///
/// TranP: Pattern type of matcher of transition
#[derive(Clone, Debug)]
pub struct StateGraph<D, TranE, TranP> {
    pub name: String, // just for identity, temporarily.
    pub states: Vec<Rc<RefCell<State<D, TranE, TranP>>>>,
}


#[derive(Clone)]
pub struct State<D, TranE, TranP> {
    pub id: usize,
    pub acceptable: bool,
    pub transitions: Vec<Transition<D, TranE, TranP>>,
    pub data: Option<D>,
}


#[derive(Clone, Debug)]
pub struct DFAStateGraph<D: Dtrait, TranE, TranP> {
    #[allow(unused)]
    name: String, // just for identity, temporarily.
    _nfa_states: Vec<Rc<RefCell<State<D, TranE, TranP>>>>,
    states: IndexMap<usize, Rc<RefCell<DFAState<D, TranE, TranP>>>>,
}


pub struct Transition<D, TranE, TranP> {
    pub data: Option<Rc<dyn TransData<D, TranE, TranP>>>,
    pub to_state: Weak<RefCell<State<D, TranE, TranP>>>,
}


pub type StatesSet<D, TranE, TranP> =
    KeyHashSet<Weak<RefCell<State<D, TranE, TranP>>>, usize>;


/// DFA Data
pub enum DFAData<D: Dtrait, TranE, TranP> {
    States(Rc<RefCell<StatesSet<D, TranE, TranP>>>),
    Data(D),
}


/// Try Next Result
pub enum TryNxtRes<S> {
    Ok(Rc<RefCell<S>>),
    Multi(Vec<Rc<RefCell<S>>>),
    None,
}


pub struct DFAState<D: Dtrait, TranE, TranP> {
    pub id: usize,
    pub transitions: Vec<DFATransition<D, TranE, TranP>>,
    pub data: DFAData<D, TranE, TranP>,
}


pub struct DFATransition<D: Dtrait, TranE, TranP> {
    pub data: Rc<RefCell<dyn TransData<D, TranE, TranP>>>,
    pub to_state: Weak<RefCell<DFAState<D, TranE, TranP>>>,
}


/// Match Result
pub enum MatchResult<S> {
    Unfinished(Rc<RefCell<S>>),
    PartialMatched(usize),
    FullMatched, // include empty match
}


////////////////////////////////////////////////////////////////////////////////
//// Implementations

impl<D, TranE, TranP> StateGraph<D, TranE, TranP> {
    pub fn top(
        &self,
    ) -> Option<&Rc<RefCell<State<D, TranE, TranP>>>> {
        self.states.first()
    }

    pub fn tail(
        &self,
    ) -> Option<&Rc<RefCell<State<D, TranE, TranP>>>> {
        self.states.last()
    }

    /// Concatenate two graph
    /// (don't extend states)
    pub fn just_concat(
        self,
        to_graph: &StateGraph<D, TranE, TranP>,
    ) {
        let from_end_state = self.tail().unwrap();
        let to_begin_state = to_graph.top().unwrap();

        (*from_end_state)
            .borrow_mut()
            .copy_transitions(to_begin_state);

        (*from_end_state).borrow_mut().acceptable = false;
    }
}

impl<D, TranE, TranP> fmt::Display for StateGraph<D, TranE, TranP> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.top().unwrap().as_ref().borrow())
    }
}

////////////////////////////////////////////////////////////////////////////////
/// State： (AKA NFA State)


impl<D, TranE, TranP> State<D, TranE, TranP> {
    ///
    /// Static Create Method
    ///
    pub fn with_counter(counter: &mut dyn CounterType) -> Self {
        Self {
            id: counter(),
            acceptable: false,
            transitions: Vec::new(),
            data: None,
        }
    }

    /// with acceptable is true
    pub fn with_counter_accept(
        counter: &mut dyn CounterType,
    ) -> Self {
        let mut state = Self::with_counter(counter);
        state.acceptable = true;
        state
    }

    ///
    /// Instance Update Method
    ///
    pub fn insert_transition(
        &mut self,
        transition: Transition<D, TranE, TranP>,
    ) {
        self.transitions.push(transition)
    }

    pub fn copy_transitions(&mut self, state: &Rc<RefCell<Self>>) {
        self.transitions =
            state.as_ref().borrow().transitions.clone();
    }

    pub fn insert_epsilon_transition(
        &mut self,
        to_state: Weak<RefCell<Self>>,
    ) {
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

        self.transitions
            .iter()
            .filter(|trans| trans.is_epsilon())
            .any(|trnas| {
                let next_state = trnas.to_state.clone();
                if next_state
                    .upgrade()
                    .unwrap()
                    .as_ref()
                    .borrow()
                    .can_reach_acceptable_state()
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
impl<D, TranE, TranP> hash::Hash for State<D, TranE, TranP> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_usize(self.id)
    }
}

impl<D, TranE, TranP> PartialEq<State<D, TranE, TranP>>
    for State<D, TranE, TranP>
{
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl<D, TranE, TranP> Eq for State<D, TranE, TranP> {}

/// Show one state
impl<D, TranE, TranP> fmt::Display for State<D, TranE, TranP> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut show_str = String::from(format!("{}", self.id));

        if self.transitions.len() > 0 {
            for transition in self.transitions.iter() {
                let to_state = &transition.to_state;
                let to_state_id =
                    to_state.upgrade().unwrap().as_ref().borrow().id;

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
impl<D, TranE, TranP> fmt::Debug for State<D, TranE, TranP> {
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

impl<D, TranE, TranP> Transition<D, TranE, TranP> {
    pub fn epsilon(
        to_state: Weak<RefCell<State<D, TranE, TranP>>>,
    ) -> Self {
        Self {
            data: None,
            to_state,
        }
    }

    pub fn is_match(&self, inputc: &TranP) -> bool {
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
            None => true,
        }
    }
}

impl<D, TranE, TranP> Clone for Transition<D, TranE, TranP> {
    fn clone(&self) -> Self {
        Self {
            data: self.data.clone(),
            to_state: self.to_state.clone(),
        }
    }
}

impl<D, TranE, TranP> fmt::Display for Transition<D, TranE, TranP> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.data {
            None => {
                write!(f, "{}", "ε")
            }
            Some(data) => {
                write!(f, "{}", data.as_ref())
            }
        }
    }
}


////////////////////////////////////////////////////////////////////////////////
/////// DFA State Graph: 建立在普通的State(NFA State)的集合的基础之上

impl<D: Dtrait, TranE, TranP> DFAStateGraph<D, TranE, TranP> {
    pub fn empty() -> Self {
        Self {
            name: "".to_owned(),
            _nfa_states: vec![],
            states: indexmap![],
        }
    }

    pub fn top(
        &self,
    ) -> Option<&Rc<RefCell<DFAState<D, TranE, TranP>>>> {
        self.states.values().next()
    }

    pub fn tail(
        &self,
    ) -> Option<&Rc<RefCell<DFAState<D, TranE, TranP>>>> {
        let lastpos = self.states.len() - 1;

        if let Some(res) = self.states.get_index(lastpos) {
            Some(res.1)
        } else {
            None
        }
    }

    /// States Order by state id
    pub fn ordered_states(
        &self,
    ) -> Vec<Rc<RefCell<DFAState<D, TranE, TranP>>>> {
        self.states.values().cloned().collect_vec()
    }

    pub fn get_state(
        &self,
        state_id: &usize,
    ) -> Option<&Rc<RefCell<DFAState<D, TranE, TranP>>>> {
        self.states.get(state_id)
    }
}

impl<D: Dtrait, TranE, TranP>
    From<IndexMap<usize, Rc<RefCell<DFAState<D, TranE, TranP>>>>>
    for DFAStateGraph<D, TranE, TranP>
{
    fn from(
        value: IndexMap<
            usize,
            Rc<RefCell<DFAState<D, TranE, TranP>>>,
        >,
    ) -> Self {
        Self {
            name: "".to_owned(),
            _nfa_states: vec![],
            states: value,
        }
    }
}

impl<D: Dtrait, TranE, TranP> DFAStateGraph<D, TranE, TranP> {
    /// NFA to DFA
    pub fn from_nfa_graph(
        nfa_g: StateGraph<D, TranE, TranP>,
        alphabet: Box<dyn AlphabetSet<TranE>>,
        empty_set_getter: impl Fn() -> Rc<
            RefCell<dyn TransData<D, TranE, TranP>>,
        >,
    ) -> Self {
        let begin_state = nfa_g.top().unwrap();
        let mut counter = gen_counter();

        // state的epsilon闭包缓存
        let mut state_closure_cache = HashMap::new();

        let cur_states_set = single_state_epsilon_states_set(
            Rc::downgrade(begin_state),
            &mut state_closure_cache,
        );

        // 从开始状态的epsilon闭包开始
        let cur_dfa_state =
            Rc::new(RefCell::new(DFAState::from_counter_states(
                &mut counter,
                Rc::clone(&cur_states_set),
            ))); // state set => dfa state

        // 保存每次计算出的新的states_set
        let mut new_dfa_state_vec =
            Vec::<Rc<RefCell<DFAState<D, TranE, TranP>>>>::new();
        // 保存所有已计算的dfa state
        let mut dfa_state_coll = indexmap! {
            cur_dfa_state.as_ref().borrow().id.clone() => Rc::clone(&cur_dfa_state)
        };
        new_dfa_state_vec.push(Rc::clone(&cur_dfa_state));

        while new_dfa_state_vec.len() > 0 {
            let wait_for_calc = new_dfa_state_vec;
            new_dfa_state_vec = vec![];

            for taken_state in wait_for_calc.into_iter() {
                for c in alphabet.iter() {
                    let taken_states_set = Rc::clone(
                        &(*taken_state)
                            .borrow()
                            .data
                            .states()
                            .unwrap(),
                    );

                    let statedata = empty_set_getter();
                    let pat =
                        statedata.as_ref().borrow().item2pat(&c);

                    // move(s, c)
                    let next_states_set =
                        r#move(Rc::clone(&taken_states_set), &pat);

                    if (*next_states_set).borrow().is_empty() {
                        continue;
                    } // 在一个有一定规模的字符集里大部分都是这样的情况

                    //  ε-closure
                    let next_states_set_closure =
                        states_set_epsilon_closure(
                            Rc::clone(&next_states_set),
                            &mut state_closure_cache,
                        );

                    let next_dfa_state;
                    match find_dfa_state_by_states_set(
                        &dfa_state_coll,
                        Rc::clone(&next_states_set_closure),
                    ) {
                        Some(old_dfa_state) => {
                            next_dfa_state = old_dfa_state;

                            if !taken_state
                                .as_ref()
                                .borrow()
                                .has_transition(Rc::downgrade(
                                    &next_dfa_state,
                                ))
                            {
                                let trans = DFATransition::new(
                                    statedata,
                                    Rc::downgrade(&next_dfa_state),
                                );
                                taken_state
                                    .as_ref()
                                    .borrow_mut()
                                    .insert_transition(trans);
                            }
                        }

                        None => {
                            let new_dfa_state =
                                Rc::new(RefCell::new(
                                    DFAState::from_counter_states(
                                        &mut counter,
                                        Rc::clone(
                                            &next_states_set_closure,
                                        ),
                                    ),
                                ));

                            dfa_state_coll.insert(
                                new_dfa_state
                                    .as_ref()
                                    .borrow()
                                    .id
                                    .clone(),
                                Rc::clone(&new_dfa_state),
                            );
                            new_dfa_state_vec
                                .push(Rc::clone(&new_dfa_state));

                            (*taken_state)
                                .borrow_mut()
                                .insert_transition(
                                    DFATransition::new(
                                        statedata,
                                        Rc::downgrade(
                                            &new_dfa_state,
                                        ),
                                    ),
                                );

                            next_dfa_state = new_dfa_state;
                        }
                    }

                    insert_pat_on_transition(
                        taken_state.clone(),
                        next_dfa_state,
                        c,
                    );
                }
            }
        }

        Self {
            name: nfa_g.name,
            _nfa_states: nfa_g.states,
            states: dfa_state_coll,
        }
    }
}

impl<D: Dtrait, TranE, TranP> fmt::Display
    for DFAStateGraph<D, TranE, TranP>
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Total {} DFA states:\n", self.states.len())?;
        writeln!(f, "{}", "-".repeat(80))?;

        for state in self.ordered_states() {
            write!(f, "{}", state.as_ref().borrow())?;
            writeln!(f, "{}", "-".repeat(80))?;
        }

        Ok(())
    }
}

////////////////////////////////////////////////////////////////////////////////
/////// DFA State: 建立在普通的State(NFA State)的集合的基础之上

impl<D: Dtrait, TranE, TranP> DFAData<D, TranE, TranP> {
    pub fn states(
        &self,
    ) -> Option<&Rc<RefCell<StatesSet<D, TranE, TranP>>>> {
        if let Self::States(states) = self {
            Some(states)
        } else {
            None
        }
    }

    pub fn data(&self) -> Option<&D> {
        if let Self::Data(data) = self {
            Some(data)
        } else {
            None
        }
    }
}


impl<D: Dtrait, TranE, TranP> ObjId for DFAState<D, TranE, TranP> {
    fn id(&self) -> usize {
        self.id
    }
}

impl<D: Dtrait, TranE, TranP> DFAState<D, TranE, TranP> {
    pub fn dfa_states_get_key(
        x: &Weak<RefCell<DFAState<D, TranE, TranP>>>,
    ) -> usize {
        (*x.upgrade().unwrap()).borrow().id
    }

    pub fn states_set_getkey(
        x: &Weak<RefCell<State<D, TranE, TranP>>>,
    ) -> usize {
        (*x.upgrade().unwrap()).borrow().id
    }

    pub fn new_key_set() -> Rc<RefCell<StatesSet<D, TranE, TranP>>> {
        Rc::new(RefCell::new(KeyHashSet::new(
            DFAState::states_set_getkey,
        )))
    }

    pub fn from_counter_states(
        counter: &mut dyn CounterType,
        states: Rc<RefCell<StatesSet<D, TranE, TranP>>>,
    ) -> Self {
        Self {
            id: counter(),
            data: DFAData::States(states),
            transitions: vec![],
        }
    }

    pub fn from_counter_data(
        counter: &mut dyn CounterType,
        data: D,
    ) -> Self {
        Self {
            id: counter(),
            data: DFAData::Data(data),
            transitions: vec![],
        }
    }

    pub fn is_acceptable(&self) -> bool {
        if let DFAData::States(states) = &self.data {
            states.as_ref().borrow().iter().any(|state| {
                (*state.upgrade().unwrap())
                    .borrow()
                    .can_reach_acceptable_state()
            })
        } else {
            true
        }
    }

    pub fn insert_transition(
        &mut self,
        transition: DFATransition<D, TranE, TranP>,
    ) {
        self.transitions.push(transition)
    }

    pub fn has_transition(
        &self,
        to_state: Weak<RefCell<DFAState<D, TranE, TranP>>>,
    ) -> bool {
        self.transitions.iter().any(|trans| {
            trans.to_state.upgrade().unwrap()
                == to_state.upgrade().unwrap()
        })
    }

    pub fn try_next(&self, input: &TranP) -> TryNxtRes<Self> {
        let mut states_coll: Vec<Rc<RefCell<Self>>> = self
            .transitions
            .iter()
            .filter(|trans| trans.is_match(input))
            .map(|trans| trans.to_state.upgrade().unwrap())
            .collect_vec();

        let stateslen = states_coll.len();
        if stateslen == 0 {
            TryNxtRes::None
        } else if stateslen == 1 {
            TryNxtRes::Ok(states_coll.pop().unwrap())
        } else {
            TryNxtRes::Multi(states_coll)
        }
    }
}

/// Hash
impl<D: Dtrait, TranE, TranP> hash::Hash
    for DFAState<D, TranE, TranP>
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_usize(self.id)
    }
}

impl<D: Dtrait, TranE, TranP> Eq for DFAState<D, TranE, TranP> {}

/// DFAState 比较相等根据它所包含的State
/// 而State 比较相等根据它的id
impl<D: Dtrait, TranE, TranP> PartialEq
    for DFAState<D, TranE, TranP>
{
    fn eq(&self, other: &Self) -> bool {
        match &self.data {
            DFAData::States(states) => {
                *states.as_ref().borrow()
                    == *other
                        .data
                        .states()
                        .unwrap()
                        .as_ref()
                        .borrow()
            }
            DFAData::Data(data) => {
                *data == *other.data.data().unwrap()
            }
        }
    }
}


impl<D: Dtrait, TranE, TranP> DFAState<D, TranE, TranP> {
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

/// Show one state
impl<D: Dtrait, TranE, TranP> fmt::Display
    for DFAState<D, TranE, TranP>
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut show_str = String::from(format!("({})", self.id));

        if self.transitions.len() > 0 {
            for transition in self.transitions.iter() {
                let to_state = &transition.to_state;
                let to_state_id =
                    (*to_state.upgrade().unwrap()).borrow().id;

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

        match &self.data {
            DFAData::States(states) => {
                write!(f, "\tNFA states: ",).ok();
                write!(
                    f,
                    "{}\n",
                    (*states)
                        .as_ref()
                        .borrow()
                        .iter()
                        .map(|state| format!(
                            "_{}",
                            state
                                .upgrade()
                                .unwrap()
                                .as_ref()
                                .borrow()
                                .id
                        ))
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
            }
            DFAData::Data(data) => {
                writeln!(f, "DFA data: ")?;
                writeln!(f, "{}", data)?;
            }
        }


        Ok(())
    }
}

/// Show full state
impl<D: Dtrait, TranE, TranP> fmt::Debug
    for DFAState<D, TranE, TranP>
{
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


impl<D: Dtrait, TranE, TranP> DFATransition<D, TranE, TranP> {
    pub fn new(
        data: Rc<RefCell<dyn TransData<D, TranE, TranP>>>,
        to_state: Weak<RefCell<DFAState<D, TranE, TranP>>>,
    ) -> Self {
        Self { data, to_state }
    }

    pub fn is_match(&self, inputc: &TranP) -> bool {
        self.data.as_ref().borrow().is_match(inputc)
    }
}


impl<D: Dtrait, TranE, TranP> Clone
    for DFATransition<D, TranE, TranP>
{
    fn clone(&self) -> Self {
        Self {
            data: self.data.clone(),
            to_state: self.to_state.clone(),
        }
    }
}

impl<D: Dtrait, TranE, TranP> fmt::Display
    for DFATransition<D, TranE, TranP>
{
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
fn insert_pat_on_transition<D: Dtrait, TranE, TranP>(
    from_state: Rc<RefCell<DFAState<D, TranE, TranP>>>,
    to_state: Rc<RefCell<DFAState<D, TranE, TranP>>>,
    c: TranE,
) {
    let from_state_ref = from_state.as_ref().borrow();
    let trans_ind = from_state_ref
        .transitions
        .iter()
        .position(|trans| {
            trans.to_state.upgrade().unwrap() == to_state
        })
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
pub fn single_state_epsilon_states_set<D: Dtrait, TranE, TranP>(
    state: Weak<RefCell<State<D, TranE, TranP>>>,
    cache: &mut HashMap<
        usize,
        Rc<RefCell<StatesSet<D, TranE, TranP>>>,
    >,
) -> Rc<RefCell<StatesSet<D, TranE, TranP>>> {
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

            let other =
                single_state_epsilon_states_set(next_state, cache);
            clousure_ref
                .extend(other.as_ref().borrow().iter().cloned());
        }
    }

    // 加入缓存中
    cache.insert(state_ref.id, Rc::clone(&closure));

    Rc::clone(&closure)
}

/// 得到一个状态集合的闭包
fn states_set_epsilon_closure<D: Dtrait, TranE, TranP>(
    states_set: Rc<RefCell<StatesSet<D, TranE, TranP>>>,
    cache: &mut HashMap<
        usize,
        Rc<RefCell<StatesSet<D, TranE, TranP>>>,
    >,
) -> Rc<RefCell<StatesSet<D, TranE, TranP>>> {
    let states_set_ref = (*states_set).borrow();
    let res = DFAState::new_key_set();
    let res_here = Rc::clone(&res);
    let mut res_ref = (*res_here).borrow_mut();

    for state in states_set_ref.iter() {
        let state_closure =
            single_state_epsilon_states_set(state.clone(), cache);
        res_ref
            .extend(state_closure.as_ref().borrow().iter().cloned());
    }

    // 包括起始状态
    res_ref.extend((*states_set_ref).borrow().iter().cloned());

    res
}

/// 从StatesSet到StatesSet， 不直接用DFAState是因为counter有id分配的问题
fn r#move<D: Dtrait, TranE, TranP>(
    s0: Rc<RefCell<StatesSet<D, TranE, TranP>>>,
    c: &TranP,
) -> Rc<RefCell<StatesSet<D, TranE, TranP>>> {
    let s0_states_set_ref = (*s0).borrow();

    let mut s1 = KeyHashSet::new(DFAState::states_set_getkey);

    for state in s0_states_set_ref.iter() {
        for trans in state
            .upgrade()
            .unwrap()
            .as_ref()
            .borrow()
            .transitions
            .iter()
        {
            if trans.is_match(c) {
                s1.insert(trans.to_state.clone());
            }
        }
    }

    Rc::new(RefCell::new(s1))
}

/// find if states_set exists
fn find_dfa_state_by_states_set<D: Dtrait, TranE, TranP>(
    states_set_coll: &IndexMap<
        usize,
        Rc<RefCell<DFAState<D, TranE, TranP>>>,
    >,
    target: Rc<RefCell<StatesSet<D, TranE, TranP>>>,
) -> Option<Rc<RefCell<DFAState<D, TranE, TranP>>>> {
    match states_set_coll.values().find(|&states_set| {
        let dfa_state = (**states_set).borrow();
        let matched =
            *(dfa_state.data.states().unwrap().as_ref().borrow())
                == *(*target).borrow();

        matched
    }) {
        Some(res) => Some(Rc::clone(res)),
        None => None,
    }
}

///// Debug tool in NFA2DFA
// fn display_states_set<D, TranE, TranP>(states_set: &Rc<RefCell<StatesSet<D, TranE, TranP>>>) {
//     let states_set = states_set.as_ref().borrow();
//     let states_set_ref = (*states_set).borrow();

//     for state in states_set_ref.iter() {
//         let state_rc = state.upgrade().unwrap();
//         let state_ref = state_rc.as_ref().borrow();
//         println!("{}", *state_ref);
//     }
// }


pub fn match_with_nfa<D, TranE, TranP>(
    nfa_g: &StateGraph<D, TranE, TranP>,
    input: &[TranP],
) -> MatchResult<State<D, TranE, TranP>> {
    let state = nfa_g.top().unwrap();

    if input.is_empty() {
        if state.as_ref().borrow().can_reach_acceptable_state() {
            return MatchResult::FullMatched;
        }

        return MatchResult::Unfinished(state.clone());
    }

    let (final_state_rc, final_ind) =
        _match_with_nfa(state.clone(), input, 0);

    // println!("final state: {}", final_state_rc.as_ref().borrow());
    // println!("final index: {}", final_ind);

    if final_state_rc
        .as_ref()
        .borrow()
        .can_reach_acceptable_state()
    {
        if final_ind == input.len() {
            return MatchResult::FullMatched;
        }

        return MatchResult::PartialMatched(final_ind);
    }

    return MatchResult::Unfinished(state.clone());
}


fn _match_with_nfa<D, TranE, TranP>(
    state_rc: Rc<RefCell<State<D, TranE, TranP>>>,
    chars_input: &[TranP],
    init_index: usize,
) -> (Rc<RefCell<State<D, TranE, TranP>>>, usize) {
    let cloned_state_rc = state_rc.clone();
    let state = cloned_state_rc.as_ref().borrow();
    // #[cfg(debug_assertions)]
    // println!(
    //     "{}>>> trying state : {}, index ={}",
    //     "    ".repeat(init_index), state.id, init_index
    // );

    // 注意Transition条件存在重叠的情况
    for transition in state.transitions.iter() {
        let next_state_rc = transition.to_state.upgrade().unwrap();
        let final_state;
        let final_ind;

        if transition.is_epsilon() {
            // 自动跳过epsilon转移，到达下一个状态
            // epsilon 自动转换状态
            (final_state, final_ind) = _match_with_nfa(
                next_state_rc,
                chars_input,
                init_index,
            );
        } else if transition.is_match(&chars_input[init_index]) {
            if init_index + 1 == chars_input.len() {
                final_state = next_state_rc;
                final_ind = init_index + 1; // !! final_ind should point to the lastpost + 1
            } else {
                (final_state, final_ind) = _match_with_nfa(
                    next_state_rc,
                    chars_input,
                    init_index + 1,
                );
            }
        } else {
            continue;
        }

        // Full Match
        if final_state
            .clone()
            .as_ref()
            .borrow()
            .can_reach_acceptable_state()
            && final_ind == chars_input.len()
        {
            return (final_state, final_ind);
        }
    }

    (state_rc, init_index)
}


pub fn match_with_dfa<D: Dtrait, TranE, TranP>(
    dfa_g: &DFAStateGraph<D, TranE, TranP>,
    input: &[TranP],
) -> MatchResult<DFAState<D, TranE, TranP>> {
    let state = dfa_g.top().unwrap();

    if input.is_empty() {
        if state.as_ref().borrow().is_acceptable() {
            return MatchResult::FullMatched;
        }

        return MatchResult::Unfinished(state.clone());
    }

    let (final_state_rc, final_ind) =
        _match_with_dfa(state.clone(), input, 0);

    println!("final state: {}", final_state_rc.as_ref().borrow());
    println!("final index: {}", final_ind);
    if final_state_rc.as_ref().borrow().is_acceptable() {
        if final_ind == input.len() {
            return MatchResult::FullMatched;
        }

        return MatchResult::PartialMatched(final_ind);
    }

    return MatchResult::Unfinished(state.clone());
}


fn _match_with_dfa<D: Dtrait, TranE, TranP>(
    state_rc: Rc<RefCell<DFAState<D, TranE, TranP>>>,
    chars_input: &[TranP],
    init_index: usize,
) -> (Rc<RefCell<DFAState<D, TranE, TranP>>>, usize) {
    let state_rc_cloned = state_rc.clone();
    let state = state_rc_cloned.as_ref().borrow();

    #[cfg(debug_assertions)]
    println!(
        "trying DFA state : {}, index ={}",
        state.id, init_index
    );

    for trans in state.transitions.iter() {
        if trans.is_match(&chars_input[init_index]) {
            let next_state_rc = trans.to_state.upgrade().unwrap();
            // 递归结束条件
            if init_index + 1 == chars_input.len() {
                return (next_state_rc, init_index + 1);
            }

            return _match_with_dfa(
                next_state_rc,
                chars_input,
                init_index + 1,
            );
        }
    }

    (state_rc, init_index)
}
