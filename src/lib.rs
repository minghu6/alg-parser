#![feature(min_type_alias_impl_trait)]
#![feature(destructuring_assignment)]
#![feature(generators, generator_trait)]

#[macro_use(c)]
extern crate cute;

#[macro_use]
extern crate maplit;

pub mod data;
pub mod utils;

use std::{borrow::Borrow, cell::RefCell, usize};
use std::rc::Rc;
use std::collections::HashMap;

use key_set::{GetKeyType, KeyHashSet, KeySet};

use data::*;
use utils::{yes, CounterType, GraphWalker};

////////////////////////////////////////////////////////////////////////////////
/// Converter

/// Concatenate two subgraph
/// 也许subgraph应该是一个子图结构
pub fn concat_state(
    from_graph: &mut (Rc<RefCell<State>>, Rc<RefCell<State>>),
    to_graph: &(Rc<RefCell<State>>, Rc<RefCell<State>>),
) {
    let from_end_state = &mut from_graph.1;
    let to_begin_state = &to_graph.0;

    (*from_end_state)
        .borrow_mut()
        .copy_transitions(&(**to_begin_state).borrow());
    (*from_end_state).borrow_mut().acceptable = false;
}


/// Regex to NFA

fn add_repetition(
    from_state: Rc<RefCell<State>>,
    to_state: Rc<RefCell<State>>,
    node: &GrammarNode,
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

pub fn regex2nfa(
    counter: &mut CounterType,
    node: &GrammarNode,
) -> (Rc<RefCell<State>>, Rc<RefCell<State>>) {
    let (mut begin_state, mut end_state);

    match node.nodetype {
        GrammarNodeType::Or => {
            // 构建新的开始和结束状态，用它们连每一个转移状态
            begin_state = Rc::new(RefCell::new(State::with_counter(counter)));
            end_state = Rc::new(RefCell::new(State::with_counter_accept(counter)));

            for child in node.childen.iter() {
                let (sub_begin_state, sub_end_state) = regex2nfa(counter, child);

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

        GrammarNodeType::And => {  // 各个转移状态前后相连
            let sub_graphs: Vec<(Rc<RefCell<State>>, Rc<RefCell<State>>)> = node
                .childen
                .iter()
                .map(|child| regex2nfa(counter, child))
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

        GrammarNodeType::LexNode => {  // 根据node的condition创建两个状态
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

pub fn nfa2dfa(
    counter: &mut CounterType,
    begin_state: Rc<RefCell<State>>,
    alphabet: &CharSet
) -> KeyHashSet<Rc<RefCell<DFAState>>, usize> {

    let mut states_set_vec: Vec<Rc<RefCell<StatesSet>>>
        = vec![];

    let mut closure_cache = HashMap::new();

    calc_epsilon_closure(Rc::clone(&begin_state), &mut closure_cache);

    let begin_state_ref = (*begin_state).borrow();
    let mut cur_dfa_state
        =
        Rc::new(RefCell::new(
            DFAState::with_counter_states(
                counter,
                Rc::clone(closure_cache.get(&begin_state_ref.id).unwrap()
            )
        )));

    states_set_vec.push(Rc::clone(&(*cur_dfa_state).borrow().states));

    // new_states 每次计算出的新的stateset
    let mut new_states = Vec::<Rc<RefCell<DFAState>>>::new();
    new_states.push(Rc::clone(&cur_dfa_state));

    while new_states.len() > 0 {
        let wait_for_calc = new_states;
        new_states = vec![];

        for taken_state in wait_for_calc.iter() {
            for c in alphabet.iter() {

            }
        }
    }

    KeyHashSet::from_intoiter(
        DFAState::dfa_states_get_key,
    states_set_vec
        .into_iter()
        .map(|x| Rc::new(RefCell::new(DFAState::with_counter_states(counter, x))))
        .collect::<Vec<Rc<RefCell<DFAState>>>>()
    )

}

pub fn calc_epsilon_closure(
    state: Rc<RefCell<State>>,
    cache: &mut HashMap<usize, Rc<RefCell<KeyHashSet<Rc<RefCell<State>>, usize>>>>)
{
    let state_ref = (*state).borrow();
    if let Some(_) = cache.get(&state_ref.id) {
        return;
    }

    let closure = DFAState::new_key_set();
    let mut clousure_ref = (*closure).borrow_mut();
    clousure_ref.insert(Rc::clone(&state));  // 包含自身

    for transition in state_ref.transitions.iter() {
        if transition.is_epsilon() {
            let next_state = Rc::clone(&transition.to_state);
            let next_state_id = (*next_state).borrow().id;
            //closure.insert(Rc::clone(&next_state));
            calc_epsilon_closure(next_state, cache);
            let other = (**cache.get(&next_state_id).unwrap()).borrow();
            clousure_ref.union(&other);
        }
    }

    cache.insert(state_ref.id, Rc::clone(&closure));
}



fn calc_move(
    s0: Rc<RefCell<DFAState>>,
    c: &char,
    states_set_vec: &mut Vec<Rc<RefCell<StatesSet>>>) {

    let s0_ref = (*s0).borrow();
    let s0_states_ref = (*s0_ref.states).borrow();

    let calc_res_iter = s0_states_ref
        .iter()
        .filter(
            |&x| (**x).borrow().transitions
                .iter()
                .any(|trans| trans.is_match(c)))
        .map(|x| Rc::clone(x));

    let calc_res: Rc<RefCell<StatesSet>>
        = Rc::new(RefCell::new(
            KeySet::from_intoiter(DFAState::states_set_getkey, calc_res_iter)
        ));

    match states_set_vec.iter().find(|&states_set| *(**states_set).borrow() == *(*calc_res).borrow()) {
        None => { states_set_vec.push(calc_res); }
        _ => ()
    }
}

////////////////////////////////////////////////////////////////////////////////
/// Runner
pub fn match_with_nfa(state: &State, input: &str) -> bool {
    println!("NFA matching: {} ", input);

    let chars_input: Vec<char> = input.chars().map(|x| x).collect();

    let matched_index = _match_with_nfa(state, &chars_input[..], 0);
    let matched = matched_index == input.len();

    println!("matched? : {}\n", matched);

    matched
}

fn _match_with_nfa(state: &State, chars_input: &[char], init_index: usize) -> usize {
    println!("trying state : {}, index ={}", state.id, init_index);

    let mut next_index = init_index;

    for transition in state.transitions.iter() {
        let next_state = (*transition.to_state).borrow();
        if transition.is_epsilon() {  // 自动跳过epsilon转移，到达下一个状态
            // epsilon 自动转换状态
            next_index = _match_with_nfa(&next_state, chars_input, init_index);

            // 检查是不是可以退出了
            if next_index == chars_input.len() {
                break;
            }
        } else if transition.is_match(&chars_input[init_index]) {
            next_index += 1;

            if next_index < chars_input.len() {
                next_index = _match_with_nfa(&next_state, chars_input, next_index);
            } else {
                // 如果最终状态不可接受，意味着匹配失败
                if reach_acceptable_state(&next_state) {
                    break;
                } else {
                    next_index -= 1
                }
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
        .dfs_walk(&yes)
        .iter()
        .any(|state| (**state).borrow().acceptable)
}

////////////////////////////////////////////////////////////////////////////////
/// Unit Test

mod test {}
