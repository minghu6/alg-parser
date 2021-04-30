#![feature(min_type_alias_impl_trait)]
#![feature(destructuring_assignment)]
#![feature(generators, generator_trait)]

#[macro_use(c)]
extern crate cute;
#[macro_use]
extern crate maplit;

pub mod data;
pub mod utils;

use std::collections::HashMap;
use std::rc::Rc;
use std::{
    borrow::{Borrow},
    cell::RefCell,
};

use key_set::{KeyHashSet, KeySet};

use data::*;
use utils::{yes, CounterType, GraphWalker};

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

        GrammarNodeType::And => {
            // 各个转移状态前后相连
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

        GrammarNodeType::LexNode => {
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
pub fn nfa2dfa(
    counter: &mut CounterType,
    begin_state: Rc<RefCell<State>>,
    alphabet: &CharSet,
) -> Vec<Rc<RefCell<DFAState>>> {
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
                            let trans = DFATransition::new(Rc::downgrade(&next_dfa_state));
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
                            .add_transition(DFATransition::new(Rc::downgrade(&new_dfa_state)));

                        next_dfa_state = new_dfa_state;
                    }
                }

                insert_char_on_transition(Rc::clone(&taken_state), next_dfa_state, c);
            }
        }
    }

    //display_states_set(&(*cur_states_set).borrow());

    //cur_dfa_state
    dfa_states_vec
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
pub fn states_set_epsilon_closure(
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
