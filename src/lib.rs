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


    let mut dfa_states_vec: Vec<Rc<RefCell<DFAState>>> = vec![];
    let mut dfa_states_set: KeyHashSet<Rc<RefCell<DFAState>>, usize> = KeyHashSet::new(
        DFAState::dfa_states_get_key
    );

    let mut state_closure_cache = HashMap::new();

    calc_state_epsilon_states_set(Rc::clone(&begin_state), &mut state_closure_cache);

    let begin_state_ref = (*begin_state).borrow();
    let mut cur_states_set= state_closure_cache.get(&begin_state_ref.id).unwrap();

    let cur_dfa_state = Rc::new(RefCell::new(
        DFAState::with_counter_states(counter, cur_states_set)
    ));
    dfa_states_set.insert(
        Rc::clone(&cur_dfa_state)
    );


    // new_states 每次计算出的新的stateset
    let mut new_dfa_state_vec = Vec::<Rc<RefCell<DFAState>>>::new();
    new_dfa_state_vec.push(Rc::clone(cur_states_set));

    while new_dfa_state_vec.len() > 0 {
        let wait_for_calc = new_dfa_state_vec;
        new_dfa_state_vec = vec![];

        for taken_state in wait_for_calc.into_ter() {
            for c in alphabet.iter() {
                let taken_states_set = (*taken_state).borrow().states;
                let next_states_set = r#move(Rc::clone(&taken_states_set), &c);

                if (*next_states_set).borrow().is_empty() { continue }  // 在一个有一定规模的字符集里大部分都是这样的情况

                let mut next_dfa_state;
                match find_dfa_state_by_states_set(&new_dfa_state_vec, next_states_set) {
                    Some(old_dfa_state) => {
                        next_dfa_state = old_dfa_state;
                    }

                    None => {
                        let new_dfa_state
                            = Rc::new(RefCell::new(DFAState::with_counter_states(
                            counter,
                            Rc::clone(&next_dfa_state)
                        )));

                        dfa_states_set.insert(Rc::clone(&new_dfa_state));
                        new_dfa_state_vec.push(Rc::clone(&new_dfa_state));

                    }
                }

                {

                    new_dfa_state_vec.push(new_states_set);
                }

                (*next_states_set).borrow_mut().extend(
                    states_set_epsilon_closure(Rc::clone(&next_states_set), &mut state_closure_cache)
                );
            }
        }
    }

    let mut states_set_vec_cache: Vec<Rc<RefCell<StatesSet>>>
    = vec![];
    states_set_vec_cache.push(Rc::clone(cur_states_set));

    KeyHashSet::from_intoiter(
        DFAState::dfa_states_get_key,
    states_set_vec_cache
        .into_iter()
        .map(|x| Rc::new(RefCell::new(DFAState::with_counter_states(counter, x))))
        .collect::<Vec<Rc<RefCell<DFAState>>>>()
    )

}

/// 计算一个State通过epsilon转换所能到达的状态集合，目的是把这个计算的集合加入到缓存集中。
pub fn calc_state_epsilon_states_set(
    state: Rc<RefCell<State>>,
    cache: &mut HashMap<usize, Rc<RefCell<StatesSet>>>)
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
            calc_state_epsilon_states_set(next_state, cache);
            let other = (**cache.get(&next_state_id).unwrap()).borrow();
            clousure_ref.union(&other);
        }
    }

    cache.insert(state_ref.id, Rc::clone(&closure));
}

/// 得到一个状态集合的闭包
pub fn states_set_epsilon_closure(
    states_set: Rc<RefCell<StatesSet>>,
    cache: &mut HashMap<usize, Rc<RefCell<StatesSet>>>
) -> Rc<RefCell<StatesSet>>

{
    let states_set_ref = (*states_set).borrow();

    for state in states_set_ref.iter() {
        calc_state_epsilon_states_set(*state, cache)
    }

    let res = DFAState::new_key_set();
    let res_ref = (*res).borrow_mut();

    for states_set in states_set_ref
        .iter()
        .map(|state| (*cache.get(&(**state).borrow().id).unwrap())) {

        res_ref.extend(states_set);
    }

    // Rc::new(RefCell::new(KeySet::from_intoiter(
    //     DFAState::states_set_getkey, closure_states_set
    // )))
    Rc::new(RefCell::new(
        res
    ))
}

/// 从StatesSet到StatesSet， 不直接用DFAState是因为counter有id分配的问题
fn r#move(
    s0: Rc<RefCell<StatesSet>>,
    c: &char
) -> Rc<RefCell<StatesSet>>

{

    let s0_states_set_ref = (*s0).borrow();

    let calc_res_iter = s0_states_set_ref
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

    calc_res
}


/// find if states_set exists
fn find_dfa_state_by_states_set(
    states_set_vec: &Vec<Rc<RefCell<DFAState>>>,
    target: Rc<RefCell<StatesSet>>
) -> Option<Rc<RefCell<StatesSet>>>

{
    match states_set_vec.iter().find(|&states_set| {
        let dfa_state = *(**states_set).borrow();
        dfa_state.states == *(*target).borrow()
    })
    {
        Some(&res) => { Some(res) }
        _ => None
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
