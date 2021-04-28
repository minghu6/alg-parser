#![feature(min_type_alias_impl_trait)]
#![feature(destructuring_assignment)]
#![feature(generators, generator_trait)]

#[macro_use(c)]
extern crate cute;

#[macro_use]
extern crate maplit;

pub mod data;
pub mod utils;

use std::cell::RefCell;
use std::rc::Rc;

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
        .copy_transitions(&(*to_begin_state).borrow());
    (*from_end_state).borrow_mut().acceptable = false;
}

pub fn add_repetition(
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
            // 新的开始和结束状态
            begin_state = Rc::new(RefCell::new(State::with_counter(counter)));
            end_state = Rc::new(RefCell::new(State::with_counter_accept(counter)));

            for child in node.childen.iter() {
                let (sub_begin_state, sub_end_state) = regex2nfa(counter, child);

                // 新的开始状态通过 ε 连接到子图的开始状态
                // 子图的结束状态通过 ε 连接到子图的结束状态
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
        .any(|state| (*state).borrow().acceptable)
}

////////////////////////////////////////////////////////////////////////////////
/// Unit Test

mod test {}
