#![feature(min_type_alias_impl_trait)]
#![feature(destructuring_assignment)]

#[macro_use(c)]
extern crate cute;

#[macro_use]
extern crate maplit;

pub mod utils;
pub mod data;

use std::cell::RefCell;
use std::rc::{ Rc };

use data::*;
use utils::{ CounterType, gen_counter };


////////////////////////////////////////////////////////////////////////////////
/// Converter

/// Concatenate two subgraph
/// 也许subgraph应该是一个子图结构
pub fn concat_state(
     from_graph: &mut (Rc<RefCell<State>>, Rc<RefCell<State>>),
     to_graph: &(Rc<RefCell<State>>, Rc<RefCell<State>>)
    ) {

        let from_end_state = &mut from_graph.1;
        let to_begin_state = &to_graph.0;

    (*from_end_state).borrow_mut().copy_transitions(
        &(*to_begin_state).borrow()
    );
    (*from_end_state).borrow_mut().acceptable = false;
}

pub fn add_repetition(
    from_state: Rc<RefCell<State>>,
    to_state: Rc<RefCell<State>>,
    node:  &GrammarNode,
    counter: &mut CounterType
) -> (Rc<RefCell<State>>, Rc<RefCell<State>>)
{
    let (begin_state, end_state);

    if node.repeat_times.1 > 1 {  // 构造一个循环， to_state 反指 from_state
        (*to_state).borrow_mut().add_transition(
            Transition::from_max_times(
                node.repeat_times.1, Rc::clone(&from_state)
            )
        )
    }

    // 0次再加两个节点
    if node.repeat_times.0 == 0 {
        begin_state = Rc::new(RefCell::new(
            State::with_counter(counter)
        ));
        end_state = Rc::new(RefCell::new(
            State::with_counter_accept(counter)
        ));

        (*begin_state).borrow_mut().add_transition(
            Transition::epsilon(Rc::clone(&to_state))
        );

        (*to_state).borrow_mut().add_transition(
            Transition::epsilon(Rc::clone(&end_state))
        );
        (*to_state).borrow_mut().acceptable = false;

        (*begin_state).borrow_mut().add_transition(
            Transition::epsilon(Rc::clone(&end_state))
        );
    } else {
        begin_state = from_state;
        end_state = to_state;
    }

    (begin_state, end_state)
}

pub fn regex2nfa(counter: &mut CounterType, node: &GrammarNode)
     -> (Rc<RefCell<State>>, Rc<RefCell<State>>) {
    let (mut begin_state, mut end_state);

    match node.nodetype {
        GrammarNodeType::Or => {
            // 新的开始和结束状态
            begin_state = Rc::new(RefCell::new(
                State::with_counter(counter)
            ));
            end_state = Rc::new(RefCell::new(
                State::with_counter_accept(counter)
            ));

            for child in node.childen.iter() {
                let (sub_begin_state, sub_end_state) = regex2nfa(counter, child);

                // 新的开始状态通过 ε 连接到子图的开始状态
                // 子图的结束状态通过 ε 连接到子图的结束状态
                (*begin_state).borrow_mut().add_epsilon_transition(
                    Rc::clone(&sub_begin_state)
                );
                (*sub_end_state).borrow_mut().add_epsilon_transition(
                    Rc::clone(&end_state)
                );
                (*sub_end_state).borrow_mut().acceptable = false;
            }
        }

        GrammarNodeType::And => {
            let sub_graphs: Vec<(Rc<RefCell<State>>, Rc<RefCell<State>>)>
                = node.childen
                    .iter()
                    .map(|child| {
                        regex2nfa(counter, child)
                    })
                    .collect();

            if sub_graphs.len() > 0 {
                begin_state = Rc::clone(&sub_graphs.first().unwrap().0);
                end_state = Rc::clone(&sub_graphs.last().unwrap().1);

                for i in 1..sub_graphs.len() {
                    concat_state(
                        &mut (sub_graphs[i-1].clone()),
                        &(sub_graphs[i].clone())
                    )
                }
            } else {
                unreachable!()
            }

        }

        GrammarNodeType::LexNode => {
            begin_state = Rc::new(RefCell::new(
                State::with_counter(counter)
            ));
            end_state = Rc::new(RefCell::new(
                State::with_counter_accept(counter)
            ));

            (*begin_state).borrow_mut().add_transition(
                Transition::from_grammar_node(node, Rc::clone(&end_state))
            )
        }

        _ => unreachable!()
    }

    // 处理重复的情况
    if node.repeat_times.0 != 1 || node.repeat_times.1 != 1 {
        (begin_state, end_state)
            = add_repetition(begin_state, end_state, node, counter);
    }


    (begin_state, end_state)
}


////////////////////////////////////////////////////////////////////////////////
/// Unit Test


mod test {
}

