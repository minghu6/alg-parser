use std::cell::RefCell;
use std::collections::HashSet;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

use super::utils::{CounterType, GraphWalker};

///! 因为只有一套实现，所以不需要定义接口

////////////////////////////////////////////////////////////////////////////////
/// Charset

///! charset!{a-z | 0-9 | 啊-吧}
#[macro_export]
macro_rules! charset {
    //( $($char_scope:tt | )+ ) => (charset!($($charset) | +))

    ( $($lower:tt-$upper:tt)|* ) => {
        {
            let mut _charset = CharSet::new();

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

// #[derive(Copy, Clone)]
// pub struct CharRange {
//     begin_char: char,
//     end_char: char
// }

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CharSet {
    pub char_scopes: Vec<(char, char)>,
}

impl CharSet {
    pub fn new() -> Self {
        CharSet {
            char_scopes: Vec::new(),
        }
    }

    pub fn add(&mut self, char_scope: (char, char)) {
        self.char_scopes.push(char_scope) // return ()
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

////////////////////////////////////////////////////////////////////////////////
/// GrammarNode: Simple Regex Node

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum GrammarNodeType {
    And,
    Or,
    LexNode,    // 词法规则的字符
    SyntaxNode, // 语法规则的字符
    Epsilon,    // 空集
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct GrammarNode {
    pub childen: Vec<Box<GrammarNode>>,
    pub nodetype: GrammarNodeType,
    pub repeat_times: (usize, usize),
    pub chars: CharSet,
}

impl GrammarNode {
    ///
    /// Static Create Method
    ///
    pub fn new() -> Self {
        GrammarNode {
            childen: Vec::new(),
            nodetype: GrammarNodeType::Epsilon,
            repeat_times: (1, 1),
            chars: CharSet::new(),
        }
    }

    pub fn from_charset(chars: CharSet) -> Self {
        let mut node = GrammarNode::new();
        node.nodetype = GrammarNodeType::LexNode;
        node.chars = chars;

        node
    }

    pub fn from_charset_repeat_times(chars: CharSet, repeat_times: (usize, usize)) -> Self {
        let mut node = GrammarNode::from_charset(chars);
        node.repeat_times = repeat_times;

        node
    }

    pub fn create_or_node() -> Self {
        let mut node = GrammarNode::new();
        node.nodetype = GrammarNodeType::Or;

        node
    }

    pub fn create_and_node() -> Self {
        let mut node = GrammarNode::new();
        node.nodetype = GrammarNodeType::And;

        node
    }

    ///
    /// Instance Update Method
    ///
    pub fn add_child(&mut self, child: GrammarNode) {
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

impl fmt::Display for GrammarNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        //let mut indent = "";

        write!(f, "{}", self.in_short_text())
    }
}

////////////////////////////////////////////////////////////////////////////////
/// State

#[derive(Clone)]
pub struct State {
    pub id: usize,
    pub acceptable: bool,
    pub transitions: Vec<Transition>,
    pub grammar_node: GrammarNode,
}

impl State {
    ///
    /// Static Create Method
    ///
    pub fn with_counter(counter: &mut CounterType) -> Self {
        State {
            id: counter(),
            acceptable: false,
            transitions: Vec::new(),
            grammar_node: GrammarNode::new(),
        }
    }

    /// with acceptable is true
    pub fn with_counter_accept(counter: &mut CounterType) -> Self {
        let mut state = State::with_counter(counter);
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
        state: &State,
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
                match State::dump(&to_state, f, visited_states) {
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

/// Graph Walker
impl GraphWalker for State {
    fn get_id(&self) -> u128 {
        self.id as u128
    }

    fn get_childern(&self) -> Box<dyn Iterator<Item = Rc<RefCell<Self>>>> {
        let childern_vec: Vec<Rc<RefCell<Self>>> = self
            .transitions
            .iter()
            .map(|trans| {
                let state = Rc::clone(&(*trans).to_state);
                state
            })
            .collect();

        Box::new(childern_vec.into_iter())
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
    pub fn from_grammar_node(node: &GrammarNode, to_state: Rc<RefCell<State>>) -> Self {
        Transition {
            chars: node.chars.clone(),
            max_times: 1,
            to_state: to_state,
        }
    }

    pub fn from_max_times(max_times: usize, to_state: Rc<RefCell<State>>) -> Self {
        Transition {
            chars: CharSet::new(),
            max_times,
            to_state: to_state,
        }
    }

    pub fn epsilon(to_state: Rc<RefCell<State>>) -> Self {
        Transition {
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

mod test {
    #[test]
    fn test_struct_charset() {
        use super::CharSet;

        let mut mycharset = CharSet::new();
        mycharset.add(('a', 'z'));
        mycharset.add(('2', '2'));

        assert_eq!(format!("{}", mycharset), "a-z2");
        assert!(mycharset.contains(&'b'));
        assert!(!mycharset.contains(&'3'));
        assert!(mycharset.contains(&'2'));
    }

    #[test]
    fn test_macro_charset() {
        use super::CharSet;

        assert_eq!(
            format!("{}", charset! { a-a | 0-9 | a-z | 你-好}),
            "a0-9a-z你-好"
        );
    }
}
