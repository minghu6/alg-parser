#![feature(min_type_alias_impl_trait)]

mod utils;

use std::fmt;

#[macro_use(c)]
extern crate cute;

use utils::{ CounterType, gen_counter };

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

#[derive(Clone)]
pub struct CharSet {
    char_scopes: Vec::<(char, char)>
}

impl CharSet {
    pub fn new() -> Self {
        CharSet {
            char_scopes: Vec::new()
        }
    }

    pub fn add(&mut self, char_scope: (char, char)) {
        self.char_scopes.push(char_scope)  // return ()
    }

    pub fn contains(&self, inputc: &char) -> bool {
        for (lower, upper) in self.char_scopes.iter() {
            if lower <= inputc && inputc <= upper { return true; }
        }

        false
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
        let res = self.char_scopes.iter()
        .map(|(lower, upper)| {
            if lower == upper { format!("{}", lower) }
            else { format!("{}-{}", lower, upper) }
        })
        .collect::<Vec<String>>()
        .join("");

        write!(f, "{}", res)
    }
}


////////////////////////////////////////////////////////////////////////////////
/////// Transition

#[derive(Clone)]
pub struct Transition<'a> {
    chars: CharSet,
    max_times: usize,
    to_state: &'a State<'a>
}

impl Transition<'_> {
    pub fn is_match(&self, inputc: &char) -> bool {
        self.chars.contains(inputc)
    }

}

////////////////////////////////////////////////////////////////////////////////
/// State

#[derive(Clone)]
pub struct State<'a> {
    id: usize,
    acceptable: bool,
    transitions: Vec<Transition<'a>>,
    grammar_node: GrammarNode
}

impl <'a> State<'a> {
    ///
    /// Static Create Method
    ///
    pub fn with_counter(counter: &mut CounterType) -> Self {
        State {
            id: counter(),
            acceptable: false,
            transitions: Vec::new(),
            grammar_node: GrammarNode::new()
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

    pub fn add_transition(&mut self, transition: Transition<'a>) {
        self.transitions.push(transition)
    }

    pub fn copy_transitions(&mut self, state: &State<'a>) {
        self.transitions = state.transitions.clone();
    }
}

////////////////////////////////////////////////////////////////////////////////
/// GrammarNode: Simple Regex Node

#[derive(PartialEq, Clone)]
pub enum GrammarNodeType {
    And,
    Or,
    LexNode,    // 词法规则的字符
    SyntaxNode,   // 语法规则的字符
    Epsilon  // 空集
}

#[derive(Clone)]
pub struct GrammarNode {
    childen: Vec<GrammarNode>,
    nodetype: GrammarNodeType,
    repeat_times: (usize, usize),
    chars: CharSet
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
            chars: CharSet::new()
        }
    }

    pub fn from_charset(chars: CharSet) -> Self {
        let mut node = GrammarNode::new();
        node.nodetype = GrammarNodeType::LexNode;
        node.chars = chars;

        node
    }

    pub fn from_charset_repeat_times
        (chars: CharSet, repeat_times: (usize, usize)) -> Self {
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
        self.childen.push(child)
    }

    ///
    /// Instance Show Method
    ///
    pub fn in_short_text(&self) -> String {
        let mut text = String::new();
        if self.childen.len() == 0 {
            text.push_str(format!("[{}]", self.chars).as_str());
            text.push_str(
                match self.repeat_times {
                    (1, usize::MAX) => "+",
                    (0, usize::MAX) => "*",
                    _ => ""
                }
            );
        } else {
            let delim = if self.nodetype == GrammarNodeType::And { "" } else { " | " };

            text.push_str(
                c![child.in_short_text(), for child in self.childen.iter()].join(delim)
                .as_str()
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
/// Converter

pub fn regex2nfa<'a>(counter: &mut CounterType, node: &'a GrammarNode)
     -> (State<'a>, State<'a>) {
    let (mut begin_state, mut end_state);

    match node.nodetype {
        GrammarNodeType::Or => {
            begin_state = State::with_counter(counter);
            end_state = State::with_counter_accept(counter);
        }

        _ => {}
    }

    (begin_state, end_state)
}


////////////////////////////////////////////////////////////////////////////////
/// Unit Test


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

        assert_eq!(format!("{}", charset!{ a-a | 0-9 | a-z | 你-好}), "a0-9a-z你-好");
    }

}

