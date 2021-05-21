use std::{cell::RefCell, collections::VecDeque, fmt, iter::FromIterator, rc::Rc, vec};

use indexmap::{indexmap, IndexMap};
use itertools::Itertools;

use crate::{algs::state::DFAStateGraph, stack};

use super::algs::{gram::*, state::*};
use super::lexer::*;
use super::utils::Stack;

////////////////////////////////////////////////////////////////////////////////
/////// AST

/// AST Node
#[derive(Debug)]
pub enum ASTNode {
    Tree(Rc<RefCell<AST>>),
    Leaf(Rc<Token>),
}

impl ASTNode {
    pub fn dump(&self, f: &mut fmt::Formatter, padlevel: usize) -> fmt::Result {
        let padding = "  ".repeat(padlevel);

        match self {
            Self::Leaf(token) => writeln!(f, "{}({}){}", padding, padlevel, *token),
            Self::Tree(ast) => {
                let ast_ref = ast.as_ref().borrow();
                ast_ref.dump(f, padlevel)
            }
        }
    }

    pub fn get_token(&self) -> Option<&Rc<Token>> {
        match self {
            Self::Leaf(token) => Some(token),
            _ => None,
        }
    }

    pub fn get_ast(&self) -> Option<&Rc<RefCell<AST>>> {
        match self {
            Self::Tree(ast) => Some(ast),
            _ => None,
        }
    }

    pub fn to_gram_sym(&self) -> GramSym {
        match self {
            Self::Tree(ast) => ast.as_ref().borrow().sym().to_owned(),
            Self::Leaf(token) => token.as_ref().to_gram_sym(),
        }
    }
}

impl fmt::Display for ASTNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            Self::Tree(tree) => {
                write!(f, "{}", tree.as_ref().borrow())?;
            },
            Self::Leaf(token) => {
                writeln!(f, "{}", token.as_ref())?;
            }
        }

        Ok(())
    }
}

/// AST
#[derive(Debug)]
pub struct AST {
    /// AST's grammar type
    sym: GramSym,
    elems: IndexMap<GramSym, ASTNode>,
}

impl AST {
    pub fn new(sym: &GramSym) -> Self {
        Self {
            sym: sym.clone(),
            elems: indexmap! {},
        }
    }

    pub fn sym(&self) -> &GramSym {
        &self.sym
    }

    pub fn elem_syms(&self) -> Vec<GramSym> {
        self.elems.keys().cloned().collect_vec()
    }

    pub fn get_elem(&self, sym: &GramSym) -> Option<&ASTNode> {
        self.elems.get(sym)
    }

    pub fn insert_leaf(&mut self, token: Token) {
        let leaf_name = token.to_gram_sym();
        let leaf = ASTNode::Leaf(Rc::new(token));

        self.elems.insert(leaf_name, leaf);
    }

    pub fn insert_tree(&mut self, tree: Rc<RefCell<AST>>) {
        let tree_name = tree.as_ref().borrow().sym().clone();
        let tree = ASTNode::Tree(tree);

        self.elems.insert(tree_name, tree);
    }

    pub fn insert_node(&mut self, node: ASTNode) {
        self.elems.insert(node.to_gram_sym().to_owned(), node);
    }

    fn dump(&self, f: &mut fmt::Formatter, padlevel: usize) -> fmt::Result {
        let padding = "  ".repeat(padlevel);

        writeln!(f, "{}({}){}: ", padding, padlevel, self.sym())?;

        for (_elem_sym, elem_node) in self.elems.iter() {
            elem_node.dump(f, padlevel + 1)?;
        }

        Ok(())
    }

    /// There are no circle dependency on Tree
    fn copy_tree(&self) -> Rc<RefCell<Self>> {
        let mut new_tree = Self::new(self.sym());

        for (sym, node) in self.elems.iter() {
            match node {
                ASTNode::Leaf(token) => {
                    new_tree
                        .elems
                        .insert(sym.clone(), ASTNode::Leaf(token.clone()));
                }
                ASTNode::Tree(subtree) => {
                    new_tree.insert_tree(subtree.as_ref().borrow().copy_tree());
                }
            }
        }

        Rc::new(RefCell::new(new_tree))
    }
}

impl fmt::Display for AST {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.dump(f, 0)
    }
}

////////////////////////////////////////////////////////////////////////////////
/////// Parser Trait
pub trait Parser {
    fn parse(&self, source: &str) -> Result<Rc<RefCell<AST>>, String>;
}

////////////////////////////////////////////////////////////////////////////////
/////// Evaluator Trait
pub trait Evaluator<RES> {
    fn eval(&self, ast: &Rc<RefCell<AST>>) -> Result<RES, String>;
}

////////////////////////////////////////////////////////////////////////////////
/////// 带回溯的 变形 LL(1) Parser
pub struct LL1Parser {
    name: String,
    gram: Gram,
    lexer: Lexer,
    prediction_sets: PredLL1Sets,
}

type LL1ParseStatesStack = Vec<(Rc<RefCell<AST>>, Stack<GramSym>)>;

impl LL1Parser {
    pub fn new(name: &str, gram: Gram, lexer: Lexer) -> Self {
        let first_sets = gram.first_sets();
        let follow_sets = gram.follow_sets(&first_sets);
        let prediction_sets = gram.prediction_sets(&first_sets, &follow_sets);

        Self {
            name: name.to_string(),
            gram,
            lexer,
            prediction_sets,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn get_pred_str(
        &self,
        predsym: &PredSetSym,
        leftsym_expcted: &GramSym,
    ) -> Option<Vec<&GramSymStr>> {
        let prods_set;
        match self.prediction_sets.get(predsym) {
            Some(value) => prods_set = value,
            None => {
                return None;
            }
        };

        let matched_symstr_vec = prods_set
            .into_iter()
            .filter(|(leftsym, _symstr)| leftsym == leftsym_expcted)
            .map(|(_leftsym, symstr)| symstr)
            .collect_vec();

        if matched_symstr_vec.is_empty() {
            None
        } else {
            Some(matched_symstr_vec)
        }
    }
}

// Impl for Parser
impl Parser for LL1Parser {
    fn parse(&self, source: &str) -> Result<Rc<RefCell<AST>>, String> {
        let channels = self.lexer.tokenize(source);
        let tokens = &channels.get_index(0).unwrap().1.tokens;
        if tokens.is_empty() {
            return Err("empty tokens".to_string());
        }

        println!("tokens: {:#?}\n", tokens);
        println!("LL(1): ");

        let start_sym = self.gram.start_sym().unwrap();
        let root = Rc::new(RefCell::new(AST::new(&start_sym)));

        let mut res = Err(format!(
            "Unexpected token: `{}` for root grammar",
            tokens[0]
        ));

        // Check root， 分支预测
        if let Some(poss_brs) = self.get_pred_str(&tokens[0].to_pred_set_sym(), start_sym) {
            let mut poss_brs_iter = poss_brs.into_iter();

            while let Some(symstr) = poss_brs_iter.next() {
                if let GramSymStr::Str(gramsym_vec) = symstr {
                    // gramsym_vec rev for stack
                    let states_stack = vec![(root.clone(), Stack::from(gramsym_vec.clone()))];

                    if let Ok(_res) = _ll1_parse(&self, &tokens[..], states_stack) {
                        res = Ok(_res);
                        break;
                    }
                }
            }
        }

        res
    }
}


impl fmt::Display for Stack<GramSym> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_empty() { return Ok(()) }

        let lastpos = self.len() - 1;
        for (i, item) in self.stack_iter().enumerate() {
            if i < lastpos {
                write!(f, "{},", item)?
            } else {
                write!(f, "{}", item)?
            }
        }

        Ok(())
    }
}


/// Result: <ASTRoot, UnsupportedTokenType>
fn _ll1_parse(
    parser: &LL1Parser,
    tokens: &[Token],
    mut states_stack: LL1ParseStatesStack,
) -> Result<Rc<RefCell<AST>>, String> {
    if tokens.is_empty() {
        return Err("empty tokens".to_string());
    }

    debug_assert!(
        !states_stack.is_empty(),
        "states stack should keep filled up by invoker"
    );

    let root = states_stack[0].0.clone();
    let tokenslen = tokens.len();
    let tokenslastpos = tokenslen - 1;
    let mut i = 0;

    while let Some((cur_ast, mut symstr_stack)) = states_stack.pop() {
        println!(
            ">>> `{} => ...{}`",
            cur_ast.as_ref().borrow().sym(),
            symstr_stack
        );

        // 分支匹配，遇到终结符直接匹配，遇到非终结符就入栈回到起点
        while let Some(right_sym) = symstr_stack.pop() {
            if i > tokenslastpos {
                if let Some(_)  // 检查当前产生式是否允许结束
                = parser.get_pred_str(&PredSetSym::EndMarker, &right_sym)
                {
                    return Ok(root);
                } else {
                    return Err(format!(
                        "Unfinished production: {:?}",
                        (
                            cur_ast.as_ref().borrow().sym(),
                            symstr_stack
                        )
                    ));
                }
            }

            if right_sym.is_terminal() {
                println!("? eat terminal: `{}`", right_sym);

                if right_sym == tokens[i].to_gram_sym() {
                    cur_ast.as_ref().borrow_mut().insert_leaf(tokens[i].clone());

                    // cosume a token
                    println!("! eaten token: {:?}", tokens[i]);

                    i += 1;
                    while i < tokenslen && tokens[i].name().ends_with("comment") {
                        println!("...skip comment token: {}", tokens[i]);
                        i += 1;
                    }
                    if i == tokenslen {
                        break;
                    }
                } else {
                    return Err(format!(
                        "Unmatched token{}, a {} expected",
                        tokens[i], right_sym
                    ));
                }
            } else {
                // handle nonterminal
                // LL(1)的带回溯的分支预测
                // case-1 (just goon)
                // case-2 (push, loop continue)
                // case-3 (return res<ok/error>)
                // case-4 (error)

                if let Some(poss_brs) =
                    parser.get_pred_str(&tokens[i].to_pred_set_sym(), &right_sym)
                {
                    let sub_sym_tree = Rc::new(RefCell::new(AST::new(&right_sym)));
                    cur_ast
                        .as_ref()
                        .borrow_mut()
                        .insert_tree(sub_sym_tree.clone());
                    states_stack.push((cur_ast.clone(), symstr_stack.clone()));

                    // case-1
                    // 正经的LL(1) 匹配
                    if poss_brs.len() == 1 {
                        let pred_symstr = poss_brs[0];
                        // 在计算predsets时已经把epsilon str的情况单独提出来了
                        let norm_sym_vec = pred_symstr.get_normal().unwrap();

                        println!(
                            "?>! `{}`: `{}`",
                            right_sym,
                            GramSymStr::Str(norm_sym_vec.clone())
                        );

                        states_stack.push((sub_sym_tree, Stack::from(norm_sym_vec.clone())));

                        break;
                    }

                    // case-2, 需要实现树的复制
                    // 回溯的LL(1) 匹配
                    // else， 前面必定返回，所以不必加额外的else block来消耗宝贵的缩进资源
                    let mut poss_brs_iter = poss_brs.into_iter();

                    while let Some(pred_symstr) = poss_brs_iter.next() {
                        let norm_sym_vec = pred_symstr.get_normal().unwrap();

                        println!(
                            "?>? `{}`: `{}`",
                            right_sym,
                            GramSymStr::Str(norm_sym_vec.clone())
                        );

                        states_stack
                            .push((sub_sym_tree.clone(), Stack::from(norm_sym_vec.clone())));
                        // 实际上是拷贝了整个树
                        let new_states_stack = _copy_ll1_states_stack(&states_stack);

                        let (_, new_tokens) = tokens.split_at(i);

                        // right_sym 完成匹配
                        if let Ok(_res) = _ll1_parse(parser, new_tokens, new_states_stack) {
                            return Ok(_res);
                        } else {
                            println!("*<x `{}`", right_sym);
                        }

                        // clean env
                        states_stack.pop();
                    }

                    return Err(format!(
                        "All possible branch has been failed for grammar: `{}`",
                        right_sym
                    ));
                }
                // case-3
                // 如果找不到符合条件的分支，就试一下right_sym是否存在epsilon转换
                else if let Some(_) = parser.get_pred_str(&PredSetSym::Epsilon, &right_sym) {
                    println!(" ... skipp epsilon: `{}` (token: {})", right_sym, tokens[i]);
                    // just skip epsilon
                }
                // case-4
                else {
                    return Err(format!(
                        "Unexpected token: `{}` for grammar: `{}`",
                        tokens[i], right_sym
                    ));
                }
            }
        } // end while

        println!();
    }

    if i < tokenslastpos {
        return Err(format!(
            "Tokens remains: `{:?}`",
            tokens.get(i..tokenslen).unwrap()
        ));
    }

    Ok(root)
}

fn _copy_ll1_states_stack(states_stack: &LL1ParseStatesStack) -> LL1ParseStatesStack {
    if states_stack.is_empty() {
        return vec![];
    }

    let (root, root_stack) = &states_stack[0];
    let new_root = root.as_ref().borrow().copy_tree();
    let mut new_states_stack = vec![(new_root.clone(), root_stack.clone())];

    let mut parent = new_root;

    for (tree, sym_stack) in states_stack.iter().skip(1) {
        // 根据旧的AST的sym，查找新的AST
        // 原理是statesstack每个元素的AST是层层相连的
        // (ith AST 是 i+1th AST的直接父母)
        let new_tree = parent
            .as_ref()
            .borrow()
            .get_elem(tree.as_ref().borrow().sym())
            .unwrap()
            .get_ast()
            .unwrap()
            .clone();

        new_states_stack.push((new_tree.clone(), sym_stack.clone()));

        parent = new_tree;
    }

    new_states_stack
}

////////////////////////////////////////////////////////////////////////////////
/////// SLR(1) Parser (parser for LR(0) FSM)

pub struct SLR1Parser {
    name: String,
    gram: Gram,
    lexer: Lexer,
    dfa_g: DFAStateGraph<LR0Closure, GramSym, GramSym>,
    follow_sets: FollowSets,
}

impl SLR1Parser {
    pub fn new(name: &str, gram: Gram, lexer: Lexer) -> Self {
        let dfa_g = gram.lr0_dfa();
        let follow_sets = gram.follow_sets(&gram.first_sets());

        Self {
            name: name.to_owned(),
            gram,
            lexer,
            dfa_g,
            follow_sets,
        }
    }
}

pub type LR0DFAState = Rc<RefCell<DFAState<LR0Closure, GramSym, GramSym>>>;

pub enum LROp {
    Shift, Reduce
}

impl Parser for SLR1Parser {
    fn parse(&self, source: &str) -> Result<Rc<RefCell<AST>>, String> {
        let channels = self.lexer.tokenize(source);
        let tokens = channels.get_index(0).unwrap().1.tokens.clone();
        let start_sym = self.gram.start_sym().unwrap();

        if tokens.is_empty() {
            return Ok(Rc::new(RefCell::new(AST::new(&start_sym))));
        }

        println!("tokens: {:#?}\n", tokens);
        println!("SLR(1) {}: ", self.name);
        println!("{}", "-".repeat(80));

        // let ast = Rc::new(RefCell::new(AST::new(&start_sym)));
        let mut token_queue = VecDeque::from_iter(tokens.into_iter());

        let mut reduce_stack = stack![];

        let mut state_stack = stack![self.dfa_g.top().unwrap().to_owned()];

        let mut state_input = Some(token_queue.front().unwrap().to_gram_sym());
        let mut lookahead = FollSetSym::EndMarker;

        while !state_stack.is_empty() {

            let cur_state = state_stack.top().unwrap().to_owned();
            let cur_state_ref = cur_state.as_ref().borrow();

            let mut lrop = LROp::Shift;  // 默认Shift

            if let Some(input) = &state_input {
                match cur_state_ref.try_next(&input) {
                    TryNxtRes::Ok(state) => {
                        state_stack.push(state.clone());

                        if input.is_nonterminal() {
                            state_input = match lookahead {
                                FollSetSym::EndMarker => None,
                                FollSetSym::Sym(ref sym) => Some(GramSym::Terminal(sym.to_owned()))
                            };
                            continue;
                        } else {
                            // Shift
                        }
                    }
                    TryNxtRes::None => {
                        lrop = LROp::Reduce;
                    }
                    TryNxtRes::Multi(_) => {
                        unreachable!()
                    }
                }
            } else {
                lrop = LROp::Reduce;
            }

            match lrop {
                LROp::Shift => {
                    debug_print_lr0(&token_queue, &reduce_stack, &state_stack, "Shift");

                    state_input = slr1_shift(
                        &mut token_queue,
                        &mut lookahead,
                        &mut reduce_stack,
                    );
                },
                LROp::Reduce => {
                    debug_print_lr0(&token_queue, &reduce_stack, &state_stack, "Reduce");

                    state_input = slr1_reduce(
                        state_input,
                        &self.follow_sets,
                        &lookahead,
                        &mut reduce_stack,
                        &mut state_stack,
                    );
                }
            }
        } // end loop

        if !token_queue.is_empty() {
            return Err(format!("Tokens remains: `{:?}`", token_queue));
        }

        if reduce_stack.len() > 1 {
            return Err(format!("Unreduced stack remains: `{:?}`", reduce_stack));
        }

        let root_node = reduce_stack.pop().unwrap();
        let root_sym = root_node.to_gram_sym();
        if root_sym != *start_sym {
            return Err(format!(
                "Incorrected root sym: `{}`\n(`{}` expected)",
                root_sym, start_sym
            ));
        }

        Ok(root_node.get_ast().unwrap().clone())
    }
}

fn slr1_shift(
    token_queue: &mut VecDeque<Token>,
    lookahead: &mut FollSetSym,
    reduce_stack: &mut Stack<ASTNode>,
) -> Option<GramSym> {
    let token = token_queue.pop_front().unwrap();

    // Consume token and turn it into ast node
    let node = ASTNode::Leaf(Rc::new(token));
    reduce_stack.push(node);

    match token_queue.front() {
        Some(token) => {
            *lookahead = token.to_foll_set_sym();

            Some(token.to_gram_sym())
        },
        None => {
            *lookahead = FollSetSym::EndMarker;

            None
        },
    }
}

fn slr1_reduce(
    old_state_input: Option<GramSym>,
    follow_sets: &FollowSets,
    lookahead: &FollSetSym,
    reduce_stack: &mut Stack<ASTNode>,
    states_stack: &mut Stack<LR0DFAState>, // shouldn't be empty
) -> Option<GramSym> {
    let cur_state = states_stack.pop().unwrap();
    let cur_state_ref = cur_state.as_ref().borrow();
    let end_item = cur_state_ref
        .data
        .data()
        .unwrap()
        .end_items()
        .into_iter()
        .filter(|item| {
            let lhs = item.lhs_sym();
            follow_sets.get(lhs).unwrap().contains(lookahead)
        })
        .collect_vec()
        .pop();

    if let Some(end_item) = end_item {
        // Bottom Up!

        // reduce node to paren node
        let lhs_sym = end_item.lhs_sym();
        let mut paren = AST::new(lhs_sym);

        if let GramSymStr::Str(symstr) = end_item.symstr() {
            symstr.into_iter().map(|_sym| {
                reduce_stack.pop().unwrap()
            }).rev().for_each(|node| {
                paren.insert_node(node);
            });
        } else {
            // LR Item的 SymStr 里面不应有epsilon
            unreachable!();
        }

        let paren_node = ASTNode::Tree(Rc::new(RefCell::new(paren)));

        reduce_stack.push(paren_node);

        Some(lhs_sym.to_owned())
    } else {
        states_stack.pop();
        old_state_input
    }
}


impl fmt::Display for Stack<ASTNode> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_empty() { return Ok(()) }

        let lastpos = self.len() - 1;
        for (i, item) in self.stack_iter().enumerate() {
            if i < lastpos {
                write!(f, "{},", item)?
            } else {
                write!(f, "{}", item)?
            }
        }

        Ok(())
    }
}

fn debug_print_lr0(
    // state_input: &GramSym,
    token_queue: &VecDeque<Token>,
    reduce_stack: &Stack<ASTNode>,
    state_stack: &Stack<LR0DFAState>,
    action: &str,
)
{
    println!("Stack:  \n{}", reduce_stack);
    println!("Input:  {}", token_queue.iter().map(|token| token.value()).join(""));
    println!(
        "State:  {}",
        state_stack.queue_iter()
        .map(|state| format!("({})", state.as_ref().borrow().id))
        .collect_vec()
        .join(", ")
    );
    println!("Action: {}", action);
    println!("{}", "-".repeat(80));
}

////////////////////////////////////////////////////////////////////////////////
/////// Unit Test

#[cfg(test)]
mod test {
    use itertools::Itertools;

    #[test]
    fn test_comment_preprocor() {
        use super::{comment_partial_match, replace_with_samelenspace};
        use crate::regex::slash_line_comment_m;

        let slash_line_commentm = slash_line_comment_m();

        let test_script1 = r#"
        //下面两个的叫声会不同。在运行期动态绑定方法。
        a.speak();  //会打印牛叫
        b.speak();  //会打印羊叫
        "#;
        let test_script1_chars = test_script1.chars().collect_vec();

        let (l, r) = comment_partial_match(&slash_line_commentm, &test_script1_chars).unwrap();
        let res_1: String = test_script1_chars[l..r].iter().collect();
        println!("{}", res_1);

        let res_r_s: String = replace_with_samelenspace(&test_script1_chars, (l, r))
            .iter()
            .collect();
        println!("{}", res_r_s);
    }
}
