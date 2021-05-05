use std::{
    fmt,
    hash::{Hash, Hasher},

};

use super::regex::PriRegexMatcher;

////////////////////////////////////////////////////////////////////////////////
/////// GrammarRule Common trait
pub trait GrammarRule<'a> {
    fn name(&self) -> &str;

    fn generals(&self) -> &[Vec<& Self>] where Self: Sized;
}

impl PartialEq for dyn GrammarRule<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.name() == other.name()
    }
}

impl Hash for dyn GrammarRule<'a> {
    fn hash<H: Hasher>(&self, rule: &mut H) {
        rule.write(self.name().as_bytes())
    }
}

impl fmt::Display for dyn GrammarRule<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name())
    }
}

////////////////////////////////////////////////////////////////////////////////
/////// Lexer
pub struct Lexer<'a> {
    pub name: &'a str,
}





////////////////////////////////////////////////////////////////////////////////
/////// LL Grammar Rules



pub struct LLGrammarRule<'a> {
    pub name: &'a str,
    pub generals: Vec<Vec<LLGrammarGenerals<'a>>>,
}

impl LLGrammarRule<'a> {
    pub fn with_name(name: &'a str) -> Self {
        Self {
            name,
            generals: vec![],
        }
    }

    pub fn add_branch(&mut self, branch: Vec<LLGrammarGenerals<'a>>) {
        self.generals.push(branch)
    }

    fn name(&self) -> &'a str {
        self.name
    }

    fn generals(&self) -> &[Vec<LLGrammarGenerals>]
    where Self: Sized {
        &self.generals[..]
    }
}

impl fmt::Display for LLGrammarRule<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}:", self.name()).ok();
        for branch in self.generals() {
            write!(f, "  |").ok();
            for frag in branch.iter() {
                write!(f, " {}", frag).ok();
            }
            writeln!(f, "").ok();
        }
        Ok(())
    }
}

impl fmt::Debug for LLGrammarRule<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self).ok();

        Ok(())
    }
}


pub enum LLGrammarGenerals<'a> {
    Rule(&'a LLGrammarRule<'a>),
    Terminal(PriRegexMatcher<'a>)
}

impl fmt::Display for LLGrammarGenerals<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Rule(rule) => write!(f, "{}", rule.name),
            Self::Terminal(terminal)  => write!(f, "{}", terminal.name)
        }
    }
}



#[cfg(test)]
mod test {

}