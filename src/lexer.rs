use std::{
    fmt,
    vec,
};

use indexmap::{indexmap, IndexMap};

use super::regex::*;
use super::algs::gram::*;

////////////////////////////////////////////////////////////////////////////////
/////// Lexer

/// Token
#[derive(Debug, Clone)]
pub struct Token {
    name: String,
    value: String,
}

impl Token {
    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn value(&self) -> &str {
        &self.value
    }

    pub fn to_fst_set_sym(&self) -> FstSetSym {
        FstSetSym::Sym(self.name.clone())
    }

    /// To GramSym::Terminal
    pub fn to_gram_sym(&self) -> GramSym {
        GramSym::Terminal(self.name.clone())
    }

    pub fn to_pred_set_sym(&self) -> PredSetSym {
        PredSetSym::Sym(self.name.clone())
    }

    pub fn to_foll_set_sym(&self) -> FollSetSym {
        FollSetSym::Sym(self.name.clone())
    }
}


impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.to_gram_sym(), self.value())
    }
}

/// Channel (Token Stream)
pub struct TokenChannel {
    pub name: String,
    pub tokens: Vec<Token>,
}

impl TokenChannel {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            tokens: vec![]
        }
    }
}

/// PreProcessor
/// 一个PreProcessor对应一个Channel
pub trait PreProcessor {
    fn name(&self) -> &str;
    fn process(&self, source: Vec<char>) -> (Vec<char>, TokenChannel);
}

/// first match if multie exists
pub fn comment_partial_match(matcher: &Box<dyn TokenMatcher>, chars: &[char]) -> Option<(usize, usize)> {
    let charslen = chars.len();
    let mut unmatched = true;
    let mut left = 0;
    let mut right = 0;
    // Brute Force Match
    let mut i =0;
    while i < charslen {
        if chars[i] != '/' && chars[i] != ';' && chars[i] != '#'
        { i +=1 ;continue; }

        for j in i+1..charslen+1 {
            let poss = &chars[i..j];
            let poss_str:String = poss.into_iter().cloned().collect();
            let match_res = matcher.is_match(&poss_str);

            if unmatched&& match_res {
                unmatched=false;
                left=i;
                right=j;
            } else if !unmatched&&match_res{
                if right < j {
                    right = j;
                } else {
                    break;
                }
            }
        }

        if unmatched {
            i += 1;
        } else {
            break;
        }
    }

    if unmatched{return None}

    return Some((left,right));
}

pub fn replace_with_samelenspace(chars: &Vec<char>, range: (usize, usize)) -> Vec<char> {
    let mut new_chars = vec![];
    let (l, r) = range;
    for (i, c) in chars.iter().enumerate() {
        if l <= i && i < r {
            new_chars.push(' ');
        } else {
            new_chars.push(c.clone());
        }
    }
    new_chars
}

#[derive(Debug)]
pub struct CommentPreProcessor {
    pub name: String,
    pub tokens_map: IndexMap<String, Box<dyn TokenMatcher>>,
}

impl CommentPreProcessor {
    pub fn new(name: &str, matchers: Vec<Box<dyn TokenMatcher>>) -> Self {
        let tokens_map = matchers
            .into_iter()
            .map(|x| (x.name().to_string(), x))
            .collect();

        let name = name.to_string();

        Self { name, tokens_map }
    }
}

impl PreProcessor for CommentPreProcessor {
    fn name(&self) -> &str {
        &self.name
    }

    /// return new source string
    /// 最后有需要根据token span 对token重新排序，现在是每个处理器处理一遍，顺序插入
    // 这里处理comment方法低效到不可接受，如果能够知道comment格式，会好
    fn process(&self, source: Vec<char>) -> (Vec<char>, TokenChannel) {
        let channel_name = "comment";
        let mut channel = TokenChannel::new(channel_name);

        if source.len() == 0 {
            return (vec![], channel);
        }

        let mut tokens = vec![];
        let mut new_src = vec![];
        let mut part_new_src = &source[..];
        let mut some_replace_happends = false;
        for (token_type, matcher) in self.tokens_map.iter() {
            loop {
                if let Some((l, r)) = comment_partial_match(matcher, part_new_src) {
                    let value: String = part_new_src[l..r].iter().collect();
                    tokens.push(
                        Token {
                            name: token_type.clone(),
                            value
                        });

                    new_src.extend(part_new_src[..l].into_iter());
                    (l..r).for_each(|_| new_src.push(' '));

                    part_new_src = &part_new_src[r..];
                    some_replace_happends = true;
                } else {
                    break;
                }
            }

        }

        channel.tokens = tokens;
        if some_replace_happends {
            (new_src, channel)
        } else {
            (source, channel)
        }

    }
}


/// Lexer
pub struct Lexer {
    pub name: String,

    // epsilon 不参与
    // IndexMap: 顺序决定优先级
    pub tokens_map: IndexMap<String, Box<dyn TokenMatcher>>,
    pub preprocessors: Vec<Box<dyn PreProcessor>>
}


impl Lexer {
    pub fn new(
        name: &str,
        matchers: Vec<Box<dyn TokenMatcher>>,
        preprocessors: Vec<Box<dyn PreProcessor>>
    ) -> Self
    {
        let tokens_map = matchers
            .into_iter()
            .map(|x| (x.name().to_string(), x))
            .collect();

        let name = name.to_string();

        Self { name, tokens_map, preprocessors }
    }

    pub fn tokenize(&self, source: &str) -> IndexMap<String, TokenChannel> {

        let default_channel = TokenChannel::new("default");
        let mut channels = indexmap! {
            default_channel.name.clone() => default_channel
        };

        if source.len() == 0 {
            return channels;
        }

        let mut tokens = vec![];

        let mut old_src_chars = source.chars().collect();
        // 这点儿破代码罗里罗嗦写一天了，写着写着需求越来越多，我真tm写烦了，随便写垃圾代码凑和
        // 我就是想测一下这个LL(1)改的 Parser能到什么程度，明明就剩个comment， 内容少框架大
        // 这是真的烦，！！！！！草！！！！
        // preprocess
        for preprocor in self.preprocessors.iter() {
            println!("{} preprocess...", preprocor.name());
            let (source_chars, channel)
                = preprocor.process(old_src_chars);

            old_src_chars = source_chars;
            channels.insert(channel.name.clone(), channel);
        }

        let source_chars = old_src_chars;


        let mut cache = String::new();
        let mut i = 0;

        while i < source_chars.len() {
            cache.push(source_chars[i]);

            // turn round matcher to try match
            for (order, (token_type, matcher)) in self.tokens_map.iter().enumerate() {
                let mut j = i;
                while j < source_chars.len() - 1 && matcher.is_match(cache.trim()) {
                    j += 1;
                    cache.push(source_chars[j]);
                }

                // 找到一个token匹配
                if j > i {
                    // 退回一个贪心多吞噬的字符
                    // case 1: token stream exausted: token match or unmatch
                    // case 2: token stream unexauted: token unmatch
                    // for token unmatch
                    if !matcher.is_match(cache.trim()) {
                        j -= 1;
                        cache.pop();
                    }

                    // 查看是否有更高优先级的matcher可以匹配到token, 如果有就放弃这个匹配
                    // 这主要是为了让关键字： `if`，`return` 匹配优先于标识符id的匹配
                    if self.tokens_map.iter()
                        .enumerate()
                        .filter(|(m_order, _)| *m_order < order)
                        .any(|(_, (_, m, ))| m.is_match(cache.trim()))
                    {
                        // restore cache
                        (i..j).into_iter().for_each(|_| { cache.pop(); });
                        continue;
                    }

                    tokens.push(Token {
                        name: token_type.clone(),
                        value: cache.trim().to_string(),
                    });
                    cache = String::new();
                    i = j;

                    break;
                }
            }

            i += 1;
        }

        // tail recycle
        for (token_type, matcher) in self.tokens_map.iter() {
            if matcher.is_match(cache.trim()) {
                tokens.push(Token {
                    name: token_type.clone(),
                    value: cache.trim().to_string(),
                });
            }
        }

        channels.get_index_mut(0).unwrap().1.tokens = tokens;

        channels
    }
}

pub fn basic_lexer() -> Lexer {
    let matchers = vec![
        intlit_m(),
        id_m(),
        lparen_m(),
        rparen_m(),
        add_m(),
        sub_m(),
        mul_m(),
        div_m(),
    ];

    Lexer::new("basic lexer", matchers, vec![])
}