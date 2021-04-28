use std::cell::RefCell;
use std::hash::{Hash};
use std::{collections::HashSet, rc::Rc};

pub type CounterType = impl FnMut() -> usize;

pub fn gen_counter() -> CounterType {
    _gen_counter(0)
}

fn _gen_counter(init: usize) -> CounterType {
    let mut count = init;

    move || {
        let old_count = count;
        count += 1;
        old_count
    }
}

#[inline]
pub fn yes<T>(_: T) -> bool {
    true
}

#[inline]
pub fn no<T>(_: T) -> bool {
    false
}

/// A Tree Walker means:
/// 1. there are no circle.
/// 2. self contains
pub trait TreeWalker {
    fn get_childern(&self) -> Box<dyn Iterator<Item = Rc<&'static Self>>>;

    /// pred: used for cut branch
    /// pred maybe `|x| true`
    /// walk don't contains itself
    fn dfs_walk(
        &self,
        pred: &impl Fn(&Self) -> bool,
    ) -> Box<dyn Iterator<Item = Rc<&'static Self>>> {
        let mut walk_stack = Vec::<Rc<&Self>>::new();
        let mut res = Vec::<Rc<&Self>>::new(); // 暂时自我包含

        // walk_stack.push(Rc::new(self));
        walk_stack.extend(Self::get_childern(self));

        while !walk_stack.is_empty() {
            let cur_node = walk_stack.pop().unwrap();

            if !pred(&cur_node) {
                continue;
            } // 执行剪枝操作
            res.push(Rc::clone(&cur_node));

            walk_stack.extend(Self::get_childern(&cur_node));
        }

        Box::new(res.into_iter())
    }
}

pub trait GraphWalker: Eq + Hash {
    fn get_childern(&self) -> Box<dyn Iterator<Item = Rc<RefCell<Self>>>>;

    fn get_id(&self) -> u128;

    /// pred: used for cut branch
    /// pred maybe `|x| true`
    /// walk don't contains itself
    fn dfs_walk(&self, pred: &impl Fn(Rc<RefCell<Self>>) -> bool) -> Vec<Rc<RefCell<Self>>> {
        let mut walk_stack = Vec::<Rc<RefCell<Self>>>::new();
        let mut visited_nodes = HashSet::<u128>::new();
        let mut res = Vec::<Rc<RefCell<Self>>>::new();

        // walk_stack.push(Rc::new(self));
        walk_stack.extend(Self::get_childern(self));

        while !walk_stack.is_empty() {
            let cur_node = walk_stack.pop().unwrap();
            let cur_node_id = (*cur_node).borrow().get_id();

            // 执行剪枝操作
            if visited_nodes.contains(&cur_node_id) || !pred(Rc::clone(&cur_node)) {
                continue;
            }

            visited_nodes.insert(cur_node_id);
            res.push(Rc::clone(&cur_node));

            walk_stack.extend((*cur_node).borrow().get_childern());
        }

        res.into_iter().collect()
    }
}

mod test {
    #[test]
    fn test_counter() {
        use super::{_gen_counter, gen_counter};

        let mut c0counter = _gen_counter(0);

        assert_eq!(c0counter(), 0);
        assert_eq!(c0counter(), 1);
        assert_eq!(c0counter(), 2);

        let mut c5counter = _gen_counter(5);
        assert_eq!(c5counter(), 5);
        assert_eq!(c5counter(), 6);

        let mut counter = gen_counter();
        assert_eq!(counter(), 0);
        assert_eq!(counter(), 1);
    }

    #[test]
    fn test_rc_set() {
        use std::{collections::HashSet, rc::Rc};

        #[derive(PartialEq, Eq, Hash)]
        struct Sample {
            id: usize,
        }

        let mut set = hashset! {};

        set.insert(Rc::new(&Sample { id: 1 }));

        assert!(set.contains(&Rc::new(&Sample { id: 1 })))
    }
}
