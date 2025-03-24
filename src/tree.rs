use itertools::Itertools;
use std::{
    cell::RefCell,
    collections::HashSet,
    fmt::Display,
    rc::{Rc, Weak},
};

#[allow(dead_code)]
#[derive(Debug)]
pub struct TreeNode {
    id: u32,
    children: Vec<Rc<RefCell<TreeNode>>>,
    parent: Option<Weak<RefCell<TreeNode>>>,
}
impl PartialEq for TreeNode {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

#[allow(dead_code)]
impl TreeNode {
    pub fn new(id: u32, parent: Option<Weak<RefCell<TreeNode>>>) -> Self {
        TreeNode {
            id,
            children: vec![],
            parent,
        }
    }

    pub fn new_child(id: u32, parent: Weak<RefCell<TreeNode>>) -> Self {
        TreeNode::new(id, Some(parent))
    }

    pub fn new_root(id: u32) -> Self {
        TreeNode::new(id, None)
    }
}

impl Display for TreeNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let parent = match &self.parent {
            None => "No Parent".to_string(),
            Some(w) => w.upgrade().clone().unwrap().borrow().id.to_string(),
        };
        let children = self
            .children
            .iter()
            .map(|c| c.borrow().id.to_string())
            .join(", ");

        write!(
            f,
            "Node: {} Parent: {} children: [{}]",
            self.id, parent, children
        )
    }
}

#[allow(dead_code)]
#[derive(Debug, PartialEq)]
pub struct Tree {
    root: Rc<RefCell<TreeNode>>,
    current_node: Rc<RefCell<TreeNode>>,
}

#[allow(dead_code)]
impl Tree {
    pub fn new(item: u32) -> Self {
        let root = Rc::new(RefCell::new(TreeNode::new(item, None)));
        let current_node = root.clone();
        Tree { root, current_node }
    }

    pub fn add_children(&mut self, qids: Vec<u32>) {
        for id in qids {
            let node = Rc::new(RefCell::new(TreeNode::new_child(
                id,
                Rc::downgrade(&self.current_node),
            )));
            self.current_node.borrow_mut().children.push(node);
        }
    }
}
#[allow(dead_code)]
struct TreeIter {
    current_node: Rc<RefCell<TreeNode>>,
    visited: HashSet<u32>,
}

impl Iterator for TreeIter {
    type Item = Rc<RefCell<TreeNode>>;

    fn next(&mut self) -> Option<Rc<RefCell<TreeNode>>> {
        let mut current_node = self.current_node.clone();
        let mut current_id = current_node.borrow().id;
        if !self.visited.contains(&current_id) {
            self.visited.insert(current_id);
            return Some(current_node);
        }
        let mut res = None;
        while res.is_none() {
            // go to my first unvisited child...
            let children;
            {
                let borrow = current_node.borrow();
                children = borrow.children.clone();
            }

            res = children
                .iter()
                .find(|c| !self.visited.contains(&c.borrow().id))
                .cloned();

            // I have not unvisted children... ask my parent...
            match &res {
                None => {
                    let parent = &current_node.borrow().parent.clone();
                    if let Some(newnode) = parent {
                        current_node = newnode
                            .upgrade()
                            .expect(&format!("Tree Dropped parent of {}", current_id));
                        current_id = current_node.borrow().id;
                    } else {
                        return None;
                    }
                }
                Some(ret) => {
                    self.current_node = ret.clone();
                    self.visited.insert(ret.borrow().id);
                }
            }
        }

        res
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn test_create_tree() {
        let mut tree = Tree::new(21);
        tree.add_children(vec![1, 2, 3]);
        assert_eq!(tree.root.borrow().children.len(), 3);

        let mut ti = TreeIter {
            current_node: tree.root.clone(),
            visited: HashSet::new(),
        };

        while let Some(tn) = ti.next() {
            println!("{}", tn.borrow());
        }
    }
}
