use std::{mem, rc::Rc};

use tree_sitter::{Node, Tree};

#[derive(Clone)]
pub struct SyntaxNode(Rc<Tree>, Node<'static>);

impl PartialEq for SyntaxNode {
    fn eq(&self, other: &Self) -> bool {
        Rc::as_ptr(&self.0) == Rc::as_ptr(&other.0) && self.1 == other.1
    }
}

impl Eq for SyntaxNode {}

impl std::hash::Hash for SyntaxNode {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Rc::as_ptr(&self.0).hash(state);
        self.1.hash(state)
    }
}

impl SyntaxNode {
    pub fn new<'a>(tree: Rc<Tree>, node: Node<'a>) -> SyntaxNode {
        // Safety: the lifetime parameter of the node relates to
        // the lifetime of the tree - since we store the two together,
        // and only hand out references to Node with the lifetime
        // limited back to that of the whole SyntaxNode reference
        // (and hence the Tree itself), it should be safe
        let node = unsafe { mem::transmute(node) };
        SyntaxNode(tree, node)
    }

    fn node<'a>(&'a self) -> Node<'a> {
        self.1
    }
}
