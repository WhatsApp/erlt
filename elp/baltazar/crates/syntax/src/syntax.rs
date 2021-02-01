use std::{fmt, iter::FusedIterator, mem, rc::Rc};

use tree_sitter::{Node, Tree, TreeCursor};

pub use crate::generated::syntax_kind::SyntaxKind;

#[derive(Debug, Clone)]
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

    pub fn kind(&self) -> SyntaxKind {
        // TODO: proper conversion
        unsafe { mem::transmute(self.1.kind_id()) }
    }

    pub fn field_children(&self, field_id: u16) -> SyntaxNodeFieldChildren {
        let mut cursor = self.1.walk();
        let done = cursor.goto_first_child();
        SyntaxNodeFieldChildren { tree: self.0.clone(), field_id, done, raw: cursor }
    }

    // fn node<'a>(&'a self) -> Node<'a> {
    //     self.1
    // }
}

pub struct SyntaxNodeFieldChildren {
    tree: Rc<Tree>,
    field_id: u16,
    done: bool,
    raw: TreeCursor<'static>,
}

impl Clone for SyntaxNodeFieldChildren {
    fn clone(&self) -> Self {
        SyntaxNodeFieldChildren {
            tree: self.tree.clone(),
            field_id: self.field_id,
            done: self.done,
            raw: self.raw.node().walk(),
        }
    }
}

impl fmt::Debug for SyntaxNodeFieldChildren {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SyntaxNodeFieldChildren")
            .field("parent", &self.raw.node().parent())
            .field("field_id", &self.field_id)
            .finish()
    }
}

impl Iterator for SyntaxNodeFieldChildren {
    type Item = SyntaxNode;
    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }

        while self.raw.field_id() != Some(self.field_id) {
            if !self.raw.goto_next_sibling() {
                return None;
            }
        }

        let node = self.raw.node();
        self.done = self.raw.goto_next_sibling();

        Some(SyntaxNode::new(self.tree.clone(), node))
    }
}

impl FusedIterator for SyntaxNodeFieldChildren {}
