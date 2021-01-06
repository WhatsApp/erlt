use std::rc::Rc;

use tree_sitter::Node;

pub struct SyntaxNode<'tree>(Rc<Node<'tree>>);
