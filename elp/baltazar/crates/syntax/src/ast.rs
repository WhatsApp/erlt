use std::marker::PhantomData;

use crate::syntax::{SyntaxNode, SyntaxKind, SyntaxNodeFieldChildren};

pub use crate::generated::nodes::*;

pub trait AstNode {
    fn can_cast(syntax: SyntaxKind) -> bool
    where
        Self: Sized;

    fn cast(syntax: SyntaxNode) -> Option<Self>
    where
        Self: Sized;

    fn syntax(&self) -> &SyntaxNode;
}

#[derive(Debug, Clone)]
pub struct AstChildren<N> {
    inner: SyntaxNodeFieldChildren,
    ph: PhantomData<N>,
}

impl<N> AstChildren<N> {
    fn new(parent: &SyntaxNode, field_id: u16) -> Self {
        AstChildren { inner: parent.field_children(field_id), ph: PhantomData }
    }
}

impl<N: AstNode> Iterator for AstChildren<N> {
    type Item = N;

    fn next(&mut self) -> Option<N> {
        self.inner.find_map(N::cast)
    }
}

pub(crate) mod support {
    use super::{AstNode, AstChildren, SyntaxNode};

    pub(crate) fn child<N: AstNode>(parent: &SyntaxNode, field_id: u16) -> Option<N> {
        parent.field_children(field_id).find_map(N::cast)
    }

    pub(crate) fn children<N: AstNode>(parent: &SyntaxNode, field_id: u16) -> AstChildren<N> {
        AstChildren::new(parent, field_id)
    }
}
