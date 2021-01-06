use crate::syntax::SyntaxNode;
pub trait AstNode {
    fn cast(syntax: SyntaxNode) -> Option<Self>
    where Self: Sized;

    fn syntax(&self) -> &SyntaxNode;
}
