use std::rc::Rc;

use ast::{AstNode, SourceFile};
use syntax::SyntaxNode;

pub mod ast;
pub mod syntax;
mod generated;

pub struct Parser(tree_sitter::Parser);

impl Parser {
    pub fn new() -> Self {
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(tree_sitter_erlang::language()).expect("incompatible tree-sitter");
        Parser(parser)
    }

    pub fn parse(&mut self, text: &str) -> SourceFile {
        let tree = self.0.parse(text, None).unwrap();
        SourceFile::cast(SyntaxNode::root(Rc::new(tree))).unwrap()
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::*;

    use super::Parser;

    #[test]
    fn basic_ast() {
        let mut parser = Parser::new();
        let file = parser.parse("foo(1) -> 2, 3.");

        assert_eq!(file.forms().count(), 1);
        assert!(matches!(file.forms().nth(0), Some(Form::Function(_))))
    }
}
