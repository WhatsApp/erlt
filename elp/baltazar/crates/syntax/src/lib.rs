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
    fn function_clause_ast() {
        let mut parser = Parser::new();
        let text = "foo(1) -> 2, 3.";
        let file = parser.parse(text);

        assert_eq!(file.forms().count(), 1);
        if let Form::Function(function) = file.forms().nth(0).unwrap() {
            assert_eq!(function.clauses().count(), 1);
            let clause = function.clauses().nth(0).unwrap();

            assert!(clause.arg_list().is_some());
            assert_eq!(clause.arg_list().unwrap().args().count(), 1);

            let arg = clause.arg_list().unwrap().args().nth(0).unwrap();
            if let Expr::Integer(integer) = arg {
                assert_eq!(&text[integer.syntax().byte_range()], "1");
            } else {
                assert!(matches!(arg, Expr::Integer(_)));
            }

            assert!(clause.body().is_some());
            let body = clause.body().unwrap();
            assert_eq!(body.exprs().count(), 2);
            assert!(matches!(body.exprs().nth(0).unwrap(), Expr::Integer(_)));
            assert!(matches!(body.exprs().nth(1).unwrap(), Expr::Integer(_)));
        } else {
            assert!(matches!(file.forms().nth(0), Some(Form::Function(_))));
        }
    }
}
