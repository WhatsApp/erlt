# Semantic support for Erlang (ELP)

This package implements `semantic` support for Erlang (ELP) using the `semantic-core` intermediate language.

## Generating AST

TODO:AZ update the following
```
cd semantic-erlang
cabal repl
λ> :seti -XOverloadedStrings
λ> :seti -XTypeApplications
λ> import Source.Span
λ> import Source.Range
λ> import AST.Unmarshal
λ> TS.parseByteString @Language.Erlang.AST.SourceFile @(Source.Span.Span, Source.Range.Range) Language.Erlang.Grammar.tree_sitter_erlang_elp "-module (foo)."
Right (SourceFile {ann = (Span {start = Pos {line = 0, column = 0}, end = Pos {line = 0, column = 10}},Range {start = 0, end = 10}), extraChildren = [L1 (DeclarationStatement {getDeclarationStatement = R1 (L1 (L1 (R1 (LetDeclaration {ann = (Span {start = Pos {line = 0, column = 0}, end = Pos {line = 0, column = 10}},Range {start = 0, end = 10}), pattern = Pattern {getPattern = L1 (R1 (L1 (L1 (Identifier {ann = (Span {start = Pos {line = 0, column = 4}, end = Pos {line = 0, column = 5}},Range
{start = 4, end = 5}), text = "x"}))))}, value = Just (Expression {getExpression = L1 (L1 (L1 (L1 (L1 (Literal {getLiteral = R1 (L1 (IntegerLiteral {ann = (Span {start = Pos {line = 0, column = 8}, end = Pos {line = 0, column = 9}},Range {start = 8, end = 9}), text = "1"}))})))))}), type' = Nothing, extraChildren = Nothing}))))})]})
```
