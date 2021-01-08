# Semantic support for Erlang (ELP)

This package implements `semantic` support for Erlang (ELP) using the `semantic-core` intermediate language.

## Generating AST

```
cd semantic-erlang
cabal repl
λ> :seti -XOverloadedStrings
λ> :seti -XTypeApplications
λ> import Source.Span
λ> import Source.Range
λ> import AST.Unmarshal
λ> TS.parseByteString @Language.Erlang.AST.SourceFile @(Source.Span.Span, Source.Range.Range) Language.Erlang.Grammar.tree_sitter_erlang_elp "-module (foo)."

Right (SourceFile {ann = (Span {start = Pos {line = 0, column = 0}, end = Pos {line = 0, column = 14}},Range {start = 0, end = 14}), extraChildren = [R1 (R1 (ModuleAttribute {ann = (Span {start = Pos {line = 0, column = 0}, end = Pos {line = 0, column = 14}},Range {start = 0, end = 14}), extraChildren = Atom {ann = (Span {start = Pos {line = 0, column = 9}, end = Pos {line = 0, column = 12}},Range {start = 9, end = 12}), text = "foo"}}))]})
```

A reformatting of the result gives

```haskell
Right
  (SourceFile
    { ann = ( Span { start = Pos {line = 0, column = 0}
                   , end = Pos {line = 0, column = 14}}
            , Range {start = 0, end = 14})
    , extraChildren =
       [R1 (R1 (ModuleAttribute
                 { ann = ( Span { start = Pos {line = 0, column = 0}
                                , end = Pos {line = 0, column = 14}}
                         , Range {start = 0, end = 14})
                 , extraChildren = Atom { ann = ( Span { start = Pos {line = 0, column = 9}
                                                       , end = Pos {line = 0, column = 12}}
                                                , Range {start = 9, end = 12})
                                        , text = "foo"}}))]})
```
