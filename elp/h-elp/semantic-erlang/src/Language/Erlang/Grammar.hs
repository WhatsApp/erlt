{-# LANGUAGE TemplateHaskell #-}
module Language.Erlang.Grammar
( tree_sitter_erlang_elp
, Grammar(..)
) where

import AST.Grammar.TH
import Language.Haskell.TH
import TreeSitter.ErlangELP (tree_sitter_erlang_elp)

-- | Statically-known rules corresponding to symbols in the grammar.
mkStaticallyKnownRuleGrammarData (mkName "Grammar") tree_sitter_erlang_elp
