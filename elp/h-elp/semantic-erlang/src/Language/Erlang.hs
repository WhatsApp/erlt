-- | Semantic functionality for Erlang programs.
module Language.Erlang
( Term(..)
, Language.Erlang.Grammar.tree_sitter_erlang_elp
) where

import           Data.Proxy
import qualified Language.Erlang.AST as Erlang
import qualified Language.Erlang.Tags as ErlangTags
import qualified Tags.Tagging.Precise as Tags
import qualified Language.Erlang.Grammar (tree_sitter_erlang_elp)
import qualified AST.Unmarshal as TS

newtype Term a = Term { getTerm :: Erlang.SourceFile a }

instance TS.SymbolMatching Term where
  matchedSymbols _ = TS.matchedSymbols (Proxy :: Proxy Erlang.SourceFile)
  showFailure _ = TS.showFailure (Proxy :: Proxy Erlang.SourceFile)

instance TS.Unmarshal Term where
  matchers = fmap (fmap (TS.hoist Term)) TS.matchers

instance Tags.ToTags Term where
  tags src = Tags.runTagging src . ErlangTags.tags . getTerm
