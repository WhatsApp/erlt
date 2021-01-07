{-# LANGUAGE CPP #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Main (main) where

import           AST.TestHelpers
import           AST.Unmarshal (parseByteString)
import           Language.Erlang.Grammar
import qualified Language.Erlang.AST as Erlang
import qualified System.Path as Path
import           Test.Tasty
import Control.Monad (liftM)

main :: IO ()
main = do
#if BAZEL_BUILD
  rf <- Fixture.create
  let ?project = Path.relDir "external/tree-sitter-erlang"
      ?runfiles = rf
  let dirs = Fixture.absRelDir "test/corpus"
#else
  dirs <- Path.absRel <$> Erlang.getTestCorpusDir
#endif


  excludeMacrosCorpus (readCorpusFiles' dirs)
    >>= traverse (testCorpus parse)
    >>= defaultMain . tests
  where
    parse = parseByteString @Erlang.SourceFile @() tree_sitter_erlang_elp
    excludeMacrosCorpus l = liftM (filter (f "expressions") ) l
      where f p bn = p /= (Path.toString . Path.takeBaseName) bn

tests :: [TestTree] -> TestTree
tests = testGroup "tree-sitter-erlang corpus tests"
