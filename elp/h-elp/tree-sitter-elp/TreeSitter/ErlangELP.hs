module TreeSitter.ErlangELP
( tree_sitter_erlang_elp
, getNodeTypesPath
, getTestCorpusDir
) where

import Foreign.Ptr
import TreeSitter.Language
import Paths_tree_sitter_elp

foreign import ccall unsafe "vendor/tree-sitter-erlang/src/parser.c tree_sitter_erlang_elp" tree_sitter_erlang_elp :: Ptr Language

getNodeTypesPath :: IO FilePath
getNodeTypesPath = getDataFileName "vendor/tree-sitter-erlang/src/node-types.json"

getTestCorpusDir :: IO FilePath
getTestCorpusDir = getDataFileName "vendor/tree-sitter-erlang/test/corpus"
