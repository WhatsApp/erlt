{- # LANGUAGE OverloadedStrings # -}
{-# LANGUAGE RecordWildCards #-}
module Main where

import           TreeSitter.Parser
import           TreeSitter.Tree
-- import           TreeSitter.Language
import           TreeSitter.ErlangELP
import           TreeSitter.Node
import           Foreign.C.String
-- import           Foreign.C.Types
import           Foreign.Ptr
import           Foreign.Marshal.Alloc
-- import           Foreign.Marshal.Array          ( mallocArray )
import           Foreign.Storable
-- import           Foreign.Marshal.Utils          ( new )
import           Control.Monad


main :: IO ()
main = do
  print "start"
  -- v <- ts_language_version tree_sitter_erlang_elp
  -- print $ "language version:" ++ show v
 
  parser <- ts_parser_new
  print $ "got parser:" ++ show parser
  r <- ts_parser_set_language parser tree_sitter_erlang_elp
  print $ "set language:" ++ show (r, tree_sitter_erlang_elp)

  let source =
        "-module(foo).\n."

-- tree-sitter parse -- foo.elp
-- (source_file [0, 0] - [2, 0]
--   (module_attribute [0, 0] - [0, 13]
--     (atom [0, 8] - [0, 11])))

  (str, len) <- newCStringLen source
  print "newCStringLen"
  tree       <- ts_parser_parse_string parser nullPtr str len
  print $ "got tree:" ++ show tree

  n          <- malloc
  print "after malloc"
  ts_tree_root_node_p tree n
  print $ "after ts_tree_root_node_p"

  print "module (root) ------------"
  Node {..} <- peek n
  let childCount :: Integer
      childCount = fromIntegral nodeChildCount
  putStrLn $ "childCount=" ++ show childCount

  -- children <- mallocArray childCount
  -- tsNode   <- malloc
  -- poke tsNode nodeTSNode
  -- ts_node_copy_child_nodes tsNode children

  -- printChildren children childCount

  -- print "where ------------"
  -- n@Node {..} <- peekElemOff children 3
  -- let nextChildCount = fromIntegral nodeChildCount

  -- nextChildren <- mallocArray nextChildCount
  -- nextTsNode   <- malloc
  -- poke nextTsNode nodeTSNode
  -- ts_node_copy_child_nodes nextTsNode nextChildren

  -- printChildren nextChildren nextChildCount

  print "END"

printChildren :: Ptr Node -> Int -> IO ()
printChildren children count = forM_
  [0 .. count - 1]
  (\n -> do
    child <- peekElemOff children n
    printNode child
  )

printNode :: Node -> IO ()
printNode node@Node {..} = do
  theType <- peekCString nodeType
  let TSPoint {..} = nodeStartPoint node
      start        = "(" ++ show pointRow ++ "," ++ show pointColumn ++ ")"
  let TSPoint {..} = nodeEndPoint
      end          = "(" ++ show pointRow ++ "," ++ show pointColumn ++ ")"
  print $ theType ++ start ++ "-" ++ end
