{- # LANGUAGE OverloadedStrings # -}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NumericUnderscores #-}
module Main where

import           TreeSitter.Parser
import           TreeSitter.Tree
import           TreeSitter.ErlangELP
import           TreeSitter.Node
import           Foreign.C.String
import           Foreign.Ptr
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array          ( mallocArray )
import           Foreign.Storable
import           Control.Monad
import           System.IO

main :: IO ()
main = do
  print "start"

  parser <- ts_parser_new
  print $ "got parser:" ++ show parser
  timeout <- ts_parser_timeout_micros parser
  print $ "timeout:" ++ show timeout

  ts_parser_set_timeout_micros parser 3_000_0000

  timeout <- ts_parser_timeout_micros parser
  print $ "timeout:" ++ show timeout

  r <- ts_parser_set_language parser tree_sitter_erlang_elp
  print $ "set language:" ++ show (r, tree_sitter_erlang_elp)

  let source =
        "-module(foo).\n-x(y).\n"
        -- "\n-module('foo).\n"

  (str, len) <- newCStringLen source
  tree       <- ts_parser_parse_string parser nullPtr str len
  print $ "got tree:" ++ show tree

  let edit1 = TSInputEdit
                { editStartByte   = 8
                , editOldEndByte  = 9
                , editNewEndByte  = 8
                , editStartPoint  = TSPoint 0 8
                , editOldEndPoint = TSPoint 0 9
                , editNewEndPoint = TSPoint 0 8
                }
  let edit2 = TSInputEdit
                { editStartByte   = 3
                , editOldEndByte  = 4
                , editNewEndByte  = 4
                , editStartPoint  = TSPoint 0 3
                , editOldEndPoint = TSPoint 0 4
                , editNewEndPoint = TSPoint 0 4
                }
  inp <- malloc
  poke inp edit1
  ts_tree_edit tree inp

  -- -----------------------------------
  let source2 =
        -- "-moable(foo).\n-x(y).\n"
        "-moablefoo).\n-x(y).\n"

{-
Adding one character to a line


[Trace - 03:14:54 pm] Sending notification 'textDocument/didChange'.
Params: {
  "textDocument": {
    "uri": "file:///Users/alanzimm/repos/WhatsApp/erlt/elp/h-elp/tree-sitter-elp/examples/Demo.hs",
    "version": 723
  },
  "contentChanges": [
    {
      "range": {
        "start": {
          "line": 61,
          "character": 9
        },
        "end": {
          "line": 61,
          "character": 9
        }
      },
      "rangeLength": 0,
      "text": "_"
    }
  ]
}


-}

  (str2, _len2) <- newCStringLen source2

{-
from treesitter api.h

 * The `TSInput` parameter lets you specify how to read the text. It has the
 * following three fields:
 * 1. `read`: A function to retrieve a chunk of text at a given byte offset
 *    and (row, column) position. The function should return a pointer to the
 *    text and write its length to the the `bytes_read` pointer. The parser
 *    does not take ownership of this buffer; it just borrows it until it has
 *    finished reading it. The function should write a zero value to the
 *    `bytes_read` pointer to indicate the end of the document.
-}
  let
    iread :: ReadFunction
    iread ppayload offset ppos lenPtr = do
      hPutStrLn stderr $ "******iread entered"
      hPutStrLn stderr $ "******iread ppayload=" ++ show (ppayload, offset, ppos)
      -- type ReadFunction = (Ptr Payload -> Word32 -> Ptr TSPoint -> Ptr Word32 -> IO CString)
      -- type Payload = TSHInput
      tin <- peek ppayload
      let pstr = plusPtr (inputPayload tin) (fromIntegral offset)
      str <- peekCString pstr
      hPutStrLn stderr $ "******iread str=[" ++ show str ++ "]"
      len <- withCStringLen str $ \(_s,l) -> return l
      poke lenPtr (fromIntegral len)
      -- hPutStrLn stderr $ "******iread ending=" ++ show (pstr, len)
      return pstr

  fp <- mkReadFunction iread
  let i = TSHInput
            { inputPayload = castPtr str2
            , inputRead = fp
            , inputEncoding = TSInputEncodingUTF8
            }
  print $ "input:" ++ show i
  ip <- malloc
  poke ip i
  ts_parser_log_to_stderr parser
  -- tree2       <- ts_parser_parse_string parser tree str2 len2
  print $ "invoking ts_parser_parse_p1"
  tree2       <- ts_parser_parse_p1 parser tree ip
  print $ "got tree2:" ++ show tree2
  -- -----------------------------------

  n          <- malloc
  ts_tree_root_node_p tree2 n
  print $ "after ts_tree_root_node_p"

  -- -----------------------------------
  nn <- peek n
  print $ "Node:" ++ show nn
  let tsn = nodeTSNode nn
  print $ "tsn:" ++ show tsn
  tsnp <- malloc
  poke tsnp tsn
  print $ "after poke"
  strp <- ts_node_string_p tsnp
  print $ "after poke 2"
  str <- peekCString strp
  print $ "after poke"
  print $ "node sexpr:" ++ str

  -- -----------------------------------
  print "module (root) ------------"
  Node {..} <- peek n
  let  childCount = fromIntegral nodeChildCount
  putStrLn $ "childCount=" ++ show childCount

  children <- mallocArray childCount
  tsNode   <- malloc
  poke tsNode nodeTSNode
  ts_node_copy_child_nodes tsNode children

  printChildren children childCount

  print "where ------------"
  n@Node {..} <- peekElemOff children 0
  let nextChildCount = fromIntegral nodeChildCount

  nextChildren <- mallocArray nextChildCount
  nextTsNode   <- malloc
  poke nextTsNode nodeTSNode
  ts_node_copy_child_nodes nextTsNode nextChildren

  printChildren nextChildren nextChildCount

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
