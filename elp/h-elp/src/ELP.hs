{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module ELP where

import Control.Monad
import Data.Hashable
import qualified Data.Map as Map
import Data.Rope.UTF16 (Rope)
import qualified Data.Rope.UTF16 as Rope
import qualified Data.Text as T
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array (mallocArray)
import Foreign.Ptr
import Foreign.Storable
import GHC.Word
import Language.Haskell.LSP.Test
import Language.LSP.Types
import Language.LSP.Types.Lens
import Language.LSP.VFS
import System.Directory
import System.FilePath
import System.IO
import System.IO.Temp
import System.Log.Logger
import TreeSitter.ErlangELP
import TreeSitter.Node
import TreeSitter.Parser
import TreeSitter.Tree

-- ---------------------------------------------------------------------

-- | Print st stderr, where the haskell tree-sitter logging goes
-- too. Convenient, it does not get in the way of tasty output either.
pp :: String -> IO ()
pp = hPutStrLn stderr

-- TODO: free the parser, or bracket it with a free
treeSitterParser :: IO (Ptr Parser)
treeSitterParser = do
  parser <- ts_parser_new
  pp $ "got parser:" ++ show parser
  timeout <- ts_parser_timeout_micros parser
  pp $ "timeout:" ++ show timeout

  ts_parser_set_timeout_micros parser 3_000_0000

  timeout <- ts_parser_timeout_micros parser
  pp $ "timeout:" ++ show timeout

  r <- ts_parser_set_language parser tree_sitter_erlang_elp
  pp $ "set language:" ++ show (r, tree_sitter_erlang_elp)
  return parser

treeSitterParseFull :: Ptr Parser -> String -> IO (Ptr Tree, Node)
treeSitterParseFull parser source = do
  (str, len) <- newCStringLen source
  tree <- ts_parser_parse_string parser nullPtr str len
  -- pp $ "got tree:" ++ show tree
  n <- malloc
  ts_tree_root_node_p tree n
  -- pp $ "after ts_tree_root_node_p"
  nn <- peek n
  -- pp $ "Node:" ++ show nn
  return (tree, nn)

treeSitterParseEdit :: TSInputEdit -> Ptr Tree -> IO (Ptr Tree)
treeSitterParseEdit edit tree = do
  inp <- malloc
  poke inp edit
  ts_tree_edit tree inp
  return tree

-- Based on Language.LSP.VFS.applyChange.
-- TODO: move this somewhere meaningful
lspChangeAsTSInputEdit :: Rope -> TextDocumentContentChangeEvent -> TSInputEdit
lspChangeAsTSInputEdit rope change = edit
  where
    edit = case change of
      (TextDocumentContentChangeEvent Nothing Nothing txt) ->
        -- replace the entire string
        let
         in TSInputEdit
              { editStartByte = 0,
                editOldEndByte = fromIntegral $ Rope.length rope,
                editNewEndByte = fromIntegral $ T.length txt,
                editStartPoint = TSPoint 0 0,
                editOldEndPoint = TSPoint (fromIntegral $ Rope.rows rope) 0,
                -- probably have to use something other than hard coded
                -- col of zero. First approximation.
                editNewEndPoint = TSPoint (fromIntegral $ Rope.rows (Rope.fromText txt)) 0
                -- If we integrate this with the VFS change processing,
                -- the Rope.fromText will not be wasted.
              }
      (TextDocumentContentChangeEvent (Just (Range from to)) _ txt) ->
        -- replace a portion with a new string.  The length parameter
        -- is optional and deprecated
        let startByte = byteAddress rope from
            endByte = byteAddress rope to
            startPoint = position2TSPoint from
         in TSInputEdit
              { editStartByte = startByte,
                editOldEndByte = endByte,
                editNewEndByte = startByte + fromIntegral (T.length txt),
                editStartPoint = startPoint,
                editOldEndPoint = position2TSPoint to,
                -- probably have to use something other than hard coded
                -- col of zero. First approximation.
                editNewEndPoint = addText startPoint txt
                -- If we integrate this with the VFS change processing,
                -- the Rope.fromText will not be wasted.
              }
      (TextDocumentContentChangeEvent Nothing (Just _) _txt) -> undefined

    byteAddress :: Rope -> Position -> Word32
    byteAddress rope (Position line col) =
      fromIntegral $ Rope.rowColumnCodeUnits (Rope.RowColumn line col) rope

    position2TSPoint :: Position -> TSPoint
    position2TSPoint (Position line col) = TSPoint (fromIntegral line) (fromIntegral col)

    addText :: TSPoint -> T.Text -> TSPoint
    addText (TSPoint row col) txt = TSPoint row' col'
      where
        rope = Rope.fromText txt
        newRows = Rope.rows rope
        lastCol = Rope.length $ snd $ Rope.splitAtLine newRows rope
        (row', col') = case newRows of
          0 ->
            ( row,
              col + fromIntegral lastCol
            )
          r ->
            ( row + fromIntegral r,
              fromIntegral lastCol
            )

treeSitterParseIncrement :: Ptr Parser -> Rope -> Ptr Tree -> IO (Ptr Tree, Node)
treeSitterParseIncrement parser rope tree = do
  -- let source2 =
  --       -- "-moable(foo).\n-x(y).\n"
  --       "-abcdef(foo).\n-xxxx(yyy).\n"
  let source2 = Rope.toString rope
  (str2, _len2) <- newCStringLen source2

  fp <- mkReadFunction vfsReadFunction
  let i =
        TSHInput
          { inputPayload = castPtr str2,
            inputRead = fp,
            inputEncoding = TSInputEncodingUTF8
          }
  pp $ "input:" ++ show i
  ip <- malloc
  poke ip i
  ts_parser_log_to_stderr parser
  -- tree2       <- ts_parser_parse_string parser tree str2 len2
  pp $ "invoking ts_parser_parse_p1"
  tree <- ts_parser_parse_p1 parser tree ip
  pp $ "got tree:" ++ show tree
  n <- malloc
  ts_tree_root_node_p tree n
  -- pp $ "after ts_tree_root_node_p"
  nn <- peek n
  -- pp $ "Node:" ++ show nn
  return (tree, nn)

-- | Tree Sitter 'ReadFunction' using a VFS as the payload
vfsReadFunction :: ReadFunction
vfsReadFunction ppayload offset ppos lenPtr = do
  pp $ "******vfsReadFunction entered"
  pp $ "******vfsReadFunction ppayload=" ++ show (ppayload, offset, ppos)
  -- type ReadFunction = (Ptr Payload -> Word32 -> Ptr TSPoint -> Ptr Word32 -> IO CString)
  tin <- peek ppayload
  let pstr = plusPtr (inputPayload tin) (fromIntegral offset)
  str <- peekCString pstr
  hPutStrLn stderr $ "******vfsReadFunction str=[" ++ show str ++ "]"
  len <- withCStringLen str $ \(_s, l) -> return l
  poke lenPtr (fromIntegral len)
  -- pp $ "******vfsReadFunction ending=" ++ show (pstr, len)
  return pstr

nodeAsSexpr :: Node -> IO String
nodeAsSexpr nn = do
  let tsn = nodeTSNode nn
  pp $ "tsn:" ++ show tsn
  tsnp <- malloc
  poke tsnp tsn
  pp $ "after poke"
  strp <- ts_node_string_p tsnp
  pp $ "after poke 2"
  str <- peekCString strp
  free strp
  pp $ "after poke"
  pp $ "node sexpr:" ++ str
  return str
