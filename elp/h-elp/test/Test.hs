{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}

import           Control.Monad
import           Data.Hashable
import           Data.Rope.UTF16 ( Rope )
import qualified Data.Rope.UTF16 as Rope
import qualified Data.Text as T
import           Foreign.C.String
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array          ( mallocArray )
import           Foreign.Ptr
import           Foreign.Storable
import           Language.Haskell.LSP.Test
import           Language.LSP.Types           as J
import           Language.LSP.Types.Lens      as J
import           Language.LSP.VFS
import           System.Directory
import           System.FilePath
import           System.IO
import           System.IO.Temp
import           System.Log.Logger
import           TreeSitter.ErlangELP
import           TreeSitter.Node
import           TreeSitter.Parser
import           TreeSitter.Tree

import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ testCase "2+2=4" $
      2+2 @?= 4
  , testCase "7 is odd" $
      assertBool "Oops, 7 is even" (odd 7)
  , testVFS
  ]

testVFS :: TestTree
testVFS = testGroup "VFS"
  [ testCase "VFS" $ do
      initVFS $ \vfs -> do
        let
          aContent = T.unlines
               [ "-module(foo)."
               , "-xxxx(yyy)."
               ]
          itemA = J.TextDocumentItem uriA "erlang" 0 aContent
          uriA = filePathToUri "A/A.hs"
          a = J.TextDocumentIdentifier uriA
          msg = NotificationMessage "2.0" STextDocumentDidOpen (DidOpenTextDocumentParams itemA)
        let r = openVFS vfs msg
        parser <- treeSitterParser
        (tree, node) <- treeSitterParseFull parser (T.unpack aContent)
        sexp <- nodeAsSexpr node
        sexp @?= "(source_file (module_attribute (atom)) (attribute (atom) (atom)))"

        tree2 <- treeSitterParseEdit () tree
        (tree3,node2) <- treeSitterParseIncrement parser () tree2
        sexp2 <- nodeAsSexpr node
        sexp2 @?= "(source_file (module_attribute (atom)) (attribute (atom) (atom)))"

        assertBool "show me the logs!" False
        free parser
        return ()
  ]

{-

data Node = Node
  { nodeTSNode     :: !TSNode
  , nodeType       :: !CString
  , nodeSymbol     :: !TSSymbol
  , nodeEndPoint   :: !TSPoint
  , nodeEndByte    :: !Word32
  , nodeChildCount :: !Word32
  , nodeFieldName  :: !CString
  , nodeIsNamed    :: !CBool
  , nodeIsExtra    :: !CBool
  }
-}

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

-- treeSitterParseFull :: Ptr Parser -> String -> IO (Ptr Tree)
treeSitterParseFull :: Ptr Parser -> String -> IO (Ptr Tree, Node)
treeSitterParseFull parser source = do
  (str, len) <- newCStringLen source
  tree       <- ts_parser_parse_string parser nullPtr str len
  -- pp $ "got tree:" ++ show tree
  n          <- malloc
  ts_tree_root_node_p tree n
  -- pp $ "after ts_tree_root_node_p"
  nn <- peek n
  -- pp $ "Node:" ++ show nn
  return (tree, nn)

treeSitterParseEdit :: p -> Ptr Tree -> IO (Ptr Tree)
treeSitterParseEdit edit tree = do

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
  poke inp edit2
  ts_tree_edit tree inp
  return tree


treeSitterParseIncrement :: Ptr Parser -> p -> Ptr Tree -> IO (Ptr Tree, Node)
treeSitterParseIncrement parser edit tree = do

  let source2 =
        -- "-moable(foo).\n-x(y).\n"
        "-moable(foo).\n-x(y).\n"
  (str2, _len2) <- newCStringLen source2

  fp <- mkReadFunction vfsReadFunction
  let i = TSHInput
            { inputPayload = castPtr str2
            , inputRead = fp
            , inputEncoding = TSInputEncodingUTF8
            }
  pp $ "input:" ++ show i
  ip <- malloc
  poke ip i
  ts_parser_log_to_stderr parser
  -- tree2       <- ts_parser_parse_string parser tree str2 len2
  pp $ "invoking ts_parser_parse_p1"
  tree       <- ts_parser_parse_p1 parser tree ip
  pp $ "got tree:" ++ show tree
  n          <- malloc
  ts_tree_root_node_p tree n
  -- pp $ "after ts_tree_root_node_p"
  nn <- peek n
  -- pp $ "Node:" ++ show nn
  return (tree, nn)


-- | Tree Sitter 'ReadFunction' using a VFS as the payload
vfsReadFunction :: ReadFunction
vfsReadFunction ppayload offset ppos lenPtr = do
  pp $ "******iread entered"
  pp $ "******iread ppayload=" ++ show (ppayload, offset, ppos)
  -- type ReadFunction = (Ptr Payload -> Word32 -> Ptr TSPoint -> Ptr Word32 -> IO CString)
  tin <- peek ppayload
  let pstr = plusPtr (inputPayload tin) (fromIntegral offset)
  str <- peekCString pstr
  hPutStrLn stderr $ "******iread str=[" ++ show str ++ "]"
  len <- withCStringLen str $ \(_s,l) -> return l
  poke lenPtr (fromIntegral len)
  -- pp $ "******iread ending=" ++ show (pstr, len)
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
