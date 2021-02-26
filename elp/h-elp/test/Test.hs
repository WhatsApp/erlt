{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import AST.Unmarshal
  ( TSDiagnostic (TSDError),
    UnmarshalDiagnostics (..),
    parseByteString,
  )
-- import Control.Carrier.Reader
-- import Control.Carrier.State.Strict
-- import Control.Exception
-- import Control.Monad
-- import Control.Monad.IO.Class
import Data.ByteString (ByteString)
-- import qualified Data.ByteString as B
-- import Data.Hashable
import qualified Data.Map as Map
-- import Data.Rope.UTF16 (Rope)

-- import qualified Data.Rope.UTF16.Internal as Rope

-- import GHC.Word

-- import Language.Haskell.LSP.Test

import Data.Rope.UTF16 (codeUnitsRowColumn)
import qualified Data.Rope.UTF16 as Rope
import qualified Data.Rope.UTF16.Internal.Position as Rope
import Data.SplayTree (Measured (measure))
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import ELP
  ( lspChangeAsTSInputEdit,
    nodeAsSexpr,
    pp,
    treeSitterParseEdit,
    treeSitterParseFull,
    treeSitterParseIncrement,
    treeToAstText,
  )
import Foreign.C.String
  ( newCStringLen,
    peekCString,
    withCStringLen,
  )
import Foreign.Marshal.Alloc (free, malloc)
import Foreign.Ptr (plusPtr)
import Foreign.Storable (Storable (peek, poke))
import Language.Erlang.AST (SourceFile)
import qualified Language.Erlang.AST as Erlang
import Language.Erlang.Grammar (tree_sitter_erlang_elp)
import Language.LSP.Types
  ( DidOpenTextDocumentParams (DidOpenTextDocumentParams),
    NotificationMessage (NotificationMessage),
    Position (Position),
    Range (Range),
    SMethod (STextDocumentDidOpen),
    TextDocumentContentChangeEvent (TextDocumentContentChangeEvent),
    TextDocumentIdentifier (TextDocumentIdentifier),
    TextDocumentItem (TextDocumentItem),
    filePathToUri,
    toNormalizedUri,
  )
import Language.LSP.Types.Lens ()
import Language.LSP.VFS
  ( VFS (vfsMap),
    VirtualFile (VirtualFile),
    applyChange,
    initVFS,
    openVFS,
  )
import qualified Source.Range as S
import qualified Source.Span as S
import System.Directory ()
import System.FilePath ()
import System.IO (hPutStrLn, stderr)
import System.IO.Temp ()
import System.Log.Logger ()
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TreeSitter.Cursor ()
import TreeSitter.ErlangELP ()
import TreeSitter.Node
  ( Node (nodeTSNode),
    TSPoint (..),
    ts_node_string_extra_p,
  )
import TreeSitter.Parser (ReadFunction, TSHInput (inputPayload), withParser)
import TreeSitter.Query
  ( TSQueryCapture (qcIndex, qcNode),
    TSQueryError (TSQueryErrorNone),
    TSQueryMatch (qmCaptures),
    ts_query_cursor_exec_p,
    ts_query_cursor_new,
    ts_query_cursor_next_match,
    ts_query_new,
  )
import TreeSitter.Tree
  ( TSInputEdit (..),
  )

-- ---------------------------------------------------------------------

main :: IO ()
main = defaultMain tests

logToStderr :: Bool
logToStderr = True

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ testVFS,
      testQuery,
      testAST
    ]

testVFS :: TestTree
testVFS =
  testGroup
    "VFS"
    [ testCase "VFS" $ do
        initVFS $ \vfs -> do
          let aContent =
                T.unlines
                  [ "-module(foo).",
                    "-xxxx(yyy)."
                  ]
              content2 =
                T.unlines
                  [ "-abcdef(foo).",
                    "-xxxx(yyy)."
                  ]
              content3 =
                T.unlines
                  [ "-module(foo).",
                    "-abcd(yyy)."
                  ]
              content4 =
                T.unlines
                  [ "-module(foo).",
                    "-abcd(yyy).",
                    "-g(h)."
                  ]
              itemA = TextDocumentItem uriA "erlang" 0 aContent
              uriA = filePathToUri "src/foo.erl"
              nUriA = toNormalizedUri uriA
              a = TextDocumentIdentifier uriA
              msg = NotificationMessage "2.0" STextDocumentDidOpen (DidOpenTextDocumentParams itemA)
          let (v1, _) = openVFS vfs msg
          withParser tree_sitter_erlang_elp $ \parser -> do
            -- parser <- treeSitterParser
            (tree, node) <- treeSitterParseFull parser (T.unpack aContent)
            sexp <- nodeAsSexpr node
            sexp @?= "(source_file forms: (module_attribute name: (atom)) forms: (attribute name: (atom) value: (atom)))"

            let Just (VirtualFile _ _ rope1) = Map.lookup nUriA (vfsMap v1)
            let change = TextDocumentContentChangeEvent Nothing Nothing content4
            lspChangeAsTSInputEdit (Rope.fromText "") rope1 change @?= Nothing
            (tree2,_) <- treeSitterParseFull parser (T.unpack content4)

            let rope2 = applyChange rope1 change
            (tree3, node2) <- treeSitterParseIncrement parser rope2 tree2
            sexp2 <- nodeAsSexpr node2
            sexp2 @?= "(source_file forms: (module_attribute name: (atom)) forms: (attribute name: (atom) value: (atom)) forms: (attribute name: (atom) value: (atom)))"

            let change2 =
                  TextDocumentContentChangeEvent
                    (Just (Range (Position 0 1) (Position 0 3)))
                    Nothing
                    "jj"

            let rope3 = applyChange rope1 change2
            Rope.toString rope1 @?= "-module(foo).\n-xxxx(yyy).\n"
            Rope.toString rope3 @?= "-jjdule(foo).\n-xxxx(yyy).\n"
            let Just edit2 = lspChangeAsTSInputEdit rope2 rope3 change2
            edit2
              @?= TSInputEdit
                { editStartByte = 1,
                  editOldEndByte = 3,
                  editNewEndByte = 3,
                  editStartPoint = TSPoint {pointRow = 0, pointColumn = 1},
                  editOldEndPoint = TSPoint {pointRow = 0, pointColumn = 3},
                  editNewEndPoint = TSPoint {pointRow = 0, pointColumn = 3}
                }

          -- assertBool "show me the logs!" False
          return ()
    ]

-- ---------------------------------------------------------------------
-- See https://github.com/tree-sitter/tree-sitter/blob/master/docs/section-2-using-parsers.md#the-query-api
testQuery :: TestTree
testQuery =
  testGroup
    "Query"
    [ testCase "Query no error" $ do
        initVFS $ \vfs -> do
          let aContent =
                T.unlines
                  [ "-odule(foo).",
                    "-xxxx(yyy)."
                  ]
              itemA = TextDocumentItem uriA "erlang" 0 aContent
              uriA = filePathToUri "src/foo.erl"
              nUriA = toNormalizedUri uriA
              a = TextDocumentIdentifier uriA
              msg = NotificationMessage "2.0" STextDocumentDidOpen (DidOpenTextDocumentParams itemA)
          let (v1, _) = openVFS vfs msg
          withParser tree_sitter_erlang_elp $ \parser -> do
            -- parser <- treeSitterParser
            (tree, node) <- treeSitterParseFull parser (T.unpack aContent)
            sexp <- nodeAsSexpr node
            sexp @?= "(source_file forms: (attribute name: (atom) value: (atom)) forms: (attribute name: (atom) value: (atom)))"

            -- let queryStr = "(module_attribute)"
            -- let queryStr = "(attribute)"
            let queryStr = "(attribute (atom) @a (atom) @b)"
            -- let queryStr = "(arg_list)"
            (str, len) <- newCStringLen queryStr
            e <- malloc
            r <- malloc
            query <- ts_query_new tree_sitter_erlang_elp str (fromIntegral len) e r
            ee <- peek e
            rr <- peek r
            rr @?= TSQueryErrorNone

            qc <- ts_query_cursor_new
            -- TODO: use withNode instead
            n <- malloc
            poke n (nodeTSNode node)
            ts_query_cursor_exec_p qc query n
            -- bool ts_query_cursor_next_match(TSQueryCursor *, TSQueryMatch *match);
            pmatch <- malloc
            matched <- ts_query_cursor_next_match qc pmatch
            match <- peek pmatch
            matched @?= True
            take 70 (show match) @?= "TSQueryMatch {qmId = 0, qmPatternIndex = 0, qmCaptureCount = 2, qmCapt"

            c1 <- peek (qmCaptures match)
            take 55 (show (qcNode c1)) @?= "TSNode 1 (TSPoint {pointRow = 0, pointColumn = 1}) 0 0x"
            qcIndex c1 @?= 0

            -- ---------------------------
            matched <- ts_query_cursor_next_match qc pmatch
            match <- peek pmatch
            matched @?= True
            take 70 (show match) @?= "TSQueryMatch {qmId = 1, qmPatternIndex = 0, qmCaptureCount = 2, qmCapt"

            c2 <- peek (qmCaptures match)
            take 55 (show (qcNode c2)) @?= "TSNode 14 (TSPoint {pointRow = 1, pointColumn = 1}) 0 0"
            qcIndex c2 @?= 0

          -- assertBool "show me the logs!" False
          return (),
      -- -------------------------------------------
      testCase "Query with MISSING" $ do
        initVFS $ \vfs -> do
          let aContent =
                T.unlines
                  [ "-odule(foo)",
                    "-xxxx(yyy)."
                  ]
              itemA = TextDocumentItem uriA "erlang" 0 aContent
              uriA = filePathToUri "src/foo.erl"
              nUriA = toNormalizedUri uriA
              a = TextDocumentIdentifier uriA
              msg = NotificationMessage "2.0" STextDocumentDidOpen (DidOpenTextDocumentParams itemA)
          let (v1, _) = openVFS vfs msg
          withParser tree_sitter_erlang_elp $ \parser -> do
            -- parser <- treeSitterParser
            (tree, node) <- treeSitterParseFull parser (T.unpack aContent)
            sexp <- nodeAsSexpr node
            sexp @?= "(source_file forms: (attribute name: (atom) value: (atom) (MISSING \".\")) forms: (attribute name: (atom) value: (atom)))"
            let queryStr = "(attribute (atom) @a (atom) @b)"
            -- let queryStr = "(MISSING)"
            (str, len) <- newCStringLen queryStr
            e <- malloc
            r <- malloc
            query <- ts_query_new tree_sitter_erlang_elp str (fromIntegral len) e r
            ee <- peek e
            rr <- peek r
            rr @?= TSQueryErrorNone

            qc <- ts_query_cursor_new
            -- TODO: use withNode instead
            n <- malloc
            poke n (nodeTSNode node)
            ts_query_cursor_exec_p qc query n
            -- bool ts_query_cursor_next_match(TSQueryCursor *, TSQueryMatch *match);
            pmatch <- malloc
            matched <- ts_query_cursor_next_match qc pmatch
            match <- peek pmatch
            matched @?= True
            take 70 (show match) @?= "TSQueryMatch {qmId = 0, qmPatternIndex = 0, qmCaptureCount = 2, qmCapt"

            c1 <- peek (qmCaptures match)
            take 55 (show (qcNode c1)) @?= "TSNode 1 (TSPoint {pointRow = 0, pointColumn = 1}) 0 0x"
            qcIndex c1 @?= 0

            -- ---------------------------
            matched <- ts_query_cursor_next_match qc pmatch
            match <- peek pmatch
            matched @?= True
            take 70 (show match) @?= "TSQueryMatch {qmId = 1, qmPatternIndex = 0, qmCaptureCount = 2, qmCapt"

            c2 <- peek (qmCaptures match)
            take 55 (show (qcNode c2)) @?= "TSNode 13 (TSPoint {pointRow = 1, pointColumn = 1}) 0 0"
            qcIndex c2 @?= 0

          -- assertBool "show me the logs!" False
          return ()
    ]

-- ---------------------------------------------------------------------
testAST :: TestTree
testAST =
  testGroup
    "Read AST"
    [ testCase "AST full file" $ do
        initVFS $ \vfs -> do
          let aContent =
                T.unlines
                  [ "-xxx(yyy).",
                    "-odu le(foo)",
                    "-aaaa(bbb)."
                  ]
              itemA = TextDocumentItem uriA "erlang" 0 aContent
              uriA = filePathToUri "src/foo.erl"
              nUriA = toNormalizedUri uriA
              a = TextDocumentIdentifier uriA
              msg = NotificationMessage "2.0" STextDocumentDidOpen (DidOpenTextDocumentParams itemA)
          let (v1, _) = openVFS vfs msg
          withParser tree_sitter_erlang_elp $ \parser -> do
            -- parser <- treeSitterParser
            (tree, node) <- treeSitterParseFull parser (T.unpack aContent)
            -- sexp <- nodeAsSexpr node
            sexp <- nodeAsSexprDetail node
            sexp @?= "(source_file forms: (source_file_repeat1 (source_file_repeat1 (attribute (-) name: (atom (_raw_atom)) (() value: (atom (_raw_atom)) ()) (.)) (attribute (-) name: (atom (_raw_atom)) (ERROR (_ERROR (_raw_atom))) (() value: (atom (_raw_atom)) ()) (MISSING \".\"))) (attribute (-) name: (atom (_raw_atom)) (() value: (atom (_raw_atom)) ()) (.))) (end))"

            -- ast <- erlangAst (encodeUtf8 aContent)
            -- ast <- erlangAstBare (encodeUtf8 aContent)
            ast <- erlangAstText (encodeUtf8 aContent)
            let expectedAstStr = "Right (UnmarshalDiagnostics [(Range {start = 11, end = 23},[((16,19),TSDError),((24,24),TSDMissing \".\")]),(Range {start = 11, end = 23},[((16,19),TSDError),((24,24),TSDMissing \".\")])],SourceFile {ann = \"-xxx(yyy).\\n-odu le(foo)\\n-aaaa(bbb).\\n\", forms = [Form {getForm = L1 (Attribute {ann = \"-xxx(yyy).\", value = Expr {getExpr = L1 (Atom {ann = \"yyy\", text = \"yyy\"})}, name = Atom {ann = \"xxx\", text = \"xxx\"}})},Form {getForm = L1 (Attribute {ann = \"-odu le(foo)\", value = Expr {getExpr = L1 (Atom {ann = \"foo\", text = \"foo\"})}, name = Atom {ann = \"odu\", text = \"odu\"}})},Form {getForm = L1 (Attribute {ann = \"-aaaa(bbb).\", value = Expr {getExpr = L1 (Atom {ann = \"bbb\", text = \"bbb\"})}, name = Atom {ann = \"aaaa\", text = \"aaaa\"}})}]})"
            -- [ast] @?= []
            show ast @?= expectedAstStr

            r <- treeToAstText (encodeUtf8 aContent) tree
            show r @?= expectedAstStr

            let Right (UnmarshalDiagnostics ds, _) = r
                [(_, [d1, d2])] = ds
            [d1] @?= [((16, 19), TSDError)]
            let rope = Rope.fromText aContent
                (r1, r2) = Rope.splitAt 16 rope
            show r1 @?= "Rope {unrope = Fork Leaf \"-xxx(yyy).\\n-odu \" Leaf (Position {codeUnits = 16, rowColumn = RowColumn {row = 1, column = 5}})}"
            [measure r1]
              @?= [ Rope.Position
                      { Rope.codeUnits = 16,
                        Rope.rowColumn = Rope.RowColumn {Rope.row = 1, Rope.column = 5}
                      }
                  ]
            [codeUnitsRowColumn 16 rope] @?= [Rope.RowColumn {Rope.row = 1, Rope.column = 5}]

          -- assertBool "show me the logs!" False
          -- free parser
          return ()
    ]

-- ---------------------------------------------------------------------

-- -- TODO: move this to AST.Unmarshal
-- unmarshallTree :: (UnmarshalAnn a, Unmarshal t) => ByteString -> Ptr Tree -> IO (Either String (UnmarshalDiagnostics, t a))
-- unmarshallTree source treePtr =
--   if treePtr == nullPtr
--     then pure (Left "error: didn't get a root node")
--     else do
--       r <-
--         withRootNode treePtr $ \rootPtr ->
--           withCursor (castPtr rootPtr) $ \cursor ->
--             (Right <$> runState (UnmarshalState source cursor mempty) (liftIO (peek rootPtr) >>= unmarshalNode))
--               `catch` (pure . Left . getUnmarshalError)
--       case r of
--         Left e -> pure $ Left e
--         Right (s, res) -> pure $ Right (diagnostics s, res)

-- treeToAst :: ByteString -> Ptr Tree -> IO (Either String (UnmarshalDiagnostics, SourceFile (S.Range, S.Span)))
-- treeToAst = unmarshallTree

-- treeToAstBare :: ByteString -> Ptr Tree -> IO (Either String (UnmarshalDiagnostics, SourceFile ()))
-- treeToAstBare = unmarshallTree

-- treeToAstText :: ByteString -> Ptr Tree -> IO (Either String (UnmarshalDiagnostics, SourceFile T.Text))
-- treeToAstText = unmarshallTree

-- ---------------------------------------------------------------------

erlangAst :: ByteString -> IO (Either String (UnmarshalDiagnostics, SourceFile (S.Range, S.Span)))
erlangAst = parseByteString @Erlang.SourceFile @(S.Range, S.Span) tree_sitter_erlang_elp

erlangAstBare :: ByteString -> IO (Either String (UnmarshalDiagnostics, SourceFile ()))
erlangAstBare = parseByteString @Erlang.SourceFile @() tree_sitter_erlang_elp

erlangAstText :: ByteString -> IO (Either String (UnmarshalDiagnostics, SourceFile T.Text))
erlangAstText = parseByteString @Erlang.SourceFile @T.Text tree_sitter_erlang_elp

-- ---------------------------------------------------------------------
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

-- -- | Print st stderr, where the haskell tree-sitter logging goes
-- -- too. Convenient, it does not get in the way of tasty output either.
-- pp :: String -> IO ()
-- pp str = when logToStderr $ hPutStrLn stderr str

-- treeSitterParseFull :: Ptr Parser -> String -> IO (Ptr Tree, Node)
-- treeSitterParseFull parser source = do
--   (str, len) <- newCStringLen source
--   tree <- ts_parser_parse_string parser nullPtr str len
--   -- pp $ "got tree:" ++ show tree
--   n <- malloc
--   ts_tree_root_node_p tree n
--   -- pp $ "after ts_tree_root_node_p"
--   nn <- peek n
--   -- pp $ "Node:" ++ show nn
--   return (tree, nn)

-- treeSitterParseEdit :: TSInputEdit -> Ptr Tree -> IO (Ptr Tree)
-- treeSitterParseEdit edit tree = do
--   inp <- malloc
--   poke inp edit
--   ts_tree_edit tree inp
--   return tree

-- -- Based on Language.LSP.VFS.applyChange.
-- -- TODO: move this somewhere meaningful
-- lspChangeAsTSInputEdit :: Rope -> TextDocumentContentChangeEvent -> TSInputEdit
-- lspChangeAsTSInputEdit rope change = edit
--   where
--     edit = case change of
--       (TextDocumentContentChangeEvent Nothing Nothing txt) ->
--         -- replace the entire string
--         let
--          in TSInputEdit
--               { editStartByte = 0,
--                 editOldEndByte = fromIntegral $ Rope.length rope,
--                 editNewEndByte = fromIntegral $ T.length txt,
--                 editStartPoint = TSPoint 0 0,
--                 editOldEndPoint = TSPoint (fromIntegral $ Rope.rows rope) 0,
--                 -- probably have to use something other than hard coded
--                 -- col of zero. First approximation.
--                 editNewEndPoint = TSPoint (fromIntegral $ Rope.rows (Rope.fromText txt)) 0
--                 -- If we integrate this with the VFS change processing,
--                 -- the Rope.fromText will not be wasted.
--               }
--       (TextDocumentContentChangeEvent (Just (Range from to)) _ txt) ->
--         -- replace a portion with a new string.  The length parameter
--         -- is optional and deprecated
--         let startByte = byteAddress rope from
--             endByte = byteAddress rope to
--             startPoint = position2TSPoint from
--          in TSInputEdit
--               { editStartByte = startByte,
--                 editOldEndByte = endByte,
--                 editNewEndByte = startByte + fromIntegral (T.length txt),
--                 editStartPoint = startPoint,
--                 editOldEndPoint = position2TSPoint to,
--                 -- probably have to use something other than hard coded
--                 -- col of zero. First approximation.
--                 editNewEndPoint = addText startPoint txt
--                 -- If we integrate this with the VFS change processing,
--                 -- the Rope.fromText will not be wasted.
--               }
--       (TextDocumentContentChangeEvent Nothing (Just _) _txt) -> undefined

--     byteAddress :: Rope -> Position -> Word32
--     byteAddress rope (Position line col) =
--       fromIntegral $ Rope.rowColumnCodeUnits (Rope.RowColumn line col) rope

--     position2TSPoint :: Position -> TSPoint
--     position2TSPoint (Position line col) = TSPoint (fromIntegral line) (fromIntegral col)

--     addText :: TSPoint -> T.Text -> TSPoint
--     addText (TSPoint row col) txt = TSPoint row' col'
--       where
--         rope = Rope.fromText txt
--         newRows = Rope.rows rope
--         lastCol = Rope.length $ snd $ Rope.splitAtLine newRows rope
--         (row', col') = case newRows of
--           0 ->
--             ( row,
--               col + fromIntegral lastCol
--             )
--           r ->
--             ( row + fromIntegral r,
--               fromIntegral lastCol
--             )

-- treeSitterParseIncrement :: Ptr Parser -> Rope -> Ptr Tree -> IO (Ptr Tree, Node)
-- treeSitterParseIncrement parser rope tree = do
--   -- let source2 =
--   --       -- "-moable(foo).\n-x(y).\n"
--   --       "-abcdef(foo).\n-xxxx(yyy).\n"
--   let source2 = Rope.toString rope
--   (str2, _len2) <- newCStringLen source2

--   fp <- mkReadFunction vfsReadFunction
--   let i =
--         TSHInput
--           { inputPayload = castPtr str2,
--             inputRead = fp,
--             inputEncoding = TSInputEncodingUTF8
--           }
--   pp $ "input:" ++ show i
--   ip <- malloc
--   poke ip i
--   when logToStderr $ ts_parser_log_to_stderr parser
--   pp $ "invoking ts_parser_parse_p1"
--   tree <- ts_parser_parse_p1 parser tree ip
--   pp $ "got tree:" ++ show tree
--   n <- malloc
--   ts_tree_root_node_p tree n
--   -- pp $ "after ts_tree_root_node_p"
--   nn <- peek n
--   -- pp $ "Node:" ++ show nn
--   return (tree, nn)

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

-- nodeAsSexpr :: Node -> IO String
-- nodeAsSexpr nn = do
--   let tsn = nodeTSNode nn
--   tsnp <- malloc
--   poke tsnp tsn
--   strp <- ts_node_string_p tsnp
--   str <- peekCString strp
--   free strp
--   return str

nodeAsSexprDetail :: Node -> IO String
nodeAsSexprDetail nn = do
  let tsn = nodeTSNode nn
  tsnp <- malloc
  poke tsnp tsn
  strp <- ts_node_string_extra_p tsnp
  str <- peekCString strp
  free strp
  return str
