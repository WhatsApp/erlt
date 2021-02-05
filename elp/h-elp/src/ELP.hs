{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module ELP where

import Data.Rope.UTF16 (Rope)
import qualified Data.Rope.UTF16 as Rope
-- import qualified Data.Rope.UTF16.Internal as Rope
import qualified Data.Rope.UTF16.Internal.Position as Rope
import AST.Unmarshal
    ( UnmarshalDiagnostics,
      unmarshalNode,
      UnmarshalError(getUnmarshalError),
      Unmarshal,
      UnmarshalAnn,
      UnmarshalState(UnmarshalState, diagnostics) )
-- import Control.Carrier.Reader
import Control.Carrier.State.Strict ( runState )
-- import qualified Data.ByteString as B
-- import qualified Language.Erlang.AST as Erlang
-- import System.IO.Temp
import Control.Exception ( catch )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Data.ByteString (ByteString)
-- import Data.Rope.UTF16 (Rope)
-- import qualified Data.Rope.UTF16 as Rope
import qualified Data.Text as T
import Foreign.C.String
    ( withCStringLen, peekCString, newCStringLen )
import Foreign.Marshal.Alloc ( free, malloc )
import Foreign.Ptr ( Ptr, plusPtr, castPtr, nullPtr )
import Foreign.Storable ( Storable(peek, poke) )
import GHC.Word ( Word32 )
import Language.Erlang.AST (SourceFile)
import Language.Erlang.Grammar ( tree_sitter_erlang_elp )
import Language.LSP.Types
    ( Position(Position),
      Range(Range),
      TextDocumentContentChangeEvent(TextDocumentContentChangeEvent) )
import Data.SplayTree ( Measured(measure) )
import qualified Source.Range as S
import qualified Source.Span as S
import System.IO ( stderr, hPutStrLn )
import TreeSitter.Cursor ( withCursor )
-- import TreeSitter.ErlangELP
import TreeSitter.Node
    ( Node(nodeTSNode), ts_node_string_p, TSPoint(TSPoint) )
import TreeSitter.Parser
    ( Parser,
      ts_parser_parse_p1,
      ts_parser_log_to_stderr,
      mkReadFunction,
      ts_parser_parse_string,
      ts_parser_set_language,
      ts_parser_set_timeout_micros,
      ts_parser_timeout_micros,
      ts_parser_new,
      TSInputEncoding(TSInputEncodingUTF8),
      TSHInput(TSHInput, inputRead, inputEncoding, inputPayload),
      ReadFunction )
-- import TreeSitter.Query
import TreeSitter.Tree
    ( Tree,
      withRootNode,
      ts_tree_edit,
      ts_tree_root_node_p,
      TSInputEdit(..) )

-- ---------------------------------------------------------------------

-- | Print st stderr, where the haskell tree-sitter logging goes
-- too. Convenient, it does not get in the way of tasty output either.
pp :: String -> IO ()
pp = hPutStrLn stderr

-- ---------------------------------------------------------------------
-- TODO: free the parser, or bracket it with a free
treeSitterParser :: IO (Ptr Parser)
treeSitterParser = do
  parser <- ts_parser_new
  pp $ "got parser:" ++ show parser
  timeout <- ts_parser_timeout_micros parser
  pp $ "timeout:" ++ show timeout

  ts_parser_set_timeout_micros parser 3_000_0000

  timeout_r <- ts_parser_timeout_micros parser
  pp $ "timeout:" ++ show timeout_r

  r <- ts_parser_set_language parser tree_sitter_erlang_elp
  pp $ "set language:" ++ show (r, tree_sitter_erlang_elp)
  return parser

-- ---------------------------------------------------------------------
treeSitterParseFull :: Ptr Parser -> String -> IO (Ptr Tree, Node)
treeSitterParseFull parser source = do
  (str, len) <- newCStringLen source
  tree <- ts_parser_parse_string parser nullPtr str len
  n <- malloc
  ts_tree_root_node_p tree n
  nn <- peek n
  return (tree, nn)

-- ---------------------------------------------------------------------
treeSitterParseEdit :: TSInputEdit -> Ptr Tree -> IO (Ptr Tree)
treeSitterParseEdit edit tree = do
  inp <- malloc
  poke inp edit
  ts_tree_edit tree inp
  return tree

-- ---------------------------------------------------------------------
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
    byteAddress rope' (Position line col) =
      fromIntegral $ Rope.rowColumnCodeUnits (Rope.RowColumn line col) rope'

    position2TSPoint :: Position -> TSPoint
    position2TSPoint (Position line col) = TSPoint (fromIntegral line) (fromIntegral col)

    addText :: TSPoint -> T.Text -> TSPoint
    addText (TSPoint row col) txt = TSPoint row' col'
      where
        rope' = Rope.fromText txt
        newRows = Rope.rows rope'
        lastCol = Rope.length $ snd $ Rope.splitAtLine newRows rope'
        (row', col') = case newRows of
          0 ->
            ( row,
              col + fromIntegral lastCol
            )
          r ->
            ( row + fromIntegral r,
              fromIntegral lastCol
            )

-- ---------------------------------------------------------------------

treeSitterParseIncrement :: Ptr Parser -> Rope -> Ptr Tree -> IO (Ptr Tree, Node)
treeSitterParseIncrement parser rope tree = do
  -- TODO: do we *have* to convert the entire rope to a string?
  -- optimise the TSHInput function to use a rope instead of a string.
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
  pp "invoking ts_parser_parse_p1"
  tree2 <- ts_parser_parse_p1 parser tree ip
  pp $ "got tree:" ++ show tree2
  n <- malloc
  ts_tree_root_node_p tree2 n
  nn <- peek n
  return (tree2, nn)

-- ---------------------------------------------------------------------

-- | Tree Sitter 'ReadFunction' using a VFS as the payload
vfsReadFunction :: ReadFunction
vfsReadFunction ppayload offset ppos lenPtr = do
  pp "******vfsReadFunction entered"
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

-- ---------------------------------------------------------------------
nodeAsSexpr :: Node -> IO String
nodeAsSexpr nn = do
  let tsn = nodeTSNode nn
  pp $ "tsn:" ++ show tsn
  tsnp <- malloc
  poke tsnp tsn
  pp "after poke"
  strp <- ts_node_string_p tsnp
  pp "after poke 2"
  str <- peekCString strp
  free strp
  pp "after poke"
  pp $ "node sexpr:" ++ str
  return str

-- ---------------------------------------------------------------------

parseChange :: Rope -> Ptr Parser -> Ptr Tree -> TextDocumentContentChangeEvent -> IO (Ptr Tree, Node)
parseChange rope parser tree lspChange = do
  let tsEdit = lspChangeAsTSInputEdit rope lspChange
  tree2 <- treeSitterParseEdit tsEdit tree
  (tree3, node) <- treeSitterParseIncrement parser rope tree2
  return (tree3, node)

-- ---------------------------------------------------------------------

-- TODO: move this to AST.Unmarshal
unmarshallTree :: (UnmarshalAnn a, Unmarshal t) => ByteString -> Ptr Tree -> IO (Either String (UnmarshalDiagnostics, t a))
unmarshallTree source treePtr =
  if treePtr == nullPtr
    then pure (Left "error: didn't get a root node")
    else do
      r <-
        withRootNode treePtr $ \rootPtr ->
          withCursor (castPtr rootPtr) $ \cursor ->
            (Right <$> runState (UnmarshalState source cursor mempty) (liftIO (peek rootPtr) >>= unmarshalNode))
              `catch` (pure . Left . getUnmarshalError)
      case r of
        Left e -> pure $ Left e
        Right (s, res) -> pure $ Right (diagnostics s, res)

treeToAst :: ByteString -> Ptr Tree -> IO (Either String (UnmarshalDiagnostics, SourceFile (S.Range, S.Span)))
treeToAst = unmarshallTree

treeToAstBare :: ByteString -> Ptr Tree -> IO (Either String (UnmarshalDiagnostics, SourceFile ()))
treeToAstBare = unmarshallTree

treeToAstText :: ByteString -> Ptr Tree -> IO (Either String (UnmarshalDiagnostics, SourceFile T.Text))
treeToAstText = unmarshallTree

-- ---------------------------------------------------------------------

codeUnitsRowColumn :: Int -> Rope -> Rope.RowColumn
codeUnitsRowColumn offset rope = Rope.rowColumn . measure . fst $ Rope.splitAt offset rope

-- ---------------------------------------------------------------------

offsetsToRange :: Rope.Rope -> (Int,Int) -> Range
offsetsToRange rope (start,end) = Range (Position r1 c1) (Position r2 c2)
  where
    Rope.RowColumn r1 c1 = codeUnitsRowColumn start rope
    Rope.RowColumn r2 c2 = codeUnitsRowColumn end rope

-- ---------------------------------------------------------------------
