{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module ELP where

import AST.Unmarshal
  ( Unmarshal,
    UnmarshalAnn,
    UnmarshalDiagnostics,
    UnmarshalError (getUnmarshalError),
    UnmarshalState (UnmarshalState, diagnostics),
    unmarshalNode,
  )
import Control.Carrier.State.Strict (runState)
import Control.Exception (catch)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString (ByteString)
import Data.Rope.UTF16 (Rope, codeUnitsRowColumn)
import qualified Data.Rope.UTF16 as Rope
import qualified Data.Rope.UTF16.Internal.Position as Rope
import qualified Data.SplayTree as Rope
import qualified Data.Text as T
import Foreign.C.String
  ( peekCString,
    withCStringLen,
  )
import Foreign.Marshal.Alloc (alloca, free)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (Storable (peek, poke))
import GHC.Word (Word32)
import Language.Erlang.AST (SourceFile)
import Language.LSP.Types
  ( Position (Position),
    Range (Range),
    TextDocumentContentChangeEvent (TextDocumentContentChangeEvent),
  )
import qualified Source.Range as S
import qualified Source.Span as S
import System.IO (hPutStrLn, stderr)
import TreeSitter.Cursor (withCursor)
import TreeSitter.Node
  ( Node (nodeTSNode),
    TSPoint (TSPoint),
    ts_node_string_p,
  )
import TreeSitter.Parser
  ( Parser,
    ReadFunction,
    TSHInput (TSHInput, inputEncoding, inputPayload, inputRead),
    TSInputEncoding (TSInputEncodingUTF8),
    mkReadFunction,
    ts_parser_log_to_stderr,
    ts_parser_parse_p1,
    ts_parser_parse_string,
  )
import TreeSitter.Tree
  ( TSInputEdit (..),
    Tree,
    ts_tree_edit,
    ts_tree_root_node_p,
    withRootNode,
  )

-- ---------------------------------------------------------------------

-- | Print st stderr, where the haskell tree-sitter logging goes
-- too. Convenient, it does not get in the way of tasty output either.
pp :: String -> IO ()
pp = hPutStrLn stderr

-- ---------------------------------------------------------------------
-- TODO: free the parser, or bracket it with a free
-- treeSitterParser :: IO (Ptr Parser)
-- treeSitterParser = do
--   parser <- ts_parser_new
--   pp $ "got parser:" ++ show parser
--   timeout <- ts_parser_timeout_micros parser
--   pp $ "timeout:" ++ show timeout

--   ts_parser_set_timeout_micros parser 3_000_0000

--   timeout_r <- ts_parser_timeout_micros parser
--   pp $ "timeout:" ++ show timeout_r

--   r <- ts_parser_set_language parser tree_sitter_erlang_elp
--   pp $ "set language:" ++ show (r, tree_sitter_erlang_elp)
--   return parser

-- ---------------------------------------------------------------------
treeSitterParseFull :: Ptr Parser -> String -> IO (Ptr Tree, Node)
treeSitterParseFull parser source = do
  withCStringLen source $ \(str, len) -> do
    tree <- ts_parser_parse_string parser nullPtr str len
    alloca $ \n -> do
      ts_tree_root_node_p tree n
      nn <- peek n
      return (tree, nn)

-- ---------------------------------------------------------------------
treeSitterParseEdit :: TSInputEdit -> Ptr Tree -> IO (Ptr Tree)
treeSitterParseEdit edit tree = do
  alloca $ \inp -> do
    poke inp edit
    ts_tree_edit tree inp
    return tree

-- ---------------------------------------------------------------------
-- Based on Language.LSP.VFS.applyChange.
-- TODO: move this somewhere meaningful
lspChangeAsTSInputEdit :: Rope -> Rope -> TextDocumentContentChangeEvent -> Maybe TSInputEdit
lspChangeAsTSInputEdit ropeOld _ropeNew change = edit
  where
    edit = case change of
      (TextDocumentContentChangeEvent Nothing Nothing _txt) ->
        -- replace the entire string, no point in generating an input edit
        Nothing
      (TextDocumentContentChangeEvent (Just (Range from to)) _ txt) ->
        -- replace a portion with a new string.  The length parameter
        -- is optional and deprecated
        let startByte = byteAddress ropeOld from
            endByte = byteAddress ropeOld to
            startPoint = position2TSPoint from
            endPoint = position2TSPoint to
         in Just $
              TSInputEdit
                { editStartByte = startByte,
                  editOldEndByte = endByte,
                  editNewEndByte = startByte + fromIntegral (T.length txt),
                  editStartPoint = startPoint,
                  editOldEndPoint = endPoint,
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

rope2TSPoint :: Rope -> TSPoint
rope2TSPoint rope = TSPoint (fromIntegral r) (fromIntegral c)
  where
    Rope.RowColumn r c = (Rope.rowColumn . Rope.measure) rope

-- ---------------------------------------------------------------------

treeSitterParseIncrement :: Ptr Parser -> Rope -> Ptr Tree -> IO (Ptr Tree, Node)
treeSitterParseIncrement parser rope tree = do
  -- TODO: do we *have* to convert the entire rope to a string?
  -- optimise the TSHInput function to use a rope instead of a string.
  withCStringLen (Rope.toString rope) $ \(str2, _len2) -> do
    -- let source2 = Rope.toString rope
    -- (str2, _len2) <- newCStringLen source2

    fp <- mkReadFunction vfsReadFunction
    let i =
          TSHInput
            { inputPayload = castPtr str2,
              inputRead = fp,
              inputEncoding = TSInputEncodingUTF8
            }
    pp $ "input:" ++ show i
    alloca $ \ip -> do
      -- ip <- malloc
      poke ip i
      ts_parser_log_to_stderr parser
      pp "invoking ts_parser_parse_p1"
      tree2 <- ts_parser_parse_p1 parser tree ip
      pp $ "got tree:" ++ show tree2
      alloca $ \n -> do
        -- n <- malloc
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
  alloca $ \tsnp -> do
    -- tsnp <- malloc
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

-- | Accept a @TextDocumentContentChangeEvent@ and a @Rope@ which
--  already has the edit applied.  Do an incremental parse if possible
parseChange :: Rope -> Rope -> Ptr Parser -> Ptr Tree -> TextDocumentContentChangeEvent -> IO (Ptr Tree, Node)
parseChange ropeOld ropeNew parser tree lspChange = do
  case lspChangeAsTSInputEdit ropeOld ropeNew lspChange of
    Nothing -> treeSitterParseFull parser (Rope.toString ropeNew)
    Just tsEdit -> do
      tree2 <- treeSitterParseEdit tsEdit tree
      (tree3, node) <- treeSitterParseIncrement parser ropeNew tree2
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

offsetsToRange :: Rope.Rope -> (Int, Int) -> Range
offsetsToRange rope (start, end) = Range (Position r1 c1) (Position r2 c2)
  where
    Rope.RowColumn r1 c1 = codeUnitsRowColumn start rope
    Rope.RowColumn r2 c2 = codeUnitsRowColumn end rope

-- ---------------------------------------------------------------------
