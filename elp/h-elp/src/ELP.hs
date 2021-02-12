{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeInType #-}

module ELP where

import AST.Unmarshal
  ( TSDiagnostic (..),
    Unmarshal,
    UnmarshalAnn,
    UnmarshalDiagnostics (..),
    UnmarshalError (getUnmarshalError),
    UnmarshalState (UnmarshalState, diagnostics),
    unmarshalNode,
  )
import Control.Carrier.State.Strict (runState)
import Control.Exception (catch)
import Control.Lens (to, (^.))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString (ByteString)
import qualified Data.Map as Map
import Data.Rope.UTF16 (Rope, codeUnitsRowColumn)
import qualified Data.Rope.UTF16 as Rope
import qualified Data.Rope.UTF16.Internal.Position as Rope
import qualified Data.SplayTree as Rope
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Development.IDE (IdeState (..))
import Development.IDE.Core.Shake (IsIdeGlobal, addIdeGlobalExtras, getIdeGlobalExtras)
import Foreign.C.String
  ( peekCString,
    withCStringLen,
  )
import Foreign.Marshal.Alloc (alloca, free)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (Storable (peek, poke))
import GHC.Word (Word32)
import Ide.Plugin.Config (Config)
import Ide.Types (PluginDescriptor (..), PluginId, PluginNotificationMethodHandler, defaultPluginDescriptor, mkPluginNotificationHandler)
import Language.Erlang.AST (SourceFile)
import qualified Language.Erlang.AST as Erlang
import Language.LSP.Diagnostics (partitionBySource)
import Language.LSP.Server
import Language.LSP.Types
import Language.LSP.Types.Lens as Lsp
  ( HasUri (uri),
    text,
    textDocument,
  )
import Language.LSP.VFS (VirtualFile (VirtualFile))
import qualified Source.Range as S
import qualified Source.Span as S
import System.IO (hPutStrLn, stderr)
import System.Log.Logger (infoM)
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

data ParserContext = ParserContext
  { pcParser :: !(Ptr Parser),
    pcsMap :: Map.Map NormalizedUri TsFile
  }

data TsFile = TsFile
  { pcTree :: !(Ptr Tree),
    pcNode :: !Node,
    pcRope :: !Rope.Rope
  }

-- We store the Tree Sitter state in an Shake IdeGlobal, to avoid
-- polluting the import space in ghcide.  It used Dynamic under the
-- hood.
instance IsIdeGlobal ParserContext

-- ---------------------------------------------------------------------

hlsPlugin :: PluginId -> PluginDescriptor IdeState
hlsPlugin plId =
  (defaultPluginDescriptor plId)
    { pluginRules = mempty,
      pluginCommands = [],
      pluginNotificationHandlers =
        mkPluginNotificationHandler STextDocumentDidOpen didOpen
          <> mkPluginNotificationHandler STextDocumentDidChange didChange
    }

-- ---------------------------------------------------------------------

-- PluginNotificationMethodHandler a m = a -> PluginId -> MessageParams m -> LspM Config ()
didOpen :: PluginNotificationMethodHandler IdeState 'TextDocumentDidOpen
didOpen cfg _pid params = do
  let doc = params ^. textDocument . uri
      fileName = uriToFilePath doc
      docUri = toNormalizedUri doc
  let aContent = params ^. textDocument . text
  liftIO $ infoM "h-elp" $ "Processing DidOpenTextDocument for: " ++ show fileName
  mdoc <- getVirtualFile docUri
  case mdoc of
    Just (VirtualFile _lspversion vsn rope) -> do
      liftIO $ infoM "h-elp" $ "Found the virtual file: " ++ show vsn
      (ParserContext parser pcsmap) <- liftIO $ getIdeGlobalExtras (shakeExtras cfg) :: LspM Config ParserContext
      -- mparser <- asks heParser
      tree <- liftIO $ do
        -- modifyMVar mparser $ \(ParserContext parser pcsmap) -> do
        (tree, node) <- treeSitterParseFull parser (T.unpack aContent)
        sexp <- nodeAsSexpr node
        liftIO $ infoM "h-elp" $ "parsed to: " ++ show sexp
        let pcsmap' = Map.insert docUri (TsFile tree node rope) pcsmap
        -- return (ParserContext parser pcsmap', tree)
        addIdeGlobalExtras (shakeExtras cfg) (ParserContext parser pcsmap')
        return tree
      reportDiagnostics docUri (Just 0) (Rope.fromText aContent) tree
      return ()
    Nothing -> do
      liftIO $ infoM "h-elp" $ "Didn't find anything in the VFS for: " ++ show doc

-- return ()

-- ---------------------------------------------------------------------

-- PluginNotificationMethodHandler a m = a -> PluginId -> MessageParams m -> LspM Config ()
didChange :: PluginNotificationMethodHandler IdeState 'TextDocumentDidChange
didChange cfg _pid (DidChangeTextDocumentParams textDoc (List changeList)) = do
  let doc = textDoc ^. uri . to toNormalizedUri
  liftIO $ infoM "h-elp" $ "Processing DidChangeTextDocument for: " ++ show doc
  -- logm "h-elp" $ "Processing DidChangeTextDocument for: " ++ show doc
  mdoc <- getVirtualFile doc
  case mdoc of
    Just (VirtualFile lspversion vsn ropeNew) -> do
      liftIO $ infoM "h-elp" $ "Found the virtual file: " ++ show vsn
      -- mparser <- asks heParser
      (ParserContext parser pcsmap) <- liftIO $ getIdeGlobalExtras (shakeExtras cfg) :: LspM Config ParserContext
      -- liftIO $ infoM "h-elp" "Got the parser"
      tree <- liftIO $ do
        -- modifyMVar mparser $ \(ParserContext parser pcsmap) -> do
        -- liftIO $ infoM "h-elp" "Got the mvar"
        (tree, node) <- case Map.lookup doc pcsmap of
          Nothing -> do
            infoM "h-elp" "ParserContext not found, doing full parse"
            treeSitterParseFull parser (Rope.toString ropeNew)
          Just (TsFile tree node ropeOld) -> do
            -- liftIO $ infoM "h-elp" "Got the TsFile"
            -- TODO: fold over the changes
            -- Question: Is this valid, given the rope has all the changes already?
            -- TODO: update lsp/vfs to provide a callback on every change
            case changeList of
              [] -> do
                infoM "h-elp" "No changes in didChange message"
                return (tree, node)
              [c] -> do
                -- infoM "h-elp" $ "One change in didChange message:" ++ show c
                parseChange ropeOld ropeNew parser tree c
              _ -> do
                infoM "h-elp" "Multiple changes in didChange message, doing full parse"
                treeSitterParseFull parser (Rope.toString ropeNew)
        -- infoM "h-elp" "Got (tree,node)"
        sexp <- nodeAsSexpr node
        liftIO $ infoM "h-elp" $ "parsed to: " ++ show sexp
        let pcsmap' = Map.insert doc (TsFile tree node ropeNew) pcsmap
        -- return (ParserContext parser pcsmap', tree)
        addIdeGlobalExtras (shakeExtras cfg) (ParserContext parser pcsmap')
        return tree
      reportDiagnostics doc (Just lspversion) ropeNew tree
      liftIO $ infoM "h-elp" "DidChange done"
      return ()
    Nothing -> do
      liftIO $ infoM "h-elp" $ "Didn't find anything in the VFS for: " ++ show doc

-- ---------------------------------------------------------------------

reportDiagnostics ::
  MonadLsp config m =>
  NormalizedUri ->
  TextDocumentVersion ->
  Rope.Rope ->
  Ptr Tree ->
  m ()
reportDiagnostics fileUri version rope tree = do
  diags <- liftIO $ makeDiagnostics rope tree
  -- liftIO $ pp $ "reportDiagnostics:diags=" ++ show diags
  sendDiagnostics fileUri version diags

makeDiagnostics :: Rope.Rope -> Ptr Tree -> IO [Diagnostic]
makeDiagnostics rope tree = do
  -- mr <- treeToAstBare (encodeUtf8 $ Rope.toText rope) tree
  mr <- treeToAstText (encodeUtf8 $ Rope.toText rope) tree
  case mr of
    Right (UnmarshalDiagnostics diags, Erlang.SourceFile a _) -> do
      let bar = concatMap snd diags
          lspDiags = map (toDiagnostic . \(p, d) -> (offsetsToRange rope p, d)) bar
          astDiag = textDiagnostic a
      return (astDiag : lspDiags)
    Left err -> return [textDiagnostic (T.pack $ "treeToAst:" <> err)]

toDiagnostic :: (Range, TSDiagnostic) -> Diagnostic
toDiagnostic (range, diag) =
  Diagnostic
    range
    (Just DsWarning) -- severity
    Nothing -- code
    (Just "ELP") -- source
    (T.pack $ show diag)
    Nothing -- tags
    (Just (List []))

textDiagnostic :: T.Text -> Diagnostic
textDiagnostic msg =
  Diagnostic
    (Range (Position 1 1) (Position 1 5))
    (Just DsWarning) -- severity
    Nothing -- code
    (Just "ELP") -- source
    msg
    Nothing -- tags
    (Just (List []))

sendDiagnostics ::
  MonadLsp config m =>
  NormalizedUri ->
  TextDocumentVersion ->
  [Diagnostic] ->
  m ()
sendDiagnostics fileUri version diags = do
  -- liftIO $ pp $ "sendDiagnostics:(fileUri,version,diags)=" ++ show (fileUri, version, diags)
  publishDiagnostics 100 fileUri version (partitionBySource diags)

-- ---------------------------------------------------------------------

-- | Analyze the file and send any diagnostics to the client in a
-- "textDocument/publishDiagnostics" notification
-- sendDiagnostics :: NormalizedUri -> Maybe Int -> LspM Config ()
sendDiagnosticsMessage :: MonadLsp config m => NormalizedUri -> TextDocumentVersion -> T.Text -> m ()
sendDiagnosticsMessage fileUri version msg = do
  let diags =
        [ Diagnostic
            (Range (Position 0 1) (Position 0 5))
            (Just DsWarning) -- severity
            Nothing -- code
            (Just "ELP") -- source
            msg
            Nothing -- tags
            (Just (List []))
        ]
  publishDiagnostics 100 fileUri version (partitionBySource diags)

-- ---------------------------------------------------------------------

-- data Config = Config {fooTheBar :: Bool, wibbleFactor :: Int}
--   deriving (Generic, ToJSON, FromJSON)

-- defaultConfig :: Config
-- defaultConfig = Config True 0

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
  pp $ "******vfsReadFunction (pstr, len)=" ++ show (pstr, len)
  poke lenPtr (fromIntegral len)
  pp "******vfsReadFunction ending="
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
