{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeInType #-}

import Control.Lens hiding (Iso, List)
import Control.Monad
import Control.Monad.Reader
import Data.Aeson hiding (Options, defaultOptions)
import qualified Data.HashMap.Strict as HM
import Data.List (isSuffixOf)
import qualified Data.Map as Map
import Data.Rope.UTF16 (Rope)
import qualified Data.Rope.UTF16 as Rope
import qualified Data.Text as T
import ELP
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array (mallocArray)
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics (Generic)
import GHC.Word
import Language.LSP.Diagnostics (partitionBySource)
import Language.LSP.Server
import Language.LSP.Types
import Language.LSP.Types.Lens as Lsp hiding (options, publishDiagnostics, textDocumentSync)
import Language.LSP.VFS
import System.Directory
import System.FilePath
import System.Log.Logger
import TreeSitter.ErlangELP
import TreeSitter.Node
import TreeSitter.Parser
import TreeSitter.Tree
import UnliftIO
import UnliftIO.Concurrent

-- ---------------------------------------------------------------------

main :: IO Int
main = do
  -- Log to stderr by default
  -- setupLogger Nothing ["h-elp"] DEBUG
  setupLogger Nothing ["h-elp"] INFO

  parser <- treeSitterParser
  let pc = ParserContext parser mempty
  handlerEnv <- HandlerEnv <$> newEmptyMVar <*> newEmptyMVar <*> newMVar pc

  runServer $
    ServerDefinition
      { doInitialize = \env _req -> pure $ Right env,
        onConfigurationChange = const $ pure $ Right defaultConfig,
        staticHandlers = handlers,
        interpretHandler = \env ->
          Iso
            (\m -> runLspT env (runReaderT m handlerEnv))
            liftIO,
        options = lspOptions
      }

-- ---------------------------------------------------------------------

lspOptions :: Options
lspOptions =
  defaultOptions
    { textDocumentSync = Just syncOptions
    }
  where
    syncOptions =
      TextDocumentSyncOptions openClose change willSave willSaveWaitUntil save
    openClose = Just True
    change = Just TdSyncIncremental
    willSave = Just False
    willSaveWaitUntil = Just False
    save = Just $ InR $ SaveOptions $ Just False

-- ---------------------------------------------------------------------

data HandlerEnv = HandlerEnv
  { relRegToken :: MVar (RegistrationToken WorkspaceDidChangeWatchedFiles),
    absRegToken :: MVar (RegistrationToken WorkspaceDidChangeWatchedFiles),
    heParser :: MVar ParserContext
  }

data ParserContext = ParserContext
  { pcParser :: !(Ptr Parser),
    pcsMap :: Map.Map NormalizedUri TsFile
  }

data TsFile = TsFile
  { pcTree :: !(Ptr Tree),
    pcNode :: !Node
  }

-- ---------------------------------------------------------------------

handlers :: Handlers (ReaderT HandlerEnv (LspM Config))
handlers =
  mconcat
    [ notificationHandler SInitialized $
        \_noti -> do
          sendNotification SWindowLogMessage $
            LogMessageParams MtLog "initialized"

          sendNotification
            SWindowShowMessage
            (ShowMessageParams MtInfo "Not turning on code lenses"),
      -- -----------------------------------

      notificationHandler STextDocumentDidOpen $ \msg -> do
        let doc = msg ^. params . textDocument . uri
            fileName = uriToFilePath doc
            docUri = toNormalizedUri doc
        let aContent = msg ^. params . textDocument . text
        liftIO $ infoM "h-elp" $ "Processing DidOpenTextDocument for: " ++ show fileName
        mdoc <- getVirtualFile docUri
        case mdoc of
          Just (VirtualFile _lspversion version rope) -> do
            liftIO $ infoM "h-elp" $ "Found the virtual file: " ++ show version
            mparser <- asks heParser
            sexp <- liftIO $
              modifyMVar mparser $ \(ParserContext parser pcsmap) -> do
                (tree, node) <- treeSitterParseFull parser (T.unpack aContent)
                sexp <- nodeAsSexpr node
                liftIO $ infoM "h-elp" $ "parsed to: " ++ show sexp
                let pcsmap' = Map.insert docUri (TsFile tree node) pcsmap
                return (ParserContext parser pcsmap', sexp)
            sendDiagnostics docUri (Just 0) (T.pack sexp)
            return ()
          Nothing -> do
            liftIO $ infoM "h-elp" $ "Didn't find anything in the VFS for: " ++ show doc
        return (),
      -- -----------------------------------

      notificationHandler STextDocumentDidChange $ \msg -> do
        let doc = msg ^. params . textDocument . uri . to toNormalizedUri
            List changeList = msg ^. params . contentChanges
        -- docUri = toNormalizedUri doc
        liftIO $ infoM "h-elp" $ "Processing DidChangeTextDocument for: " ++ show doc
        mdoc <- getVirtualFile doc
        case mdoc of
          Just (VirtualFile _lspversion version rope) -> do
            liftIO $ infoM "h-elp" $ "Found the virtual file: " ++ show version
            mparser <- asks heParser
            sexp <- liftIO $
              modifyMVar mparser $ \(ParserContext parser pcsmap) -> do
                (tree, node) <- case Map.lookup doc pcsmap of
                  Nothing -> do
                    infoM "h-elp" $ "ParserContext not found, doing full parse"
                    (tree, node) <- treeSitterParseFull parser (Rope.toString rope)
                    return (tree, node)
                  Just (TsFile tree _node) -> do
                    -- TODO: fold over the changes
                    let tsEdit = lspChangeAsTSInputEdit rope (head changeList)
                    tree2 <- treeSitterParseEdit tsEdit tree
                    (tree3, node2) <- treeSitterParseIncrement parser rope tree2
                    return (tree3, node2)
                sexp <- nodeAsSexpr node
                liftIO $ infoM "h-elp" $ "parsed to: " ++ show sexp
                let pcsmap' = Map.insert doc (TsFile tree node) pcsmap
                return (ParserContext parser pcsmap', sexp)
            sendDiagnostics doc (Just 0) (T.pack sexp)
            return ()
          Nothing -> do
            liftIO $ infoM "h-elp" $ "Didn't find anything in the VFS for: " ++ show doc,
      -- ===================================
      -- -----------------------------------

      requestHandler STextDocumentHover $
        \_req responder ->
          responder $
            Right $
              Just $
                Hover (HoverContents (MarkupContent MkPlainText "hello")) Nothing,
      -- -----------------------------------

      requestHandler STextDocumentDocumentSymbol $
        \_req responder ->
          responder $
            Right $
              InL $
                List
                  [ DocumentSymbol
                      "foo"
                      Nothing
                      SkObject
                      Nothing
                      (mkRange 0 0 3 6)
                      (mkRange 0 0 3 6)
                      Nothing
                  ],
      -- -----------------------------------

      -- notificationHandler STextDocumentDidOpen $
      --   \noti -> do
      --     let NotificationMessage _ _ (DidOpenTextDocumentParams doc) = noti
      --         TextDocumentItem uri _ _ _ = doc
      --         Just fp = uriToFilePath uri
      --         diag =
      --           Diagnostic
      --             (mkRange 0 0 0 1)
      --             (Just DsWarning)
      --             (Just (InL 42))
      --             (Just "dummy-server")
      --             "Here's a warning"
      --             Nothing
      --             Nothing
      --     withRunInIO $
      --       \runInIO -> do
      --         when (".hs" `isSuffixOf` fp) $
      --           void $
      --             forkIO $
      --               do
      --                 threadDelay (2 * 10 ^ 6)
      --                 runInIO $
      --                   sendNotification STextDocumentPublishDiagnostics $
      --                     PublishDiagnosticsParams uri Nothing (List [diag])
      --         -- also act as a registerer for workspace/didChangeWatchedFiles
      --         when (".register" `isSuffixOf` fp) $
      --           do
      --             let regOpts =
      --                   DidChangeWatchedFilesRegistrationOptions $
      --                     List
      --                       [ FileSystemWatcher
      --                           "*.watch"
      --                           (Just (WatchKind True True True))
      --                       ]
      --             Just token <- runInIO $
      --               registerCapability SWorkspaceDidChangeWatchedFiles regOpts $
      --                 \_noti ->
      --                   sendNotification SWindowLogMessage $
      --                     LogMessageParams MtLog "got workspace/didChangeWatchedFiles"
      --             runInIO $ asks relRegToken >>= \v -> putMVar v token
      --         when (".register.abs" `isSuffixOf` fp) $
      --           do
      --             curDir <- getCurrentDirectory
      --             let regOpts =
      --                   DidChangeWatchedFilesRegistrationOptions $
      --                     List
      --                       [ FileSystemWatcher
      --                           (curDir </> "*.watch")
      --                           (Just (WatchKind True True True))
      --                       ]
      --             Just token <- runInIO $
      --               registerCapability SWorkspaceDidChangeWatchedFiles regOpts $
      --                 \_noti ->
      --                   sendNotification SWindowLogMessage $
      --                     LogMessageParams MtLog "got workspace/didChangeWatchedFiles"
      --             runInIO $ asks absRegToken >>= \v -> putMVar v token
      --         -- also act as an unregisterer for workspace/didChangeWatchedFiles
      --         when (".unregister" `isSuffixOf` fp) $
      --           do
      --             Just token <- runInIO $ asks relRegToken >>= tryReadMVar
      --             runInIO $ unregisterCapability token
      --         when (".unregister.abs" `isSuffixOf` fp) $
      --           do
      --             Just token <- runInIO $ asks absRegToken >>= tryReadMVar
      --             runInIO $ unregisterCapability token,

      -- -----------------------------------

      -- requestHandler SWorkspaceExecuteCommand $ \req resp -> do
      --   let RequestMessage _ _ _ (ExecuteCommandParams Nothing "doAnEdit" (Just (List [val]))) = req
      --       Success docUri = fromJSON val
      --       edit = List [TextEdit (mkRange 0 0 0 5) "howdy"]
      --       params =
      --         ApplyWorkspaceEditParams (Just "Howdy edit") $
      --           WorkspaceEdit (Just (HM.singleton docUri edit)) Nothing
      --   resp $ Right Null
      --   void $ sendRequest SWorkspaceApplyEdit params (const (pure ())),
      -- -----------------------------------

      requestHandler STextDocumentCodeAction $ \req resp -> do
        let RequestMessage _ _ _ params = req
            CodeActionParams _ _ _ _ cactx = params
            CodeActionContext diags _ = cactx
            codeActions = fmap diag2ca diags
            diag2ca d =
              CodeAction
                "Delete this"
                Nothing
                (Just (List [d]))
                Nothing
                Nothing
                (Just (Command "" "deleteThis" Nothing))
        resp $ Right $ InR <$> codeActions,
      -- -----------------------------------

      requestHandler STextDocumentCompletion $ \_req resp -> do
        let res = CompletionList True (List [item])
            item =
              CompletionItem
                "foo"
                (Just CiConstant)
                (Just (List []))
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
        resp $ Right $ InR res
    ]

-- ---------------------------------------------------------------------

-- | Analyze the file and send any diagnostics to the client in a
-- "textDocument/publishDiagnostics" notification
-- sendDiagnostics :: NormalizedUri -> Maybe Int -> LspM Config ()
-- sendDiagnostics :: MonadLsp config m => NormalizedUri -> TextDocumentVersion -> T.Text -> m ()
sendDiagnostics fileUri version msg = do
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

data Config = Config {fooTheBar :: Bool, wibbleFactor :: Int}
  deriving (Generic, ToJSON, FromJSON)

defaultConfig :: Config
defaultConfig = Config True 0

-- ---------------------------------------------------------------------
