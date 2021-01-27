{-# LANGUAGE OverloadedStrings #-}
module Main where

-- First cut, based on Simple.hs example in the lsp package

import Language.LSP.Server
import Language.LSP.Types
import Control.Monad.IO.Class
import qualified Data.Text as T

handlers :: Handlers (LspM ())
handlers = mconcat
  [ notificationHandler SInitialized $ \_not -> do
      let params = ShowMessageRequestParams MtInfo "Turn on code lenses?"
            (Just [MessageActionItem "Turn on", MessageActionItem "Don't"])
      _ <- sendRequest SWindowShowMessageRequest params $ \res ->
        case res of
          Right (Just (MessageActionItem "Turn on")) -> do
            let regOpts = CodeLensRegistrationOptions Nothing Nothing (Just False)

            _ <- registerCapability STextDocumentCodeLens regOpts $ \_req responder -> do
              let cmd = Command "Say hello" "lsp-hello-command" Nothing
                  rsp = List [CodeLens (mkRange 0 0 0 100) (Just cmd) Nothing]
              responder (Right rsp)
            pure ()
          Right _ ->
            sendNotification SWindowShowMessage (ShowMessageParams MtInfo "Not turning on code lenses")
          Left err ->
            sendNotification SWindowShowMessage (ShowMessageParams MtError $ "Something went wrong!\n" <> T.pack (show err))
      pure ()
  , requestHandler STextDocumentHover $ \req responder -> do
      let RequestMessage _ _ _ (HoverParams _doc pos _workDone) = req
          Position _l _c' = pos
          rsp = Hover ms (Just range)
          ms = HoverContents $ markedUpContent "lsp-demo-simple-server" "Hello world"
          range = Range pos pos
      responder (Right $ Just rsp)
  ]

main :: IO Int
main = runServer $ ServerDefinition
  { onConfigurationChange = const $ pure $ Right ()
  , doInitialize = \env _req -> pure $ Right env
  , staticHandlers = handlers
  , interpretHandler = \env -> Iso (runLspT env) liftIO
  , options = defaultOptions
  }
