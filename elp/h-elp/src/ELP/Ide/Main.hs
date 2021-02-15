{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
-- modelled on haskell-language-server IDE.Main

{-# LANGUAGE CPP #-} -- To get precise GHC version
{-# OPTIONS_GHC -Wno-dodgy-imports #-} -- GHC no longer exports def in GHC 8.6 and above
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

module ELP.Ide.Main(defaultMain, runLspMode) where

import Control.Concurrent.Extra
import Control.Monad.Extra
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Exception.Safe
import Data.Default
import Data.List.Extra
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Development.IDE.Core.Debouncer
import Development.IDE.Core.FileStore
import Development.IDE.Core.OfInterest
import Development.IDE.Core.RuleTypes
import Development.IDE.Core.Rules
import Development.IDE.Core.Service
import Development.IDE.Core.Shake
import Development.IDE.LSP.LanguageServer
import Development.IDE.LSP.Protocol
import Development.IDE.Plugin
import Development.IDE.Plugin.HLS
import Development.IDE.Session (loadSession, findCradle, defaultLoadingOptions, setInitialDynFlags, getHieDbLoc, runWithDb)
import Development.IDE.Types.Diagnostics
import Development.IDE.Types.Location
import Development.IDE.Types.Logger as G
import Development.IDE.Types.Options
import qualified Language.LSP.Server as LSP
import ELP
import ELP.Ide.Arguments
import Ide.Logger
import ELP.Ide.Version
import Ide.Plugin.Config
import Ide.PluginUtils
import Ide.Types (IdePlugins, ipMap)
import Language.LSP.Types
import qualified System.Directory.Extra as IO
import System.Exit
import System.FilePath
import System.IO
import qualified System.Log.Logger as L
import System.Time.Extra
import Development.Shake (ShakeOptions (shakeThreads), action, Rules(..), Action, par)
import TreeSitter.ErlangELP
import TreeSitter.Parser
-- import HieDb.Run

ghcIdePlugins :: T.Text -> IdePlugins IdeState -> (Plugin Config, [T.Text])
ghcIdePlugins pid ps = (asGhcIdePlugin ps, allLspCmdIds' pid ps)

defaultMain :: Arguments -> IdePlugins IdeState -> IO ()
defaultMain args idePlugins = do
    -- WARNING: If you write to stdout before runLanguageServer
    --          then the language server will not work

    hlsVer <- haskellLanguageServerVersion
    case args of
        ProbeToolsMode -> do
            programsOfInterest <- findProgramVersions
            putStrLn hlsVer
            putStrLn "Tool versions found on the $PATH"
            putStrLn $ showProgramVersionOfInterest programsOfInterest

        VersionMode PrintVersion ->
            putStrLn hlsVer

        VersionMode PrintNumericVersion ->
            putStrLn haskellLanguageServerNumericVersion

        LspMode lspArgs -> do
            {- see WARNING above -}
            hPutStrLn stderr hlsVer
            runLspMode lspArgs idePlugins

-- ---------------------------------------------------------------------

hlsLogger :: G.Logger
hlsLogger = G.Logger $ \pri txt ->
    case pri of
      G.Telemetry -> logm     (T.unpack txt)
      G.Debug     -> debugm   (T.unpack txt)
      G.Info      -> logm     (T.unpack txt)
      G.Warning   -> warningm (T.unpack txt)
      G.Error     -> errorm   (T.unpack txt)

-- ---------------------------------------------------------------------

-- deriving instance Show (Plugin Config)

-- ---------------------------------------------------------------------

runLspMode :: LspArguments -> IdePlugins IdeState -> IO ()
runLspMode lspArgs@LspArguments{argsCwd} idePlugins = do
    whenJust argsCwd IO.setCurrentDirectory
    dir <- IO.getCurrentDirectory
    dbLoc <- getHieDbLoc dir
    runWithDb dbLoc $ runLspMode' lspArgs idePlugins

runLspMode' :: LspArguments -> IdePlugins IdeState -> HieDb -> IndexQueue -> IO ()
runLspMode' lspArgs@LspArguments{..} idePlugins hiedb hiechan = do
    LSP.setupLogger argsLogFile ["hls", "h-elp", "hie-bios"]
      $ if argsDebugOn then L.DEBUG else L.INFO

    -- lock to avoid overlapping output on stdout
    lock <- newLock
    let logger p = Logger $ \pri msg -> when (pri >= p) $ withLock lock $
            T.putStrLn $ T.pack ("[" ++ upper (show pri) ++ "] ") <> msg

    dir <- IO.getCurrentDirectory

    pid <- T.pack . show <$> getProcessID
    let
        (plugins, commandIds) = ghcIdePlugins pid idePlugins
        options = def { LSP.executeCommandCommands = Just commandIds
                      , LSP.completionTriggerCharacters = Just "."
                      }

    if argLSP then do
        t <- offsetTime
        hPutStrLn stderr "Starting (ELP)LSP server..."
        hPutStrLn stderr $ "  with arguments: " <> show lspArgs
        hPutStrLn stderr $ "  with plugins: " <> show (Map.keys $ ipMap idePlugins)
        hPutStrLn stderr $ "  in directory: " <> dir
        hPutStrLn stderr "If you are seeing this in a terminal, you probably should have run WITHOUT the --lsp option!"
        hPutStrLn stderr "hello there"

        withParser tree_sitter_erlang_elp $ \parser -> do
            -- ts_parser_log_to_stderr parser
            runLanguageServer options getConfigFromNotification (pluginHandlers plugins) $ \env vfs _rootPath -> do
                t <- t
                hPutStrLn stderr $ "Started LSP server in " ++ showDuration t

                _libdir <- setInitialDynFlags
                              `catchAny` (\e -> (hPutStrLn stderr $ "setInitialDynFlags: " ++ displayException e) >> pure Nothing)
                -- sessionLoader <- loadSession dir
                caps <- LSP.runLspT env LSP.getClientCapabilities
                -- config <- fromMaybe defaultLspConfig <$> getConfig
                let options = defOptions
                        { optReportProgress = clientSupportsProgress caps
                        , optShakeProfiling = argsShakeProfiling
                        , optTesting        = IdeTesting argsTesting
                        , optShakeOptions   = (optShakeOptions defOptions){shakeThreads = argsThreads}
                        }
                    defOptions = defaultIdeOptions elpSessionLoader
                debouncer <- newAsyncDebouncer
                -- initialise (mainRule >> pluginRules plugins >> action kick)
                cfg <- initialise (elpMainRule >> pluginRules plugins >> action elpKick)
                    (Just env) hlsLogger debouncer options vfs
                    hiedb hiechan
                pc <- newMVar (ParserContext parser mempty)
                addIdeGlobalExtras (shakeExtras cfg) pc
                return cfg
    else do
        -- GHC produces messages with UTF8 in them, so make sure the terminal doesn't error
        hSetEncoding stdout utf8
        hSetEncoding stderr utf8

        putStrLn $ "(haskell-language-server)Ghcide setup tester in " ++ dir ++ "."
        putStrLn "Report bugs at https://github.com/haskell/haskell-language-server/issues"
        programsOfInterest <- findProgramVersions
        putStrLn ""
        putStrLn "Tool versions found on the $PATH"
        putStrLn $ showProgramVersionOfInterest programsOfInterest

        putStrLn $ "\nStep 1/4: Finding files to test in " ++ dir
        files <- expandFiles (argFiles ++ ["." | null argFiles])
        -- LSP works with absolute file paths, so try and behave similarly
        files <- nubOrd <$> mapM IO.canonicalizePath files
        putStrLn $ "Found " ++ show (length files) ++ " files"

        putStrLn "\nStep 2/4: Looking for hie.yaml files that control setup"
        cradles <- mapM (findCradle defaultLoadingOptions) files
        let ucradles = nubOrd cradles
        let n = length ucradles
        putStrLn $ "Found " ++ show n ++ " cradle" ++ ['s' | n /= 1]
        putStrLn "\nStep 3/4: Initializing the IDE"
        vfs <- makeVFSHandle
        debouncer <- newAsyncDebouncer
        let dummyWithProg _ _ f = f (const (pure ()))
        sessionLoader <- loadSession dir
        ide <- initialise mainRule Nothing (logger Info) debouncer (defaultIdeOptions sessionLoader) vfs hiedb hiechan

        putStrLn "\nStep 4/4: Type checking the files"
        setFilesOfInterest ide $ HashMap.fromList $ map ((, OnDisk) . toNormalizedFilePath') files
        results <- runAction "User TypeCheck" ide $ uses TypeCheck (map toNormalizedFilePath' files)
        let (worked, failed) = partition fst $ zip (map isJust results) files
        when (failed /= []) $
            putStr $ unlines $ "Files that failed:" : map ((++) " * " . snd) failed

        let files xs = let n = length xs in if n == 1 then "1 file" else show n ++ " files"
        putStrLn $ "\nCompleted (" ++ files worked ++ " worked, " ++ files failed ++ " failed)"
        unless (null failed) (exitWith $ ExitFailure (length failed))

expandFiles :: [FilePath] -> IO [FilePath]
expandFiles = concatMapM $ \x -> do
    b <- IO.doesFileExist x
    if b then return [x] else do
        let recurse "." = True
            recurse x | "." `isPrefixOf` takeFileName x = False -- skip .git etc
            recurse x = takeFileName x `notElem` ["dist","dist-newstyle"] -- cabal directories
        files <- filter (\x -> takeExtension x `elem` [".hs",".lhs"]) <$> IO.listFilesInside (return . recurse) x
        when (null files) $
            fail $ "Couldn't find any .hs/.lhs files inside directory: " ++ x
        return files

-- | Print an LSP event.
showEvent :: Lock -> FromServerMessage -> IO ()
showEvent _ (EventFileDiagnostics _ []) = return ()
showEvent lock (EventFileDiagnostics (toNormalizedFilePath' -> file) diags) =
    withLock lock $ T.putStrLn $ showDiagnosticsColored $ map (file,ShowDiag,) diags
showEvent lock e = withLock lock $ print e

-- ---------------------------------------------------------------------

-- | A rule that wires per-file rules together
elpMainRule :: Rules ()
elpMainRule = do
    -- linkables <- liftIO $ newVar emptyModuleEnv
    -- addIdeGlobal $ CompiledLinkables linkables
    getParsedModuleRule
    getParsedModuleWithCommentsRule
    getLocatedImportsRule
    getDependencyInformationRule
    reportImportCyclesRule
    getDependenciesRule
    typeCheckRule
    getDocMapRule
    loadGhcSession -- Needed to trigger our GetTreeSitterDiagnostics rule
    getModIfaceFromDiskRule
    -- getModIfaceFromDiskAndIndexRule
    getModIfaceRule
    getModIfaceWithoutLinkableRule
    getModSummaryRule
    isHiFileStableRule
    getModuleGraphRule
    knownFilesRule
    getClientSettingsRule
    getHieAstsRule
    getBindingsRule
    needsCompilationRule
    generateCoreRule
    getImportMapRule
    -- getAnnotatedParsedSourceRule
    -- persistentHieFileRule
    -- persistentDocMapRule
    -- persistentImportMapRule
-- ---------------------------------------------------------------------

{-
-- | Typecheck all the files of interest.
--   Could be improved
elpKick :: Action ()
elpKick = do
    files <- HashMap.keys <$> getFilesOfInterest
    ShakeExtras{progressUpdate} <- getShakeExtras
    liftIO $ progressUpdate KickStarted

    -- -- Update the exports map for FOIs
    -- (results, ()) <- par (uses GenerateCore files) (void $ uses GetHieAst files)
    -- (results, ()) <- par (uses GenerateCore files) (void $ uses GetHieAst files)
    _r <- uses GetTreeSitterDiagnostics files

    -- -- Update the exports map for non FOIs
    -- -- We can skip this if checkProject is True, assuming they never change under our feet.
    -- IdeOptions{ optCheckProject = doCheckProject } <- getIdeOptions
    -- checkProject <- liftIO $ doCheckProject
    -- ifaces <- if checkProject then return Nothing else runMaybeT $ do
    --     deps <- MaybeT $ sequence <$> uses GetDependencies files
    --     hiResults <- lift $ uses GetModIface (nubOrd $ foldMap transitiveModuleDeps deps)
    --     return $ map hirModIface $ catMaybes hiResults

    -- ShakeExtras{exportsMap} <- getShakeExtras
    -- let mguts = catMaybes results
    --     !exportsMap' = createExportsMapMg mguts
    --     !exportsMap'' = maybe mempty createExportsMap ifaces
    -- liftIO $ modifyVar_ exportsMap $ evaluate . (exportsMap'' <>) . (exportsMap' <>)


    liftIO $ progressUpdate KickCompleted

-}
-- ---------------------------------------------------------------------

elpSessionLoader :: Action IdeGhcSession
elpSessionLoader = return (IdeGhcSession loader 0)
  where
    diag = textDiagnostic "elpSessionLoader NOP"
    loader file = return (([(toNormalizedFilePath file, ShowDiag, diag)]
                          ,Nothing),[])

-- ---------------------------------------------------------------------

