{-# LANGUAGE OverloadedStrings #-}

module Plugins where

-- fixed plugins
-- import           Ide.Plugin.Example        as Example
-- import           Ide.Plugin.Example2       as Example2
import Development.IDE (IdeState)
import Development.IDE.Plugin.HLS.GhcIde as GhcIde
import Ide.PluginUtils (pluginDescToIdePlugins)
import Ide.Types (IdePlugins)
import qualified ELP

-- ---------------------------------------------------------------------
-- packages providing plugins

-- import           Ide.Plugin.Class          as Class

-- ---------------------------------------------------------------------

-- | The plugins configured for use in this instance of the language
-- server.
-- These can be freely added or removed to tailor the available
-- features of the server.
idePlugins :: Bool -> IdePlugins IdeState
idePlugins includeExamples = pluginDescToIdePlugins allPlugins
  where
    allPlugins =
      if includeExamples
        then basePlugins ++ examplePlugins
        else basePlugins
    basePlugins =
      ELP.hlsPlugin "elp":
      -- GhcIde.descriptors
      --   ++
      -- Pragmas.descriptor  "pragmas" :
        []
    examplePlugins =
      []

-- [Example.descriptor  "eg"
-- ]
