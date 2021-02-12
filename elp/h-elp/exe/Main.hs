{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import ELP.Ide.Arguments
  ( Arguments (..),
    LspArguments (..),
    getArguments,
  )
import ELP.Ide.Main (defaultMain)
import Main.Utf8 (withUtf8)
import Plugins

main :: IO ()
main = withUtf8 $ do
  args <- getArguments "haskell-language-server"

  let withExamples =
        case args of
          LspMode LspArguments {..} -> argsExamplePlugin
          _ -> False

  defaultMain args (idePlugins withExamples)
