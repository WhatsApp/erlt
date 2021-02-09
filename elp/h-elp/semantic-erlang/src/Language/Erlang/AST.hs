{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -ddump-splices -ddump-to-file -dsuppress-uniques #-}

module Language.Erlang.AST
( module Language.Erlang.AST
, Erlang.getTestCorpusDir
) where

import           AST.GenerateSyntax
import           AST.Token
import           Language.Haskell.TH.Syntax (runIO)
import qualified TreeSitter.ErlangELP as Erlang (getNodeTypesPath, getTestCorpusDir, tree_sitter_erlang_elp)
import Prelude ((>>=))

#ifdef NODE_TYPES_PATH
astDeclarationsForLanguage Erlang.tree_sitter_erlang_elp NODE_TYPES_PATH
#else
runIO Erlang.getNodeTypesPath >>= astDeclarationsForLanguage Erlang.tree_sitter_erlang_elp
#endif
