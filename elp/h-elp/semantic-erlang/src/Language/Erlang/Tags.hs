{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Language.Erlang.Tags
( ToTags(..)
) where

import           AST.Element
import           AST.Token
import           AST.Traversable1
import           Control.Effect.Reader
import           Control.Effect.Writer
import qualified Language.Erlang.AST as Erlang
import           Source.Loc
import           Source.Source as Source
import           Tags.Tag()
import qualified Tags.Tagging.Precise as Tags

class ToTags t where
  tags
    :: ( Has (Reader Source) sig m
       , Has (Writer Tags.Tags) sig m
       )
    => t Loc
    -> m ()
  default tags
    :: ( Has (Reader Source) sig m
       , Has (Writer Tags.Tags) sig m
       , Traversable1 ToTags t
       )
    => t Loc
    -> m ()
  tags = gtags

instance (ToTags l, ToTags r) => ToTags (l :+: r) where
  tags (L1 l) = tags l
  tags (R1 r) = tags r

instance ToTags (Token sym n) where tags _ = pure ()

gtags
  :: ( Has (Reader Source) sig m
     , Has (Writer Tags.Tags) sig m
     , Traversable1 ToTags t
     )
  => t Loc
  -> m ()
gtags = traverse1_ @ToTags (const (pure ())) tags

instance ToTags Erlang.Attribute
instance ToTags Erlang.Atom
instance ToTags Erlang.ArgList
instance ToTags Erlang.FunctionClause
instance ToTags Erlang.Function
instance ToTags Erlang.Float
instance ToTags Erlang.Integer
instance ToTags Erlang.String
instance ToTags Erlang.Var
instance ToTags Erlang.Wildcard
instance ToTags Erlang.Block
instance ToTags Erlang.ModuleAttribute
instance ToTags Erlang.Char
instance ToTags Erlang.SourceFile
