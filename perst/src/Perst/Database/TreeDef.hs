{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE UndecidableInstances      #-}
module Perst.Database.TreeDef
    -- (
    -- )
    where

import           Data.Singletons.Prelude
import           Data.Singletons.TH
import           Perst.Database.DataDef
import Data.Kind(Type)

singletons [d|
  data TreeDef' s t = TreeDef
    { tdData    :: DataDef' s t
    , tdChilds  :: [(TreeDef' s t, [(s,s)])]
    , tdrChilds :: [(s,t)]
    , tdfChilds :: [s]
    }

  |]

type TreeDef = TreeDef' Symbol Type

-- type TreeD v p u = TableD' (Typ v) (FieldsGrec v) p u
--
-- type family TableD' n r p u where
--   TableD' n r p u = TableDef n r (Map FstSym0 r) p u
