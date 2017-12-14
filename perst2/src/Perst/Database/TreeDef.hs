{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE UndecidableInstances      #-}

module Perst.Database.TreeDef where

import           Data.Singletons.Prelude       (Lookup)
import           Data.Singletons.Prelude.Maybe (FromJust)
import           Data.Singletons.TH            (singletons)
import           GHC.TypeLits                  (Symbol)

import           Perst.Database.DataDef        (DataDef', DdKey)

type AppCons f = (Applicative f, Traversable f)

singletons [d|
  data TreeDef' s = TreeDefC
    { tdData :: DataDef' s
    , tdChilds :: [(s, (TreeDef' s, [(s,s)]))]
    }
  |]

type TreeDef = TreeDef' Symbol
type TopKey t    = DdKey (TdData t)
type MbChild s t = Lookup s (TdChilds t)
type Child s t   = FromJust (MbChild s t)

type ToTreeDef t = TreeDefC t '[]
