{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE UndecidableInstances      #-}
module Perst.Database.RefDef
    ( RefDef'(..), RefDef
    , RdTabFrom, RdFldsFrom, RdTabTo, RdFldsTo

    ) where

import           Data.Kind               (Type)
import           Data.Singletons.Prelude
import           Data.Singletons.TH      (singletons)
import           GHC.TypeLits            (Symbol)
import           Perst.Database.DataDef  (AllKeys, DataDef', DdFlds)
import           Perst.Types             (IsNub, IsSub)

singletons [d|

  data RefDef' s t = RefDef
    { rdTabFrom  :: DataDef' s t
    , rdFldsFrom :: [s]
    , rdTabTo    :: DataDef' s t
    , rdFldsTo   :: [s]
    }
  |]

type RefDef = RefDef' Symbol Type

type RefDefConstr (rd :: RefDef) =
  ( IsSub (RdFldsFrom rd) (DdFlds (RdTabFrom rd)) ~ True
  , IsNub (RdFldsFrom rd) ~ True
  , Elem (RdFldsTo rd) (AllKeys (RdTabTo rd)) ~ True
  )
