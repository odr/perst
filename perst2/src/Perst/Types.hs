{-# LANGUAGE ConstraintKinds            #-}
-- {-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE UndecidableInstances       #-}
module Perst.Types where
import           Data.Aeson              (FromJSON, ToJSON)
import           Data.Singletons.Prelude
import           Data.Type.Bool          (If)
-- import           GHC.Generics            (Generic)

import           Data.Type.GrecTree

newtype PChilds a = PChilds { unPChilds :: [a] }
  deriving (Eq,Show,Ord,Functor,Monad,Applicative,Traversable,Foldable,Monoid,FromJSON,ToJSON)

instance Convert (PChilds a) [b] where
  convert _ = []

instance Convert [b] (PChilds a, [b]) where
  convert = (PChilds [],)

instance SConvNames AllFld ms (PChilds a)

type family IsList t where
  IsList (PChilds a) = 'True
  IsList a = 'False

data LstFld
data NoLstFld

instance SConvNames (If (IsList a) EmptyFld AllFld) ms a
      => SConvNames NoLstFld ms a where
  type SFldNames NoLstFld ms a = SFldNames (If (IsList a) EmptyFld AllFld) ms a
  type SFldTypes NoLstFld ms a = SFldTypes (If (IsList a) EmptyFld AllFld) ms a

instance SConvNames (If (IsList a) AllFld EmptyFld) ms a
      => SConvNames LstFld ms a where
  type SFldNames LstFld ms a = SFldNames (If (IsList a) AllFld EmptyFld) ms a
  type SFldTypes LstFld ms a = SFldTypes (If (IsList a) AllFld EmptyFld) ms a

type Fsts rs = Map FstSym0 rs
type Snds rs = Map SndSym0 rs
