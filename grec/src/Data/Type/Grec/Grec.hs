{-# LANGUAGE UndecidableInstances #-}
module Data.Type.Grec.Grec( Grec(..), grec, GrecF) where

import           Data.Bifunctor                    (bimap)
import           Data.Kind                         (Type)
import           Data.Type.Grec.Convert            (Convert (..))
import           Data.Type.Grec.ConvList           (ConvList (..))
import           GHC.Generics                      (Generic, Rep, from, to)
import           GHC.TypeLits                      (Symbol)

import           Data.Type.Grec.Internal.GGrecList (GListFromGrec (..),
                                                    GListToGrec (..))
import           Data.Type.Grec.Type               (Fields)

newtype Grec a (fs :: [(Symbol,Type)]) = Grec { unGrec :: a } deriving (Eq, Show)

type GrecF a = Grec a (Fields a)

grec :: a -> GrecF a
grec = Grec

instance (GListToGrec a (Rep r), Generic r)
    => Convert (ConvList a) (ConvList a, Grec r fs) where
  convert = bimap ConvList (Grec . to) . gListToGrec . unConvList

instance (GListFromGrec a (Rep r), Generic r)
    => Convert (ConvList a, Grec r fs) (ConvList a) where
  convert (x,y)= x `mappend` ConvList (gListFromGrec $ from $ unGrec y)
