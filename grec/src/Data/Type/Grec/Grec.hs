{-# LANGUAGE UndecidableInstances #-}
module Data.Type.Grec.Grec( Grec(..)) where

import           Data.Bifunctor                    (bimap)
import           Data.Kind                         (Type)
import           Data.Type.Grec.Convert            (Convert (..))
import           Data.Type.Grec.ConvList           (ConvList (..))
import           GHC.Generics                      (Generic, Rep, from, to)
import           GHC.TypeLits                      (Symbol)

import           Data.Type.Grec.Internal.GGrecList (GListFromGrec (..),
                                                    GListToGrec (..))
import           Data.Type.Grec.Type               (Fields)

newtype Grec a = Grec { unGrec :: a } deriving (Eq, Show)

instance (GListToGrec a (Rep r), Generic r)
    => Convert (ConvList a) (ConvList a, Grec r) where
  convert = bimap ConvList (Grec . to) . gListToGrec . unConvList

instance (GListFromGrec a (Rep r), Generic r)
    => Convert (ConvList a, Grec r) (ConvList a) where
  convert (x,y) = x `mappend` ConvList (gListFromGrec $ from $ unGrec y)
