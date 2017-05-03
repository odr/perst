{-# LANGUAGE UndecidableInstances #-}
module Data.Type.Grec.Grec( Grec(..)) where

import           Data.Type.Grec.Convert            (Convert (..))
import           Data.Type.Grec.ConvList           (ConvList (..))
import           Data.Type.Grec.ConvTree           (ConvTree (..))
import           GHC.Generics                      (Generic, Rep (..))

import           Data.Type.Grec.Internal.GGrecList (GListFromGrec (..),
                                                    GListToGrec (..))
import           Data.Type.Grec.Internal.GGrecTree (GTreeFromGrec (..),
                                                    GTreeToGrec (..))

newtype Grec a = Grec { unGrec :: a } deriving (Eq, Show)

instance (GListToGrec a (Rep r), Generic r) => Convert (ConvList a) (Grec r) where
  convert = Grec . to . snd . gListToGrec . unConvList

instance (GListFromGrec a (Rep r), Generic r) => Convert (Grec r) (ConvList a) where
  convert = ConvList . gListFromGrec . from . unGrec

instance (GTreeToGrec a (Rep r), Generic r) => Convert (ConvTree a) r where
  convert = to . snd . gTreeToGrec

instance (GTreeFromGrec a (Rep r), Generic r) => Convert r (ConvTree a) where
  convert = gTreeFromGrec . from
