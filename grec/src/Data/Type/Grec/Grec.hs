module Data.Type.Grec.Grec( Grec(..)) where

-- import           Data.Bifunctor                    (first)
import           Data.Type.Grec.Convert            (Convert (..))
-- import           Data.Type.Grec.ConvList           (ConvList (..))
import           GHC.Generics                      (Generic, Rep, from, to)

import           Data.Type.Grec.Internal.GGrecList (GListFromGrec (..),
                                                    GListToGrec (..))

newtype Grec a = Grec { unGrec :: a } deriving (Eq, Show, Ord)

instance (GListToGrec a (Rep r), Generic r) => Convert [a] ([a], Grec r) where
  convert = fmap (Grec . to) . gListToGrec

instance (GListFromGrec a (Rep r), Generic r)
    => Convert (Grec r) [a] where
  convert = gListFromGrec . from . unGrec
