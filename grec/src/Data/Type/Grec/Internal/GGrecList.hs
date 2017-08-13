{-# LANGUAGE UndecidableInstances #-}
module Data.Type.Grec.Internal.GGrecList
  ( GListToGrec(..)
  , GListFromGrec(..)
  ) where

import           Data.Bifunctor         (second)
import           Data.Tagged            (Tagged (..), untag)
import           Data.Type.Grec.Convert (ConvType, Convert (..))
import           Data.Type.Grec.Type    (GrecGroup (..))
import           GHC.Generics

-- GListToGrec and GListFromGrec are devided into two classes
-- (instead of class with two functions)
-- because of some differences in condition
-- (<Convert a b> vs <Convert b a> and <Monoid m> vs nothing)

class GListToGrec a g where
  gListToGrec :: [a] -> ([a], g b)
class GListFromGrec a g where
  gListFromGrec :: g b -> [a]

class GListToGrec' a b where
  gListToGrec' :: [a] -> ([a], b)
class GListFromGrec' a b where
  gListFromGrec' :: b -> [a]

---------------------------------------
instance Monoid b => GListToGrec' a (Tagged 0 b) where
  gListToGrec' as  = (as, Tagged mempty)
instance GListFromGrec' a (Tagged 0 b) where
  gListFromGrec' _ = []

instance Convert a b => GListToGrec' a (Tagged 1 b) where
  gListToGrec' []     = error "Not enough values to convert in 'GListToGrec a b'"
  gListToGrec' (a:as) = (as, Tagged $ convert a)
instance Convert b a => GListFromGrec' a (Tagged 1 b) where
  gListFromGrec' b    = [convert $ untag b]

instance GListToGrec' a b => GListToGrec' a (Tagged 2 b) where
  gListToGrec' = second Tagged . gListToGrec'
instance GListFromGrec' a b => GListFromGrec' a (Tagged 2 b) where
  gListFromGrec' = gListFromGrec' . unTagged

instance (Generic b, GListToGrec a (Rep b)) => GListToGrec' a (GrecGroup b) where
  gListToGrec' = second (GrecGroup . to) . gListToGrec
instance (Generic b, GListFromGrec a (Rep b)) => GListFromGrec' a (GrecGroup b) where
  gListFromGrec' = gListFromGrec . from . getGrecGroup
---------------------------------------

instance GListToGrec' a (Tagged (ConvType b) b)
    => GListToGrec a (S1 c (Rec0 b)) where
  gListToGrec as = (M1 . K1 . untag)
                <$> (gListToGrec' as :: ([a],Tagged (ConvType b) b))
instance GListFromGrec' a (Tagged (ConvType b) b)
    => GListFromGrec a (S1 c (Rec0 b)) where
  gListFromGrec (M1 (K1 b)) = gListFromGrec' (Tagged b :: Tagged (ConvType b) b)

instance (GListToGrec a x, GListToGrec a y) => GListToGrec a (x :*: y) where
  gListToGrec xs = let (r,x) = gListToGrec xs in (x :*:) <$> gListToGrec r
instance (GListFromGrec a x, GListFromGrec a y)
    => GListFromGrec a (x :*: y) where
  gListFromGrec (x :*: y) = gListFromGrec x ++ gListFromGrec y

instance GListToGrec a b
    => GListToGrec a (D1 (MetaData md1 md2 md3 False) (C1 mc b)) where
  gListToGrec = fmap (M1 . M1) . gListToGrec
instance GListFromGrec a b
    => GListFromGrec a (D1 (MetaData md1 md2 md3 False) (C1 mc b)) where
  gListFromGrec (M1 (M1 b)) = gListFromGrec b

-- newtype => convert internal type
instance (Generic b, GListToGrec a (Rep b))
    => GListToGrec a (D1 (MetaData md1 md2 md3 True) (C1 mc (S1 c (Rec0 b))))
    where
  gListToGrec = fmap (M1 . M1 . M1 . K1 . to) . gListToGrec
instance (Generic b, GListFromGrec a (Rep b))
    => GListFromGrec a (D1 (MetaData md1 md2 md3 True) (C1 mc (S1 c (Rec0 b))))
    where
  gListFromGrec (M1 (M1 (M1 (K1 b)))) = gListFromGrec $ from b
