module Data.Type.Grec.Internal.GGrecList where

import           GHC.Generics

import           Data.Type.Grec.Convert (Convert (..))


class GListToGrec a g where
  gListToGrec :: [a] -> ([a], g b)

instance Convert a b => GListToGrec a (S1 c (Rec0 b)) where
  gListToGrec []     = error "Not enough values to convert in 'GListToGrec a b'"
  gListToGrec (a:as) = (as, M1 . K1 $ convert a)

instance (GListToGrec a x, GListToGrec a y) => GListToGrec a (x :*: y) where
  gListToGrec xs = let (r,x) = gListToGrec xs in (x :*:) <$> gListToGrec r

instance GListToGrec a b => GListToGrec a (D1 (MetaData md1 md2 md3 False) (C1 mc b)) where
  gListToGrec = fmap (M1 . M1) . gListToGrec

instance (Generic b, GListToGrec a (Rep b))
    => GListToGrec a (D1 (MetaData md1 md2 md3 True) (C1 mc (S1 c (Rec0 b)))) where
      -- newtype => convert internal type
  gListToGrec = fmap (M1 . M1 . M1 . K1 . to) . gListToGrec


class GListFromGrec a g where
  gListFromGrec :: g b -> [a]

instance Convert b a => GListFromGrec a (S1 c (K1 i b)) where
  gListFromGrec (M1 (K1 b)) = [convert b]

instance (GListFromGrec a x, GListFromGrec a y) => GListFromGrec a (x :*: y) where
  gListFromGrec (x :*: y) = gListFromGrec x ++ gListFromGrec y

instance GListFromGrec a b => GListFromGrec a (D1 (MetaData md1 md2 md3 False) (C1 mc b)) where
  gListFromGrec (M1 (M1 b)) = gListFromGrec b

instance (Generic b, GListFromGrec a (Rep b))
    => GListFromGrec a (D1 (MetaData md1 md2 md3 True) (C1 mc (S1 c (K1 i b)))) where
      -- newtype => convert internal type
  gListFromGrec (M1 (M1 (M1 (K1 b)))) = gListFromGrec $ from b
