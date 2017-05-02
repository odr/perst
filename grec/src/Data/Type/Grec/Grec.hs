{-# LANGUAGE ConstraintKinds #-}
module Data.Type.Grec.Grec( Grec(..)) where

import           Data.Type.Grec.Convert  (Convert (..))
import           Data.Type.Grec.ConvList (ConvList (..))
import           GHC.Generics

newtype Grec a = Grec { unGrec :: a } deriving (Eq, Show)

type ToGrecConstr a r = (GToGrec a (Rep r))
type FromGrecConstr r a = (GFromGrec a (Rep r))

instance (ToGrecConstr a r, Generic r) => Convert (ConvList a) (Grec r) where
  convert = Grec . to . snd . gToGrec . unConvList

instance (FromGrecConstr r a, Generic r) => Convert (Grec r) (ConvList a) where
  convert = ConvList . gFromGrec . from . unGrec

-------------------------------------------------------

class GToGrec a g where
  gToGrec :: [a] -> ([a], g b)

instance Convert a b => GToGrec a (S1 c (Rec0 b)) where
  gToGrec []     = error "Not enough values to convert in 'GToGrec a b'"
  gToGrec (a:as) = (as, M1 . K1 $ convert a)

instance (GToGrec a x, GToGrec a y) => GToGrec a (x :*: y) where
  gToGrec xs = let (r,x) = gToGrec xs in (x :*:) <$> gToGrec r

instance GToGrec a b => GToGrec a (D1 (MetaData md1 md2 md3 False) (C1 mc b)) where
  gToGrec = fmap (M1 . M1) . gToGrec

instance (Generic b, GToGrec a (Rep b))
    => GToGrec a (D1 (MetaData md1 md2 md3 True) (C1 mc (S1 c (Rec0 b)))) where
      -- newtype => convert internal type
  gToGrec = fmap (M1 . M1 . M1 . K1 . to) . gToGrec


class GFromGrec a g where
  gFromGrec :: g b -> [a]

instance Convert b a => GFromGrec a (S1 c (K1 i b)) where
  gFromGrec (M1 (K1 b)) = [convert b]

instance (GFromGrec a x, GFromGrec a y) => GFromGrec a (x :*: y) where
  gFromGrec (x :*: y) = gFromGrec x ++ gFromGrec y

instance GFromGrec a b => GFromGrec a (D1 (MetaData md1 md2 md3 False) (C1 mc b)) where
  gFromGrec (M1 (M1 b)) = gFromGrec b

instance (Generic b, GFromGrec a (Rep b))
    => GFromGrec a (D1 (MetaData md1 md2 md3 True) (C1 mc (S1 c (K1 i b)))) where
      -- newtype => convert internal type
  gFromGrec (M1 (M1 (M1 (K1 b)))) = gFromGrec $ from b
