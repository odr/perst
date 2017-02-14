{-# LANGUAGE UndecidableInstances #-}
module Data.Type.Grec.Convert(Convert(..), toGrec, fromGrec) where

import           GHC.Generics

class Convert a b where
  convert :: a -> b

toGrec :: (Generic b, GToGrec a (Rep b)) => [a] -> b
toGrec xs = to $ gToGrec xs

class GToGrec a g where
  gToGrec :: [a] -> g b

instance Convert a b => GToGrec a (S1 c (K1 i b)) where
  gToGrec []    = error "Not enough values to convert in 'GToGrec a b'"
  gToGrec (a:_) = M1 . K1 $ convert a

instance (GToGrec a x, GToGrec a y) => GToGrec a (x :*: y) where
  gToGrec [] = error "Not enough values to convert in 'GToGrec a (x :*: y)'"
  gToGrec (a:as) = gToGrec [a] :*: gToGrec as

instance GToGrec a b => GToGrec a (D1 (MetaData md1 md2 md3 False) (C1 mc b)) where
  gToGrec as = M1 . M1 $ gToGrec as

instance (Generic b, GToGrec a (Rep b))
    => GToGrec a (D1 (MetaData md1 md2 md3 True) (C1 mc (S1 c (K1 i b)))) where
  gToGrec as = M1 . M1 . M1 . K1 $ toGrec as

fromGrec :: (Generic b, GFromGrec a (Rep b)) => b -> [a]
fromGrec = gFromGrec . from

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
  gFromGrec (M1 (M1 (M1 (K1 b)))) = fromGrec b
