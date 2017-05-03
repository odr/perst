module Data.Type.Grec.Internal.GGrecTree where

import           Data.Semigroup          ((<>))
-- import           Data.Tree
import           GHC.Generics

import           Data.Type.Grec.Convert  (Convert (..))
import           Data.Type.Grec.ConvTree (ConvTree (..))


class GTreeToGrec a g where
  gTreeToGrec :: ConvTree a -> (ConvTree a, g b)

instance {-# OVERLAPPABLE #-} Convert a b => GTreeToGrec a (S1 c (Rec0 b)) where
  gTreeToGrec (ConvTree [] _)
    = error "Error in 'GTreeToGrec a b'. There is not enough attributes"
  gTreeToGrec (ConvTree (a:as) ts) = (ConvTree as ts, M1 . K1 $ convert a)

-- string ?
instance {-# OVERLAPPING #-} Convert (ConvTree a) b
    => GTreeToGrec a (S1 c (Rec0 [b])) where
  gTreeToGrec (ConvTree _ [])
    = error "Error in 'GTreeToGrec a b'. There is not enough childs."
  gTreeToGrec (ConvTree as (t:ts)) = (ConvTree as ts, M1 . K1 $ map convert t)

instance {-# OVERLAPS #-} Convert a [Char]
    => GTreeToGrec a (S1 c (Rec0 [Char])) where
  gTreeToGrec (ConvTree [] _)
    = error "Error in 'GTreeToGrec a b'. There is not enough attributes"
  gTreeToGrec (ConvTree (a:as) ts) = (ConvTree as ts, M1 . K1 $ convert a)

instance (GTreeToGrec a x, GTreeToGrec a y) => GTreeToGrec a (x :*: y) where
  gTreeToGrec t = let (r,x) = gTreeToGrec t in (x :*:) <$> gTreeToGrec r

instance GTreeToGrec a b
    => GTreeToGrec a (D1 (MetaData md1 md2 md3 False) (C1 mc b)) where
  gTreeToGrec = fmap (M1 . M1) . gTreeToGrec

instance (Generic b, GTreeToGrec a (Rep b))
    => GTreeToGrec a (D1 (MetaData md1 md2 md3 True) (C1 mc (S1 c (Rec0 b))))
    where
      -- newtype => convert internal type
  gTreeToGrec = fmap (M1 . M1 . M1 . K1 . to) . gTreeToGrec

-----------

class GTreeFromGrec a g where
  gTreeFromGrec :: g b -> ConvTree a

instance {-# OVERLAPPABLE #-} Convert b a
    => GTreeFromGrec a (S1 c (K1 i b)) where
  gTreeFromGrec (M1 (K1 b)) = ConvTree [convert b] []

instance {-# OVERLAPPING #-} Convert b (ConvTree a)
    => GTreeFromGrec a (S1 c (K1 i [b])) where
  gTreeFromGrec (M1 (K1 bs)) = ConvTree [] [map convert bs]

instance {-# OVERLAPS #-} Convert [Char] a
    => GTreeFromGrec a (S1 c (K1 i [Char])) where
  gTreeFromGrec (M1 (K1 b)) = ConvTree [convert b] []

instance (GTreeFromGrec a x, GTreeFromGrec a y)
    => GTreeFromGrec a (x :*: y) where
  gTreeFromGrec (x :*: y) = gTreeFromGrec x <> gTreeFromGrec y

instance GTreeFromGrec a b
    => GTreeFromGrec a (D1 (MetaData md1 md2 md3 False) (C1 mc b)) where
  gTreeFromGrec (M1 (M1 b)) = gTreeFromGrec b

instance (Generic b, GTreeFromGrec a (Rep b))
    => GTreeFromGrec a (D1 (MetaData md1 md2 md3 True) (C1 mc (S1 c (K1 i b))))
    where
      -- newtype => convert internal type
  gTreeFromGrec (M1 (M1 (M1 (K1 b)))) = gTreeFromGrec $ from b
