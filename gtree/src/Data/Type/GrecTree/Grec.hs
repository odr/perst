{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}
module Data.Type.GrecTree.Grec where

import           Data.Kind                  (Type)
import           Data.Tagged                (Tagged (..), retag, untag)
import           Data.Text                  (Text)
import           GHC.Generics
import           GHC.TypeLits               (type (+), type (-), type (<=?),
                                             Nat, Symbol)

import           Data.Type.GrecTree.BTree
import           Data.Type.GrecTree.Convert (ConvNames (..))
-- import           Data.Type.GrecTree.Internal.GGrec (GGrec (..))

class (Generic a, GGrec (Rep a)) => Grec a where
  -- type GrecTypeTree a :: BTree Nat Type
  -- type GrecTypeTree a = GTypeTree (Rep a)
  type GrecTagged a
  type GrecTagged a = GTagged (Rep a)

  toTagged :: a -> GrecTagged a
  default toTagged :: GrecTagged a ~ GTagged (Rep a) => a -> GrecTagged a
  toTagged = gGrecToTagged . from

  fromTagged :: GrecTagged a -> a
  default fromTagged :: GrecTagged a ~ GTagged (Rep a) => GrecTagged a -> a
  fromTagged = to . gTaggedToGrec

  fieldNames :: [Text]
  default fieldNames :: ConvNames (GrecTagged a) => [Text]
  fieldNames = getFldNames @(GrecTagged a)

instance (GGrec (Rep (a1,a2)), ConvNames (GTagged (Rep (a1,a2))))
      => Grec (a1,a2)
instance (GGrec (Rep (a1,a2,a3)), ConvNames (GTagged (Rep (a1,a2,a3))))
      => Grec (a1,a2,a3)
instance (GGrec (Rep (a1,a2,a3,a4)), ConvNames (GTagged (Rep (a1,a2,a3,a4))))
      => Grec (a1,a2,a3,a4)
instance ( GGrec (Rep (a1,a2,a3,a4,a5))
         , ConvNames (GTagged (Rep (a1,a2,a3,a4,a5)))
         )
      => Grec (a1,a2,a3,a4,a5)
instance ( GGrec (Rep (a1,a2,a3,a4,a5,a6))
         , ConvNames (GTagged (Rep (a1,a2,a3,a4,a5,a6)))
         )
      => Grec (a1,a2,a3,a4,a5,a6)
instance ( GGrec (Rep (a1,a2,a3,a4,a5,a6,a7))
         , ConvNames (GTagged (Rep (a1,a2,a3,a4,a5,a6,a7)))
         )
      => Grec (a1,a2,a3,a4,a5,a6,a7)

---------------
class GGrec g where
  -- type GTypeTree g :: BTree Nat Type
  type GTagged g
  gGrecToTagged   :: g b -> GTagged g
  gTaggedToGrec   :: GTagged g -> g b

class GrecGT (ft::GroupType) s b where
  -- type TypeTreeGT ft s b :: BTree Nat Type
  type TaggedGT ft s b
  toTaggedGT   :: b -> TaggedGT ft s b
  fromTaggedGT :: TaggedGT ft s b -> b

instance GrecGT GTSimple s b where
  -- type TypeTreeGT GTSimple s b = Leaf 1 b
  type TaggedGT GTSimple s b = Tagged (Leaf s) b
  toTaggedGT b = Tagged b
  fromTaggedGT = untag

type family TagTop s t where
  TagTop s (Tagged bt v) = Tagged (ChangeTop s bt) v

type family ChangeTop (s::k1) (t::BTree k) :: BTree k where
  ChangeTop (s::k) (Node l (tt::k) r) = Node l s r
  ChangeTop (s::k) (Leaf (tt::k)) = Leaf s
  ChangeTop (s::k) (Node l (tt::Maybe k) r) = Node l (Just s) r
  ChangeTop (s::k) (Leaf (tt::Maybe k)) = Leaf (Just s)
  ChangeTop (s::k1) (Node l (tt::k) r) = Node l (Default k) r
  ChangeTop (s::k1) (Leaf (tt::k)) = Leaf (Default k)

instance  ( Grec v
          , GrecTagged v ~ Tagged bt vv
          , TagTop g (GrecTagged v) ~ Tagged bt' vv
          )
          => GrecGT GTGroup s (Tagged g v) where
  -- type TypeTreeGT (GTGroup g) s b = Leaf 1 (GrecTypeTree b)
  type TaggedGT GTGroup s (Tagged g v)
    = Tagged (Leaf s) (TagTop g (GrecTagged v))
  toTaggedGT = Tagged . (retag :: GrecTagged v -> TagTop g (GrecTagged v))
             . toTagged . untag
  fromTaggedGT = Tagged . fromTagged
               . (retag :: TagTop g (GrecTagged v) -> GrecTagged v) . untag

instance GrecGT (GetGroupType b) c b
      => GGrec (S1 (MetaSel c md2 md3 md4) (Rec0 b)) where
  -- type GTypeTree (S1 ('MetaSel c md2 md3 md4) (Rec0 b))
  --   = TypeTreeGT (GroupType b) c b
  type GTagged (S1 (MetaSel c md2 md3 md4) (Rec0 b))
    = TaggedGT (GetGroupType b) c b
  gGrecToTagged (M1 (K1 b)) = toTaggedGT @(GetGroupType b) @c b
  gTaggedToGrec = M1 . K1 . fromTaggedGT @(GetGroupType b) @c

instance  ( GGrec x
          , GGrec y
          , GTagged x ~ Tagged l vl
          , GTagged y ~ Tagged r vr
          , TaggedAppend (GTagged x) (GTagged y)
              ~ Tagged (Node l t r) (vl,vr)
          -- , n ~ (BTreeCount (GTypeTree x) + BTreeCount (GTypeTree y))
          )
      => GGrec (x :*: y) where
  -- type GTypeTree (x :*: y) = BTreeAppend (GTypeTree x) (GTypeTree y)
  type GTagged (x :*: y)  = TaggedAppend (GTagged x) (GTagged y)
  gTaggedToGrec (Tagged (vl,vr)) =
    let x = gTaggedToGrec (Tagged vl :: GTagged x)
        y = gTaggedToGrec (Tagged vr :: GTagged y)
    in x :*: y
  gGrecToTagged (x :*: y) =
    let (Tagged vl) = gGrecToTagged x
        (Tagged vr) = gGrecToTagged y
    in
      Tagged (vl,vr)

instance GGrec b => GGrec (D1 (MetaData md1 md2 md3 False) (C1 mc b)) where
  -- type GTypeTree (D1 (MetaData md1 md2 md3 False) (C1 mc b)) = GTypeTree b
  type GTagged (D1 (MetaData md1 md2 md3 False) (C1 mc b)) = GTagged b
  gTaggedToGrec = M1 . M1 . gTaggedToGrec
  gGrecToTagged (M1 (M1 b)) = gGrecToTagged b

-- newtype => convert internal type
instance (Generic b, GGrec (Rep b))
    => GGrec (D1 (MetaData md1 md2 md3 True) (C1 mc (S1 c (Rec0 b))))
    where
  -- type GTypeTree (D1 (MetaData md1 md2 md3 True) (C1 mc (S1 c (Rec0 b))))
  --     = GTypeTree (Rep b)
  type GTagged (D1 (MetaData md1 md2 md3 True) (C1 mc (S1 c (Rec0 b))))
      = GTagged (Rep b)
  gTaggedToGrec = M1 . M1 . M1 . K1 . to . gTaggedToGrec
  gGrecToTagged (M1 (M1 (M1 (K1 b)))) = gGrecToTagged $ from b
