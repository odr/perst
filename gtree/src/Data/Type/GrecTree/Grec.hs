{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}
module Data.Type.GrecTree.Grec where

import           Data.Kind                (Type)
-- import           Data.Singletons.Prelude.Maybe (FromMaybe)
import           Data.Tagged              (Tagged (..), retag, untag)
import           GHC.Generics
import           GHC.TypeLits             (type (+), type (-), type (<=?), Nat,
                                           Symbol)

import           Data.Type.GrecTree.BTree
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

instance GGrec (Rep (a1,a2)) => Grec (a1,a2)
instance GGrec (Rep (a1,a2,a3)) => Grec (a1,a2,a3)
instance GGrec (Rep (a1,a2,a3,a4)) => Grec (a1,a2,a3,a4)
instance GGrec (Rep (a1,a2,a3,a4,a5)) => Grec (a1,a2,a3,a4,a5)
instance GGrec (Rep (a1,a2,a3,a4,a5,a6)) => Grec (a1,a2,a3,a4,a5,a6)
instance GGrec (Rep (a1,a2,a3,a4,a5,a6,a7)) => Grec (a1,a2,a3,a4,a5,a6,a7)


type family TaggedTag a where
  TaggedTag (Tagged (n :: BTree Nat (Maybe Symbol)) x) = n

type family Untag a where
  Untag (Tagged n x) = x

---------------
class GGrec g where
  -- type GTypeTree g :: BTree Nat Type
  type GTagged g
  gGrecToTagged   :: g b -> GTagged g
  gTaggedToGrec   :: GTagged g -> g b

type family TaggedAppend a b where
  TaggedAppend (Tagged (l::BTree Nat (Maybe s)) vl)
               (Tagged (r::BTree Nat (Maybe s)) vr)
    = Tagged (BTreeAppend l r) (vl,vr)

data GroupType = GTSimple | GTGroup

type family GetGroupType v :: GroupType where
  GetGroupType (Tagged 'Nothing v) = GTGroup
  GetGroupType (Tagged (s::Maybe Symbol) v) = GTGroup
  GetGroupType x = GTSimple

class GrecGT (ft::GroupType) s b where
  -- type TypeTreeGT ft s b :: BTree Nat Type
  type TaggedGT ft s b
  toTaggedGT   :: b -> TaggedGT ft s b
  fromTaggedGT :: TaggedGT ft s b -> b

instance GrecGT GTSimple s b where
  -- type TypeTreeGT GTSimple s b = Leaf 1 b
  type TaggedGT GTSimple s b = Tagged (Leaf 1 s) b
  toTaggedGT b = Tagged b
  fromTaggedGT = untag

type family TagTop s t where
  TagTop s (Tagged bt v) = Tagged (ChangeTop s bt) v

type family ChangeTop (s::k) (t::BTree Nat k) :: BTree Nat k where
  ChangeTop s (Node l n tt r) = Node l n s r
  ChangeTop s (Leaf 1 tt) = Leaf 1 s

instance  ( Grec v
          , GrecTagged v ~ Tagged bt vv
          , TagTop g (GrecTagged v) ~ Tagged bt' vv
          )
          => GrecGT GTGroup s (Tagged g v) where
  -- type TypeTreeGT (GTGroup g) s b = Leaf 1 (GrecTypeTree b)
  type TaggedGT GTGroup s (Tagged g v)
    = Tagged (Leaf 1 s) (TagTop g (GrecTagged v))
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
              ~ Tagged (Node l n t r) (vl,vr)
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
