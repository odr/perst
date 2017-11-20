{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE UndecidableInstances      #-}

module Data.Type.GTree where

import           Data.Bifunctor                (bimap)
import           Data.Kind                     (Type)
import           Data.Singletons.Prelude
import           Data.Singletons.Prelude.List
import           Data.Singletons.Prelude.Maybe (FromMaybe)
import           Data.Singletons.TH
import           Data.Singletons.TH
import           Data.Tagged                   (Tagged (..), untag)
import           GHC.Generics
import           GHC.TypeLits                  (type (+), type (-), type (<=?),
                                                Nat, Symbol)

singletons [d|
  data GTree n t = Node (GTree n t) n (GTree n t)
                 | Leaf n t

  treeHas :: Eq a => a -> GTree n a -> Bool
  treeHas a (Leaf _ b) = a == b
  -- NB! type calculation is eager!
  treeHas a (Node l _ r) = treeHasB (treeHas a l)
    where
      treeHasB True  = True
      treeHasB False = treeHas a r
  |]
--
type family ZipTree (va::GTree n a) (vb::GTree n b) :: GTree n (a,b) where
  ZipTree (Leaf 1 va) (Leaf 1 vb) = Leaf 1 '(va,vb)
  ZipTree (Node la n ra) (Node lb n rb) = Node (ZipTree la lb) n (ZipTree lb rb)

type family TreeCount (a :: GTree Nat t) :: Nat where
  TreeCount (Node l n r) = n
  TreeCount (Leaf 1 t) = 1

type TreeAppend a b = Node a (TreeCount a + TreeCount b) b

type family TaggedAppend a b where
  TaggedAppend (Tagged (l::GTree Nat Symbol) vl) (Tagged (r::GTree Nat Symbol) vr)
    = Tagged (TreeAppend l r) (vl,vr)

type family TaggedTag a where
  TaggedTag (Tagged n x) = n

type family Untag a where
  Untag (Tagged n x) = x

class GTaggedGrec g where
  type GTypeTree g :: GTree Nat Type
  type GTagged g
  gTaggedToGrec   :: GTagged g -> g b
  gGrecToTagged   :: g b -> GTagged g

instance GTaggedGrec (S1 ('MetaSel c md2 md3 md4) (Rec0 b)) where
  type GTypeTree (S1 ('MetaSel c md2 md3 md4) (Rec0 b)) = Leaf 1 b
  type GTagged (S1 ('MetaSel c md2 md3 md4) (Rec0 b))
      = Tagged (Leaf 1 (FromMaybe "" c)) b
  gTaggedToGrec = M1 . K1 . untag
  gGrecToTagged (M1 (K1 b)) = Tagged b

instance  ( GTaggedGrec x
          , GTaggedGrec y
          , GTagged x ~ Tagged l vl
          , GTagged y ~ Tagged r vr
          , TaggedAppend (GTagged x) (GTagged y)
              ~ Tagged (Node l n r) (vl,vr)
          , n ~ (TreeCount (GTypeTree x) + TreeCount (GTypeTree y))
          )
      => GTaggedGrec (x :*: y) where
  type GTypeTree (x :*: y) = TreeAppend (GTypeTree x) (GTypeTree y)
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

instance GTaggedGrec b
    => GTaggedGrec (D1 (MetaData md1 md2 md3 False) (C1 mc b)) where
  type GTypeTree (D1 (MetaData md1 md2 md3 False) (C1 mc b)) = GTypeTree b
  type GTagged (D1 (MetaData md1 md2 md3 False) (C1 mc b)) = GTagged b
  gTaggedToGrec = M1 . M1 . gTaggedToGrec
  gGrecToTagged (M1 (M1 b)) = gGrecToTagged b

-- newtype => convert internal type
instance (Generic b, GTaggedGrec (Rep b))
    => GTaggedGrec (D1 (MetaData md1 md2 md3 True) (C1 mc (S1 c (Rec0 b))))
    where
  type GTypeTree (D1 (MetaData md1 md2 md3 True) (C1 mc (S1 c (Rec0 b))))
      = GTypeTree (Rep b)
  type GTagged (D1 (MetaData md1 md2 md3 True) (C1 mc (S1 c (Rec0 b))))
      = GTagged (Rep b)
  gTaggedToGrec = M1 . M1 . M1 . K1 . to . gTaggedToGrec
  gGrecToTagged (M1 (M1 (M1 (K1 b)))) = gGrecToTagged $ from b

class (Generic a, GTaggedGrec (Rep a)) => CGrec a where
  type GrecTypeTree a :: GTree Nat Type
  type GrecTypeTree a = GTypeTree (Rep a)
  type GrecTagged a
  type GrecTagged a = GTagged (Rep a)
  toTagged :: a -> GrecTagged a
  default toTagged :: GrecTagged a ~ GTagged (Rep a) => a -> GrecTagged a
  toTagged = gGrecToTagged . from
  fromTagged :: GrecTagged a -> a
  default fromTagged :: GrecTagged a ~ GTagged (Rep a) => GrecTagged a -> a
  fromTagged = to . gTaggedToGrec

instance CGrec (a1,a2)
instance CGrec (a1,a2,a3)
instance CGrec (a1,a2,a3,a4)
instance CGrec (a1,a2,a3,a4,a5)
instance CGrec (a1,a2,a3,a4,a5,a6)
instance CGrec (a1,a2,a3,a4,a5,a6,a7)

class Convert a b where
  convert :: a -> b

-- Functor?
instance Convert va vb
      => Convert (Tagged (Leaf 1 a) va) (Tagged (Leaf 1 b) vb) where
  convert = Tagged . convert . untag

instance ( Convert (Tagged la vla) (Tagged lb vlb)
         , Convert (Tagged ra vra) (Tagged rb vrb)
         )
      => Convert (Tagged (Node la n ra) (vla,vra))
                 (Tagged (Node lb n rb) (vlb,vrb)) where
  convert = Tagged . bimap convertL convertR . untag
    where
      convertL = untag . (convert :: Tagged la vla -> Tagged lb vlb) . Tagged
      convertR = untag . (convert :: Tagged ra vra -> Tagged rb vrb) . Tagged

-- Fold?
instance Convert va mb => Convert (Tagged (Leaf 1 na) va) mb where
  convert = convert . untag

instance (Convert (Tagged la vla) mb, Convert (Tagged ra vra) mb, Monoid mb)
      => Convert (Tagged (Node la n ra) (vla,vra)) mb where
  convert = uncurry mappend . bimap convertL convertR . untag
    where
      convertL  = (convert :: Tagged la vla -> mb) . Tagged
      convertR  = (convert :: Tagged ra vra -> mb) . Tagged

-- Lens?
class NLensed n b a where
  nlens :: Functor f => (a -> f a) -> b -> f b

-- by name
instance NLensed a (Tagged (Leaf n a) va) va where
  nlens f = fmap Tagged . f . untag

-- if there are more than one valid name -
-- leftest will be checked and there would be an error if wrong type.
-- More intelligence would be to check name with type as pair
-- and use the first if exists
-- אבל לא חשוב
instance NLensedB (TreeHas a l) a (Tagged (Node l n r) v) va
      => NLensed a (Tagged (Node l n r) v) va where
  nlens = nlensB @(TreeHas a l) @a

class NLensedB (x::Bool) n b a where
  nlensB :: Functor f => (a -> f a) -> b -> f b

instance NLensed a (Tagged l vl) va
      => NLensedB True a (Tagged (Node l n r) (vl,vr)) va where
  nlensB f (Tagged (vl,vr))
    = (Tagged . (,vr) . untag) <$> nlens @a @(Tagged l vl) f (Tagged vl)

instance NLensed a (Tagged r vr) va
      => NLensedB False a (Tagged (Node l n r) (vl,vr)) va where
  nlensB f (Tagged (vl,vr))
    = (Tagged . (vl,) . untag) <$> nlens @a @(Tagged r vr) f (Tagged vr)
-- end by name

-- by num
instance NLensed (n::Nat) (Tagged (Leaf n a) va) va where
  nlens f = fmap Tagged . f . untag

instance NLensedB (num <=? TreeCount l)
                  (If (num <=? TreeCount l) num (num - (TreeCount l)))
                  (Tagged (Node l n r) v)
                  va
      => NLensed (num::Nat) (Tagged (Node l (n::Nat) r) v) va where
  nlens = nlensB @(num <=? TreeCount l)
                 @(If (num <=? TreeCount l) num (num - (TreeCount l)))
-- end by num
