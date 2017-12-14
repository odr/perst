{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Type.GrecTree.Lens(TLens(..), TGetSet(..), TLens'(..), LType) where

import           Data.Tagged              (Tagged (..), retag, untag)
-- import           Data.Type.Bool           (If)
-- import           Data.Type.Equality       (type (==))
-- import           GHC.TypeLits             (type (+), type (-), type (<=?), Nat,
import           GHC.TypeLits             (Symbol)
import           Lens.Micro               (set, (^.))

import           Data.Type.GrecTree.BTree

-- Lens?
type family LType (n::k) b where
  LType a (Tagged (Leaf Nothing) (Tagged b vb)) = LType a (Tagged b vb)
  LType a (Tagged (Leaf (Just a)) va) = va
  LType a (Tagged (Leaf a) va) = va
  LType a (Tagged (Node l t r) v)
    = LTypeB (BTreeHas a l) a (Tagged (Node l t r) v)
  LType a (Tagged (ns::[Symbol]) va) = LType a (Tagged (BTreeFromList ns) va)

type family LTypeB x (n::k) b where
  LTypeB True a (Tagged (Node l t r) (vl,vr)) = LType a (Tagged l vl)
  LTypeB False a (Tagged (Node l t r) (vl,vr)) = LType a (Tagged r vr)

class TLens n b n' b' where
  tlens :: Functor f => (LType n b -> f (LType n' b')) -> b -> f b'

-- by name
instance TLens a (Tagged b vb) a' (Tagged b' vb')
      => TLens (a :: Symbol)  (Tagged (Leaf Nothing) (Tagged b vb))
               (a' :: Symbol) (Tagged (Leaf Nothing) (Tagged b' vb'))
  where
  tlens f = fmap Tagged . tlens @a @(Tagged b vb) @a' @(Tagged b' vb') f . untag

instance TLens (a :: Symbol) (Tagged (Leaf (Just a)) va)
               (a' :: Symbol) (Tagged (Leaf (Just a')) va')
  where
  -- type LType a (Tagged (Leaf (Just a)) va) = va
  tlens f = fmap Tagged . f . untag

instance TLens (a :: Symbol) (Tagged (Leaf a) va)
               (a' :: Symbol) (Tagged (Leaf a') va')
  where
  -- type LType a (Tagged (Leaf a) va) = va
  tlens f = fmap Tagged . f . untag

instance ( TLensB (BTreeHas a l) a (Tagged (Node l t r) v)
                                 a' (Tagged (Node l' t' r') v')
          -- tree should be similar... It is not good,,,
          -- but mostly we want change type, not name
         , BTreeHas a l ~ BTreeHas a' l'
         )
      => TLens (a::Symbol) (Tagged (Node l t r) v)
               (a'::Symbol) (Tagged (Node l' t' r') v')
  where
  tlens = tlensB @(BTreeHas a l) @a @(Tagged (Node l t r) v)
                                 @a' @(Tagged (Node l' t' r') v')

instance
  ( TLens a (Tagged (BTreeFromList ns) va) a' (Tagged (BTreeFromList ns') va')
  , LType a  (Tagged (BTreeFromList ns) va)   ~ LType a  (Tagged ns va)
  , LType a' (Tagged (BTreeFromList ns') va') ~ LType a' (Tagged ns' va')
  )
  => TLens a (Tagged (ns::[Symbol]) va) a' (Tagged (ns'::[Symbol]) va')
  where
    tlens f = fmap retag
            . tlens @a  @(Tagged (BTreeFromList ns)  va)
                    @a' @(Tagged (BTreeFromList ns') va') f
            . retag

class TLens' n b where
  tlens' :: Functor f => (LType n b -> f (LType n b)) -> b -> f b
instance TLens n b n b => TLens' n b where
  tlens' = tlens @n @b @n @b

-- multiple get/set by btree
class TGetSet a b where
  type GSType a b
  tget :: b -> GSType a b
  tset :: GSType a b -> b -> b

instance TLens' a b => TGetSet (Leaf (Just a) :: BTree (Maybe Symbol)) b where
  type GSType (Leaf (Just a)) b = LType a b
  tget = (^. tlens' @a)
  tset = set (tlens' @a)

instance TLens' a b => TGetSet (Leaf a :: BTree Symbol) b where
  type GSType (Leaf a) b = LType a b
  tget = (^. tlens' @a)
  tset = set (tlens' @a)

instance (TGetSet l b, TGetSet r b) => TGetSet (Node l x r::BTree t) b where
  type GSType (Node l x r) b = (GSType l b, GSType r b)
  tget = (,) <$> tget @l <*> tget @r
  tset (a,b) = tset @l a . tset @r b

instance TGetSet (BTreeFromList ns) b => TGetSet (ns :: [Symbol]) b where
  type GSType ns b = GSType (BTreeFromList ns) b
  tget = tget @(BTreeFromList ns)
  tset = tset @(BTreeFromList ns)

-- instance TLens' a b => TGetSet ('[] :: [k]) b where
--   type GSType '[] b = ()
--   tget _ = ()
--   tset _ = id
--
-- instance TLens' a b => TGetSet ('[a] :: [k]) b where
--   type GSType '[a] b = LType a b
--   tget = (^. tlens' @a)
--   tset = set (tlens' @a)
--
-- instance (TLens' a1 b, TGetSet (a2 ':as) b)
--       => TGetSet (a1 ':a2 ':as::[k]) b where
--   type GSType (a1 ':a2 ':as) b = (LType a1 b, GSType (a2 ':as) b)
--   tget b = (b ^. tlens' @a1, tget @(a2 ':as) b)
--   tset (a1,as) = set (tlens' @a1) a1 . tset @(a2 ':as) as

-- TLensB
class TLensB (x::Bool) n b n' b' where
  tlensB :: Functor f => (LTypeB x n b -> f (LTypeB x n' b')) -> b -> f b'

instance TLens a (Tagged l vl) a' (Tagged l' vl')
      => TLensB True a (Tagged (Node l t r) (vl,vr))
                     a' (Tagged (Node l' t' r) (vl',vr)) where
  tlensB f (Tagged (vl,vr))
    = (Tagged . (,vr) . untag)
    <$> tlens @a @(Tagged l vl) @a' @(Tagged l' vl') f (Tagged vl)

instance TLens a (Tagged r vr) a' (Tagged r' vr')
      => TLensB False a (Tagged (Node l t r) (vl,vr))
                      a' (Tagged (Node l t' r') (vl,vr')) where
  tlensB f (Tagged (vl,vr))
    = (Tagged . (vl,) . untag)
    <$> tlens @a @(Tagged r vr) @a' @(Tagged r' vr') f (Tagged vr)
