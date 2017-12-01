{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Type.GrecTree.Lens(TLens(..), TGetSet(..), TLens'(..)) where

import           Data.Tagged              (Tagged (..), untag)
import           Data.Type.Bool           (If)
import           Data.Type.Equality       (type (==))
import           GHC.TypeLits             (type (+), type (-), type (<=?), Nat,
                                           Symbol)
import           Lens.Micro               (set, (^.))

import           Data.Type.GrecTree.BTree

-- Lens?
type family LType (n::Symbol) b where
  LType a (Tagged (Leaf (Just a)) va) = va
  LType a (Tagged (Leaf a) va) = va
  -- LType 1 (Tagged (Leaf a) va) = va
  LType a (Tagged (Node l t r) v)
    = LTypeB (BTreeHas a l) a (Tagged (Node l t r) v)
  -- LType (num::Nat) (Tagged (Node l t r) v)
  --   = LTypeB (num <=? BTreeCount l)
  --             (If (num <=? BTreeCount l) num (num - (BTreeCount l)))
  --             (Tagged (Node l t r) v)

type family LTypeB x (n::Symbol) b where
  LTypeB True a (Tagged (Node l t r) (vl,vr)) = LType a (Tagged l vl)
  LTypeB False a (Tagged (Node l t r) (vl,vr)) = LType a (Tagged r vr)

class TLens n b n' b' where
  -- type LType n b
  -- type RType n' b'
  tlens :: Functor f => (LType n b -> f (LType n' b')) -> b -> f b'

-- by name
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

-- if there are more than one valid name -
-- leftest will be checked and there would be a compile-type error if wrong type.
-- More intelligence would be to check name with its type as pair
-- and use the first if exists
-- אבל לא חשוב
instance ( TLensB (BTreeHas a l) a (Tagged (Node l t r) v)
                                 a' (Tagged (Node l' t' r') v')
          -- tree should be similar... It is not good,,,
          -- but mostly we want change type, not name
         , BTreeHas a l ~ BTreeHas a' l'
         )
      => TLens (a::Symbol) (Tagged (Node l t r) v)
               (a'::Symbol) (Tagged (Node l' t' r') v')
  where
  -- type LType a (Tagged (Node l t r) v)
  --   = LTypeB (BTreeHas a l) a (Tagged (Node l t r) v)
  tlens = tlensB @(BTreeHas a l) @a @(Tagged (Node l t r) v)
                                 @a' @(Tagged (Node l' t' r') v')

-- -- by num
-- instance TLens 1 (Tagged (Leaf a) va) 1 (Tagged (Leaf a') va') where
--   -- type LType 1 (Tagged (Leaf a) va) = va
--   tlens f = fmap Tagged . f . untag
--
-- instance TLensB (num <=? BTreeCount l)
--                   (If (num <=? BTreeCount l) num (num - (BTreeCount l)))
--                   (Tagged (Node l t r) v)
--                   (If (num <=? BTreeCount l) num (num - (BTreeCount l)))
--                   (Tagged (Node l' t' r') v')
--       => TLens (num::Nat) (Tagged (Node l t r) v)
--                num (Tagged (Node l' t' r') v')
--   where
--   -- type LType (num::Nat) (Tagged (Node l t r) v)
--   --   = LTypeB (num <=? BTreeCount l)
--   --             (If (num <=? BTreeCount l) num (num - (BTreeCount l)))
--   --             (Tagged (Node l t r) v)
--   tlens = tlensB @(num <=? BTreeCount l)
--                  @(If (num <=? BTreeCount l) num (num - (BTreeCount l)))

class TLens' n b where
  tlens' :: Functor f => (LType n b -> f (LType n b)) -> b -> f b
instance TLens n b n b => TLens' n b where
  tlens' = tlens @n @b @n @b

-- multiple get/set by btree
class TGetSet a b where
  type GSType a b
  tget :: b -> GSType a b
  tset :: GSType a b -> b -> b

-- by names
instance TLens' a b => TGetSet (Leaf a :: BTree Symbol) b where
  type GSType (Leaf a) b = LType a b
  tget = (^. tlens' @a)
  tset = set (tlens' @a)

-- -- by nums
-- instance TLens a b => TGetSet (Leaf a :: BTree Nat) b where
--   type GSType (Leaf a) b = LType a b
--   tget = (^. tlens @a)
--   tset = set (tlens @a)

-- for both
instance (TGetSet l b, TGetSet r b) => TGetSet (Node l x r::BTree t) b where
  type GSType (Node l x r) b = (GSType l b, GSType r b)
  tget = (,) <$> tget @l <*> tget @r
  tset (a,b) = tset @l a . tset @r b


-- TLensB
class TLensB (x::Bool) n b n' b' where
  -- type LTypeB x n b
  tlensB :: Functor f => (LTypeB x n b -> f (LTypeB x n' b')) -> b -> f b'

instance TLens a (Tagged l vl) a' (Tagged l' vl')
      => TLensB True a (Tagged (Node l t r) (vl,vr))
                     a' (Tagged (Node l' t' r) (vl',vr)) where
  -- type LTypeB True a (Tagged (Node l t r) (vl,vr)) = LType a (Tagged l vl)
  tlensB f (Tagged (vl,vr))
    = (Tagged . (,vr) . untag)
    <$> tlens @a @(Tagged l vl) @a' @(Tagged l' vl') f (Tagged vl)

instance TLens a (Tagged r vr) a' (Tagged r' vr')
      => TLensB False a (Tagged (Node l t r) (vl,vr))
                      a' (Tagged (Node l t' r') (vl,vr')) where
  -- type LTypeB False a (Tagged (Node l t r) (vl,vr)) = LType a (Tagged r vr)
  tlensB f (Tagged (vl,vr))
    = (Tagged . (vl,) . untag)
    <$> tlens @a @(Tagged r vr) @a' @(Tagged r' vr') f (Tagged vr)
