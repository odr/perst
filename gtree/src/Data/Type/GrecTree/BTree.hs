{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE UndecidableInstances      #-}
-- {-# LANGUAGE TypeOperators #-}
module Data.Type.GrecTree.BTree where

import           Data.Singletons.Prelude
import           Data.Singletons.Prelude.List
import           Data.Type.Equality           (type (==))
  --  (type (:++))
import           Data.Singletons.TH           (singletons)
import           Data.Tagged                  (Tagged)
import           GHC.TypeLits                 (type (+), Nat, Symbol)


singletons [d|
  data BTree t = Node (BTree t) t (BTree t)
                 | Leaf t
                 deriving (Show, Eq, Functor)
  |]
--

type family IsLeaf (a :: BTree t) :: Bool where
  IsLeaf (Leaf a) = True
  IsLeaf x = False

type family BTreeHas (a::k1) (t::BTree k2) :: Bool where
  BTreeHas a (Leaf a) = True
  -- NB! type calculation is eager!
  BTreeHas a (Node l t r) = BTreeHasB (BTreeHas a l) a r
  BTreeHas a (Leaf (Just a)) = True
  BTreeHas a b = False

type family BTreeHasB (b::Bool) (a::k1) (t::BTree k2) :: Bool where
  BTreeHasB True a r = True
  BTreeHasB False a r = BTreeHas a r

type family ZipBTree (va::BTree a) (vb::BTree b) :: BTree (a,b) where
  ZipBTree (Leaf va) (Leaf vb) = Leaf '(va,vb)
  ZipBTree (Node la ta ra) (Node lb tb rb)
    = Node (ZipBTree la lb) '(ta,tb) (ZipBTree lb rb)

type family BTreeCount (a :: BTree t) :: Nat where
  BTreeCount (Node l t r) = BTreeCount l + BTreeCount r
  BTreeCount (Leaf t) = 1

type family BTreeType (bt::BTree t) :: t where
  BTreeType (Node l t r) = t
  BTreeType (Leaf t) = t

type family Default k :: k where
  Default (Maybe x) = Nothing
  Default [x] = '[]
  Default Symbol = ""
  Default () = '()

type BTreeAppend (a :: BTree k) (b :: BTree k) = Node a (Default k) b

-- some Tagged types
type family TaggedTagBT a where
  TaggedTagBT (Tagged (n::BTree k) x) = n

type family Untag a where
  Untag (Tagged n x) = x

type family TaggedAppend a b where
  TaggedAppend (Tagged (l::BTree k) vl)
               (Tagged (r::BTree k) vr)
    = Tagged (BTreeAppend l r) (vl,vr)

type family GetMbSym (x::k) :: Maybe Symbol where
  GetMbSym () = Nothing
  GetMbSym ('()) = Nothing
  GetMbSym Nothing = Nothing
  GetMbSym (s::Symbol) = Just s
  GetMbSym (s::Maybe Symbol) = s

----------------
-- data GroupType = GTSimple | GTGroup
-- type family EqGroupType (a::GroupType) (b::GroupType) where
--   EqGroupType GTSimple GTSimple = True
--   EqGroupType GTGroup GTGroup = True
--   EqGroupType a b = False
--
-- type instance a == b = EqGroupType a b
--
-- type family GetGroupType v :: GroupType where
--   GetGroupType (Tagged Nothing v) = GTGroup
--   GetGroupType (Tagged (s::Symbol) v) = GTGroup
--   GetGroupType (Tagged () v) = GTGroup
--   GetGroupType (Tagged (s::()) v) = GTGroup
--   GetGroupType (Tagged (s::Maybe Symbol) v) = GTGroup
--   GetGroupType x = GTSimple
