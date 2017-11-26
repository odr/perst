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
  data BTree n t = Node (BTree n t) n t (BTree n t)
                 | Leaf n t
                 deriving (Show, Eq, Functor)
  |]
--

type family IsLeaf (a :: BTree n t) :: Bool where
  IsLeaf (Leaf x a) = True
  IsLeaf x = False

type family BTreeHas (a::k1) (t::BTree n k2) :: Bool where
  BTreeHas a (Leaf x a) = True
  -- NB! type calculation is eager!
  BTreeHas a (Node l n t r) = BTreeHasB (BTreeHas a l) a r
  BTreeHas a (Leaf x (Just a)) = True
  BTreeHas a b = False

type family BTreeHasB (b::Bool) (a::k1) (t::BTree n k2) :: Bool where
  BTreeHasB True a r = True
  BTreeHasB False a r = BTreeHas a r

type family ZipBTree (va::BTree n a) (vb::BTree n b) :: BTree n (a,b) where
  ZipBTree (Leaf 1 va) (Leaf 1 vb) = Leaf 1 '(va,vb)
  ZipBTree (Node la n ta ra) (Node lb n tb rb)
    = Node (ZipBTree la lb) n '(ta,tb) (ZipBTree lb rb)

type family BTreeCount (a :: BTree Nat t) :: Nat where
  BTreeCount (Node l n t r) = n
  BTreeCount (Leaf 1 t) = 1

type family BTreeType (bt::BTree n t) :: t where
  BTreeType (Node l n t r) = t
  BTreeType (Leaf n t) = t

type family Default k :: k where
  Default (Maybe x) = Nothing
  Default [x] = '[]
  Default Symbol = ""

type BTreeAppend (a :: BTree Nat k) (b :: BTree Nat k)
  = Node a (BTreeCount a + BTreeCount b) (Default k) b

-- some Tagged types
type family TaggedTag a where
  TaggedTag (Tagged (n :: BTree Nat k) x) = n

type family Untag a where
  Untag (Tagged n x) = x

type family TaggedAppend a b where
  TaggedAppend (Tagged (l::BTree Nat k) vl)
               (Tagged (r::BTree Nat k) vr)
    = Tagged (BTreeAppend l r) (vl,vr)

data GroupType = GTSimple | GTGroup
type family EqGroupType (a::GroupType) (b::GroupType) where
  EqGroupType GTSimple GTSimple = True
  EqGroupType GTGroup GTGroup = True
  EqGroupType a b = False

type instance a == b = EqGroupType a b

type family GetMbSym (x::k) :: Maybe Symbol where
  GetMbSym () = Nothing
  GetMbSym ('()) = Nothing
  GetMbSym Nothing = Nothing
  GetMbSym (s::Symbol) = Just s
  GetMbSym (s::Maybe Symbol) = s

type family GetGroupType v :: GroupType where
  GetGroupType (Tagged Nothing v) = GTGroup
  GetGroupType (Tagged (s::Symbol) v) = GTGroup
  GetGroupType (Tagged () v) = GTGroup
  GetGroupType (Tagged (s::()) v) = GTGroup
  GetGroupType (Tagged (s::Maybe Symbol) v) = GTGroup
  GetGroupType x = GTSimple
