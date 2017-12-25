{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitNamespaces        #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
--
{-# LANGUAGE DataKinds                 #-}

module Data.Type.GrecTree.BTree where

import           Data.Singletons.Prelude
import           Data.Singletons.TH      (singletons)
import           Data.Tagged             (Tagged)
import           GHC.TypeLits            (type (+), Nat, Symbol)

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

type family BTreeHasNum s t where
  BTreeHasNum s (Node l x r)
    = If (BTreeHas s l) '(1,IsLeaf l)
        (If (BTreeHas s r) '(2,IsLeaf r) '(0,False))

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

type family BTreeFromList (x::[k]) :: BTree (Maybe k) where
  BTreeFromList '[x] = Leaf (Just x)
  BTreeFromList (x ':xs) = Node (Leaf (Just x)) Nothing (BTreeFromList xs)

type family BTreeToList (x::BTree (Maybe k)) :: [k] where
  BTreeToList (Leaf (Just x)) = '[x]
  BTreeToList (Node l Nothing r) = BTreeToList l :++ BTreeToList r

type family BTreePath s x :: Maybe [Bool] where
  BTreePath s (Leaf s') = If (Just s:==s') (Just '[]) Nothing
  BTreePath s (Node l x r) = BTreePath' (BTreePath s l) False s l r

type family BTreePath' xs b s l r where
  BTreePath' Nothing False s l r = BTreePath' (BTreePath s r) True s l r
  BTreePath' Nothing True s l r  = Nothing
  BTreePath' (Just xs) b s l r   = Just (b ': xs)
