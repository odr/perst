{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE UndecidableInstances      #-}
-- {-# LANGUAGE TypeOperators #-}
module Data.Type.GrecTree.BTree where

import           Data.Singletons.Prelude
import           Data.Singletons.Prelude.List
  --  (type (:++))
import           Data.Singletons.TH           (singletons)
import           GHC.TypeLits                 (type (+), Nat, Symbol)


singletons [d|
  data BTree n t = Node (BTree n t) n t (BTree n t)
                 | Leaf n t
                 deriving (Show, Eq, Functor)
  |]
--

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

type BTreeAppend (a :: BTree Nat (Maybe t)) (b :: BTree Nat (Maybe t))
  = Node a (BTreeCount a + BTreeCount b) (Nothing :: Maybe t) b

-- type family BTreeToList (a :: BTree n k) :: [k] where
--   BTreeToList (Leaf 1 k) = '[k]
--   BTreeToList (Node l n t r) = BTreeToList l :++ BTreeToList r
