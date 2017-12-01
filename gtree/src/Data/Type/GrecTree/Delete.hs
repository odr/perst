{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Type.GrecTree.Delete(TDelete(..)) where

import           Data.Tagged              (Tagged (..), untag)
import           Data.Type.Bool           (If)
import           Data.Type.Equality       (type (==))
import           GHC.TypeLits             (type (+), type (-), type (<=?), Nat,
                                           Symbol)

import           Data.Type.GrecTree.BTree

class TDelete (n::k) b where
  type DeletedType n b
  tdel :: b -> DeletedType n b

instance TDelete ('[]::[k]) b where
  type DeletedType ('[]::[k]) b = b
  tdel = id

instance (TDelete ns (DeletedType n b), TDelete n b) => TDelete (n ':ns) b where
  type DeletedType (n ':ns) b = DeletedType ns (DeletedType n b)
  tdel = tdel @ns . tdel @n

class TDeleteB (x::(k,Bool)) (n::k2) b where
  type DeletedTypeB x n b
  tdelB :: b -> DeletedTypeB x n b

type family BTreeHasNum s t where
  BTreeHasNum s (Node l x r)
    = If (BTreeHas s l) '(1,IsLeaf l)
        (If (BTreeHas s r) '(2,IsLeaf r) '(0,False))

instance TDeleteB '(Just s==s1,False) s (Tagged (Leaf s1) v)
      => TDelete (s::Symbol) (Tagged (Leaf s1) v) where
  type DeletedType s (Tagged (Leaf s1) v)
    = DeletedTypeB '(Just s==s1,False) s (Tagged (Leaf s1) v)
  tdel = tdelB @('(Just s==s1,False)) @s

instance TDeleteB (BTreeHasNum s (Node l t r)) s (Tagged (Node l t r) v)
      => TDelete (s::Symbol) (Tagged (Node l t r) v) where
  type DeletedType s (Tagged (Node l t r) v)
    = DeletedTypeB (BTreeHasNum s (Node l t r)) s (Tagged (Node l t r) v)
  tdel = tdelB @(BTreeHasNum s (Node l t r)) @s

instance TDeleteB '(True,b) s v where
  type DeletedTypeB '(True,b) s v = ()
  tdelB = const ()

instance TDeleteB '(False,b) s v where
  type DeletedTypeB '(False,b) s v = v
  tdelB = id

instance TDeleteB '(0,b) s v where
  type DeletedTypeB '(0,b) s v = v
  tdelB = id

instance TDeleteB '(1,True) s (Tagged (Node l t r) (vl,vr)) where
  type DeletedTypeB '(1,True) s (Tagged (Node l t r) (vl,vr)) = Tagged r vr
  tdelB = Tagged . snd . untag

instance TDeleteB '(2,True) s (Tagged (Node l t r) (vl,vr)) where
  type DeletedTypeB '(2,True) s (Tagged (Node l t r) (vl,vr)) = Tagged l vl
  tdelB = Tagged . fst . untag

instance ( TDelete s (Tagged l vl)
         , DeletedType s (Tagged l vl)
              ~ Tagged s0 (Untag (DeletedType s (Tagged l vl)))
         )
      => TDeleteB '(1,False) s (Tagged (Node l t r) (vl,vr)) where
  type DeletedTypeB '(1,False) s (Tagged (Node l t r) (vl,vr))
    = Tagged (Node (TaggedTagBT (DeletedType s (Tagged l vl))) t r)
             (Untag (DeletedType s (Tagged l vl)),vr)
  tdelB (Tagged (vl,vr)) = Tagged (untag $ tdel @s $ Tagged @l vl, vr)

instance ( TDelete s (Tagged r vr)
         , DeletedType s (Tagged r vr)
              ~ Tagged s0 (Untag (DeletedType s (Tagged r vr)))
         )
      => TDeleteB '(2,False) s (Tagged (Node l t r) (vl,vr)) where
  type DeletedTypeB '(2,False) s (Tagged (Node l t r) (vl,vr))
    = Tagged (Node l t (TaggedTagBT (DeletedType s (Tagged r vr))))
             (vl,Untag (DeletedType s (Tagged r vr)))
  tdelB (Tagged (vl,vr)) = Tagged (vl,untag $ tdel @s $ Tagged @r vr)
