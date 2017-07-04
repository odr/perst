{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Type.Grec.Type
    ( (:::), Cons, Fields, Typ, GFields, ListToTagged, TaggedToList, ListToPairs
    , ListToTaggedPairs
    , GrecGroup(..), UnGrecGroup, GUnGroupFields
    ) where

import           Data.Kind               (Type)
import           Data.Singletons.Prelude
import           Data.Tagged             (Tagged (..))
import           GHC.Generics
import           GHC.OverloadedLabels    (IsLabel (..))
import           GHC.TypeLits            (ErrorMessage (..), TypeError)

type (:::) a b = '(a,b)

type Cons a = GCons (Rep a)
type family GCons (a :: k1) :: Symbol where
  GCons (D1 _ (C1 (MetaCons s _ _) _)) = s
  GCons a = TypeError (
      Text "GCons is supported for Representation of data or newtype with one constructor"
      :$$: Text "Checked type is " :<>: ShowType a
      )

type Typ a = GTyp (Rep a)
type family GTyp (a :: k1) :: Symbol where
  GTyp (D1 (MetaData s _ _ _) _) = s
  GTyp a = TypeError (
      Text "GTyp is supported for Representation of data or newtype"
      :$$: Text "Checked type is " :<>: ShowType a
      )

newtype GrecGroup a = GrecGroup { getGrecGroup :: a } deriving (Show, Eq, Ord, Generic)
type family UnGrecGroup (a :: [(Symbol, Type)]) where
  UnGrecGroup '[] = '[]
  UnGrecGroup ('(s, GrecGroup b) ': as) = Fields b :++ UnGrecGroup as
  UnGrecGroup (a ': as) = a ': UnGrecGroup as


type Fields a = GUnGroupFields (Rep a)
type GUnGroupFields g = UnGrecGroup (GFields g)

type family GFields (a :: k1) :: [(Symbol,Type)] where
  GFields (S1 (MetaSel ('Just s) _ _ _) (Rec0 v)) = '[ '(s, v)]
  GFields (C1 _ s) = GFields s
  GFields (a :*: b) = GFields a :++ GFields b
  -- data
  GFields (D1 (MetaData _ _ _ 'False) (C1 _ s)) = GFields s
  -- newtype
  GFields (D1 (MetaData _ _ _ 'True) (C1 _ (S1 _ (Rec0 dt)))) = GFields (Rep dt)
  GFields a = TypeError (
      Text "Grec.Type.hs: GFields is supported only for Representation of record with one constructor"
      :$$: Text "and at least one field or newtype for such record"
      :$$: Text "Checked type is " :<>: ShowType a
      )

type family ListToTagged (t :: [(Symbol,Type)]) :: Type where
  ListToTagged '[ '(a,b)] = Tagged a b
  ListToTagged ( '(n,a) ': b ': c) = (Tagged n a, ListToTagged (b ': c))

type family TaggedToList (t :: Type) :: [(Symbol,Type)] where
  TaggedToList (Tagged (n ': n1 ': ns :: [Symbol]) (a,b))
      = '(n,a) ': TaggedToList (Tagged (n1 ': ns) b)
  TaggedToList (Tagged ('[n] :: [Symbol]) a)  = '[ '(n,a)]
  TaggedToList (Tagged ('[] :: [Symbol]) a)  = '[ ]
  TaggedToList (Tagged (ns :: [Symbol]) (v1,v2,v3))
      = TaggedToList (Tagged ns (v1,(v2,v3)))
  TaggedToList (Tagged (ns :: [Symbol]) (v1,v2,v3,v4))
      = TaggedToList (Tagged ns (v1,(v2,(v3,v4))))
  TaggedToList (Tagged (ns :: [Symbol]) (v1,v2,v3,v4,v5))
      = TaggedToList (Tagged ns (v1,(v2,(v3,(v4,v5)))))
  TaggedToList (Tagged (ns :: [Symbol]) (v1,v2,v3,v4,v5,v6))
      = TaggedToList (Tagged ns (v1,(v2,(v3,(v4,(v5,v6))))))
  TaggedToList (Tagged (ns :: [Symbol]) (v1,v2,v3,v4,v5,v6,v7))
      = TaggedToList (Tagged ns (v1,(v2,(v3,(v4,(v5,(v6,v7)))))))

type family ListToPairs (t :: [Type]) :: Type where
  ListToPairs '[] = ()
  ListToPairs '[a] = a
  ListToPairs (a1 ': a2 ': as) = (a1, ListToPairs (a2 ': as))

type ListToTaggedPairs t = Tagged (Map FstSym0 t) (ListToPairs (Map SndSym0 t))
