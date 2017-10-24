{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE UndecidableInstances      #-}
module Data.Type.Grec.Type
    -- ( (:::), Cons, Fields, Typ, GFields, ListToTagged, TaggedToList, ListToPairs
    -- , ListToTaggedPairs
    -- , GrecGroup(..), UnGrecGroup, GUnGroupFields
    -- , IsSub, IsSubSym0, IsSubSym1, sIsSub
    -- , AllIsSub, AllIsSubSym0, AllIsSubSym1, sAllIsSub
    -- , Submap, SubmapSym0, SubmapSym1, sSubmap
    -- , Submap2, Submap2Sym0, Submap2Sym1, sSubmap2
    -- , Contain, ContainSym0, ContainSym1
    -- )
    where

import           Data.Kind                     (Constraint, Type)
import           Data.List                     (find, nub)
import           Data.Maybe                    (fromJust, isNothing)
import           Data.Singletons.Prelude
import           Data.Singletons.Prelude.List
import           Data.Singletons.Prelude.Maybe
import           Data.Singletons.TH
import           Data.Tagged                   (Tagged (..))
import           GHC.Generics
import           GHC.OverloadedLabels          (IsLabel (..))
import           GHC.TypeLits                  (ErrorMessage (..), TypeError)

type (:::) a b = '(a,b)
newtype Grec a = Grec { unGrec :: a } deriving (Eq, Show, Ord)

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

-- newtype GrecGroup a = GrecGroup { getGrecGroup :: a } deriving (Show, Eq, Ord, Generic)

type family UnGrecGroup (a :: [(Symbol, Type)]) where
  UnGrecGroup '[] = '[]
  UnGrecGroup ('(s, Grec b) ': as) = Fields b :++ UnGrecGroup as
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

genDefunSymbols [''Fields]

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

promote
  [d| isSub :: Eq a => [a] -> [a] -> Bool
      isSub as bs = all (`elem` bs) as -- null $ aa \\ bs

      allIsSub :: Eq a => [[a]] -> [a] -> Bool
      allIsSub ass ps = isSub (nub $ concat ass) ps
      --all (`isSub` ps) ass

      submap :: Eq a => [a] -> [(a,b)] -> Maybe [b]
      submap [] ps = Just []
      -- submap (a:as) ps = go $ lookup a ps
      --  where
      --   go Nothing = Nothing
      --   go (Just x) = go' $ submap as ps
      --    where
      --     go' Nothing   = Nothing
      --     go' (Just xs) = Just (x : xs)
      submap (a:as) ps = case lookup a ps of
        Nothing -> Nothing
        Just x  -> case submap as ps of
          Nothing -> Nothing
          Just xs -> Just (x : xs)

      -- submap as ps = let rs = map (`lookup` ps) as in
      --   if any_ isNothing rs then Nothing else Just (map fromJust rs)

      submap2 :: Eq a => [a] -> [(a,b)] -> Maybe [(a,b)]
      submap2 [] _ = Just []
      submap2 (a:as) ps = case find ((a==).fst) ps of
        Nothing -> Nothing
        Just x  -> case submap2 as ps of
          Nothing -> Nothing
          Just xs -> Just (x : xs)

      -- submap2 as ps = case submap as ps of
      --   Nothing -> Nothing
      --   Just rs -> Just (zip as rs)
  |]

type Contain a b = Submap (Map FstSym0 b) a ~ Just (Map SndSym0 b)
genDefunSymbols [''Contain]

type family AllC (cs :: [Constraint]) :: Constraint where
  AllC '[] = ()
  AllC (c ': cs) = (c, AllC cs)
