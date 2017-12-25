{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Type.GrecTree.Lens where

import           Data.Kind                     (Constraint)
import           Data.Singletons.Prelude       (If, Sing (..), SingI (..))
import           Data.Singletons.Prelude.Maybe (FromJust, IsJust)
import           Data.Tagged                   (Tagged (..), untag)
import           GHC.TypeLits                  (Symbol)
import           Lens.Micro                    (set, (%~), (^.))

import           Data.Type.GrecTree.BTree
import           Data.Type.GrecTree.Convert    (Hidden (..))

class LensPath (path :: [Bool]) b a where
  type LPathType path b
  type LResType path b a
  lensPath :: Functor f => (LPathType path b -> f a)
                        -> b -> f (LResType path b a)

instance LensPath '[] b a where
  type LPathType '[] b = b
  type LResType '[] b a = a
  lensPath = id

instance  ( SingI x
          , If x (LensPath xs b2 a) (LensPath xs b1 a)
          , If x (LensPath xs b2 a) (LensPath xs b1 a)
          )
      => LensPath (x ':xs) (b1,b2) a where
  type LPathType (x ': xs) (b1,b2) = If x (LPathType xs b2) (LPathType xs b1)
  type LResType (x ': xs) (b1,b2) a =
    If x (b1, LResType xs b2 a) (LResType xs b1 a, b2)
  lensPath f (b1,b2) = case (sing :: Sing x) of
    SFalse -> (,b2) <$> lensPath @xs f b1
    STrue  -> (b1,) <$> lensPath @xs f b2

-----------------
type PathType s b = FromJust (BTreePath s b)

class TLens s b a where
  type LType s b
  type RType s b a
  tlens :: Functor f => (LType s b -> f a) -> b -> f (RType s b a)

instance LensPath (PathType s b) v a =>  TLens s (Tagged b v) a where
  type LType s (Tagged b v) = LPathType (PathType s b) v
  type RType s (Tagged b v) a = Tagged b (LResType (PathType s b) v a)
  tlens f = fmap Tagged . lensPath @(PathType s b) f . untag

tlens' :: (Functor f, TLens s b a, a ~ LType s b, b ~ RType s b a)
       => Sing s -> (a -> f a) -> b -> f b
tlens' (_::Sing s) = tlens @s

--
-- multiple get/set by btree
class TGetSet a b where
  type GSType a b
  tget :: b -> GSType a b
  tset :: GSType a b -> b -> b

instance TGetSet (Leaf a) b => TGetSet (Leaf (Just a) :: BTree (Maybe Symbol)) b where
  type GSType (Leaf (Just a)) b = GSType (Leaf a) b
  tget = tget @(Leaf a)
  tset = tset @(Leaf a)

instance (TLens a b c, c ~ LType a b, b ~ RType a b c, SingI a)
      => TGetSet (Leaf a :: BTree Symbol) b where
  type GSType (Leaf a) b = LType a b
  tget = (^. tlens' (sing :: Sing a))
  tset = set (tlens' (sing :: Sing a))

instance (TGetSet l b, TGetSet r b) => TGetSet (Node l x r::BTree t) b where
  type GSType (Node l x r) b = (GSType l b, GSType r b)
  tget = (,) <$> tget @l <*> tget @r
  tset (a,b) = tset @l a . tset @r b

instance TGetSet (BTreeFromList ns) b => TGetSet (ns :: [Symbol]) b where
  type GSType ns b = GSType (BTreeFromList ns) b
  tget = tget @(BTreeFromList ns)
  tset = tset @(BTreeFromList ns)

------
class TShowHide a b where
  type HiddenType a b
  tshow :: HiddenType a b -> b
  thide :: b -> HiddenType a b

instance TShowHide ('[] :: [Symbol]) b where
  type HiddenType '[] b = b
  tshow = id
  thide = id

instance ( SingI (IsJust (BTreePath a b))
         , If (IsJust (BTreePath a b))
            ( bv ~ Tagged b v
            , TLens a bv (Hidden l), l ~ (LType a bv), r ~ RType a bv (Hidden l)
            , TLens a r l, bv ~ RType a r l, LType a r ~ Hidden l
            )
            (()::Constraint)
         )
      => TShowHide (a :: Symbol) (Tagged b v) where
  type HiddenType a (Tagged b v) =
    If (IsJust (BTreePath a b))
      (RType a (Tagged b v) (Hidden (LType a (Tagged b v))))
      (Tagged b v)
  tshow = case (sing :: Sing (IsJust (BTreePath a b))) of
    SFalse -> id
    STrue  -> tlens @a @(HiddenType a bv) @(LType a bv) %~ unHidden
  thide = case (sing :: Sing (IsJust (BTreePath a b))) of
    SFalse -> id
    STrue  -> tlens @a @bv @(Hidden (LType a bv)) %~ Hidden

instance (TShowHide a b, TShowHide as (HiddenType a b))
      => TShowHide (a ':as) b where
  type HiddenType (a ':as) b = HiddenType as (HiddenType a b)
  tshow = tshow @a . tshow @as
  thide = thide @as . thide @a
