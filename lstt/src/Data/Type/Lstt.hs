{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitNamespaces        #-}
{-# LANGUAGE RankNTypes                #-}
-- {-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE UndecidableInstances      #-}
module Data.Type.Lstt
    -- ( Bst(..)
    -- -- | Strong balanced binary tree on type level
    -- , ListToBst
    -- -- | Type family to convert List to Binary Tree
    -- --   ListToBst doesn't nub data because it can silently remove some value.
    -- --   Application should check that there is no repetitions in some way
    -- , BstToList
    -- -- | Convert Tree to List
    -- , Rep(..)
    -- -- | Representation of types with many kinds as value
    -- , RepLens(..)
    -- -- | Lenses to working with fields by labels
    -- , BstCreate(..)
    -- -- | class to create BST and instance to create it from List
    -- , (:::)
    -- -- | convenient type to write "a" ::: Int instead of '("a", Int)
    -- , ShowBst(..)
    -- ) where
    where
import           Data.Kind                    (Type)
import           Data.List                    (sort, splitAt)
import           Data.Proxy                   (Proxy (..))

import           Data.Singletons.Prelude
import           Data.Singletons.Prelude.List
import           Data.Singletons.TH           ((:<), promote, singletons)
import           Data.Type.Bool               (If)
import           Data.Type.Equality           (type (==))
import           GHC.Prim                     (Proxy#, proxy#)
import           GHC.TypeLits                 (Nat)
import           Lens.Micro                   (Lens', lens, (&), (.~), (^.))

type (:::) (a::k1) (b::k2) = '(a,b)

type family ToStar (t :: k) :: Type where
    -- Representation of types with many kinds as value
    ToStar (a :: Type) = a

    ToStar '[] = ()
    ToStar ('[a] :: [k1]) = ToStar a
    ToStar ((a ': b) :: [k1]) = (ToStar a, ToStar b)

    ToStar ('(a, b) :: (k1,k2)) = {- Tagged a -} ToStar b


class RepLens (a :: k1) (b :: k2) where
    repLens :: Proxy# a -> Proxy# b -> Lens' (ToStar a) (ToStar b)

instance RepLens (a :: [k]) ('[] :: [k]) where
    repLens _ _ f v = const v <$> f ()

instance (RepLens a b)
        => RepLens (a :: [k]) ('[b] :: [k]) where
    repLens pa _ = repLens pa (proxy# :: Proxy# b)

instance (RepLens as b1, RepLens as (b2 ': bs)) => RepLens as (b1 ': b2 ': bs)
  where
    repLens pas _ = lens get set
      where
        get = (,) <$> (^. repLens pas (proxy# :: Proxy# b1))
                  <*> (^. repLens pas (proxy# :: Proxy# (b2 ': bs)))
        set x (v1,v2) = x & repLens pas (proxy# :: Proxy# b1) .~ v1
                          & repLens pas (proxy# :: Proxy# (b2 ': bs)) .~ v2

instance RepLens ('[a] :: [k]) (a :: k) where
    repLens _ _ = id

instance (RepLensB (If (a == b) 1 0) (a ': a1 ': as) b)
        => RepLens ((a ': a1 ': as) :: [k]) (b :: k) where
    repLens pa pb = repLensB pa pb (proxy# :: Proxy# (If (a == b) 1 0))

-- internal class to avoid infinite loop in type calculation (reason: If is not lazy)

class RepLensB (n::Nat) a b
  where
    repLensB :: Proxy# a -> Proxy# b -> Proxy# n -> Lens' (ToStar a) (ToStar b)

instance ToStar (a ': as) ~ (ToStar a, ToStar as)
    => RepLensB 1 ((a ': as) :: [k]) (a :: k)
  where
    repLensB _ _ _ f (x,y) = (,y) <$> f x

instance    ( ToStar (a ': as) ~ (ToStar a, ToStar as)
            , RepLens as b
            )
    => RepLensB 0 ((a ': as) :: [k]) (b :: k)
  where
    repLensB _ pb _ f (x,y) = (x,) <$> repLens (proxy# :: Proxy# as) pb f y
