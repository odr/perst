{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitNamespaces        #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE UndecidableInstances      #-}
module Data.Type.Bst
    ( Bst(..)
    -- | Strong balanced binary tree on type level
    , ListToBst
    -- | Type family to convert List to Binary Tree
    --   ListToBst doesn't nub data because it can silently remove some value.
    --   Application should check that there is no repetitions in some way
    , BstToList
    -- | Convert Tree to List
    , Rep(..)
    -- | Representation of types with many kinds as value
    , RepLens(..)
    -- | Lenses to working with fields by labels
    , BstCreate(..)
    -- | class to create BST and instance to create it from List
    , (:::)
    -- | convenient type to write "a" ::: Int instead of '("a", Int)
    , ShowBst(..)
    ) where

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

singletons [d|
    data Bst a  = Bst1 a
                | Bst2 a a
                | Bst3 (Bst a) a (Bst a)
        deriving Show
    bstToList :: Bst a -> [a]
    -- bstToList TNil            = []
    bstToList (Bst1 z)        = [z]
    bstToList (Bst2 z1 z2)    = [z1, z2]
    bstToList (Bst3 z1 z2 z3) = bstToList z1 ++ [z2] ++ bstToList z3

    cmpLst [] []         = True
    cmpLst [x] []        = True
    cmpLst [] (x:xs)     = False
    cmpLst (x1:x2:xs) [] = False
    cmpLst (x:xs) (y:ys) = cmpLst xs ys

    |]
promote [d|
    spl :: [a] -> [a] -> Bst a
    spl xs ys
        | cmpLst xs ys = Bst3 (listToB ys) (head xs) (listToB $ tail xs)
        | otherwise = spl (tail xs) (head xs : ys)
    listToB :: [a] -> Bst a
    listToB [z]     = Bst1 z
    listToB [z1,z2] = Bst2 z1 z2
    listToB zs      = spl zs []

    listToBst :: Ord a => [a] -> Bst a
    listToBst xs = listToB (sort xs)
    |]

type family Rep (t :: k) :: Type where
    -- Representation of types with many kinds as value
    Rep (a :: Type) = a

    Rep '[] = ()
    Rep ('[a] :: [k1]) = Rep a
    Rep ((a ': b) :: [k1]) = (Rep a, Rep b)

    Rep ('(a, b) :: (k1,k2)) = {- Tagged a -} (Rep b)

    Rep (Bst1 a :: Bst k1) = Rep a
    Rep (Bst2 a b :: Bst k1) = (Rep a, Rep b)
    Rep (Bst3 a b c :: Bst k1) = (Rep a, Rep b, Rep c)


class RepLens (a :: k1) (b :: k2) where
    repLens :: Proxy# a -> Proxy# b -> Lens' (Rep a) (Rep b)

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

instance RepLens (Bst1 a) a where
    repLens _ _ = id

instance RepLensB (If (a == c) 1 (If (b == c) 2 0)) (Bst2 a b) c
        => RepLens (Bst2 a b :: Bst k) (c :: k) where
    repLens pa pb = repLensB pa pb (proxy# :: Proxy# (If (a == c) 1 (If (b == c) 2 0)))

instance RepLensB (If (b == d) 2 (If (b :< d) 1 3)) (Bst3 a b c) d
        => RepLens (Bst3 a b c :: Bst k) (d :: k) where
    repLens pa pb = repLensB pa pb (proxy# :: Proxy# (If (b == d) 2 (If (b :< d) 1 3)))

-- instance RepLens (a :: Bst k) (TNil :: Bst k) where
--     repLens _ f v = const v <$> f ()

instance RepLens a b => RepLens (a :: Bst k) (Bst1 b :: Bst k) where
    repLens pa _ = repLens pa (proxy# :: Proxy# b)

instance (RepLens a b, RepLens a c)
        => RepLens (a :: Bst k) (Bst2 b c :: Bst k) where
    repLens pa _ = lens get set
      where
        get = (,) <$> (^. repLens pa (proxy# :: Proxy# b))
                  <*> (^. repLens pa (proxy# :: Proxy# c))
        set x (v1,v2) = x & repLens pa (proxy# :: Proxy# b) .~ v1
                          & repLens pa (proxy# :: Proxy# c) .~ v2

instance (RepLens a b, RepLens a c, RepLens a d)
        => RepLens (a :: Bst k) (Bst3 b c d :: Bst k) where
    repLens pa _ = lens get set
      where
        get =  (,,) <$> (^. repLens pa (proxy# :: Proxy# b))
                    <*> (^. repLens pa (proxy# :: Proxy# c))
                    <*> (^. repLens pa (proxy# :: Proxy# d))
        set x (v1,v2,v3) = x & repLens pa (proxy# :: Proxy# b) .~ v1
                             & repLens pa (proxy# :: Proxy# c) .~ v2
                             & repLens pa (proxy# :: Proxy# d) .~ v3

-- internal class to avoid infinite loop in type calculation (reason: If is not lazy)

class RepLensB (n::Nat) a b
  where
    repLensB :: Proxy# a -> Proxy# b -> Proxy# n -> Lens' (Rep a) (Rep b)

instance Rep (a ': as) ~ (Rep a, Rep as)
    => RepLensB 1 ((a ': as) :: [k]) (a :: k)
  where
    repLensB _ _ _ f (x,y) = (,y) <$> f x

instance    ( Rep (a ': as) ~ (Rep a, Rep as)
            , RepLens as b
            )
    => RepLensB 0 ((a ': as) :: [k]) (b :: k)
  where
    repLensB _ pb _ f (x,y) = (x,) <$> repLens (proxy# :: Proxy# as) pb f y

instance RepLensB 1 (Bst2 a b) a
  where
    repLensB _ _ _ f (x,y) = (,y) <$> f x

instance RepLensB 2 (Bst2 a b) b
  where
    repLensB _ _ _ f (x,y) = (x,) <$> f y

instance RepLensB 2 (Bst3 a b c) b
  where
    repLensB _ _ _ f (x,y,z) = (x,,z) <$> f y

instance RepLens a d => RepLensB 1 (Bst3 a b c) d
  where
    repLensB _ pd _ f (x,y,z) = (,y,z) <$> repLens (proxy# :: Proxy# a) pd f x

instance RepLens c d => RepLensB 3 (Bst3 a b c) d
  where
    repLensB _ pd _ f (x,y,z) = (x,y,) <$> repLens (proxy# :: Proxy# c) pd f z

class BstCreate (a :: k1) (b :: Bst k2) where
    bstCreate :: Proxy# a -> Proxy# b -> Rep a -> Rep b

instance RepLens a b => BstCreate (a :: [k1]) (Bst1 b :: Bst k1) where
    bstCreate pa _ x = x ^. repLens pa (proxy# :: Proxy# b)

instance (RepLens a b, RepLens a c)
        => BstCreate (a :: [k1]) (Bst2 b c :: Bst k1) where
    bstCreate pa _ = (,)
                <$> (^. repLens pa (proxy# :: Proxy# b))
                <*> (^. repLens pa (proxy# :: Proxy# c))

instance (BstCreate a l, BstCreate a r, RepLens a t)
        => BstCreate (a :: [k1]) (Bst3 l t r :: Bst k1) where
    bstCreate pa _ x =
        ( bstCreate pa (proxy# :: Proxy# l) x
        , x ^. repLens pa (proxy# :: Proxy# t)
        , bstCreate pa (proxy# :: Proxy# r) x
        )
class ShowBst a where
    showBst :: Int -> a -> String
instance {-# OVERLAPS #-} Show a => ShowBst a where
    showBst n a = replicate n '\t' ++ show a ++ "\n"
instance (ShowBst a, ShowBst b) => ShowBst (a,b) where
    showBst n (a,b) =  showBst n a ++ showBst n b
instance (ShowBst a, ShowBst b, ShowBst c) => ShowBst (a,b,c) where
    showBst n (a,b,c) = showBst n b
                        ++ showBst (n+1) a
                        ++ showBst (n+1) c
