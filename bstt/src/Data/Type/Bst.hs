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
    , (:::) -- | convenient type to write "a" ::: Int instead of '("a", Int)
    ) where

import           Data.Kind                   (Type)
import           Data.List                   (sort, splitAt)
import           Data.Promotion.Prelude      (type ($$), (:+$), (:-$), (:>=$))
import           Data.Promotion.Prelude.List
import           Data.Promotion.TH           ((:<), promote)
import           Data.Proxy                  (Proxy (..))
import           Data.Tagged                 (Tagged)
import           Data.Type.Bool              (If)
import           Data.Type.Equality          (type (==))
import           GHC.Prim                    (Proxy#, proxy#)
import           GHC.TypeLits                (Nat)
import           Lens.Micro                  (Lens', lens, (&), (.~), (^.))

type (:::) (a::k1) (b::k2) = '(a,b)

promote [d|
    data Bst a  = TNil
                | Bst1 a
                | Bst2 a a
                | Bst3 (Bst a) a (Bst a)
        deriving Show

    listToBst :: Ord a => [a] -> Bst a
    listToBst xs = go (sort xs) $ length xs
      where
        go [] _ = TNil
        go [z] _ = Bst1 z
        go [z1,z2] _ = Bst2 z1 z2
        go zs cnt = Bst3 (go ls lc) r (go rs $ cnt - lc - 1)
          where
            lc = d2 cnt
            (ls,r:rs) = splitAt lc zs
            d2 n = g n 0
                where
                  g m k
                    | k >= m = m
                    | otherwise = g (m-1) (k+1)

    bstToList :: Bst a -> [a]
    bstToList TNil            = []
    bstToList (Bst1 z)        = [z]
    bstToList (Bst2 z1 z2)    = [z1, z2]
    bstToList (Bst3 z1 z2 z3) = bstToList z1 ++ [z2] ++ bstToList z3

    |]

type family Rep (t :: k) :: Type where
    -- | Representation of types with many kinds as value
    Rep '[] = ()
    Rep ('[a] :: [k1]) = Rep a
    Rep (a :: Type) = a -- Rep () = Rep '[]  ==> not injection
    Rep ((a ': b) :: [k1]) = (Rep a, Rep b)
    Rep ('(a, b) :: (k1,k2)) = {- Tagged a -} (Rep b)
    Rep (TNil :: Bst k1) = ()
    Rep (Bst1 a :: Bst k1) = Rep a
    Rep (Bst2 a b :: Bst k1) = (Rep a, Rep b)
    Rep (Bst3 a b c :: Bst k1) = (Rep a, Rep b, Rep c)

class RepLens (a :: k1) (b :: k2) where
    repLens :: Proxy# '(a,b) -> Lens' (Rep a) (Rep b)

instance RepLens (a :: [k]) ('[] :: [k]) where
    repLens _ f v = const v <$> f ()

instance (RepLens a b)
        => RepLens (a :: [k]) ('[b] :: [k]) where
    repLens _ = repLens (proxy# :: Proxy# '(a,b))

instance (RepLens as b1, RepLens as (b2 ': bs)) => RepLens as (b1 ': b2 ': bs)
  where
    repLens _ = lens get set
      where
        get = (,) <$> (^. repLens (proxy# :: Proxy# '(as,b1)))
                  <*> (^. repLens (proxy# :: Proxy# '(as,(b2 ': bs))))
        set x (v1,v2) = x & repLens (proxy# :: Proxy# '(as,b1)) .~ v1
                          & repLens (proxy# :: Proxy# '(as,(b2 ': bs))) .~ v2

instance RepLens ('[a] :: [k]) (a :: k) where
    repLens _ = id

instance (RepLensB (If (a == b) 1 0) (a ': a1 ': as) b)
        => RepLens ((a ': a1 ': as) :: [k]) (b :: k) where
    repLens p = repLensB p (proxy# :: Proxy# (If (a == b) 1 0))

instance RepLens (Bst1 a) a where
    repLens _ = id

instance RepLensB (If (a == c) 1 (If (b == c) 2 0)) (Bst2 a b) c
        => RepLens (Bst2 a b :: Bst k) (c :: k) where
    repLens p = repLensB p (proxy# :: Proxy# (If (a == c) 1 (If (b == c) 2 0)))

instance RepLensB (If (b == d) 2 (If (b :< d) 1 3)) (Bst3 a b c) d
        => RepLens (Bst3 a b c :: Bst k) (d :: k) where
    repLens p = repLensB p (proxy# :: Proxy# (If (b == d) 2 (If (b :< d) 1 3)))

instance RepLens (a :: Bst k) (TNil :: Bst k) where
    repLens _ f v = const v <$> f ()

instance RepLens a b => RepLens (a :: Bst k) (Bst1 b :: Bst k) where
    repLens _ = repLens (proxy# :: Proxy# '(a,b))

instance (RepLens a b, RepLens a c)
        => RepLens (a :: Bst k) (Bst2 b c :: Bst k) where
    repLens _ = lens get set
      where
        get = (,) <$> (^. repLens (proxy# :: Proxy# '(a,b)))
                  <*> (^. repLens (proxy# :: Proxy# '(a,c)))
        set x (v1,v2) = x & repLens (proxy# :: Proxy# '(a,b)) .~ v1
                          & repLens (proxy# :: Proxy# '(a,c)) .~ v2

instance (RepLens a b, RepLens a c, RepLens a d)
        => RepLens (a :: Bst k) (Bst3 b c d :: Bst k) where
    repLens _ = lens get set
      where
        get =  (,,) <$> (^. repLens (proxy# :: Proxy# '(a,b)))
                    <*> (^. repLens (proxy# :: Proxy# '(a,c)))
                    <*> (^. repLens (proxy# :: Proxy# '(a,d)))
        set x (v1,v2,v3) = x & repLens (proxy# :: Proxy# '(a,b)) .~ v1
                             & repLens (proxy# :: Proxy# '(a,c)) .~ v2
                             & repLens (proxy# :: Proxy# '(a,d)) .~ v3

-- | internal class to avoid infinite loop in type calculation (reason: If is not lazy)
class RepLensB (n::Nat) a b
  where
    repLensB :: Proxy# '(a,b) -> Proxy# n -> Lens' (Rep a) (Rep b)

instance Rep (a ': as) ~ (Rep a, Rep as)
    => RepLensB 1 ((a ': as) :: [k]) (a :: k)
  where
    repLensB _ _ f (x,y) = (,y) <$> f x

instance    ( Rep (a ': as) ~ (Rep a, Rep as)
            , RepLens as b
            )
    => RepLensB 0 ((a ': as) :: [k]) (b :: k)
  where
    repLensB _ _ f (x,y) = (x,) <$> repLens (proxy# :: Proxy# '(as,b)) f y

instance RepLensB 1 (Bst2 a b) a
  where
    repLensB _ _ f (x,y) = (,y) <$> f x

instance RepLensB 2 (Bst2 a b) b
  where
    repLensB _ _ f (x,y) = (x,) <$> f y

instance RepLensB 2 (Bst3 a b c) b
  where
    repLensB _ _ f (x,y,z) = (x,,z) <$> f y

instance RepLens a d => RepLensB 1 (Bst3 a b c) d
  where
    repLensB _ _ f (x,y,z) = (,y,z) <$> repLens (proxy# :: Proxy# '(a,d)) f x

instance RepLens c d => RepLensB 3 (Bst3 a b c) d
  where
    repLensB _ _ f (x,y,z) = (x,y,) <$> repLens (proxy# :: Proxy# '(c,d)) f z

class BstCreate (a :: k1) (b :: Bst k2) where
    bstCreate :: Proxy# '(a,b) -> Rep a -> Rep b

instance BstCreate (a :: [k1]) (TNil :: Bst k1) where
    bstCreate _ _ = ()

instance RepLens a b => BstCreate (a :: [k1]) (Bst1 b :: Bst k1) where
    bstCreate _ x = x ^. repLens (proxy# :: Proxy# '(a, b))

instance (RepLens a b, RepLens a c)
        => BstCreate (a :: [k1]) (Bst2 b c :: Bst k1) where
    bstCreate _ = (,)
                <$> (^. repLens (proxy# :: Proxy# '(a, b)))
                <*> (^. repLens (proxy# :: Proxy# '(a, c)))

instance (BstCreate a l, BstCreate a r, RepLens a t)
        => BstCreate (a :: [k1]) (Bst3 l t r :: Bst k1) where
    bstCreate _ x =
        ( bstCreate (proxy# :: Proxy# '(a,l)) x
        , x ^. repLens (proxy# :: Proxy# '(a, t))
        , bstCreate (proxy# :: Proxy# '(a,r)) x
        )
