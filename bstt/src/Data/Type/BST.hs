{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitNamespaces        #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE UndecidableInstances      #-}
module Data.Type.BST
    ( BST(..)
    , ListToBST
    , BstToList
    , Rep(..)
    -- , RepL(..)
    -- , RepN(..)
    , RepLens(..)
    , Rec1
    , (:::)
    , Data.Promotion.Prelude.List.Sort
    ) where

import           Data.Kind                   (Type)
import           Data.List                   (sort, splitAt)
import           Data.Promotion.Prelude      (type ($$), (:+$), (:-$), (:>=$))
import           Data.Promotion.Prelude.List
import           Data.Promotion.TH           ((:<), promote)
import           Data.Proxy                  (Proxy (..))
import           Data.Tagged                 (Tagged, tagWith)
import           Data.Type.Bool              (If)
import           Data.Type.Equality          (type (==))
import           GHC.Prim                    (Proxy#, proxy#)
import           GHC.TypeLits                (Nat)
import           Lens.Micro                  (Lens', lens, (&), (.~), (^.))

type (:::) (a::k1) (b::k2) = '(a,b)

promote [d|
    data BST a  = TNil
                | BST1 a
                | BST2 a a
                | BST3 (BST a) a (BST a)
        deriving Show

    -- listToBST doesn't nub data because it can silently remove some value.
    -- Application should check that there is no repetitions in some way
    listToBST :: Ord a => [a] -> BST a
    listToBST xs = go (sort xs) $ length xs
      where
        go [] _ = TNil
        go [z] _ = BST1 z
        go [z1,z2] _ = BST2 z1 z2
        go zs cnt = BST3 (go ls lc) r (go rs $ cnt - lc - 1)
          where
            lc = d2 cnt
            (ls,r:rs) = splitAt lc zs
            d2 n = g n 0
                where
                  g m k
                    | k >= m = m
                    | otherwise = g (m-1) (k+1)

    bstToList :: BST a -> [a]
    bstToList TNil            = []
    bstToList (BST1 z)        = [z]
    bstToList (BST2 z1 z2)    = [z1, z2]
    bstToList (BST3 z1 z2 z3) = bstToList z1 ++ [z2] ++ bstToList z3

    |]

type family Rep (t :: k) :: Type where
    Rep '[] = ()
    Rep ('[a] :: [k1]) = Rep a
    Rep (a :: Type) = a -- Rep () = Rep '[]  ==> not injection
    Rep ((a ': b) :: [k1]) = (Rep a, Rep b)
    Rep ('(a, b) :: (k1,k2)) = Tagged a (Rep b)
    Rep (TNil :: BST k1) = ()
    Rep (BST1 a :: BST k1) = Rep a
    Rep (BST2 a b :: BST k1) = (Rep a, Rep b)
    Rep (BST3 a b c :: BST k1) = (Rep a, Rep b, Rep c)

    {-
--------- Initialization, Conversion ----------------
-- | We need interface to make strong-typed Named Record from weak-typed generalized struct.
--
-- So there is type 'Lifted' where field values lifted in some functor.
-- Common case is @Lifted Maybe@. Then we have 'Default' instance for @Lifted Maybe T@.
-- And then fill it using Lenses.
type family Lifted f a where
    Lifted f (Tagged (n::k) v) = Tagged n (Lifted f v)
    Lifted f (a,b) = (Lifted f a, Lifted f b)
    Lifted f (v :: Type) = f v
-}
{-

type family RepL (f :: Type -> Type) (t :: k) :: Type where
    RepL f '[] = ()
    RepL f ('[a] :: [k1]) = RepL f a
    RepL f (a :: Type) = f a
    RepL f ((a ': b) :: [k1]) = (RepL f a, RepL f b)
    RepL f ('(a, b) :: (k1,k2)) = Tagged a (RepL f b)
    RepL f (TNil :: BST k1) = ()
    RepL f (BST1 a :: BST k1) = RepL f a
    RepL f (BST2 a b :: BST k1) = (RepL f a, RepL f b)
    RepL f (BST3 a b c :: BST k1) = (RepL f a, RepL f b, RepL f c)

class RepN (a :: k) where
    repN :: Proxy# a -> RepL Maybe a
    -- repToMaybe :: Proxy# a -> Rep a -> RepL Maybe a
    -- repFromMaybe :: Proxy# a -> RepL Maybe a -> Maybe (Rep a)

instance RepN (a :: Type) where
    repN _ = Nothing

instance RepN a => RepN ('[a] :: [k]) where
    repN _ = repN (proxy# :: Proxy# a)

instance (RepN a, RepN (b ': c)) => RepN ((a ': b ': c) :: [k]) where
    repN _ = (repN (proxy# :: Proxy# a), repN (proxy# :: Proxy# (b ': c)))

instance RepN b => RepN ( '(a,b) :: (k1,k2)) where
    repN _ = tagWith (Proxy :: Proxy a) $ repN (proxy# :: Proxy# b)

instance RepN a => RepN (BST1 a :: BST k) where
    repN _ = repN (proxy# :: Proxy# a)

instance (RepN a, RepN b) => RepN (BST2 a b :: BST k) where
    repN _ = (repN (proxy# :: Proxy# a), repN (proxy# :: Proxy# b))

instance (RepN a, RepN b, RepN c) => RepN (BST3 a b c :: BST k) where
    repN _ = (repN (proxy# :: Proxy# a), repN (proxy# :: Proxy# b), repN (proxy# :: Proxy# c))
-}
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

instance (RepLensB (If (a == b) 1 0) (a : as) b)
        => RepLens ((a ': as) :: [k]) (b :: k) where
    repLens p = repLensB p (proxy# :: Proxy# (If (a == b) 1 0))

instance RepLens (BST1 a) a where
    repLens _ = id

instance RepLensB (If (a == c) 1 (If (b == c) 2 0)) (BST2 a b) c
        => RepLens (BST2 a b :: BST k) (c :: k) where
    repLens p = repLensB p (proxy# :: Proxy# (If (a == c) 1 (If (b == c) 2 0)))

instance RepLensB (If (b == d) 2 (If (b :< d) 1 3)) (BST3 a b c) d
        => RepLens (BST3 a b c :: BST k) (d :: k) where
    repLens p = repLensB p (proxy# :: Proxy# (If (b == d) 2 (If (b :< d) 1 3)))

instance RepLens (a :: BST k) (TNil :: BST k) where
    repLens _ f v = const v <$> f ()

instance RepLens a b => RepLens (a :: BST k) (BST1 b :: BST k) where
    repLens _ = repLens (proxy# :: Proxy# '(a,b))

instance (RepLens a b, RepLens a c)
        => RepLens (a :: BST k) (BST2 b c :: BST k) where
    repLens _ = lens get set
      where
        get = (,) <$> (^. repLens (proxy# :: Proxy# '(a,b)))
                  <*> (^. repLens (proxy# :: Proxy# '(a,c)))
        set x (v1,v2) = x & repLens (proxy# :: Proxy# '(a,b)) .~ v1
                          & repLens (proxy# :: Proxy# '(a,c)) .~ v2

instance (RepLens a b, RepLens a c, RepLens a d)
        => RepLens (a :: BST k) (BST3 b c d :: BST k) where
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

instance RepLensB 1 (BST2 a b) a
  where
    repLensB _ _ f (x,y) = (,y) <$> f x

instance RepLensB 2 (BST2 a b) b
  where
    repLensB _ _ f (x,y) = (x,) <$> f y

instance RepLensB 2 (BST3 a b c) b
  where
    repLensB _ _ f (x,y,z) = (x,,z) <$> f y

instance RepLens a d => RepLensB 1 (BST3 a b c) d
  where
    repLensB _ _ f (x,y,z) = (,y,z) <$> repLens (proxy# :: Proxy# '(a,d)) f x

instance RepLens c d => RepLensB 3 (BST3 a b c) d
  where
    repLensB _ _ f (x,y,z) = (x,y,) <$> repLens (proxy# :: Proxy# '(c,d)) f z


type Rec = ListToBST (Map HeadSym0 (Group (Sort '[
        3,4,2,1,6,7,12,32,12,42,35
        -- ,64,78,23,32,45,65,84,32,18
        -- ,43,55,64,1,2,3,4,5,6,7
        -- ,24,8,9,10,11,12,13,14,15,16
        -- ,81,82,83,84,85,86,87,88,89,90
        -- ,51,52,53,54,55,56,57,58,59,60
        -- ,17,18,19,20,21,22
        ,23,24,25,26,27,28,29,30])))
type Rec1 = '[ "t1":::Int, "a":::String, "z":::Char, "d":::[Int]

    ]
        {--------------------------------------------------
test = Proxy :: Proxy Rec
-- test = Proxy :: Proxy (ListToBST (Map HeadSym0 (Group (Sort Rec))))
-}
