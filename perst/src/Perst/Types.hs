{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE UndecidableInstances      #-}
module Perst.Types where
import           Data.Kind                    (Type)
import           Data.List                    (groupBy, nub, sortBy, (\\))
import           Data.Singletons.Prelude
import           Data.Singletons.Prelude.List
import           Data.Singletons.TH           (singletons)
import           GHC.Exts                     (Constraint)
import           GHC.Prim                     (Proxy#, proxy#)
import           GHC.TypeLits

type (:::) a b = '(a,b)

singletons
    [d| isSub :: Eq a => [a] -> [a] -> Bool
        isSub a b = null $ a \\ b

        isSubFst :: Eq a => [a] -> [(a,b)] -> Bool
        isSubFst a b = isSub a $ map fst b

        allSubFst :: Eq a => [[a]] -> [(a,b)] -> Bool
        allSubFst ass ps = all (\as -> null $ as \\ map fst ps ) ass

        checkFK :: Eq a => [([(a,b)],(c,d))] -> [(a,e)] -> Bool
        checkFK fks = allSubFst (map (map fst . fst) fks)

        isNub :: Eq a => [a] -> Bool
        isNub xs = xs == nub xs

        allIsNub :: Eq a => [[a]] -> Bool
        allIsNub = all isNub

        pIsNub :: (Eq a, Eq b) => ([a],[b]) -> Bool
        pIsNub (as,bs) = isNub as && isNub bs

        fkIsNub :: (Eq a, Eq b) => [([(a,b)],(c,d))] -> Bool
        fkIsNub = all (pIsNub . unzip) . map fst

        -- comparing :: (Ord a) => (b -> a) -> b -> b -> Ordering
        -- comparing p x y = compare (p x) (p y)
        --
        -- sortByFst :: (Ord a) => [(a,b)] -> [(a,b)]
        -- sortByFst = sortBy (comparing fst)
        --
        -- on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
        -- on g f x y = g (f x) (f y)
        --
        -- groupByFst :: (Eq a) => [(a,b)] -> [[(a,b)]]
        -- groupByFst = groupBy (on (==) fst)
        --
        -- checkRec :: (Ord a, Eq a) => [(a,b)] -> Bool
        -- checkRec = all isS . groupByFst . sortByFst
        --   where
        --     isS xs = length (take 2 xs) == 1

        backTypes :: a -> (a -> c -> Symbol) -> [(b,c)] -> [Symbol]
        backTypes a f = map (f a . snd)

    |]

-- class KindToStar (a :: k) (b :: Type) where
--     k2s :: Proxy# a -> b
--
-- instance KindToStar '[] [a] where
--     k2s _ = []
--
-- instance KnownSymbol a => KindToStar a String where
--     k2s _ = symbolVal' (proxy# :: Proxy# a)
--
-- instance (KindToStar a b, KindToStar as [b]) => KindToStar ((a :: k) ': as) [b] where
--     k2s _ = k2s (proxy# :: Proxy# a) : k2s (proxy# :: Proxy# as)
--
-- instance (KindToStar a b, KindToStar c d) => KindToStar '(a,c) (b,d) where
--     k2s _ = (k2s (proxy# :: Proxy# a), k2s (proxy# :: Proxy# c))
