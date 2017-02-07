{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE UndecidableInstances      #-}
module Pers.Types where
import           Data.List                    (groupBy, nub, (\\))
import           Data.Promotion.TH            (promoteOnly)
import           Data.Singletons.Prelude
import           Data.Singletons.Prelude.List
import           GHC.Exts                     (Constraint)
import           GHC.Prim                     (Proxy#, proxy#)
import           GHC.TypeLits

type (:::) a b = '(a,b)

promoteOnly
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

        comparing :: (Ord a) => (b -> a) -> b -> b -> Ordering
        comparing p x y = compare (p x) (p y)

        sortByFst :: (Ord a) => [(a,b)] -> [(a,b)]
        sortByFst = sortBy (comparing fst)

        on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
        on g f x y = g (f x) (f y)

        groupByFst :: (Eq a) => [(a,b)] -> [[(a,b)]]
        groupByFst = groupBy (on (==) fst)

        isSingleton :: [a] -> Bool
        isSingleton [x] = True
        isSingleton _   = False

        checkRec :: [(a,b)] -> Bool
        checkRec = all isSingleton . groupByFst . sortByFst

        -- uniRec :: [(a,b)] -> [(a,b)]
        -- uniRec = map head . groupByFst . sortByFst

    |]

class KindToStar (a :: k) (b :: *) where
    k2s :: Proxy# a -> b

instance KindToStar '[] [a] where
    k2s _ = []

instance KnownSymbol a => KindToStar a String where
    k2s _ = symbolVal' (proxy# :: Proxy# a)

instance (KindToStar a b, KindToStar as [b]) => KindToStar ((a :: k) ': as) [b] where
    k2s _ = k2s (proxy# :: Proxy# a) : k2s (proxy# :: Proxy# as)

instance (KindToStar a b, KindToStar c d) => KindToStar '(a,c) (b,d) where
    k2s _ = (k2s (proxy# :: Proxy# a), k2s (proxy# :: Proxy# c))
