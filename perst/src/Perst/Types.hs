{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE UndecidableInstances      #-}
module Perst.Types where
import           Data.Kind                    (Type)
import           Data.List                    (groupBy, nub, sortBy, (\\))
import           Data.Singletons.Prelude
import           Data.Singletons.Prelude.List
import           Data.Singletons.TH           (promote, singletons)
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

      backTypes :: a -> (c -> (d, Bool)) -> (a -> d -> Symbol) -> [(b,c)] -> [(Symbol,Bool)]
      backTypes a nul f = map ((\(d,b) -> (f a d, b)) . nul . snd)

      mandatoryFields :: (c -> (d, Bool)) -> [(b,c)] -> [b]
      mandatoryFields f = map fst . filter (\(b,c) -> not $ snd $ f c)
  |]

promote
  [d|
      subset :: Eq a => [a] -> [a] -> Bool
      subset as = null . (as \\)
      -- subNub [] _ = True
      -- subNub (a:_) [] = False
      -- subNub (a:as) bs = let (b1,b2) = span (/= a) bs in
      --   case b2 of
      --     []    -> False
      --     b:bs2 -> subNub as $ b1 ++ bs2
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
