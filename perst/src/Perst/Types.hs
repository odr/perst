{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE UndecidableInstances      #-}
module Perst.Types where
import           Data.Kind                     (Type)
import           Data.List                     (groupBy, nub, sortBy, (\\))
import           Data.Maybe                    (fromJust, isNothing)
import           Data.Singletons.Prelude
import           Data.Singletons.Prelude.List
import           Data.Singletons.Prelude.Maybe
import           Data.Singletons.TH            (promote, singletons,
                                                singletonsOnly)
import           GHC.Exts                      (Constraint)
import           GHC.Prim                      (Proxy#, proxy#)
import           GHC.TypeLits

-- type (:::) a b = '(a,b)
--
-- -- To write like (5 // "s" // Nothing) and get (5,("s",Nothing))
-- infixr 5 //
-- (//) :: a -> b -> (a,b)
-- a // b = (a,b)

singletons
  [d| isSub :: Eq a => [a] -> [a] -> Bool
      isSub as bs = all (`elem` bs) as -- null $ aa \\ bs

      -- isSubFst :: Eq a => [a] -> [(a,b)] -> Bool
      -- isSubFst a b = isSub a $ map fst b
      --
      -- allSubFst :: Eq a => [[a]] -> [(a,b)] -> Bool
      -- allSubFst ass ps = all (\as -> null $ as \\ map fst ps ) ass
      --
      -- checkFK :: Eq a => [([(a,b)],(c,d))] -> [(a,e)] -> Bool
      -- checkFK fks = allSubFst (map (map fst . fst) fks)

      allIsSub :: Eq a => [[a]] -> [a] -> Bool
      allIsSub ass ps = all (`isSub` ps) ass

      checkFK :: Eq a => [([(a,b)],(c,d))] -> [a] -> Bool
      -- checkFK fks = allIsSub (map (map fst . fst) fks)
      -- -- checkFK fks = allIsSub (map (fst . unzip) $ fst $ unzip fks)
      checkFK fks rs = all (\(fs,_) -> all (\(f,_) -> f `elem` rs) fs) fks

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

      -- subset :: Eq a => [a] -> [a] -> Bool
      -- subset as = null . (as \\)

      submap :: Eq a => [a] -> [(a,b)] -> Maybe [b]
      submap as ps = let rs = map (`lookup` ps) as in
        if any_ isNothing rs then Nothing else Just (map fromJust rs)
  |]

singletonsOnly
  [d| posList :: Eq a => [a] -> [a] -> Maybe [Nat]
      posList as xs = let rs = map (`elemIndex` xs) as in
        if any_ isNothing rs then Nothing else Just (map fromJust rs)
  |]
