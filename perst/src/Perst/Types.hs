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
import           Data.Singletons.TH
import           GHC.Exts                      (Constraint)
import           GHC.Prim                      (Proxy#, proxy#)
import           GHC.TypeLits

singletons
  [d| isSub :: Eq a => [a] -> [a] -> Bool
      isSub as bs = all (`elem` bs) as -- null $ aa \\ bs

      -- data Test a b = Test1 a (Test a b)
      --               | Test2 b

      allIsSub :: Eq a => [[a]] -> [a] -> Bool
      allIsSub ass ps = all (`isSub` ps) ass

      checkFK :: Eq a => [([(a,b)],(c,d))] -> [a] -> Bool
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

      submap :: Eq a => [a] -> [(a,b)] -> Maybe [b]
      submap as ps = let rs = map (`lookup` ps) as in
        if any_ isNothing rs then Nothing else Just (map fromJust rs)
  |]

singletonsOnly
  [d| posList :: Eq a => [a] -> [a] -> Maybe [Nat]
      posList as xs = let rs = map (`elemIndex` xs) as in
        if any_ isNothing rs then Nothing else Just (map fromJust rs)
  |]

-- type family OrC (a::Constraint) (b::Constraint) :: Constraint where
--   OrC () b  = ()
--   OrC a ()  = ()
