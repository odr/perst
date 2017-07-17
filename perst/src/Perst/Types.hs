{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE UndecidableInstances      #-}
module Perst.Types where
import           Data.Singletons.Prelude
import           Data.Singletons.Prelude.List
import           Data.Singletons.Prelude.Maybe
import           Data.Singletons.TH
import           GHC.Exts                      (Constraint)

singletons
  [d|
  fsts rs = map fst rs
  snds rs = map snd rs

  mandatoryFields :: (c -> (d, Bool)) -> [(b,c)] -> [b]
  mandatoryFields f = fsts . filter (\(b,c) -> not $ snd $ f c)
  backTypes :: a -> (c -> (d, Bool)) -> (a -> d -> s) -> [(b,c)] -> [(s,Bool)]
  backTypes a nul f = map ((\(d,b) -> (f a d, b)) . nul . snd)
  |]

singToProxy :: Sing s -> Proxy s
singToProxy _ = Proxy
