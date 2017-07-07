{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE UndecidableInstances      #-}
module Perst.Types where
import           Data.Singletons.Prelude
import           Data.Singletons.Prelude.List
import           Data.Singletons.Prelude.Maybe
import           Data.Singletons.TH
import           GHC.Exts                      (Constraint)


promoteOnly
  [d| backTypes :: a -> (c -> (d, Bool)) -> (a -> d -> Symbol) -> [(b,c)] -> [(Symbol,Bool)]
      backTypes a nul f = map ((\(d,b) -> (f a d, b)) . nul . snd)

      mandatoryFields :: (c -> (d, Bool)) -> [(b,c)] -> [b]
      mandatoryFields f = map fst . filter (\(b,c) -> not $ snd $ f c)
  |]

-- type family FromConsList (a::[Constraint]) :: Constraint where
--   FromConsList '[] = ()
--   FromConsList (a ': b) = (a,FromConsList b)
