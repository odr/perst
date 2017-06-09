{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell           #-}
module Data.Type.Grec.Convert
  ( Convert(..)
    , IsConv, IsConvSym0
    , InternalType, InternalTypeSym0
  ) where

import           Data.Singletons.TH (genDefunSymbols)

class Convert a b where
  convert :: a -> b

type family IsConv a :: Bool where
  IsConv String = True
  IsConv [a]    = False
  IsConv _      = True

type family InternalType a :: * where
  InternalType [a] = a
  InternalType a   = a

genDefunSymbols [''IsConv, ''InternalType]
