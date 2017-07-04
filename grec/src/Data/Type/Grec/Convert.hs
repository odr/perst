{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell           #-}
module Data.Type.Grec.Convert
  ( Convert(..)
    , IsConv, IsConvSym0, ConvType
    , InternalType
    -- , InternalTypeSym0
  ) where

import           Data.Singletons.TH  (genDefunSymbols)
import           GHC.TypeLits        (Nat)

import           Data.Type.Grec.Type (GrecGroup)

class Convert a b where
  convert :: a -> b

-- instance Convert (v1,v2,v3) (v1,(v2,v3)) where
--   convert (v1,v2,v3) = (v1,(v2,v3))
--
-- instance Convert (v1,(v2,v3)) (v1,v2,v3) where
--   convert (v1,(v2,v3)) = (v1,v2,v3)

type family IsConv a :: Bool where
  IsConv String = True
  IsConv [a]    = False
  IsConv _      = True

type family ConvType a :: Nat where
  ConvType String = 1
  ConvType [a]    = 0
  ConvType (GrecGroup a) = 2
  ConvType _      = 1

type family InternalType a :: * where
  InternalType [a] = a
  InternalType a   = a

genDefunSymbols [''IsConv]
