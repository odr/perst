module Data.Type.Grec.Convert(
      Convert(..)
  ) where

class Convert a b where
  convert :: a -> b

-- instance Convert a a where
--   convert = id
