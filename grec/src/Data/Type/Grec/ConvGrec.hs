{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Type.Grec.ConvGrec( ConvToGrec(..), ConvFromGrec(..)) where

import           Data.Semigroup         (Semigroup (..))
import           Data.Tagged            (Tagged)

import           Data.Type.Grec.Convert (Convert (..), Grec)
-- import           Data.Type.Grec.Grec    (Grec)

class ConvToGrec a b where
  convToGrec :: a -> b

class ConvFromGrec a b where
  convFromGrec :: a -> b

instance ConvFromGrec () [b] where
  convFromGrec _ = []

instance ConvToGrec [b] ([b],()) where
  convToGrec = (,())

instance Convert (Grec r) [a] => ConvFromGrec (Grec r) [a] where
  convFromGrec = convert

instance Convert (Tagged ns bs) [a] => ConvFromGrec (Tagged ns bs) [a] where
  convFromGrec = convert

instance Convert [a] ([a],Grec r) => ConvToGrec [a] (Grec r) where
  convToGrec = snd . (convert :: [a] -> ([a],Grec r))

instance Convert [a] ([a],Tagged ns bs) => ConvToGrec [a] (Tagged ns bs) where
  convToGrec = snd . (convert :: [a] -> ([a],Tagged ns bs))

instance (ConvFromGrec a c, ConvFromGrec b c, Semigroup c)
    => ConvFromGrec (a,b) c where
  convFromGrec (a,b) = convFromGrec a <> convFromGrec b

instance (Convert [c] ([c],a), ConvToGrec [c] b) => ConvToGrec [c] (a,b) where
  convToGrec cs = let (cs'::[c], a) = convert cs in (a, convToGrec cs')
