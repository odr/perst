{-# LANGUAGE UndecidableInstances #-}
module Data.Type.Grec.ConvGrec( ConvToGrec(..), ConvFromGrec(..)) where

import           Data.Semigroup          (Semigroup (..))
import           Data.Tagged             (Tagged (..))
import           GHC.TypeLits            (Nat, Symbol)

import           Data.Type.Grec.Convert  (Convert (..))
import           Data.Type.Grec.ConvList (ConvList (..))
import           Data.Type.Grec.Grec     (Grec (..))

class ConvToGrec a b where
  convToGrec :: a -> b

class ConvFromGrec a b where
  convFromGrec :: a -> b

instance Convert (ConvList a, Grec r fs) (ConvList a)
      => ConvFromGrec (Grec r fs) [a] where
  convFromGrec = unConvList . convert . (,) (ConvList ([]::[a]))

instance Convert (ConvList a) (ConvList a, Grec r fs)
      => ConvToGrec [a] (Grec r fs) where
  convToGrec = snd . (convert :: ConvList a -> (ConvList a, Grec r fs)) . ConvList

instance Convert (ConvList a, Tagged ns b) (ConvList a)
      => ConvFromGrec (Tagged (ns :: [Symbol]) b) [a] where
  convFromGrec = unConvList . convert . (,) (ConvList ([]::[a]))

instance Convert (ConvList a) (ConvList a, Tagged ns b)
      => ConvToGrec [a] (Tagged (ns :: [Symbol]) b) where
  convToGrec = snd . (convert :: ConvList a -> (ConvList a, Tagged ns b)) . ConvList

instance (ConvFromGrec a c, ConvFromGrec b c, Semigroup c)
    => ConvFromGrec (a,b) c where
  convFromGrec (a,b) = convFromGrec a <> convFromGrec b

instance  ( Convert (ConvList c) (ConvList c, a)
          , Convert (ConvList c) (ConvList c, b)
          )
          => ConvToGrec [c] (a,b) where
  convToGrec cs = let (cs' :: ConvList c, a) = convert (ConvList cs) in
                    (a, snd (convert cs' :: (ConvList c, b)))
