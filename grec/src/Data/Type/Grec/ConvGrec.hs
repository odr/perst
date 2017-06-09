{-# LANGUAGE UndecidableInstances #-}
module Data.Type.Grec.ConvGrec( ConvToGrec(..), ConvFromGrec(..)) where

import           Data.Kind               (Type)
import           Data.Semigroup          (Semigroup (..))
import           Data.Singletons.Prelude (Sing, SingI)
import           Data.Tagged             (Tagged (..))
import           GHC.TypeLits            (Symbol)

import           Data.Type.Grec.Convert  (Convert (..))
import           Data.Type.Grec.ConvList (ConvList (..))
-- import           Data.Type.Grec.FieldsGrec (FieldNamesGrec, GrecWith (..),
--                                             GrecWithout (..))
import           Data.Type.Grec.Grec     (Grec (..))

class ConvToGrec a b where
  convToGrec :: a -> b

class ConvFromGrec a b where
  convFromGrec :: a -> b

instance -- {-# OVERLAPPABLE #-}
      Convert (Grec r) (ConvList a)
      => ConvFromGrec (Grec r) [a] where
  convFromGrec = unConvList . convert -- . Grec

instance -- {-# OVERLAPPABLE #-}
      Convert (ConvList a) (Grec r)
      => ConvToGrec [a] (Grec r) where
  convToGrec = {- unGrec . -} convert . ConvList

instance -- {-# OVERLAPPING #-}
      Convert (Tagged (ns :: [Symbol]) (b::Type)) (ConvList a)
      => ConvFromGrec (Tagged (ns :: [Symbol]) (b::Type)) [a] where
  convFromGrec = unConvList . convert

instance -- {-# OVERLAPPING #-}
      Convert (ConvList a) (Tagged (ns :: [Symbol]) (b::Type))
      => ConvToGrec [a] (Tagged (ns :: [Symbol]) (b::Type)) where
  convToGrec = convert . ConvList

instance (ConvFromGrec a c, ConvFromGrec b c, Semigroup c)
    => ConvFromGrec (a,b) c where
  convFromGrec (a,b) = convFromGrec a <> convFromGrec b

instance (Convert (ConvList c) (ConvList c, a), Convert (ConvList c) b)
    => ConvToGrec [c] (a,b) where
  convToGrec cs = let (cs' :: ConvList c, a) = convert (ConvList cs) in
                    (a, convert cs')
