{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Type.Grec.GrecLens(GrecLens(..)) where

import           Data.Tagged                   (Tagged (..))

import           Data.Singletons.Prelude
import           Data.Singletons.Prelude.Maybe (IsJust)
-- ((:==) , Lookup, Proxy)
import           Data.Type.Grec.FieldsGrec     (FieldsGrec, GrecWith (..),
                                                GrecWithout (..))
import           Data.Type.Grec.Grec           (Grec (..))
import           Data.Type.Grec.Lens           (LensedConstraint, nlens)

type GrecLensCons n a b = Lookup n (FieldsGrec b) ~ Just a

class GrecLens n a b where
  grecLens :: Functor f => Proxy n -> (a -> f a) -> b -> f b

instance LensedConstraint b n a => GrecLens n a (Grec b) where
  grecLens pn f = fmap Grec . nlens pn f . unGrec

--------------
class GrecLens' (eq::Bool) n a b where
  grecLens' :: Functor f => Proxy eq -> Proxy n -> (a -> f a) -> b -> f b

instance GrecLens' True n a (Tagged (n ': ns) (a,b)) where
  grecLens' _ _  f (Tagged (a,b))= Tagged . (,b) <$> f a

instance GrecLens n a (Tagged ns b)
      => GrecLens' False n a (Tagged (n' ': ns) (a',b)) where
  grecLens' _ pn f (Tagged (a,b)) = Tagged . (a,) . unTagged
                                  <$> grecLens pn f (Tagged b :: Tagged ns b)

instance GrecLens' (n :== n') n a (Tagged (n' ': n1' ': ns) (a',b))
      => GrecLens n a (Tagged (n' ': n1' ': ns) (a',b)) where
  grecLens = grecLens' (Proxy :: Proxy (n :== n'))

instance GrecLens n a (Tagged (n ': '[]) a) where
  grecLens pn f = fmap Tagged . f . unTagged
------------

instance (GrecLensCons n a (GrecWithout ns b), GrecLens n a b)
      => GrecLens n a (GrecWithout ns b) where
  grecLens pn f = fmap GWO . grecLens pn f . unGWO

instance (GrecLensCons n a (GrecWith ns b), GrecLens n a b)
      => GrecLens n a (GrecWith ns b) where
  grecLens pn f = fmap GW . grecLens pn f . unGW

-------------
instance GrecLens n a b1 => GrecLens' True n a (b1,b2) where
  grecLens' _ pn  f (b1,b2) = (,b2) <$> grecLens pn f b1

instance GrecLens n a b2 => GrecLens' False n a (b1,b2) where
  grecLens' _ pn  f (b1,b2) = (b1,) <$> grecLens pn f b2

instance GrecLens' (IsJust (Lookup n (FieldsGrec b1))) n a (b1,b2)
      => GrecLens n a (b1,b2) where
  grecLens = grecLens' (Proxy :: Proxy (IsJust (Lookup n (FieldsGrec b1))))
