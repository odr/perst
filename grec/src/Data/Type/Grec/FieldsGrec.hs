{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE UndecidableInstances      #-}
module Data.Type.Grec.FieldsGrec
    ( GrecWith(..)
    , GrecWithout(..)
    , GrecLens(..)
    , NamesGrecLens(..)
    , FieldsGrec
    , FieldsGrecSym0
    , FieldNamesGrec
    , FieldTypesGrec
    , FieldNamesGrecSym0
    , FieldTypesGrecSym0
    , FieldsConvGrec
    , FieldsConvGrecSym0
    , FieldNamesConvGrec
    , FieldTypesConvGrec
    , FieldNamesConvGrecSym0
    , FieldTypesConvGrecSym0
    , FieldsNotConvGrec
    , FieldsNotConvGrecSym0
    , FieldNamesNotConvGrec
    , FieldTypesNotConvGrec
    , FieldNamesNotConvGrecSym0
    , FieldTypesNotConvGrecSym0
    , GWPairs, gwPairs
    , GWOPairs, gwoPairs
    ) where

import           Data.Function                 (on)
import           Data.Kind                     (Type)
import           Data.List                     (partition)
import           Data.Ord                      (comparing)
import           Data.Singletons.Prelude
import           Data.Singletons.Prelude.List
import           Data.Singletons.Prelude.Maybe (IsJust)
import           Data.Singletons.TH            (genDefunSymbols, promoteOnly,
                                                singletons)
import           Data.Tagged                   (Tagged (..))
import           Lens.Micro                    (set)
import           Lens.Micro.Extras             (view)

import           Data.Type.Grec.Convert        (IsConvSym0)
import           Data.Type.Grec.ConvGrec       (ConvFromGrec (..))
import           Data.Type.Grec.Grec           (Grec (..))
-- import           Data.Type.Grec.GrecLens
import           Data.Type.Grec.Lens           (LensedConstraint, nlens)
import           Data.Type.Grec.Type           (Fields, ListToPairs,
                                                TaggedToList)

newtype GrecWith    (ns :: [Symbol]) a = GW  { unGW :: a } deriving (Show)
newtype GrecWithout (ns :: [Symbol]) a = GWO { unGWO :: a } deriving (Show)

-------------

singletons
  [d| without :: Eq a => [a] -> [(a,b)] -> [(a,b)]
      without ns = filter ((`notElem` ns) . fst)

      with :: Eq a => [a] -> [(a,b)] -> [(a,b)]
      with ns = filter ((`elem` ns) . fst)

      split :: Eq a => [a] -> [(a,b)] -> ([(a,b)], [(a,b)])
      split ns = partition ((`elem` ns) . fst)

      filterBySnd :: (b -> Bool) -> [(a,b)] -> [(a,b)]
      filterBySnd f = filter (f . snd)

      filterByNotSnd :: (b -> Bool) -> [(a,b)] -> [(a,b)]
      filterByNotSnd f = filter (not . f . snd)
  |]

type family FieldsGrec a :: [(Symbol, Type)] where
  FieldsGrec (Tagged (ns :: [Symbol]) b) = TaggedToList (Tagged ns b)
  FieldsGrec (GrecWithout ns a) = Without ns (FieldsGrec a)
  FieldsGrec (GrecWith ns a) = With ns (FieldsGrec a)
  FieldsGrec (Grec a) = Fields a
  FieldsGrec (a,b) = FieldsGrec a :++ FieldsGrec b

genDefunSymbols [''FieldsGrec]

type FieldNamesGrec a = Map FstSym0 (FieldsGrec a)

type FieldTypesGrec a = Map SndSym0 (FieldsGrec a)

type FieldsConvGrec a = FilterBySnd IsConvSym0 (FieldsGrec a)

type FieldNamesConvGrec a = Map FstSym0 (FieldsConvGrec a)

type FieldTypesConvGrec a = Map SndSym0 (FieldsConvGrec a)

type FieldsNotConvGrec a = FilterByNotSnd IsConvSym0 (FieldsGrec a)

type FieldNamesNotConvGrec a = Map FstSym0 (FieldsNotConvGrec a)

type FieldTypesNotConvGrec a = Map SndSym0 (FieldsNotConvGrec a)

genDefunSymbols
  [ ''FieldNamesGrec, ''FieldTypesGrec
  , ''FieldsConvGrec, ''FieldNamesConvGrec, ''FieldTypesConvGrec
  , ''FieldsNotConvGrec, ''FieldNamesNotConvGrec, ''FieldTypesNotConvGrec
  ]

---------------
type GWPairs ns a  = ListToPairs (Map SndSym0 (FieldsGrec (GrecWith    ns a)))
type GWOPairs ns a = ListToPairs (Map SndSym0 (FieldsGrec (GrecWithout ns a)))

gwPairs :: NamesGrecLens ns (GWPairs ns a) (GrecWith ns a)
        => GrecWith ns a -> GWPairs ns a
gwPairs (x :: GrecWith ns a)
  = namesGrecGet (Proxy :: Proxy ns) x :: GWPairs ns a

gwoPairs  :: NamesGrecLens ns (GWOPairs ns a) (GrecWithout ns a)
          => GrecWithout ns a -> GWOPairs ns a
gwoPairs (x :: GrecWithout ns a)
  = namesGrecGet (Proxy :: Proxy ns) x :: GWOPairs ns a

instance (Eq (GWPairs ns a), NamesGrecLens ns (GWPairs ns a) (GrecWith ns a))
      => Eq (GrecWith ns a) where
  (==) = (==) `on` gwPairs

instance (Eq (GWOPairs ns a), NamesGrecLens ns (GWOPairs ns a) (GrecWithout ns a))
      => Eq (GrecWithout ns a) where
  (==) = (==) `on` gwoPairs

instance (Ord (GWPairs ns a), NamesGrecLens ns (GWPairs ns a) (GrecWith ns a))
      => Ord (GrecWith ns a) where
  compare = comparing gwPairs

instance (Ord (GWOPairs ns a), NamesGrecLens ns (GWOPairs ns a) (GrecWithout ns a))
      => Ord (GrecWithout ns a) where
  compare = comparing gwoPairs

---------------
instance (SingI ns, SingI (FieldNamesConvGrec r), ConvFromGrec r [a])
      => ConvFromGrec (GrecWithout ns r) [a] where
  convFromGrec = map snd . without sns . zip sr . convFromGrec . unGWO
   where
    sns = fromSing (sing :: Sing ns)
    sr = fromSing (sing :: Sing (FieldNamesConvGrec r))

instance (SingI ns, SingI (FieldNamesConvGrec r), ConvFromGrec r [a])
      => ConvFromGrec (GrecWith ns r) [a] where
  convFromGrec = map snd . with sns . zip sr . convFromGrec . unGW
   where
    sns = fromSing (sing :: Sing ns)
    sr = fromSing (sing :: Sing (FieldNamesConvGrec r))
-------------

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

type GrecLensCons n a b = Lookup n (FieldsGrec b) ~ Just a

instance (GrecLensCons n a (GrecWithout ns b), GrecLens n a b)
      => GrecLens n a (GrecWithout ns b) where
  grecLens pn f = fmap GWO . grecLens pn f . unGWO

instance (GrecLensCons n a (GrecWith ns b), GrecLens n a b)
      => GrecLens n a (GrecWith ns b) where
  grecLens pn f = fmap GW . grecLens pn f . unGW

------------
instance GrecLens n a b1 => GrecLens' True n a (b1,b2) where
  grecLens' _ pn  f (b1,b2) = (,b2) <$> grecLens pn f b1

instance GrecLens n a b2 => GrecLens' False n a (b1,b2) where
  grecLens' _ pn  f (b1,b2) = (b1,) <$> grecLens pn f b2

instance GrecLens' (IsJust (Lookup n (FieldsGrec b1))) n a (b1,b2)
      => GrecLens n a (b1,b2) where
  grecLens = grecLens' (Proxy :: Proxy (IsJust (Lookup n (FieldsGrec b1))))

-----------
class NamesGrecLens (ns :: [Symbol]) a b where
  namesGrecGet :: Proxy ns -> b -> a
  namesGrecSet :: Proxy ns -> a -> b -> b

instance NamesGrecLens '[] () b where
  namesGrecGet _ _ = ()
  namesGrecSet _ _ = id

instance GrecLens n a b => NamesGrecLens '[n] a b where
  namesGrecGet _ = view $ grecLens (Proxy :: Proxy n)
  namesGrecSet _ = set  $ grecLens (Proxy :: Proxy n)

instance (NamesGrecLens '[n] a1 b, NamesGrecLens (n1 ': ns) a2 b)
      => NamesGrecLens (n ': n1 ': ns) (a1,a2) b where
  namesGrecGet _  = (,) <$> namesGrecGet (Proxy :: Proxy '[n])
                        <*> namesGrecGet (Proxy :: Proxy (n1 ': ns))
  namesGrecSet _ (a1,a2)  = namesGrecSet (Proxy :: Proxy (n1 ': ns)) a2
                          . namesGrecSet (Proxy :: Proxy '[n]) a1
