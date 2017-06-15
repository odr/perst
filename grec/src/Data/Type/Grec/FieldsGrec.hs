{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE UndecidableInstances      #-}
module Data.Type.Grec.FieldsGrec
    ( GrecWith(..)
    , GrecWithout(..)
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
    ) where

import           Data.Kind                    (Type)
import           Data.List                    (partition)
import           Data.Singletons.Prelude
import           Data.Singletons.Prelude.List
import           Data.Singletons.TH           (genDefunSymbols, singletons)
import           Data.Tagged                  (Tagged (..))

import           Data.Type.Grec.Convert       (IsConvSym0)
import           Data.Type.Grec.ConvGrec      (ConvFromGrec (..))
import           Data.Type.Grec.Grec          (Grec (..))
import           Data.Type.Grec.Type          (Fields, TaggedToList)

newtype GrecWithout (ns :: [Symbol]) a = GWO { unGWO :: a }
newtype GrecWith    (ns :: [Symbol]) a = GW  { unGW :: a }

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

instance -- {-# OVERLAPPING #-}
      (SingI ns, SingI (FieldNamesConvGrec r), ConvFromGrec r [a])
      => ConvFromGrec (GrecWithout ns r) [a] where
  convFromGrec = map snd . without sns . zip sr . convFromGrec . unGWO
   where
    sns = fromSing (sing :: Sing ns)
    sr = fromSing (sing :: Sing (FieldNamesConvGrec r))

instance -- {-# OVERLAPPING #-}
      (SingI ns, SingI (FieldNamesConvGrec r), ConvFromGrec r [a])
      => ConvFromGrec (GrecWith ns r) [a] where
  convFromGrec = map snd . with sns . zip sr . convFromGrec . unGW
   where
    sns = fromSing (sing :: Sing ns)
    sr = fromSing (sing :: Sing (FieldNamesConvGrec r))


type family FieldsGrec a :: [(Symbol, Type)] where
  -- FieldsGrec (Proxy (ns :: [(Symbol,Type)])) = ns
  FieldsGrec (Tagged (ns :: [Symbol]) (b::Type))
    = TaggedToList (Tagged ns b)
  FieldsGrec (GrecWithout ns a) = Without ns (FieldsGrec a)
  FieldsGrec (GrecWith ns a) = With ns (FieldsGrec a)
  FieldsGrec (Grec a) = Fields a
  FieldsGrec (a,b) = FieldsGrec a :++ FieldsGrec b

type FieldNamesGrec a = Map FstSym0 (FieldsGrec a)

type FieldTypesGrec a = Map SndSym0 (FieldsGrec a)

type FieldsConvGrec a = FilterBySnd IsConvSym0 (FieldsGrec a)

type FieldNamesConvGrec a = Map FstSym0 (FieldsConvGrec a)

type FieldTypesConvGrec a = Map SndSym0 (FieldsConvGrec a)

type FieldsNotConvGrec a = FilterByNotSnd IsConvSym0 (FieldsGrec a)

type FieldNamesNotConvGrec a = Map FstSym0 (FieldsNotConvGrec a)

type FieldTypesNotConvGrec a = Map SndSym0 (FieldsNotConvGrec a)

genDefunSymbols
  [ ''FieldsGrec, ''FieldNamesGrec, ''FieldTypesGrec
  , ''FieldsConvGrec, ''FieldNamesConvGrec, ''FieldTypesConvGrec
  , ''FieldsNotConvGrec, ''FieldNamesNotConvGrec, ''FieldTypesNotConvGrec
  ]
