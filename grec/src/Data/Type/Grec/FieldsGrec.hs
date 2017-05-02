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
    ) where

import           Data.Kind                    (Type)
import           Data.List                    (partition)
import           Data.Singletons.Prelude
import           Data.Singletons.Prelude.List
import           Data.Singletons.TH           (genDefunSymbols, singletons)
import           Data.Tagged                  (Tagged (..))

import           Data.Type.Grec.ConvGrec      (ConvFromGrec (..))
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
  |]

instance {-# OVERLAPPING #-}
      (SingI ns, SingI (FieldNamesGrec r), ConvFromGrec r [a])
      => ConvFromGrec (GrecWithout ns r) [a] where
  convFromGrec = map snd . without sns . zip sr . convFromGrec . unGWO
   where
    sns = fromSing (sing :: Sing ns)
    sr = fromSing (sing :: Sing (FieldNamesGrec r))

instance {-# OVERLAPPING #-}
      (SingI ns, SingI (FieldNamesGrec r), ConvFromGrec r [a])
      => ConvFromGrec (GrecWith ns r) [a] where
  convFromGrec = map snd . with sns . zip sr . convFromGrec . unGW
   where
    sns = fromSing (sing :: Sing ns)
    sr = fromSing (sing :: Sing (FieldNamesGrec r))

type family FieldsGrec a :: [(Symbol, Type)] where
  FieldsGrec (Tagged (ns :: [Symbol]) (b::Type))
    = TaggedToList (Tagged (ns :: [Symbol]) (b::Type))
  FieldsGrec (GrecWithout ns a) = Without ns (FieldsGrec a)
  FieldsGrec (GrecWith ns a) = With ns (FieldsGrec a)
  FieldsGrec a = Fields a

type FieldNamesGrec a = Map FstSym0 (FieldsGrec a)

type FieldTypesGrec a = Map SndSym0 (FieldsGrec a)

genDefunSymbols [''FieldsGrec, ''FieldNamesGrec, ''FieldTypesGrec]
