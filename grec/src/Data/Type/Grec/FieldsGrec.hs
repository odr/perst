{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE UndecidableInstances      #-}
module Data.Type.Grec.FieldsGrec
    ( GrecWith(..)
    , GrecWithout(..)
    , GrecLens(..)
    , NamesGrecLens(..)
    , ConvGrecInfo(..)
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
    , GWPairs, gwPairs, gwTPairs
    ) where

import           Data.Function                 (on)
import           Data.Kind                     (Constraint, Type)
import           Data.List                     (intersect, partition, (\\))
import           Data.Ord                      (comparing)
import           Data.Singletons.Prelude
import           Data.Singletons.Prelude.List
import           Data.Singletons.Prelude.Maybe (IsJust)
import           Data.Singletons.TH            (genDefunSymbols, promoteOnly,
                                                singletons)
import           Data.Tagged                   (Tagged (..))
import           Data.Text                     (Text)
import qualified Data.Text.Lazy                as TL
import           Lens.Micro                    (set)
import           Lens.Micro.Extras             (view)

import           Data.Type.Grec.Convert        (IsConvSym0)
import           Data.Type.Grec.ConvGrec       (ConvFromGrec (..))
import           Data.Type.Grec.Grec           (Grec (..))
import           Data.Type.Grec.Lens           (NLensed (..))
import           Data.Type.Grec.Type           (Fields, IsSub, ListToPairs,
                                                TaggedToList)

newtype GrecWith    (ns :: [Symbol]) a = GW  { unGW :: a } deriving (Show)
newtype GrecWithout (ns :: [Symbol]) a = GWO { unGWO :: a } deriving (Show)

-------------
singletons
  [d| without :: Eq a => [a] -> [(a,b)] -> [(a,b)]
      without ns = filter ((`notElem` ns) . fst)

      with :: Eq a => [a] -> [(a,b)] -> [(a,b)]
      with ns = filter ((`elem` ns) . fst)
  |]
promoteOnly
  [d| split :: Eq a => [a] -> [(a,b)] -> ([(a,b)], [(a,b)])
      split ns = partition ((`elem` ns) . fst)

      filterBySnd :: (b -> Bool) -> [(a,b)] -> [(a,b)]
      filterBySnd f = filter (f . snd)

      filterByNotSnd :: (b -> Bool) -> [(a,b)] -> [(a,b)]
      filterByNotSnd f = filter (not . f . snd)

      fsts :: [(a,b)] -> [a]
      fsts = map fst
      snds :: [(a,b)] -> [b]
      snds = map snd

      fieldNamesConvGrec', fieldNamesNotConvGrec' :: (b -> Bool) -> [(a,b)] -> [a]
      fieldTypesConvGrec', fieldTypesNotConvGrec' :: (b -> Bool) -> [(a,b)] -> [b]
      fieldNamesConvGrec'    f = map fst . filter (f . snd)
      fieldTypesConvGrec'    f = map snd . filter (f . snd)
      fieldNamesNotConvGrec' f = map fst . filter (not . f . snd)
      fieldTypesNotConvGrec' f = map snd . filter (not . f . snd)

  |]

-- class GrecInfo a where
--   type FieldsGrecInfo a :: [(Symbol, Type)]
--   sGrec :: Proxy a -> Sing (FieldsGrecInfo a)
--
-- instance SingI (TaggedToList (Tagged ns b))
--         => GrecInfo (Tagged (ns :: [Symbol]) b) where
--   type FieldsGrecInfo (Tagged (ns :: [Symbol]) b) = TaggedToList (Tagged ns b)
--   sGrec _ = sing :: Sing (TaggedToList (Tagged ns b))

type family FieldsGrec (a::k) :: [(Symbol, Type)] where
  FieldsGrec (xs :: [(Symbol,Type)]) = xs
  FieldsGrec () = '[]
  FieldsGrec (Tagged (ns :: [Symbol]) b) = TaggedToList (Tagged ns b)
  FieldsGrec (GrecWithout ns a) = Without ns (FieldsGrec a)
  FieldsGrec (GrecWith ns a) = With ns (FieldsGrec a)
  FieldsGrec (Grec a) = Fields a
  FieldsGrec (a,b) = FieldsGrec a :++ FieldsGrec b

genDefunSymbols [''FieldsGrec]

type FieldNamesGrec a = Fsts (FieldsGrec a)

type FieldTypesGrec a = Snds (FieldsGrec a)

type FieldsConvGrec a = FilterBySnd IsConvSym0 (FieldsGrec a)
type FieldsNotConvGrec a = FilterByNotSnd IsConvSym0 (FieldsGrec a)

type FieldNamesConvGrec a = FieldNamesConvGrec' IsConvSym0 (FieldsGrec a)
type FieldTypesConvGrec a = FieldTypesConvGrec' IsConvSym0 (FieldsGrec a)
type FieldNamesNotConvGrec a = FieldNamesNotConvGrec' IsConvSym0 (FieldsGrec a)
type FieldTypesNotConvGrec a = FieldTypesNotConvGrec' IsConvSym0 (FieldsGrec a)

-- type FieldNamesConvGrec a = Map FstSym0 (FieldsConvGrec a)
-- type FieldTypesConvGrec a = Map SndSym0 (FieldsConvGrec a)
-- type FieldNamesNotConvGrec a = Map FstSym0 (FieldsNotConvGrec a)
-- type FieldTypesNotConvGrec a = Map SndSym0 (FieldsNotConvGrec a)

genDefunSymbols
  [ ''FieldNamesGrec, ''FieldTypesGrec
  , ''FieldsConvGrec, ''FieldNamesConvGrec, ''FieldTypesConvGrec
  , ''FieldsNotConvGrec, ''FieldNamesNotConvGrec, ''FieldTypesNotConvGrec
  ]

-- grecToTagged :: ListToTaggedPairs (FieldsGrec r)

---------------
-- promoteOnly [d|
--   gwPairsL :: [Symbol] -> Type -> [Type]
--   gwPairsL ns r = map snd $ nubBy (\a b -> fst a == fst b)
--                 $ filter ((`elem` ns). fst) $ fieldsGrec r
--   gwoPairsL :: [Symbol] -> Type -> [Type]
--   gwoPairsL ns r = map snd $ nubBy (\a b -> fst a == fst b)
--                  $ filter (not . (`elem` ns). fst) $ fieldsGrec r
--   |]
type GWPairs ns a  = ListToPairs (Map SndSym0 (FieldsGrec (GrecWith    ns a)))
type GWOPairs ns a = ListToPairs (Map SndSym0 (FieldsGrec (GrecWithout ns a)))
-- (GwPairsL ns a) --
-- (GwoPairsL ns a) --

gwPairs :: (NamesGrecLens ns (GWPairs ns a) a)
        => GrecWith ns a -> GWPairs ns a
gwPairs (x :: GrecWith ns a) = namesGrecGet @ns (unGW x) :: GWPairs ns a

gwTPairs :: (NamesGrecLens ns (GWPairs ns a) a)
        => GrecWith ns a -> Tagged ns (GWPairs ns a)
gwTPairs = Tagged . gwPairs

gwoPairs :: (NamesGrecLens (FieldNamesGrec a :\\ ns) (GWOPairs ns a) a)
        => GrecWithout ns a -> GWOPairs ns a
gwoPairs (x :: GrecWithout ns a)
  = namesGrecGet @(FieldNamesGrec a :\\ ns) (unGWO x) :: GWOPairs ns a

gwoTPairs :: (NamesGrecLens (FieldNamesGrec a :\\ ns) (GWOPairs ns a) a)
  => GrecWithout ns a -> Tagged (FieldNamesGrec a :\\ ns) (GWOPairs ns a)
gwoTPairs = Tagged . gwoPairs

instance (Eq (GWPairs ns a), NamesGrecLens ns (GWPairs ns a) a)
      => Eq (GrecWith ns a) where
  (==) = (==) `on` gwPairs

instance (Ord (GWPairs ns a), NamesGrecLens ns (GWPairs ns a) a)
      => Ord (GrecWith ns a) where
  compare = comparing gwPairs

---------------
instance (SingI ns, ConvGrecInfo r, ConvFromGrec r [a])
      => ConvFromGrec (GrecWithout ns r) [a] where
  convFromGrec = map snd . without sns . zip sr . convFromGrec . unGWO
   where
    sns = fromSing (sing :: Sing ns)
    sr = fieldNames @r

instance ( SingI ns, ConvGrecInfo r, ConvFromGrec r [a]
         , IsSub ns (FieldNamesGrec r) ~ True
         )
      => ConvFromGrec (GrecWith ns r) [a] where
  convFromGrec = map snd . with sns . zip sr . convFromGrec . unGW
   where
    sns = fromSing (sing :: Sing ns)
    sr = fieldNames @r
-------------

class GrecLens n a b where
  grecLens :: Functor f => (a -> f a) -> b -> f b

instance NLensed n b a => GrecLens n a (Grec b) where
  grecLens f = fmap Grec . nlens @n f . unGrec

instance GrecLens '(True, n) a (Tagged (n ': ns) (a,b)) where
  grecLens f (Tagged (a,b)) = Tagged . (,b) <$> f a

instance GrecLens n a (Tagged ns b)
      => GrecLens '(False, n) a (Tagged (n' ': ns) (a',b)) where
  grecLens f (Tagged (a,b)) = Tagged . (a,) . unTagged
                            <$> grecLens @n f (Tagged b :: Tagged ns b)

instance GrecLens '((n :== n'), n) a (Tagged (n' ': n1' ': ns) (a',b))
      => GrecLens n a (Tagged (n' ': n1' ': ns) (a',b)) where
  grecLens = grecLens @'(n :== n', n)

instance GrecLens n a (Tagged (n ': '[]) a) where
  grecLens f = fmap Tagged . f . unTagged
------------

instance (Elem n ns ~ 'False, GrecLens n a b)
      => GrecLens n a (GrecWithout ns b) where
  grecLens f = fmap GWO . grecLens @n f . unGWO

instance (Elem n ns ~ 'True, GrecLens n a b)
      => GrecLens n a (GrecWith ns b) where
  grecLens f = fmap GW . grecLens @n f . unGW

------------
instance GrecLens n a b1 => GrecLens '(True, n) a (b1,b2) where
  grecLens f (b1,b2) = (,b2) <$> grecLens @n f b1

instance GrecLens n a b2 => GrecLens '(False, n) a (b1,b2) where
  grecLens f (b1,b2) = (b1,) <$> grecLens @n f b2

instance GrecLens '(IsJust (Lookup n (FieldsGrec b1)), n) a (b1,b2)
      => GrecLens n a (b1,b2) where
  grecLens = grecLens @'(IsJust (Lookup n (FieldsGrec b1)), n)

-----------
class NamesGrecLens (ns :: [Symbol]) a b where
  namesGrecGet :: b -> a
  namesGrecSet :: a -> b -> b

instance NamesGrecLens '[] () b where
  namesGrecGet _ = ()
  namesGrecSet _ = id

instance GrecLens n a b => NamesGrecLens '[n] a b where
  namesGrecGet = view $ grecLens @n
  namesGrecSet = set  $ grecLens @n

instance (NamesGrecLens '[n] a1 b, NamesGrecLens (n1 ': ns) a2 b)
      => NamesGrecLens (n ': n1 ': ns) (a1,a2) b where
  namesGrecGet = (,) <$> namesGrecGet @'[n] <*> namesGrecGet @(n1 ': ns)
  namesGrecSet (a1,a2) = namesGrecSet @(n1 ': ns) a2 . namesGrecSet @'[n] a1

-----------
class ConvGrecInfo r where
  fieldNames :: [Text]

  fieldNamesTL :: [TL.Text]
  fieldNamesTL = map TL.fromStrict $ fieldNames @r

instance ConvGrecInfo () where
  fieldNames = []

instance SingI ns => ConvGrecInfo (Tagged (ns :: [Symbol]) b) where
  fieldNames = fromSing (sing :: Sing ns)

instance (SingI ns, ConvGrecInfo a) => ConvGrecInfo (GrecWithout ns a) where
  fieldNames = fieldNames @a \\ fromSing (sing :: Sing ns)

instance (SingI ns, ConvGrecInfo a) => ConvGrecInfo (GrecWith ns a) where
  fieldNames = intersect (fieldNames @a) (fromSing (sing :: Sing ns))

instance SingI (FieldNamesConvGrec (Grec a)) => ConvGrecInfo (Grec a) where
  fieldNames = fromSing (sing :: Sing (FieldNamesConvGrec (Grec a)))

instance (ConvGrecInfo a, ConvGrecInfo b) => ConvGrecInfo (a,b) where
  fieldNames = fieldNames @a ++ fieldNames @b
