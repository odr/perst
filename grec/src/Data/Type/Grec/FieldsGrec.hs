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
    , GWPairs, gwPairs
    , GWOPairs, gwoPairs
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
import           Lens.Micro                    (set)
import           Lens.Micro.Extras             (view)

import           Data.Type.Grec.Convert        (IsConvSym0)
import           Data.Type.Grec.ConvGrec       (ConvFromGrec (..))
import           Data.Type.Grec.Grec           (Grec (..))
-- import           Data.Type.Grec.GrecLens
import           Data.Text                     (Text)
import qualified Data.Text.Lazy                as TL
import           Data.Type.Grec.Lens           (NLensed (..))
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

class GrecInfo a where
  type FieldsGrecInfo a :: [(Symbol, Type)]
  sGrec :: Proxy a -> Sing (FieldsGrecInfo a)

instance SingI (TaggedToList (Tagged ns b))
        => GrecInfo (Tagged (ns :: [Symbol]) b) where
  type FieldsGrecInfo (Tagged (ns :: [Symbol]) b) = TaggedToList (Tagged ns b)
  sGrec _ = sing :: Sing (TaggedToList (Tagged ns b))

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

gwPairs :: (NamesGrecLens ns (GWPairs ns a) (GrecWith ns a))
        => GrecWith ns a -> GWPairs ns a
gwPairs (x :: GrecWith ns a) = namesGrecGet @ns x :: GWPairs ns a

gwoPairs  :: (NamesGrecLens ns (GWOPairs ns a) (GrecWithout ns a))
          => GrecWithout ns a -> GWOPairs ns a
gwoPairs (x :: GrecWithout ns a) = namesGrecGet @ns x :: GWOPairs ns a

instance  ( Eq (GWPairs ns a)
          , NamesGrecLens ns (GWPairs ns a) (GrecWith ns a)
          )
          => Eq (GrecWith ns a) where
  (==) = (==) `on` gwPairs

instance  ( Eq (GWOPairs ns a)
          , NamesGrecLens ns (GWOPairs ns a) (GrecWithout ns a)
          )
          => Eq (GrecWithout ns a) where
  (==) = (==) `on` gwoPairs

instance  ( Ord (GWPairs ns a)
          , NamesGrecLens ns (GWPairs ns a) (GrecWith ns a)
          )
      => Ord (GrecWith ns a) where
  compare = comparing gwPairs

instance  ( Ord (GWOPairs ns a)
          , NamesGrecLens ns (GWOPairs ns a) (GrecWithout ns a)
          )
      => Ord (GrecWithout ns a) where
  compare = comparing gwoPairs

---------------
instance (SingI ns, ConvGrecInfo r, ConvFromGrec r [a])
      => ConvFromGrec (GrecWithout ns r) [a] where
  convFromGrec = map snd . without sns . zip sr . convFromGrec . unGWO
   where
    sns = fromSing (sing :: Sing ns)
    sr = fieldNames @r

instance (SingI ns, ConvGrecInfo r, ConvFromGrec r [a])
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

--------------
class GrecLens' (eq::Bool) n a b where
  grecLens' :: Functor f => (a -> f a) -> b -> f b

instance GrecLens' True n a (Tagged (n ': ns) (a,b)) where
  grecLens' f (Tagged (a,b)) = Tagged . (,b) <$> f a

instance GrecLens n a (Tagged ns b)
      => GrecLens' False n a (Tagged (n' ': ns) (a',b)) where
  grecLens' f (Tagged (a,b)) = Tagged . (a,) . unTagged
                            <$> grecLens @n f (Tagged b :: Tagged ns b)

instance GrecLens' (n :== n') n a (Tagged (n' ': n1' ': ns) (a',b))
      => GrecLens n a (Tagged (n' ': n1' ': ns) (a',b)) where
  grecLens = grecLens' @(n :== n') @n

instance GrecLens n a (Tagged (n ': '[]) a) where
  grecLens f = fmap Tagged . f . unTagged
------------

instance GrecLens n a b => GrecLens n a (GrecWithout ns b) where
  grecLens f = fmap GWO . grecLens @n f . unGWO

instance GrecLens n a b => GrecLens n a (GrecWith ns b) where
  grecLens f = fmap GW . grecLens @n f . unGW

------------
instance GrecLens n a b1 => GrecLens' True n a (b1,b2) where
  grecLens' f (b1,b2) = (,b2) <$> grecLens @n f b1

instance GrecLens n a b2 => GrecLens' False n a (b1,b2) where
  grecLens' f (b1,b2) = (b1,) <$> grecLens @n f b2

instance GrecLens' (IsJust (Lookup n (FieldsGrec b1))) n a (b1,b2)
      => GrecLens n a (b1,b2) where
  grecLens = grecLens' @(IsJust (Lookup n (FieldsGrec b1))) @n

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
  -- scgi :: Sing (FieldNamesConvGrec r)
  -- scgi = sing

  fieldNames :: [Text]
  -- fieldNames = fromSing (scgi @r)

  fieldNamesTL :: [TL.Text]
  fieldNamesTL = map TL.fromStrict $ fieldNames @r

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

-- instance (SingI (a :: [Symbol]), SingI (b :: [Symbol])) => SingI (a :++ b) where
--   sing = (sing @a) %:++ (sing @b)


-- -----------------
-- -- singletons [d|
-- --   data DelInfo k = DelInfo { [k] k
-- --   |]
-- --
-- -- instance SingI (DelInfo '[] val) where
-- --   sing = SDelInfo SNil
--
-- -- data DelInfo k = DelInfo { [k] k
--
-- class SingDel (xs :: [k]) where
--   singDel :: Proxy (a :: k) -> Sing (Delete a xs)
--
-- class SingDelA (a :: k) (xs :: [k]) where
--   singDelA :: Sing (Delete a xs)
--
-- class SingDelB (b :: Bool) (xs :: [k])  where
--   singDelB :: Proxy (a :: k) -> Sing (Delete a xs)
-- --
-- -- class SingDelBB (b :: Bool) (xs ::[k]) where
-- --   singDelBB :: Proxy (a :: k) -> Sing (Delete a xs)
-- --
-- instance SingDel '[] where
--   singDel _ = SNil
-- --
-- instance SingDel ((x :: k) ': xs) where
--   singDel (pa :: Proxy a) = undefined --singDelB @k @(a :== x) @(x ': xs) pa
-- -- instance SingDelB b (x ': xs) => SingDel ((x :: k) ': xs) where
-- --   singDel (pa :: Proxy a) = singDelB @k @(a :== x) @(x ': xs) pa
--
-- -- instance SingDelB (xs :: [k]) where
-- --   singDelB (_ :: Proxy b) (pa :: Proxy a) = singDelBB @k @b @xs pa
-- --
--
-- -- instance (SingI xs) => SingDelB 'True (x ': xs) where
-- --   singDelB = sing @[k] @xs
-- --
-- -- instance (SingI x, SingDel xs a, Delete a (x ': xs) ~ (x ': Delete a xs))
-- --     => SingDelB 'False ((x :: k) ': xs) a where
-- --   singDelB = SCons (sing @k @x) (singDelete @k @xs @a)
--
--
--
-- class SingDelete (xs :: [k]) (a :: k) where
--   singDelete :: Sing (Delete a xs)
--
-- class SingDeleteB (b::Bool) (xs :: [k]) (a :: k) where
--   singDeleteB :: Sing (Delete a xs)
--
-- instance SingDelete '[] a where
--   singDelete = SNil
--
-- instance SingDeleteB (x :== a) (x ': xs) a
--       => SingDelete ((x :: k) ': xs) a where
--   singDelete = singDeleteB @k @(x :== a) @(x ': xs) @a
--
-- instance (SingI xs, Delete a (a ': xs) ~ xs)
--       => SingDeleteB 'True (a ': xs) (a :: k) where
--   singDeleteB = sing @[k] @xs
--
-- instance (SingI x, SingDelete xs a, Delete a (x ': xs) ~ (x ': Delete a xs))
--     => SingDeleteB 'False ((x :: k) ': xs) a where
--   singDeleteB = SCons (sing @k @x) (singDelete @k @xs @a)
--
-- -- f :: SingDel xs => Proxy (xs :: [k]) -> Proxy (a :: k) -> Sing (Delete a xs)
-- -- f (_ :: Proxy (xs::[k])) pa = singDel @k @xs pa
--
--
--
-- -- class SingDeleteList (xs :: [k]) (zs :: [k]) where
-- --   singDeleteList :: Sing (xs :\\ zs)
-- --
-- -- instance SingI xs => SingDeleteList (xs :: [k]) '[] where
-- --   singDeleteList = sing @[k] @xs
-- --
-- -- instance (('[] :\\ (z ': zs)) ~ '[])
-- --       => SingDeleteList '[] (z ': zs :: [k]) where
-- --   singDeleteList = SNil
-- --
-- -- instance SingDeleteList ((x :: k) ': xs) (z ': zs) where
--
-- -- instance (SingDeleteList xs zs, SingDelete xs z)
-- --       => SingDeleteList (xs :: [k]) (z ': zs) where
-- --   singDeleteList = case singDeleteList @k @xs @zs
--
-- -- instance GrecWith ns r
--
-- -- instance SingI
-- --
-- -- SingI (n1 ': ns) => (SingI n1, SingI ns)
-- -- instance (SingI (n ': ns), SingI (Delete a ns)) => SingI (Delete a (n ': ns)) where
--   -- sing = singB @(a :== n)
--
-- -- class SingIB (b::Bool) x where
-- --   singB :: Sing x
-- --
-- -- instance Sing ns => SingIB True (Delete a (a ': ns)) where
-- --   singB = case testEquality (Sing (Deflete a (a ': ns))) (Sing ())
-- --   sing :: Sing ns
--
--
-- -- ns :~: ns :\\ '[]
-- -- -- s :: Sing (ns :\\ a)
-- -- s = SCons n0 (SCons n1 (... (SCons SNil)))
-- --
-- -- instance SingI (ns :\\ as) => SingI (ns :\\ (a ': as)) where
-- --   sing = if Elem a ns
-- --
-- -- instance SingI (ns :: [Symbol]) => SingI ns :\\ ds
