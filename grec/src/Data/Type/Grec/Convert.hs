{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
-- {-# LANGUAGE IncoherentInstances       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE UndecidableInstances      #-}
module Data.Type.Grec.Convert(
      Grec(..)
    , ConvList(..)
    , Convert(..)
    , FieldsGrec
    , ConvToGrec(..)
    , ConvFromGrec(..)
    , GrecWithout(..)
    , GrecWith(..)
    , FieldNamesGrec
    , FieldTypesGrec
  ) where

import           Control.Arrow                ((***))
import           Data.Kind                    (Type)
import           Data.List                    (partition)
import           Data.Proxy                   (Proxy (..))
import           Data.Singletons.Prelude
import           Data.Singletons.Prelude.List
import           Data.Singletons.TH           (singletons)
import           Data.Tagged                  (Tagged (..), proxy, tagWith,
                                               untag)
import           Data.Type.Grec.Type          (Fields, TaggedToList)
import           GHC.Generics
import           GHC.TypeLits                 (Symbol)

class Convert a b where
  convert :: a -> b

instance Convert a a where
  convert = id

newtype ConvList a = ConvList { unConvList :: [a] }

instance (Convert b a, Convert (Tagged (cc ': ccc) c) (ConvList a))
        => Convert (Tagged (bb ': cc ': ccc) (b,c)) (ConvList a) where
  convert = ConvList . uncurry (:)
          . (convert *** unConvList . convert . tagWith (Proxy :: Proxy (cc ': ccc)))
          . unTagged

instance Convert b a => Convert (Tagged '[bb] b) (ConvList a) where
  convert = ConvList . (:[]) . convert . unTagged

instance (Convert a b, Convert (ConvList a) (Tagged (cc ': ccc) c))
      => Convert (ConvList a) (Tagged (bb ': (cc ': ccc)) (b,c)) where
  convert (ConvList (a1:a2:as))
    = Tagged (convert a1, untag (convert $ ConvList (a2:as) :: Tagged (cc ': ccc) c))
  convert _ = error "Invalid convert from ConvList to pair representation. List too short"

instance Convert a b => Convert (ConvList a) (Tagged '[bb] b) where
  convert (ConvList [a]) = Tagged $ convert a
  convert _ = error "Invalid convert from pair representation to ConvList. List too long"

newtype Grec a = Grec { unGrec :: a } deriving (Eq, Show)

type ToGrecConstr a r = (GToGrec a (Rep r))
type FromGrecConstr r a = (GFromGrec a (Rep r))

instance (ToGrecConstr a r, Generic r) => Convert (ConvList a) (Grec r) where
  convert = Grec . to . snd . gToGrec . unConvList

instance (FromGrecConstr r a, Generic r) => Convert (Grec r) (ConvList a) where
  convert = ConvList . gFromGrec . from . unGrec

-------------------------------------------------------

class GToGrec a g where
  gToGrec :: [a] -> ([a], g b)

instance Convert a b => GToGrec a (S1 c (Rec0 b)) where
  gToGrec []     = error "Not enough values to convert in 'GToGrec a b'"
  gToGrec (a:as) = (as, M1 . K1 $ convert a)

instance (GToGrec a x, GToGrec a y) => GToGrec a (x :*: y) where
  gToGrec xs = let (r,x) = gToGrec xs in (x :*:) <$> gToGrec r

instance GToGrec a b => GToGrec a (D1 (MetaData md1 md2 md3 False) (C1 mc b)) where
  gToGrec = fmap (M1 . M1) . gToGrec

instance (Generic b, GToGrec a (Rep b))
    => GToGrec a (D1 (MetaData md1 md2 md3 True) (C1 mc (S1 c (Rec0 b)))) where
      -- newtype => convert internal type
  gToGrec = fmap (M1 . M1 . M1 . K1 . to) . gToGrec


class GFromGrec a g where
  gFromGrec :: g b -> [a]

instance Convert b a => GFromGrec a (S1 c (K1 i b)) where
  gFromGrec (M1 (K1 b)) = [convert b]

instance (GFromGrec a x, GFromGrec a y) => GFromGrec a (x :*: y) where
  gFromGrec (x :*: y) = gFromGrec x ++ gFromGrec y

instance GFromGrec a b => GFromGrec a (D1 (MetaData md1 md2 md3 False) (C1 mc b)) where
  gFromGrec (M1 (M1 b)) = gFromGrec b

instance (Generic b, GFromGrec a (Rep b))
    => GFromGrec a (D1 (MetaData md1 md2 md3 True) (C1 mc (S1 c (K1 i b)))) where
      -- newtype => convert internal type
  gFromGrec (M1 (M1 (M1 (K1 b)))) = gFromGrec $ from b

------------------------------------------------------
newtype GrecWithout (ns :: [Symbol]) a = GWO { unGWO :: a }
newtype GrecWith (ns :: [Symbol]) a = GW { unGW :: a }

singletons
  [d| without :: Eq a => [a] -> [(a,b)] -> [(a,b)]
      without ns = filter ((`notElem` ns) . fst)

      with :: Eq a => [a] -> [(a,b)] -> [(a,b)]
      with ns = filter ((`elem` ns) . fst)

      split :: Eq a => [a] -> [(a,b)] -> ([(a,b)], [(a,b)])
      split ns = partition ((`elem` ns) . fst)
  |]

type family FieldsGrec a :: [(Symbol, Type)] where
  FieldsGrec (Tagged (ns :: [Symbol]) (b::Type)) = TaggedToList (Tagged (ns :: [Symbol]) (b::Type))
  FieldsGrec (GrecWithout ns a) = Without ns (FieldsGrec a)
  FieldsGrec (GrecWith ns a) = With ns (FieldsGrec a)
  FieldsGrec a = Fields a

type FieldNamesGrec a = Map FstSym0 (FieldsGrec a)
type FieldTypesGrec a = Map SndSym0 (FieldsGrec a)

class ConvToGrec a b where
  convToGrec :: a -> b

class ConvFromGrec a b where
  convFromGrec :: a -> b

instance {-# OVERLAPPABLE #-} Convert (Grec r) (ConvList a) => ConvFromGrec r [a] where
  convFromGrec = unConvList . convert . Grec

instance {-# OVERLAPPABLE #-} Convert (ConvList a) (Grec r) => ConvToGrec [a] r where
  convToGrec = unGrec . convert . ConvList

instance {-# OVERLAPPING #-} Convert (Tagged (ns :: [Symbol]) (b::Type)) (ConvList a)
      => ConvFromGrec (Tagged (ns :: [Symbol]) (b::Type)) [a] where
  convFromGrec = unConvList . convert

instance {-# OVERLAPPING #-} Convert (ConvList a) (Tagged (ns :: [Symbol]) (b::Type))
      => ConvToGrec [a] (Tagged (ns :: [Symbol]) (b::Type)) where
  convToGrec = convert . ConvList

instance {-# OVERLAPPING #-} (SingI ns, SingI (FieldNamesGrec r), ConvFromGrec r [a])
      => ConvFromGrec (GrecWithout ns r) [a] where
  convFromGrec = map snd . without sns . zip sr . convFromGrec . unGWO
   where
    sns = fromSing (sing :: Sing ns)
    sr = fromSing (sing :: Sing (FieldNamesGrec r))

instance {-# OVERLAPPING #-} (SingI ns, SingI (FieldNamesGrec r), ConvFromGrec r [a])
      => ConvFromGrec (GrecWith ns r) [a] where
  convFromGrec = map snd . with sns . zip sr . convFromGrec . unGW
   where
    sns = fromSing (sing :: Sing ns)
    sr = fromSing (sing :: Sing (FieldNamesGrec r))

-- instance (SingI ns, SingI (FieldNamesGrec r), ConvToGrec [a] r)
--       => ConvFromGrec [a] (GrecWithout ns r) where
--   convToGrec = convToGrec
