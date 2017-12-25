{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE UndecidableInstances      #-}
module Data.Type.GrecTree.Convert where

import           Data.Bifunctor                   (bimap, first)
import           Data.Kind                        (Type)
import           Data.Promotion.TH                (genDefunSymbols)
import           Data.Singletons.Prelude
import           Data.Singletons.Prelude.Function (OnSym2)
import           Data.Singletons.Prelude.List     (IntersectBy, SortBy)
import           Data.Singletons.Prelude.Maybe    (FromMaybe)
import           Data.Tagged                      (Tagged (..), retag, untag)
import           Data.Text                        (Text)
import           GHC.TypeLits                     (AppendSymbol, Symbol)

import           Data.Type.GrecTree.BTree
import           Data.Type.GrecTree.Grec

class Convert a b where
  convert :: a -> b

instance Convert a a where
  convert = id

instance (Convert a [c], Convert b [c]) => Convert (a,b) [c] where
  convert = uncurry (++) . bimap convert convert

instance (Convert [c] (a,[c]), Convert [c] (b,[c]))
        => Convert [c] ((a,b),[c]) where
  convert cs = ((a,b),rb)
    where
      (a,ra) = convert @_ @(a,[c]) cs
      (b,rb) = convert ra

convToMaybe :: (Convert [b] (a,[b]), Eq b) => b -> [b] -> (Maybe a,[b])
convToMaybe def (x:xs)
  | x == def  = (Nothing,xs)
  | otherwise = first Just $ convert (x:xs)
convToMaybe _ [] = error "Empty input list in convToMaybe"

-- Functor?
instance Convert va vb
      => Convert (Tagged (Leaf a) va) (Tagged (Leaf b) vb) where
  convert = Tagged . convert . untag

instance ( Convert (Tagged la vla) (Tagged lb vlb)
         , Convert (Tagged ra vra) (Tagged rb vrb)
         )
      => Convert (Tagged (Node la t ra) (vla,vra))
                 (Tagged (Node lb t rb) (vlb,vrb)) where
  convert = Tagged . bimap convertL convertR . untag
    where
      convertL = untag @lb . convert . Tagged @la
      convertR = untag @rb . convert . Tagged @ra

-- Fold?
-- data ConvType = CTSkip | CTSimple --  CTGroup
-- type family GetConvType x :: ConvType where
--   GetConvType [z] = CTSkip
--   -- GetConvType (Tagged (x::BTree k) v) = CTGroup
--   GetConvType x = CTSimple

instance Convert va mb => Convert (Tagged (Leaf na :: BTree k) va) mb where
  convert = convert . untag

instance (Convert (Tagged la vla) mb, Convert (Tagged ra vra) mb, Monoid mb)
      => Convert (Tagged (Node la t ra :: BTree k) (vla,vra)) mb where
  convert = uncurry mappend
          . bimap (convert . Tagged @la) (convert . Tagged @ra)
          . untag

instance (Convert mb (Tagged la vla, mb), Convert mb (Tagged ra vra, mb))
      => Convert mb (Tagged (Node la t ra :: BTree k) (vla,vra), mb) where
  convert xs = (Tagged (vla,vra), rs')
    where
      (Tagged vla,rs)  = convert @mb @(Tagged la vla, mb) xs
      (Tagged vra,rs') = convert @mb @(Tagged ra vra, mb) rs

instance Convert (Tagged (BTreeFromList ns) a) [b]
      => Convert (Tagged (ns::[Symbol]) a) [b] where
  convert = convert @(Tagged (BTreeFromList ns) a) . retag

instance Convert [b] (Tagged (BTreeFromList ns) a, [b])
      => Convert [b] (Tagged (ns :: [Symbol]) a, [b]) where
  convert = first retag . convert @_ @(Tagged (BTreeFromList ns) a, [b])


convWithRest :: Convert mb (v,mb) => mb -> (v,mb)
convWithRest = convert

convNoRest :: Convert mb (v,mb) => mb -> v
convNoRest = fst . convWithRest

instance Convert mb (v, mb)
      => Convert mb (Tagged (Leaf t :: BTree k) v, mb) where
  convert xs = first Tagged $ convert @mb @(v, mb) xs

instance (Grec a, Convert (GrecTagged a) [b])
      => Convert (Tagged () a) [b] where
  convert = convert . toTagged . untag
instance (Grec a, Convert (GrecTagged a) [b])
      => Convert (Tagged (s::Symbol) a) [b] where
  convert = convert . toTagged . untag
instance (Grec a, Convert (GrecTagged a) [b])
      => Convert (Tagged (s::Maybe Symbol) a) [b] where
  convert = convert . toTagged . untag

instance (Grec a, Convert [b] (GrecTagged a, [b]) )
      => Convert [b] (Tagged () a, [b]) where
  convert = first (Tagged . fromTagged) . convert
instance (Grec a, Convert [b] (GrecTagged a, [b]) )
      => Convert [b] (Tagged (s::Symbol) a, [b]) where
  convert = first (Tagged . fromTagged) . convert
instance (Grec a, Convert [b] (GrecTagged a, [b]) )
      => Convert [b] (Tagged (s::Maybe Symbol) a, [b]) where
  convert = first (Tagged . fromTagged) . convert

------------------------ ConvNames -----------------
class SConvNames tag (s::Maybe Symbol) (v::Type) where
  type SFldNames tag s v :: [Symbol]
  type SFldNames tag s v = '[FromMaybe "" s]
  type SFldTypes tag s v :: [*]
  type SFldTypes tag s v = '[v]
  sFldNames :: SingI (SFldNames tag s v) => [Text]
  sFldNames = fromSing (sing :: Sing (SFldNames tag s v))

type SConsNames tag s v = (SConvNames tag s v, SingI (SFldNames tag s v))

class ConvNames tag (v :: Type) where
  type FldNames tag v :: [Symbol]
  type FldNames tag v = FldNames tag (GrecTagged v)
  type FldTypes tag v :: [*]
  type FldTypes tag v = FldTypes tag (GrecTagged v)
  type FldNamesTypes tag v :: [(Symbol,Type)]
  type FldNamesTypes tag v = Zip (FldNames tag v) (FldTypes tag v)
  type FldNamesTypesSort tag v :: [(Symbol,Type)]
  type FldNamesTypesSort tag v = SortBy (ComparingSym1 FstSym0) (FldNamesTypes tag v)
  fldNames :: SingI (FldNames tag v) => [Text]
  fldNames = fromSing (sing :: Sing (FldNames tag v))

type SubsetNamesTypes tag a b =
  (IntersectBy (OnSym2 (:==$) FstSym0)
               (FldNamesTypesSort tag a) (FldNamesTypesSort tag b))
  ~ FldNamesTypesSort tag a

type ConsNames tag v = (ConvNames tag v, SingI (FldNames tag v))

instance SConsNames tag t a => ConvNames tag (Tagged (Leaf t) a) where
  type FldNames tag (Tagged (Leaf t) a) = SFldNames tag t a
  type FldTypes tag (Tagged (Leaf t) a) = SFldTypes tag t a

instance (ConvNames tag (Tagged l vl), ConvNames tag (Tagged r vr))
      => ConvNames tag (Tagged (Node l t r) (vl,vr)) where
  type FldNames tag (Tagged (Node l t r) (vl,vr))
        = FldNames tag (Tagged l vl) :++ FldNames tag (Tagged r vr)
  type FldTypes tag (Tagged (Node l t r) (vl,vr))
        = FldTypes tag (Tagged l vl) :++ FldTypes tag (Tagged r vr)

instance ConvNames tag (Tagged (BTreeFromList ns) a)
      => ConvNames tag (Tagged (ns :: [Symbol]) a) where
  type FldNames tag (Tagged ns a) = FldNames tag (Tagged (BTreeFromList ns) a)
  type FldTypes tag (Tagged ns a) = FldTypes tag (Tagged (BTreeFromList ns) a)

data AllFld
instance (Grec v, ConvNames AllFld (GrecTagged v))
      => SConvNames AllFld ms1 (Tagged ('Nothing :: Maybe Symbol) v) where
  type SFldNames AllFld ms1 (Tagged ('Nothing :: Maybe Symbol) v)
    = FldNames AllFld (GrecTagged v)
  type SFldTypes AllFld ms1 (Tagged ('Nothing :: Maybe Symbol) v)
    = FldTypes AllFld (GrecTagged v)

instance SConvNames AllFld s (Maybe a) where
  type SFldTypes AllFld s (Maybe a) = '[Maybe a]

data EmptyFld
instance SConvNames EmptyFld ms a where
  type SFldNames EmptyFld ms a = '[]
  type SFldTypes EmptyFld ms a = '[]

newtype Hidden a = Hidden { unHidden :: a } deriving (Eq, Show, Ord)
instance SConvNames AllFld ms (Hidden a) where
  type SFldNames AllFld ms (Hidden a) = '[]
  type SFldTypes AllFld ms (Hidden a) = '[]
type instance GPlus (Hidden a) = False
instance Convert (Hidden a) [b] where convert _ = []


genDefunSymbols [''AppendSymbol]

instance (Grec v, ConvNames AllFld (GrecTagged v))
      => SConvNames AllFld ms1 (Tagged ('Just s :: Maybe Symbol) v) where
  type SFldNames AllFld ms1 (Tagged ('Just s) v) =
    Map (AppendSymbolSym1 (AppendSymbol (FromMaybe "" ms1) s))
        (FldNames AllFld (GrecTagged v))
  type SFldTypes AllFld ms1 (Tagged ('Just s) v) = FldTypes AllFld (GrecTagged v)

instance SConvNames AllFld ms (Tagged ('Just s) v)
      => SConvNames AllFld ms (Tagged (s :: Symbol) v) where
  type SFldNames AllFld ms (Tagged s v) = SFldNames AllFld ms (Tagged (Just s) v)
  type SFldTypes AllFld ms (Tagged s v) = SFldTypes AllFld ms (Tagged (Just s) v)

instance SConvNames AllFld ms (Tagged ('Nothing :: Maybe Symbol) v)
      => SConvNames AllFld ms (Tagged () v) where
  type SFldNames AllFld ms (Tagged () v) =
    SFldNames AllFld ms (Tagged ('Nothing :: Maybe Symbol) v)
  type SFldTypes AllFld ms (Tagged () v) =
    SFldTypes AllFld ms (Tagged ('Nothing :: Maybe Symbol) v)

instance SConvNames AllFld ms (Tagged (BTreeFromList ns) a)
      => SConvNames AllFld ms (Tagged (ns :: [Symbol]) a) where
  type SFldNames AllFld ms (Tagged ns a) =
    SFldNames AllFld ms (Tagged (BTreeFromList ns) a)
  type SFldTypes AllFld ms (Tagged ns a) =
    SFldTypes AllFld ms (Tagged (BTreeFromList ns) a)

instance ConvNames tag (a1,a2)
instance ConvNames tag (a1,a2,a3)
instance ConvNames tag (a1,a2,a3,a4)
instance ConvNames tag (a1,a2,a3,a4,a5)
instance ConvNames tag (a1,a2,a3,a4,a5,a6)
instance ConvNames tag (a1,a2,a3,a4,a5,a6,a7)
