{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE UndecidableInstances      #-}
module Data.Type.GrecTree.Convert where

import           Data.Bifunctor                (bimap, first)
import           Data.Kind                     (Type)
import           Data.Promotion.TH             (genDefunSymbols)
import           Data.Singletons.Prelude
import           Data.Singletons.Prelude.Maybe (FromMaybe)
import           Data.Tagged                   (Tagged (..), retag, untag)
import           Data.Text                     (Text)
-- import           Data.Type.Bool                (If)
-- import           Data.Type.Equality            (type (==))
import           GHC.TypeLits                  (AppendSymbol, Symbol)

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
      (b,rb) = convert rb

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
data ConvType = CTSkip | CTSimple --  CTGroup
type family GetConvType x :: ConvType where
  GetConvType [z] = CTSkip
  -- GetConvType (Tagged (x::BTree k) v) = CTGroup
  GetConvType x = CTSimple

instance Convert (Tagged (GetConvType va) va) mb
      => Convert (Tagged (Leaf na :: BTree k) va) mb where
  convert = convert . Tagged @(GetConvType va) . untag

instance Monoid mb => Convert (Tagged CTSkip va) mb where
  convert _ = mempty
instance Convert va mb => Convert (Tagged CTSimple va) mb where
  convert = convert . untag
-- instance Convert va mb => Convert (Tagged CTGroup va) mb where
--   convert = convert . untag

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

instance Convert mb (Tagged (GetConvType v) v, mb)
      => Convert mb (Tagged (Leaf t :: BTree k) v, mb) where
  convert xs = first retag $ convert @mb @(Tagged (GetConvType v) v, mb) xs

instance Monoid x => Convert mb (Tagged CTSkip x, mb) where
  convert mb = (Tagged mempty, mb)

instance Convert mb (x,mb) => Convert mb (Tagged CTSimple x, mb) where
  -- convert []     = error "Empty list in convert."
  convert = first Tagged . convert

-- instance Convert mb (x,mb) => Convert mb (Tagged CTGroup x, mb) where
--   convert = first Tagged . convert
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


class SConvNames (s::Maybe Symbol) (v::Type) where
  type SFldNames s v :: [Symbol]
  type SFldNames s v = '[FromMaybe "" s]
  type SFldTypes s v :: [*]
  type SFldTypes s v = '[v]
  getSFldNames :: SingI (SFldNames s v) => [Text]
  getSFldNames = fromSing (sing :: Sing (SFldNames s v))

class ConvNames (v::Type) where
  type FldNames v :: [Symbol]
  type FldNames v = FldNames (GrecTagged v)
  type FldTypes v :: [*]
  type FldTypes v = FldTypes (GrecTagged v)
  getFldNames :: SingI (FldNames v) => [Text]
  getFldNames = fromSing (sing :: Sing (FldNames v))

instance ConvNames (Tagged '(t, GetConvType a) a)
      => ConvNames (Tagged (Leaf t) a) where
  type FldNames (Tagged (Leaf t) a) = FldNames (Tagged '(t, GetConvType a) a)
  type FldTypes (Tagged (Leaf t) a) = FldTypes (Tagged '(t, GetConvType a) a)
  -- getFldNames = getFldNames @(Tagged '(t, GetConvType a) a)

instance (SingI (SFldNames s a), SConvNames s a)
      => ConvNames (Tagged ('(s, CTSimple)::(Maybe Symbol,ConvType)) a) where
  type FldNames (Tagged '(s, CTSimple) a) = SFldNames s a
  type FldTypes (Tagged '(s, CTSimple) a) = SFldTypes s a
  -- getFldNames = getSFldNames @s @a

instance ConvNames (Tagged '(s, CTSkip) a) where
  type FldNames (Tagged '(s, CTSkip) a) = '[]
  type FldTypes (Tagged '(s, CTSkip) a) = '[]
  -- getFldNames = []

-- instance (ConvNames (Tagged bt a), SingI (GetMbSym s), SingI (BTreeType bt))
--       => ConvNames (Tagged '(s, CTGroup)
--                       (Tagged (bt::BTree (Maybe Symbol)) a)) where
--   -- type FldNames = If (IsNothing (BTreeType bt)) (FldNames (Tagged bt a))
--   --                     (...)
--   type FldTypes (Tagged '(s, CTGroup) (Tagged bt a)) = FldTypes (Tagged bt a)
--   getFldNames = case fromSing (sing :: Sing (BTreeType bt)) of
--       Nothing -> grpns
--       Just p  -> (maybe "" id (fromSing (sing :: Sing (GetMbSym s)))
--                   `mappend` p `mappend`) <$> grpns
--     where
--       grpns = getFldNames @(Tagged bt a)

instance (ConvNames (Tagged l vl), ConvNames (Tagged r vr))
      => ConvNames (Tagged (Node l t r) (vl,vr)) where
  type FldNames (Tagged (Node l t r) (vl,vr))
        = FldNames (Tagged l vl) :++ FldNames (Tagged r vr)
  type FldTypes (Tagged (Node l t r) (vl,vr))
        = FldTypes (Tagged l vl) :++ FldTypes (Tagged r vr)
  -- getFldNames = getFldNames @(Tagged l vl) ++ getFldNames @(Tagged r vr)
--
instance (Grec v, ConvNames (GrecTagged v))
      => SConvNames ms1 (Tagged ('Nothing :: Maybe Symbol) v) where
  type SFldNames ms1 (Tagged ('Nothing :: Maybe Symbol) v)
    = FldNames (GrecTagged v)
  type SFldTypes ms1 (Tagged ('Nothing :: Maybe Symbol) v)
    = FldTypes (GrecTagged v)
  -- getSFldNames = getFldNames @(GrecTagged v)

genDefunSymbols [''AppendSymbol]

instance (Grec v, ConvNames (GrecTagged v))
      => SConvNames ms1 (Tagged ('Just s :: Maybe Symbol) v) where
  type SFldNames ms1 (Tagged ('Just s) v)
    = Map (AppendSymbolSym1 (AppendSymbol (FromMaybe "" ms1) s))
          (FldNames (GrecTagged v))
  type SFldTypes ms1 (Tagged ('Just s) v) = FldTypes (GrecTagged v)

instance SConvNames ms (Tagged ('Just s) v)
      => SConvNames ms (Tagged (s :: Symbol) v) where
  type SFldNames ms (Tagged s v) = SFldNames ms (Tagged (Just s) v)
  type SFldTypes ms (Tagged s v) = SFldTypes ms (Tagged (Just s) v)
  -- getSFldNames = getSFldNames @ms @(Tagged (Just s) v)

instance SConvNames ms (Tagged ('Nothing :: Maybe Symbol) v)
      => SConvNames ms (Tagged () v) where
  type SFldNames ms (Tagged () v)
    = SFldNames ms (Tagged ('Nothing :: Maybe Symbol) v)
  type SFldTypes ms (Tagged () v)
    = SFldTypes ms (Tagged ('Nothing :: Maybe Symbol) v)
  -- getSFldNames = getSFldNames @ms @(Tagged ('Nothing :: Maybe Symbol) v)

instance ConvNames (a1,a2)
instance ConvNames (a1,a2,a3)
instance ConvNames (a1,a2,a3,a4)
instance ConvNames (a1,a2,a3,a4,a5)
instance ConvNames (a1,a2,a3,a4,a5,a6)
instance ConvNames (a1,a2,a3,a4,a5,a6,a7)
