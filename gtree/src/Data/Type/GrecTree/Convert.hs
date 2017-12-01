{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Type.GrecTree.Convert where

import           Data.Bifunctor           (bimap, first)
import           Data.Singletons.Prelude
import           Data.Tagged              (Tagged (..), retag, untag)
import           Data.Text                (Text)
import           Data.Type.Bool           (If)
import           Data.Type.Equality       (type (==))
import           GHC.TypeLits             (Nat)

import           Data.Type.GrecTree.BTree

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
data ConvType = CTSkip | CTSimple | CTGroup
type family GetConvType x :: ConvType where
  GetConvType [z] = CTSkip
  GetConvType (Tagged (x::BTree k) v) = CTGroup
  GetConvType x = CTSimple

instance Convert (Tagged (GetConvType va) va) mb
      => Convert (Tagged (Leaf na :: BTree k) va) mb where
  convert = convert . Tagged @(GetConvType va) . untag

instance Monoid mb => Convert (Tagged CTSkip va) mb where
  convert _ = mempty
instance (Convert va b, Applicative f)
      => Convert (Tagged CTSimple va) (f b) where
  convert = pure . convert . untag
instance Convert va mb => Convert (Tagged CTGroup va) mb where
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

instance Convert mb (Tagged (GetConvType v) v, mb)
      => Convert mb (Tagged (Leaf t :: BTree k) v, mb) where
  convert xs = first retag $ convert @mb @(Tagged (GetConvType v) v, mb) xs

instance Monoid x => Convert mb (Tagged CTSkip x, mb) where
  convert mb = (Tagged mempty, mb)

instance Convert b x => Convert [b] (Tagged CTSimple x, [b]) where
  convert []     = error "Empty list in convert."
  convert (b:bs) = (Tagged $ convert b, bs)

instance Convert mb (x,mb) => Convert mb (Tagged CTGroup x, mb) where
  convert = first Tagged . convert

class ConvNames a where
  -- type FldNames :: [Symbol]
  type FldTypes a :: [*]
  getFldNames :: [Text]

instance ConvNames (Tagged '(t, GetConvType a) a)
      => ConvNames (Tagged (Leaf t) a) where
  -- type FldNames = FldNames (Tagged '(t, GetConvType a) a)
  type FldTypes (Tagged (Leaf t) a) = FldTypes (Tagged '(t, GetConvType a) a)
  getFldNames = getFldNames @(Tagged '(t, GetConvType a) a)

instance SingI s
      => ConvNames (Tagged ('(s, CTSimple)::(Maybe Symbol,ConvType)) a) where
  -- type FldNames = '[FromMaybe "" s]
  type FldTypes (Tagged '(s, CTSimple) a) = '[a]
  getFldNames = [maybe "" id $ fromSing (sing :: Sing s)]

instance ConvNames (Tagged '(s, CTSkip) a) where
  -- type FldNames = '[]
  type FldTypes (Tagged '(s, CTSkip) a) = '[]
  getFldNames = []

instance (ConvNames (Tagged bt a), SingI (GetMbSym s), SingI (BTreeType bt))
      => ConvNames (Tagged '(s, CTGroup)
                      (Tagged (bt::BTree (Maybe Symbol)) a)) where
  -- type FldNames = If (IsNothing (BTreeType bt)) (FldNames (Tagged bt a))
  --                     (...)
  type FldTypes (Tagged '(s, CTGroup) (Tagged bt a)) = FldTypes (Tagged bt a)
  getFldNames = case fromSing (sing :: Sing (BTreeType bt)) of
      Nothing -> grpns
      Just p  -> (maybe "" id (fromSing (sing :: Sing (GetMbSym s)))
                  `mappend` p `mappend`) <$> grpns
    where
      grpns = getFldNames @(Tagged bt a)

instance (ConvNames (Tagged l vl), ConvNames (Tagged r vr))
      => ConvNames (Tagged (Node l t r) (vl,vr)) where
  type FldTypes (Tagged (Node l t r) (vl,vr))
        = FldTypes (Tagged l vl) :++ FldTypes (Tagged r vr)
  getFldNames = getFldNames @(Tagged l vl) ++ getFldNames @(Tagged r vr)
