{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DefaultSignatures    #-}
-- {-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE PolyKinds            #-}
-- {-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Type.GrecTree.Grec where
import           Data.Singletons.Prelude       (Sing (..), SingI (..))
import           Data.Singletons.Prelude.Maybe (FromJust, IsNothing)
-- import           Data.Singletons.TH            (singletons)
import           Data.Tagged                   (Tagged (..), retag, untag)
import           Data.Type.Bool                (If)
import           GHC.Generics
import           GHC.TypeLits                  (AppendSymbol, Symbol)

import           Data.Type.GrecTree.BTree

class GrecContraint a => Grec a where
  type GrecTagged a
  type GrecTagged a = GTagged (Rep a)

  toTagged :: a -> GrecTagged a
  default toTagged :: GrecTagged a ~ GTagged (Rep a) => a -> GrecTagged a
  toTagged = gGrecToTagged . from

  fromTagged :: GrecTagged a -> a
  default fromTagged :: GrecTagged a ~ GTagged (Rep a) => GrecTagged a -> a
  fromTagged = to . gTaggedToGrec

type GrecContraint t = (Generic t, GGrec (Rep t))

instance GrecContraint (a1,a2) => Grec (a1,a2)
instance GrecContraint (a1,a2,a3) => Grec (a1,a2,a3)
instance GrecContraint (a1,a2,a3,a4) => Grec (a1,a2,a3,a4)
instance GrecContraint (a1,a2,a3,a4,a5) => Grec (a1,a2,a3,a4,a5)
instance GrecContraint (a1,a2,a3,a4,a5,a6) => Grec (a1,a2,a3,a4,a5,a6)
instance GrecContraint (a1,a2,a3,a4,a5,a6,a7) => Grec (a1,a2,a3,a4,a5,a6,a7)

---------------
class GGrec g where
  type GTagged g
  gGrecToTagged   :: g b -> GTagged g
  gTaggedToGrec   :: GTagged g -> g b

-- set GPlus a = True to convert
-- (Leaf (Tagged (s :: Symbol | Maybe Symbol | ()) a))
-- to (TopTag (s' :: Maybe Symbol) (GrecTagged a)).
-- if GPlus a = False, (Leaf (Tagged s a)) is not converted
type family GPlus a :: Bool
type instance GPlus (a1,a2) = True
type instance GPlus (a1,a2,a3) = True
type instance GPlus (a1,a2,a3,a4) = True
type instance GPlus (a1,a2,a3,a4,a5) = True
type instance GPlus (a1,a2,a3,a4,a5,a6) = True
type instance GPlus (a1,a2,a3,a4,a5,a6,a7) = True

data FieldType s = Regular | TaggedField s | TaggedRec s | TaggedGrec

type family GetFT a :: FieldType (Maybe Symbol) where
  GetFT (Tagged (s::Symbol) a) =
    If (GPlus a) (TaggedRec (Just s)) (TaggedField (Just s))
  GetFT (Tagged (s::Maybe Symbol) a) =
    If (GPlus a) (TaggedRec s) (TaggedField s)
  GetFT (Tagged () a) =
    If (GPlus a) (TaggedRec Nothing) (TaggedField Nothing)
  GetFT (Tagged (bt :: BTree (Maybe Symbol)) a) = TaggedGrec
  GetFT (Tagged (ns :: [Symbol]) a) = TaggedGrec
  GetFT a = Regular

type AppendMbSymbol s1 s2 =
  If (IsNothing s2) 'Nothing
    (If (IsNothing s1) s2 (Just (AppendSymbol (FromJust s1) (FromJust s2))))

class GrecFT (ft::FieldType (Maybe Symbol)) (c::Maybe Symbol) b where
  type TaggedFT ft c b
  grecToTaggedFT   :: b -> TaggedFT ft c b
  taggedToGrecFT   :: TaggedFT ft c b -> b

instance GrecFT Regular c b where
  type TaggedFT Regular c b = Tagged (Leaf c) b
  grecToTaggedFT = Tagged
  taggedToGrecFT = untag
--
instance GrecFT (TaggedField s) c (Tagged s' b) where
  type TaggedFT (TaggedField s) c (Tagged s' b) =
    Tagged (Leaf (AppendMbSymbol c s)) b
  grecToTaggedFT = retag
  taggedToGrecFT = retag
--
type family AppendSymbolTree s bt where
  AppendSymbolTree s1 (Leaf s2) = Leaf (AppendMbSymbol s1 s2)
  AppendSymbolTree s (Node l t r) =
    Node (AppendSymbolTree s l) (AppendMbSymbol s t) (AppendSymbolTree s r)

type AppendSymbolTagged s t =
  Tagged (AppendSymbolTree s (TaggedTagBT t)) (Untag t)

instance (Grec b, GrecTagged b ~ Tagged s1 (Untag (GrecTagged b)))
      => GrecFT (TaggedRec s) c (Tagged s' b) where
  type TaggedFT (TaggedRec s) c (Tagged s' b) =
    AppendSymbolTagged (AppendMbSymbol c s) (GrecTagged b)
  grecToTaggedFT = retag . toTagged . untag
  taggedToGrecFT = Tagged . fromTagged . retag
--
instance GrecFT TaggedGrec c (Tagged (bt :: BTree (Maybe Symbol)) a) where
  type TaggedFT TaggedGrec c (Tagged bt a) = Tagged (AppendSymbolTree c bt) a
  grecToTaggedFT = retag
  taggedToGrecFT = retag
--
instance GrecFT TaggedGrec c (Tagged (ns :: [Symbol]) a) where
  type TaggedFT TaggedGrec c (Tagged ns a) =
    Tagged (AppendSymbolTree c (BTreeFromList ns)) a
  grecToTaggedFT = retag
  taggedToGrecFT = retag

instance GrecFT (GetFT b) c b => GGrec (S1 (MetaSel c m2 m3 m4) (Rec0 b)) where
  type GTagged (S1 (MetaSel c m2 m3 m4) (Rec0 b)) = TaggedFT (GetFT b) c b
  gGrecToTagged (M1 (K1 b)) = grecToTaggedFT @(GetFT b) @c b
  gTaggedToGrec = M1 . K1 . taggedToGrecFT @(GetFT b) @c

instance  ( GGrec x
          , GGrec y
          , GTagged x ~ Tagged l vl
          , GTagged y ~ Tagged r vr
          , TaggedAppend (GTagged x) (GTagged y)
              ~ Tagged (Node l t r) (vl,vr)
          )
      => GGrec (x :*: y) where
  type GTagged (x :*: y)  = TaggedAppend (GTagged x) (GTagged y)
  gTaggedToGrec (Tagged (vl,vr)) =
    let x = gTaggedToGrec (Tagged vl :: GTagged x)
        y = gTaggedToGrec (Tagged vr :: GTagged y)
    in x :*: y
  gGrecToTagged (x :*: y) =
    let (Tagged vl) = gGrecToTagged x
        (Tagged vr) = gGrecToTagged y
    in
      Tagged (vl,vr)

instance GGrec b => GGrec (D1 (MetaData md1 md2 md3 False) (C1 mc b)) where
  type GTagged (D1 (MetaData md1 md2 md3 False) (C1 mc b)) = GTagged b
  gTaggedToGrec = M1 . M1 . gTaggedToGrec
  gGrecToTagged (M1 (M1 b)) = gGrecToTagged b

-- newtype => convert internal type
instance ( If (GPlus b) (Generic b, GGrec (Rep b)) (GrecFT Regular c b)
         , SingI (GPlus b)
         )
  => GGrec (D1 (MetaData md1 md2 md3 True)
               (C1 mc (S1 (MetaSel c m2 m3 m4) (Rec0 b))))
  where
  type GTagged (D1 (MetaData md1 md2 md3 True)
                   (C1 mc (S1 (MetaSel c m2 m3 m4) (Rec0 b)))) =
    If (GPlus b) (GTagged (Rep b)) (TaggedFT Regular c b)
  gTaggedToGrec = M1 . M1 . M1 . K1
                . case (sing :: Sing (GPlus b)) of
                    SFalse -> taggedToGrecFT @Regular @c
                    STrue  -> to . gTaggedToGrec
  gGrecToTagged (M1 (M1 (M1 (K1 b)))) = case (sing :: Sing (GPlus b)) of
    SFalse -> grecToTaggedFT @Regular @c b
    STrue  -> gGrecToTagged $ from b
