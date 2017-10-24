{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE UndecidableInstances      #-}
module Data.Type.Grec.Convert
  ( Convert(..)
    , IsConv, IsConvSym0, ConvType
    , InternalType
    , Grec(..)
    -- , InternalTypeSym0
  ) where

-- import           Data.Bifunctor      (first)
import           Data.Singletons.TH  (genDefunSymbols)
import           Data.Tagged         (Tagged (..), retag, untag)
import           GHC.Generics
import           GHC.TypeLits        (Nat)

-- import           Data.Type.Grec.Internal.GGrecList (GListFromGrec (..),
--                                                     GListToGrec (..))
import           Data.Type.Grec.Type (Grec (..))

class Convert a b where
  convert :: a -> b

instance Convert a a where
  convert = id

class GListToGrec a g where
  gListToGrec :: [a] -> ([a],g b)
class GListFromGrec a g where
  gListFromGrec :: g b -> [a]

instance Convert [a] ([a],Tagged (ConvType b) b)
      => GListToGrec a (S1 c (Rec0 b)) where
  gListToGrec as = (M1 . K1 . untag)
                <$> (convert as :: ([a],Tagged (ConvType b) b))
instance Convert (Tagged (ConvType b) b) [a]
      => GListFromGrec a (S1 c (Rec0 b)) where
  gListFromGrec (M1 (K1 b)) = convert (Tagged b :: Tagged (ConvType b) b)

instance (GListToGrec a x, GListToGrec a y) => GListToGrec a (x :*: y) where
  gListToGrec xs = let (r,x) = gListToGrec xs in (x :*:) <$> gListToGrec r
instance (GListFromGrec a x, GListFromGrec a y)
    => GListFromGrec a (x :*: y) where
  gListFromGrec (x :*: y) = gListFromGrec x ++ gListFromGrec y

instance GListToGrec a b
    => GListToGrec a (D1 (MetaData md1 md2 md3 False) (C1 mc b)) where
  gListToGrec = fmap (M1 . M1) . gListToGrec
instance GListFromGrec a b
    => GListFromGrec a (D1 (MetaData md1 md2 md3 False) (C1 mc b)) where
  gListFromGrec (M1 (M1 b)) = gListFromGrec b

-- newtype => convert internal type
instance (Generic b, GListToGrec a (Rep b))
    => GListToGrec a (D1 (MetaData md1 md2 md3 True) (C1 mc (S1 c (Rec0 b))))
    where
  gListToGrec = fmap (M1 . M1 . M1 . K1 . to) . gListToGrec

instance (Generic b, GListFromGrec a (Rep b))
    => GListFromGrec a (D1 (MetaData md1 md2 md3 True) (C1 mc (S1 c (Rec0 b))))
    where
  gListFromGrec (M1 (M1 (M1 (K1 b)))) = gListFromGrec $ from b

instance (GListToGrec a (Rep r), Generic r) => Convert [a] ([a], Grec r) where
  convert = fmap (Grec . to) . gListToGrec

instance (GListFromGrec a (Rep r), Generic r)
    => Convert (Grec r) [a] where
  convert = gListFromGrec . from . unGrec

type family IsConv a :: Bool where
  IsConv String = True
  IsConv [a]    = False
  IsConv _      = True

type family ConvType a :: Nat where
  ConvType ()     = 0
  ConvType String = 1
  ConvType [a]    = 0
  ConvType (Grec a) = 2
  ConvType (a,b)  = ConvType a
  ConvType _      = 1

type family InternalType a :: * where
  InternalType [a] = a
  InternalType a   = a

genDefunSymbols [''IsConv]

-- Tagged elem --

instance Convert [a] ([a], Tagged '[] ()) where
  convert xs = (xs, Tagged ())
instance Convert (Tagged '[] ()) [a] where
  convert _ = []

instance Convert (Tagged 0 a) [b] where
  convert _ = []
instance Convert a b => Convert (Tagged 1 a) [b] where
  convert (Tagged a) = [convert a]
instance Convert (Grec a) [b] => Convert (Tagged 2 (Grec a)) [b] where
  convert = convert . unTagged

instance Monoid a => Convert [b] ([b],Tagged 0 a) where
  convert = (, Tagged mempty)
instance Convert b a => Convert [b] ([b],Tagged 1 a) where
  convert []     = error "Can't convert an empty list to value"
  convert (b:bs) = (bs,Tagged $ convert b)
instance Convert [b] ([b],Grec a) => Convert [b] ([b],Tagged 2 (Grec a)) where
  convert = fmap Tagged . convert

-- Tagged TList
instance Convert (Tagged (ConvType a) a) [b] => Convert (Tagged '[n] a) [b] where
  convert (Tagged a) = convert (Tagged a :: Tagged (ConvType a) a)
instance ( Convert (Tagged (ConvType a1) a1) [b]
         , Convert (Tagged (n2':ns) as) [b]
         )
    => Convert (Tagged (n1':n2':ns) (a1,as)) [b] where
  convert (Tagged (a1,as)) = convert (Tagged a1 :: Tagged (ConvType a1) a1)
                          ++ convert (Tagged as :: Tagged (n2':ns) as)

instance Convert [b] ([b],Tagged (ConvType a) a)
      => Convert [b] ([b],Tagged '[n] a) where
  convert = fmap retag . (convert :: [b] -> ([b], Tagged (ConvType a) a))

instance ( Convert [b] ([b],Tagged '[n1] a1)
         , Convert [b] ([b],Tagged (n2 ':ns) as)
         )
      => Convert [b] ([b],Tagged (n1 ':n2 ':ns) (a1,as)) where
  convert bs = let (bs', Tagged a1) = convert bs :: ([b],Tagged '[n1] a1) in
      fmap (Tagged . (a1,) . untag) (convert bs' :: ([b],Tagged (n2 ':ns) as))

----------- Sugar for tuples ------------

instance Convert (Tagged ns (v1,(v2,v3))) ([a])
    => Convert (Tagged ns (v1,v2,v3)) ([a]) where
  convert = convert . fmap (\(v1,v2,v3) -> (v1,(v2,v3)))

instance Convert ([a]) ([a], Tagged ns (v1,(v2,v3)))
    => Convert ([a]) ([a], Tagged ns (v1,v2,v3)) where
  convert = fmap (fmap (\(v1,(v2,v3)) -> (v1,v2,v3))) . convert

--

instance Convert (Tagged ns (v1,(v2,(v3,v4)))) ([a])
    => Convert (Tagged ns (v1,v2,v3,v4)) ([a]) where
  convert = convert . fmap (\(v1,v2,v3,v4) -> (v1,(v2,(v3,v4))))

instance Convert ([a]) ([a], Tagged ns (v1,(v2,(v3,v4))))
    => Convert ([a]) ([a], Tagged ns (v1,v2,v3,v4)) where
  convert = fmap (fmap (\(v1,(v2,(v3,v4))) -> (v1,v2,v3,v4))) . convert

--

instance Convert (Tagged ns (v1,(v2,(v3,(v4,v5))))) ([a])
    => Convert (Tagged ns (v1,v2,v3,v4,v5)) ([a]) where
  convert = convert . fmap (\(v1,v2,v3,v4,v5) -> (v1,(v2,(v3,(v4,v5)))))

instance Convert ([a]) ([a], Tagged ns (v1,(v2,(v3,(v4,v5)))))
    => Convert ([a]) ([a], Tagged ns (v1,v2,v3,v4,v5)) where
  convert = fmap (fmap (\(v1,(v2,(v3,(v4,v5)))) -> (v1,v2,v3,v4,v5))) . convert

--

instance Convert (Tagged ns (v1,(v2,(v3,(v4,(v5,v6)))))) ([a])
    => Convert (Tagged ns (v1,v2,v3,v4,v5,v6)) ([a]) where
  convert = convert . fmap (\(v1,v2,v3,v4,v5,v6) -> (v1,(v2,(v3,(v4,(v5,v6))))))

instance Convert ([a]) ([a], Tagged ns (v1,(v2,(v3,(v4,(v5,v6))))))
    => Convert ([a]) ([a], Tagged ns (v1,v2,v3,v4,v5,v6)) where
  convert = fmap (fmap (\(v1,(v2,(v3,(v4,(v5,v6))))) -> (v1,v2,v3,v4,v5,v6)))
          . convert

--

instance Convert (Tagged ns (v1,(v2,(v3,(v4,(v5,(v6,v7))))))) ([a])
    => Convert (Tagged ns (v1,v2,v3,v4,v5,v6,v7)) ([a]) where
  convert = convert
          . fmap (\(v1,v2,v3,v4,v5,v6,v7) -> (v1,(v2,(v3,(v4,(v5,(v6,v7)))))))

instance Convert ([a]) ([a], Tagged ns (v1,(v2,(v3,(v4,(v5,(v6,v7)))))))
    => Convert ([a]) ([a], Tagged ns (v1,v2,v3,v4,v5,v6,v7)) where
  convert = fmap (fmap (\(v1,(v2,(v3,(v4,(v5,(v6,v7)))))) -> (v1,v2,v3,v4,v5,v6,v7)))
          . convert
--
