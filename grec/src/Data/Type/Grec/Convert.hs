{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE UndecidableInstances      #-}
module Data.Type.Grec.Convert
  ( Convert(..)
    , IsConv, IsConvSym0, ConvType
    , InternalType
    -- , InternalTypeSym0
  ) where

-- import           Data.Bifunctor      (first)
import           Data.Singletons.TH  (genDefunSymbols)
import           Data.Tagged         (Tagged (..), retag, untag)
import           GHC.TypeLits        (Nat)

import           Data.Type.Grec.Type (GrecGroup (..))

class Convert a b where
  convert :: a -> b

instance Convert a a where
  convert = id

type family IsConv a :: Bool where
  IsConv String = True
  IsConv [a]    = False
  IsConv _      = True

type family ConvType a :: Nat where
  ConvType ()     = 0
  ConvType String = 1
  ConvType [a]    = 0
  ConvType (GrecGroup a) = 2
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
instance Convert a [b] => Convert (Tagged 2 (GrecGroup a)) [b] where
  convert (Tagged (GrecGroup a)) = convert a

instance Monoid a => Convert [b] ([b],Tagged 0 a) where
  convert = (, Tagged mempty)
instance Convert b a => Convert [b] ([b],Tagged 1 a) where
  convert []     = error "Can't convert an empty list to value"
  convert (b:bs) = (bs,Tagged $ convert b)
instance Convert [b] ([b],a) => Convert [b] ([b],Tagged 2 (GrecGroup a)) where
  convert = fmap (Tagged . GrecGroup) . convert

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
