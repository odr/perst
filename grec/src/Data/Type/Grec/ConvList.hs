{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}
module Data.Type.Grec.ConvList(ConvList(..)) where

import           Data.Bifunctor         (bimap)
import           Data.Proxy             (Proxy (..))
import           Data.Tagged            (Tagged (..), tagWith, untag)
import           Data.Type.Grec.Convert (Convert (..))

newtype ConvList a = ConvList { unConvList :: [a] } deriving (Eq, Show, Monoid)

instance Convert b a => Convert (Tagged '[bb] b) (ConvList a) where
  convert = ConvList . (:[]) . convert . unTagged

instance (Convert b a, Convert (Tagged (cc ': ccc) c) (ConvList a))
        => Convert (Tagged (bb ': cc ': ccc) (b,c)) (ConvList a) where
  convert = ConvList . uncurry (:)
          . bimap convert (unConvList . convert . tagWith (Proxy :: Proxy (cc ': ccc)))
          . unTagged


instance Convert b a => Convert (ConvList a, Tagged '[bb] b) (ConvList a) where
  convert (cl, Tagged b) = cl `mappend` ConvList [convert b]

instance (Convert b a, Convert (ConvList a, Tagged (cc ': ccc) c) (ConvList a))
        => Convert (ConvList a, Tagged (bb ': cc ': ccc) (b,c)) (ConvList a) where
  convert (cl, Tagged (b,c))
    = ConvList [convert b] `mappend` convert (cl, Tagged c :: Tagged (cc ': ccc) c)


instance Convert a b => Convert (ConvList a) (Tagged '[bb] b) where
  convert (ConvList [a]) = Tagged $ convert a
  convert (ConvList [])
    = error "Invalid convert from ConvList to pair representation. List too short"
  convert _
    = error "Invalid convert from ConvList to pair representation. List too long"

instance (Convert a b, Convert (ConvList a) (Tagged (cc ': ccc) c))
      => Convert (ConvList a) (Tagged (bb ': (cc ': ccc)) (b,c)) where
  convert (ConvList (a1:a2:as))
    = Tagged (convert a1, untag (convert $ ConvList (a2:as) :: Tagged (cc ': ccc) c))
  convert _ = error "Invalid convert from ConvList to pair representation. List too short"


instance Convert (ConvList a) (ConvList a, Tagged '[] ()) where
  convert cl = (cl, Tagged ())

instance Convert a b => Convert (ConvList a) (ConvList a, Tagged '[bb] b) where
  convert (ConvList (a:as)) = (ConvList as, Tagged $ convert a)
  convert _ = error "Invalid convert from ConvList to pair representation. List too short"

instance (Convert a b, Convert (ConvList a) (ConvList a, Tagged (cc ': ccc) c))
    => Convert (ConvList a) (ConvList a, Tagged (bb ': (cc ': ccc)) (b,c)) where
  convert (ConvList (a1:a2:as))
    = let (r, Tagged v :: Tagged (cc ': ccc) c) = convert (ConvList $ a2:as) in
      (r, Tagged (convert a1, v) :: Tagged (bb ': cc ': ccc) (b,c))
  convert _ = error "Invalid convert from ConvList to pair representation. List too short"


----------- Sugar for tuples ------------

instance Convert (ConvList a, Tagged ns (v1,(v2,v3))) (ConvList a)
    => Convert (ConvList a, Tagged ns (v1,v2,v3)) (ConvList a) where
  convert = convert . fmap (fmap (\(v1,v2,v3) -> (v1,(v2,v3))))

instance Convert (ConvList a) (ConvList a, Tagged ns (v1,(v2,v3)))
    => Convert (ConvList a) (ConvList a, Tagged ns (v1,v2,v3)) where
  convert = fmap (fmap (\(v1,(v2,v3)) -> (v1,v2,v3))) . convert

--

instance Convert (ConvList a, Tagged ns (v1,(v2,(v3,v4)))) (ConvList a)
    => Convert (ConvList a, Tagged ns (v1,v2,v3,v4)) (ConvList a) where
  convert = convert . fmap (fmap (\(v1,v2,v3,v4) -> (v1,(v2,(v3,v4)))))

instance Convert (ConvList a) (ConvList a, Tagged ns (v1,(v2,(v3,v4))))
    => Convert (ConvList a) (ConvList a, Tagged ns (v1,v2,v3,v4)) where
  convert = fmap (fmap (\(v1,(v2,(v3,v4))) -> (v1,v2,v3,v4))) . convert

--

instance Convert (ConvList a, Tagged ns (v1,(v2,(v3,(v4,v5))))) (ConvList a)
    => Convert (ConvList a, Tagged ns (v1,v2,v3,v4,v5)) (ConvList a) where
  convert = convert . fmap (fmap (\(v1,v2,v3,v4,v5) -> (v1,(v2,(v3,(v4,v5))))))

instance Convert (ConvList a) (ConvList a, Tagged ns (v1,(v2,(v3,(v4,v5)))))
    => Convert (ConvList a) (ConvList a, Tagged ns (v1,v2,v3,v4,v5)) where
  convert = fmap (fmap (\(v1,(v2,(v3,(v4,v5)))) -> (v1,v2,v3,v4,v5))) . convert

--

instance Convert (ConvList a, Tagged ns (v1,(v2,(v3,(v4,(v5,v6)))))) (ConvList a)
    => Convert (ConvList a, Tagged ns (v1,v2,v3,v4,v5,v6)) (ConvList a) where
  convert = convert
          . fmap (fmap (\(v1,v2,v3,v4,v5,v6) -> (v1,(v2,(v3,(v4,(v5,v6)))))))

instance Convert (ConvList a) (ConvList a, Tagged ns (v1,(v2,(v3,(v4,(v5,v6))))))
    => Convert (ConvList a) (ConvList a, Tagged ns (v1,v2,v3,v4,v5,v6)) where
  convert = fmap (fmap (\(v1,(v2,(v3,(v4,(v5,v6))))) -> (v1,v2,v3,v4,v5,v6)))
          . convert

--

instance Convert (ConvList a, Tagged ns (v1,(v2,(v3,(v4,(v5,(v6,v7))))))) (ConvList a)
    => Convert (ConvList a, Tagged ns (v1,v2,v3,v4,v5,v6,v7)) (ConvList a) where
  convert = convert
          . fmap (fmap (\(v1,v2,v3,v4,v5,v6,v7) -> (v1,(v2,(v3,(v4,(v5,(v6,v7))))))))

instance Convert (ConvList a) (ConvList a, Tagged ns (v1,(v2,(v3,(v4,(v5,(v6,v7)))))))
    => Convert (ConvList a) (ConvList a, Tagged ns (v1,v2,v3,v4,v5,v6,v7)) where
  convert = fmap (fmap (\(v1,(v2,(v3,(v4,(v5,(v6,v7)))))) -> (v1,v2,v3,v4,v5,v6,v7)))
          . convert

--
