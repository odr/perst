{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
