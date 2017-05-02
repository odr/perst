module Data.Type.Grec.ConvList(ConvList(..)) where

import           Data.Bifunctor         (bimap)
import           Data.Proxy             (Proxy (..))
import           Data.Tagged            (Tagged (..), tagWith, untag)
import           Data.Type.Grec.Convert (Convert (..))

newtype ConvList a = ConvList { unConvList :: [a] }

instance (Convert b a, Convert (Tagged (cc ': ccc) c) (ConvList a))
        => Convert (Tagged (bb ': cc ': ccc) (b,c)) (ConvList a) where
  convert = ConvList . uncurry (:)
          . bimap convert (unConvList . convert . tagWith (Proxy :: Proxy (cc ': ccc)))
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
