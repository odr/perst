module Data.Type.GrecTree.Convert where

import           Data.Bifunctor           (bimap)
import           Data.Tagged              (Tagged (..), untag)

import           Data.Type.GrecTree.BTree

class Convert a b where
  convert :: a -> b

-- Functor?
instance Convert va vb
      => Convert (Tagged (Leaf 1 a) va) (Tagged (Leaf 1 b) vb) where
  convert = Tagged . convert . untag

instance ( Convert (Tagged la vla) (Tagged lb vlb)
         , Convert (Tagged ra vra) (Tagged rb vrb)
         )
      => Convert (Tagged (Node la n ra) (vla,vra))
                 (Tagged (Node lb n rb) (vlb,vrb)) where
  convert = Tagged . bimap convertL convertR . untag
    where
      convertL = untag . (convert :: Tagged la vla -> Tagged lb vlb) . Tagged
      convertR = untag . (convert :: Tagged ra vra -> Tagged rb vrb) . Tagged

-- Fold?
instance Convert va mb => Convert (Tagged (Leaf 1 na) va) mb where
  convert = convert . untag

instance (Convert (Tagged la vla) mb, Convert (Tagged ra vra) mb, Monoid mb)
      => Convert (Tagged (Node la n ra) (vla,vra)) mb where
  convert = uncurry mappend . bimap convertL convertR . untag
    where
      convertL  = (convert :: Tagged la vla -> mb) . Tagged
      convertR  = (convert :: Tagged ra vra -> mb) . Tagged
