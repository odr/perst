module Perst.Lens where

import           Data.Proxy        (Proxy (..))
import           GHC.TypeLits      (Symbol)
import           Lens.Micro        (set)
import           Lens.Micro.Extras (view)

import           Data.Type.Grec    (GrecLens (..))


-- class NamesLens (ns :: [Symbol]) a b where
--   namesGet :: Proxy ns -> b -> a
--   namesSet :: Proxy ns -> a -> b -> b
--
-- instance NamesLens '[] () b where
--   namesGet _ _ = ()
--   namesSet _ _ = id
--
-- instance LensedConstraint b n a => NamesLens '[n] a b where
--   namesGet _ = view $ nlens (Proxy :: Proxy n)
--   namesSet _ = set  $ nlens (Proxy :: Proxy n)
--
-- instance (NamesLens '[n] a1 b, NamesLens (n1 ': ns) a2 b)
--       => NamesLens (n ': n1 ': ns) (a1,a2) b where
--   namesGet _  = (,) <$> namesGet (Proxy :: Proxy '[n])
--                     <*> namesGet (Proxy :: Proxy (n1 ': ns))
--   namesSet _ (a1,a2)  = namesSet (Proxy :: Proxy (n1 ': ns)) a2
--                       . namesSet (Proxy :: Proxy '[n]) a1
--
class NamesGrecLens (ns :: [Symbol]) a b where
  namesGrecGet :: Proxy ns -> b -> a
  namesGrecSet :: Proxy ns -> a -> b -> b

instance NamesGrecLens '[] () b where
  namesGrecGet _ _ = ()
  namesGrecSet _ _ = id

instance GrecLens n a b => NamesGrecLens '[n] a b where
  namesGrecGet _ = view $ grecLens (Proxy :: Proxy n)
  namesGrecSet _ = set  $ grecLens (Proxy :: Proxy n)

instance (NamesGrecLens '[n] a1 b, NamesGrecLens (n1 ': ns) a2 b)
      => NamesGrecLens (n ': n1 ': ns) (a1,a2) b where
  namesGrecGet _  = (,) <$> namesGrecGet (Proxy :: Proxy '[n])
                        <*> namesGrecGet (Proxy :: Proxy (n1 ': ns))
  namesGrecSet _ (a1,a2)  = namesGrecSet (Proxy :: Proxy (n1 ': ns)) a2
                          . namesGrecSet (Proxy :: Proxy '[n]) a1
