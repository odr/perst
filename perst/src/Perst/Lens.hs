module Perst.Lens where

import           Data.Proxy        (Proxy (..))
import           GHC.TypeLits      (Symbol)
import           Lens.Micro        (set)
import           Lens.Micro.Extras (view)

import           Data.Type.Grec    (LensedConstraint, nlens)


class NamesLens (ns :: [Symbol]) a b where
  namesGet :: Proxy ns -> b -> a
  namesSet :: Proxy ns -> a -> b -> b

instance NamesLens '[] () b where
  namesGet _ _ = ()
  namesSet _ _ = id

instance LensedConstraint b n a => NamesLens '[n] a b where
  namesGet _ = view $ nlens (Proxy :: Proxy n)
  namesSet _ = set  $ nlens (Proxy :: Proxy n)

instance (NamesLens '[n] a1 b, NamesLens ns a2 b)
      => NamesLens (n ': ns) (a1,a2) b where
  namesGet _ = (,) <$> namesGet (Proxy :: Proxy '[n]) <*> namesGet (Proxy :: Proxy ns)
  namesSet _ (a1,a2)  = namesSet (Proxy :: Proxy   ns) a2
                      . namesSet (Proxy :: Proxy '[n]) a1
