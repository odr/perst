{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Type.Grec.Lens(nlens, LensedConstraint) where

import           Control.Monad                 ((>=>))
import           Data.Maybe                    (isJust)
import           Data.Singletons.Prelude
import           Data.Singletons.Prelude.Maybe (IsJust)
import           Data.Tagged                   (Tagged (..))
import           Data.Type.Grec.Type
import           GHC.Generics
import           GHC.OverloadedLabels          (IsLabel (..))
import           GHC.TypeLits                  (ErrorMessage (..), TypeError)

type LensedConstraint r n t = (Generic r, GLensed n t (Rep r))

nlens :: (LensedConstraint b n a, Functor f)
    => Proxy n -> (a -> f a) -> b -> f b
nlens p f b = to <$> gnlens p f (from b)

class GLensed (n :: Symbol) a g where
  gnlens :: Functor f => Proxy n -> (a -> f a) -> g b -> f (g b)

class GLensedB (x :: Bool) (n :: Symbol) a g where
  gnlensB :: Functor f
          => Proxy x -> Proxy n -> (a -> f a) -> g b -> f (g b)

instance GLensed n a (S1 c (K1 i a)) where
  gnlens _ f (M1 (K1 x)) = M1 . K1 <$> f x

instance GLensed n a b => GLensed n a (C1 c b) where
  gnlens p f (M1 x) = M1 <$> gnlens p f x

instance GLensed n a b => GLensed n a (D1 (MetaData s1 s2 s3 False) b) where
  gnlens p f (M1 x) = M1 <$> gnlens p f x

instance (GLensed n a (Rep b), Generic b)
      => GLensed n a (D1 (MetaData s1 s2 s3 True)
                         (C1 mc (S1 sc (K1 i b)))) where
  gnlens p f (M1 (M1 (M1 (K1 x)))) = M1 . M1 . M1 . K1 <$> nlens p f x

instance GLensedB (IsJust (Lookup n (GUnGroupFields x))) n a (x :*: y)
        => GLensed n a (x :*: y) where
    gnlens p (f :: a -> f a)
        = gnlensB (Proxy :: Proxy (IsJust (Lookup n (GUnGroupFields x)))) p f

instance GLensed n a x => GLensedB True n a (x :*: y) where
    gnlensB _ p f (x :*: y) = (:*: y) <$> gnlens p f x

instance GLensed n a y => GLensedB False n a (x :*: y) where
    gnlensB _ p f (x :*: y) = (x :*:) <$> gnlens p f y
