{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Type.Grec.Lens(NLensed(..) {-nlens, LensedConstraint, SymLens(..) -}) where

import           Control.Monad                 ((>=>))
import           Data.Maybe                    (isJust)
import           Data.Singletons.Prelude       (Lookup, Symbol)
import           Data.Singletons.Prelude.Maybe (IsJust)
import           Data.Tagged                   (Tagged (..))
import           Data.Type.Grec.Type
import           GHC.Generics
import           GHC.OverloadedLabels          (IsLabel (..))
import           GHC.TypeLits                  (ErrorMessage (..), TypeError)
-- import           Lens.Simple                   (LensLike')

-- type LensedConstraint n r t = (Generic r, GLensed n t (Rep r))
--
-- nlens :: (LensedConstraint n b a, Functor f)
--     => (a -> f a) -> b -> f b
-- nlens f b = to <$> gnlens @n f (from b)

class NLensed n b a where
  nlens :: Functor f => (a -> f a) -> b -> f b

instance (Generic b, GLensed n a (Rep b)) => NLensed n b a where
  nlens f b = to <$> gnlens @n f (from b)

class GLensed (n :: Symbol) a g where
  gnlens :: Functor f => (a -> f a) -> g b -> f (g b)

class GLensedB (x :: Bool) (n :: Symbol) a g where
  gnlensB :: Functor f => (a -> f a) -> g b -> f (g b)

instance GLensed n a (S1 c (K1 i a)) where
  gnlens f (M1 (K1 x)) = M1 . K1 <$> f x

instance GLensed n a b => GLensed n a (C1 c b) where
  gnlens f (M1 x) = M1 <$> gnlens @n f x

instance GLensed n a b => GLensed n a (D1 (MetaData s1 s2 s3 False) b) where
  gnlens f (M1 x) = M1 <$> gnlens @n f x

instance (GLensed n a (Rep b), Generic b)
      => GLensed n a (D1 (MetaData s1 s2 s3 True)
                         (C1 mc (S1 sc (K1 i b)))) where
  gnlens f (M1 (M1 (M1 (K1 x)))) = M1 . M1 . M1 . K1 <$> nlens @n f x

instance GLensedB (IsJust (Lookup n (GUnGroupFields x))) n a (x :*: y)
        => GLensed n a (x :*: y) where
    gnlens (f :: a -> f a)
        = gnlensB @(IsJust (Lookup n (GUnGroupFields x))) @n f

instance GLensed n a x => GLensedB True n a (x :*: y) where
    gnlensB f (x :*: y) = (:*: y) <$> gnlens @n f x

instance GLensed n a y => GLensedB False n a (x :*: y) where
    gnlensB f (x :*: y) = (x :*:) <$> gnlens @n f y

-- type LensedConstraint r n t = SymLens n r t
--
-- nlens :: (LensedConstraint b n a, Functor f) => Sing n -> (a -> f a) -> b -> f b
-- nlens = symLens

-- class SymLens (n :: Symbol) a b where
--   symLens :: Functor f => Sing n  -> (b -> f b) -> a -> f a --LensLike' f a b
