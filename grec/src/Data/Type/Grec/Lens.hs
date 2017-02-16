{-# LANGUAGE UndecidableInstances #-}
module Data.Type.Grec.Lens((:::), Cons, Fields, nlens, FieldName(..)) where

import           Data.Kind                     (Type)
import           Data.Maybe                    (isJust)
import           Data.Singletons.Prelude
import           Data.Singletons.Prelude.Maybe (IsJust)
import           GHC.Generics
import           GHC.OverloadedLabels          (IsLabel (..))
import           GHC.TypeLits                  (ErrorMessage (..), TypeError)

type (:::) a b = '(a,b)

data FieldName (n::Symbol) = FieldName
instance (Generic b, GLensed n a (Rep b)
         , Functor f, Lookup n (GFields (Rep b)) ~ Just a
         ) => IsLabel n ((a -> f a) -> b -> f b) where
  fromLabel _ = nlens (FieldName :: FieldName n)

type Cons a = GCons (Rep a)
type family GCons (a :: k1) :: Symbol where
  GCons (D1 _ (C1 (MetaCons s _ _) _)) = s
  GCons a = TypeError (
      Text "GCons is supported for Representation of data or newtype with one constructor"
      :$$: Text "Checked type is " :<>: ShowType a
      )

type Fields a = GFields (Rep a)
type family GFields (a :: k1) :: [(Symbol,Type)] where
  -- data
  GFields (M1 D (MetaData _ _ _ 'False) s) = GFields s
  -- newtype
  GFields (M1 D (MetaData _ _ _ 'True) (C1 _ (S1 _ (Rec0 dt)))) = GFields (Rep dt)
  GFields (M1 C _ s) = GFields s
  GFields (M1 S (MetaSel ('Just s) _ _ _) (Rec0 v)) = '[ '(s, v)]
  GFields U1 = '[]
  GFields (a :*: b) = GFields a :++ GFields b
  GFields a = TypeError (
      Text "GFields is supported for Representation of record with one constructor or newtype for such record"
      :$$: Text "Checked type is " :<>: ShowType a
      )

nlens :: (Generic b, GLensed n a (Rep b)
         , Functor f, Lookup n (GFields (Rep b)) ~ Just a
         )
        => FieldName n -> (a -> f a) -> b -> f b
nlens p f b = to <$> gnlens p f (from b)

class Lookup n (GFields g) ~ Just a => GLensed (n :: Symbol) a g where
    gnlens :: Functor f => FieldName n -> (a -> f a) -> g b -> f (g b)

class Lookup n (GFields g) ~ Just a
    => GLensedB (x :: Bool) (n :: Symbol) a g where
  gnlensB :: Functor f
          => Proxy x -> FieldName n -> (a -> f a) -> g b -> f (g b)

instance (Lookup n (GFields (S1 c (K1 i a))) ~ 'Just a)
    => GLensed n a (S1 c (K1 i a)) where
  gnlens _ f (M1 (K1 x)) = M1 . K1 <$> f x

instance (GLensed n a b, Lookup n (GFields (C1 c b)) ~ Just a)
        => GLensed n a (C1 c b) where
  gnlens p f (M1 x) = M1 <$> gnlens p f x

instance  ( GLensed n a b
          , Lookup n (GFields (D1 (MetaData s1 s2 s3 False) b)) ~ Just a
          )
        => GLensed n a (D1 (MetaData s1 s2 s3 False) b) where
  gnlens p f (M1 x) = M1 <$> gnlens p f x

instance  ( GLensed n a (Rep b)
          , Generic b
          , Lookup n (GFields (D1 (MetaData s1 s2 s3 True)
                                 (C1 mc (S1 sc (K1 i b))))) ~ Just a
          )
        => GLensed n a (D1 (MetaData s1 s2 s3 True)
                           (C1 mc (S1 sc (K1 i b)))) where
  gnlens p f (M1 (M1 (M1 (K1 x)))) = M1 . M1 . M1 . K1 <$> nlens p f x

instance GLensedB (IsJust (Lookup n (GFields x))) n a (x :*: y)
        => GLensed n a (x :*: y) where
    gnlens p (f :: a -> f a)
        = gnlensB (Proxy :: Proxy (IsJust (Lookup n (GFields x)))) p f


instance (GLensed n a x, Lookup n (GFields (x :*: y)) ~ Just a)
        => GLensedB True n a (x :*: y) where
    gnlensB _ p f (x :*: y) = (:*: y) <$> gnlens p f x

instance (GLensed n a y, Lookup n (GFields (x :*: y)) ~ Just a)
        => GLensedB False n a (x :*: y) where
    gnlensB _ p f (x :*: y) = (x :*:) <$> gnlens p f y
