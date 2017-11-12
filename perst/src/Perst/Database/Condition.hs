{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}
module Perst.Database.Condition where

import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Reader       (ReaderT, ask, local,
                                                   runReaderT)
import           Control.Monad.Trans.State.Strict (State, evalState, get,
                                                   modify, put)
import           Data.Aeson                       (FromJSON, ToJSON)
import           Data.Bifunctor                   (bimap, first, second)
import           Data.Singletons.Prelude          (Fst, Sing, SingI (..), Snd,
                                                   fromSing)
-- import           Data.Default                     (Default (..))
import           Data.Foldable                    (toList)
import           Data.Kind                        (Type)
import           Data.String                      (IsString (..))
import           Data.Tagged                      (Tagged (..), retag, untag)
import qualified Data.Text                        as T
import           Data.Text.Format                 (Only (..))
import           GHC.Generics
import           GHC.Prim                         (Proxy#, proxy#)
import           GHC.TypeLits                     (KnownSymbol, Symbol,
                                                   symbolVal')

import           Data.Type.Grec                   as Grec (ConvFromGrec,
                                                           Convert (..),
                                                           FieldsGrec, Grec,
                                                           ListToTaggedPairs,
                                                           fieldNames)
import           Perst.Database.DataDef           (formatS)
import           Perst.Database.DbOption          (DbOption (..))
import           Perst.Database.TreeDef           (Child, TreeDef)

data CondVal v  = CvEq v
                | CvGe v
                | CvLe v
                | CvNull
                | CvLike v
                | CvNot (CondVal v)
                deriving (Eq,Show,Functor,Generic)
instance FromJSON v => FromJSON (CondVal v)
instance ToJSON   v => ToJSON   (CondVal v)


data CondSub (t :: (TreeDef, [(Symbol,Symbol)])) v
  = CsExists (Condition (Fst t) v)
  | CsNotExists (Condition (Fst t) v)
  deriving Generic
instance FromJSON (Condition (Fst t) v) => FromJSON (CondSub t v)
instance ToJSON   (Condition (Fst t) v) => ToJSON   (CondSub t v)



type family CondValTF (t :: TreeDef) (x :: (Symbol,Type)) :: (Symbol,Type) where
  CondValTF t '(n,[v]) = '(n, [CondSub (Child n t) v])
  CondValTF t '(n,v) = '(n, [CondVal v])

type family CondMap t (xs :: [(Symbol,Type)]) where
  CondMap t '[] = '[]
  CondMap t (x ': xs) = CondValTF t x ': CondMap t xs

type CondRec t v = ListToTaggedPairs (CondMap t (FieldsGrec (Grec v)))

data Condition t v
  = And (Condition t v) (Condition t v)
  | Or  (Condition t v) (Condition t v)
  | Not (Condition t v)
  | Rec (CondRec t v)
  deriving Generic
instance Monoid (CondRec t v) => Monoid (Condition t v) where
  mempty = Rec mempty
  mappend c1 c2 = And c1 c2


instance FromJSON (CondRec t v) => FromJSON (Condition t v)
instance ToJSON   (CondRec t v) => ToJSON (Condition t v)

type ConvCondMonad = ReaderT Int (State (Int, Int))
runConvCond :: ConvCondMonad a -> a
runConvCond x = evalState (runReaderT x 0) (0,0)

class ConvCond b t where
  convCond :: t -> ConvCondMonad (T.Text, [FieldDB b])

convCondVal :: (DbOption b, Convert v (FieldDB b))
    => Proxy# b -> T.Text -> CondVal v -> ConvCondMonad (T.Text, Maybe (FieldDB b))
convCondVal (pb::Proxy# b) name0 cv
  = formatS "t{}.{}" . (,name0) <$> ask >>= go
 where
  withPar f = lift $ do
    n <- snd <$> get
    modify $ second (+1)
    return $ f (paramName @b n)
  go name = case cv of
      CvEq v -> bi "="  v
      CvGe v -> bi ">=" v
      CvLe v -> bi "<=" v
      CvNull -> return (formatS "{} IS NULL" (Only name), Nothing)
      CvLike v -> withPar (\p -> (condLike @b name p, Just $ convert v))
      CvNot cv -> first (formatS "NOT ({})" . Only) <$> convCondVal pb name0 cv
   where
    bi (op :: T.Text) v = withPar
      $ \p -> (formatS "{} {} {}" (name, op, p), Just $ convert v)

convCondSub :: (DbOption b, ConvCond b (Condition (Fst t) v), SingI (Snd t))
    => Proxy# b -> T.Text -> CondSub t v -> ConvCondMonad (T.Text, [FieldDB b])
convCondSub (pb::Proxy# b) name (cs :: CondSub t v)
  = ask >>= \pnum -> lift (modify (first (+1)) >> fst <$> get) >>= go pnum
 where
  go pnum cnum = case cs of
    CsExists cond    -> sub "" cond
    CsNotExists cond -> sub "NOT " cond
   where
    ref = T.intercalate " AND "
        $ map (\(ch,pr) -> formatS "t{}.{} = t{}.{}" (cnum,ch,pnum,pr))
        $ fromSing (sing :: Sing (Snd t))
    sub (s :: T.Text)
      = fmap (first (\c ->
          formatS "{}EXISTS (SELECT 1 FROM {} t{} WHERE {}{}{})"
            (s, name, cnum, ref, if T.null c then (""::T.Text) else " AND ", c)
        ))
      . local (const cnum)
      . convCond @b

instance (KnownSymbol n, DbOption b, Convert v (FieldDB b))
      => ConvCond b (Tagged '[n] (CondVal v)) where
  convCond  = fmap (second toList)
            . convCondVal (proxy# :: Proxy# b)
                          (fromString $ symbolVal' (proxy# :: Proxy# n))
            . untag

instance (KnownSymbol n, DbOption b, ConvCond b (Condition (Fst t) v), SingI (Snd t))
      => ConvCond b (Tagged '[n] (CondSub t v)) where
  convCond  = convCondSub (proxy# :: Proxy# b)
                          (fromString $ symbolVal' (proxy# :: Proxy# n))
            . untag

instance ConvCond b (Tagged '[n] a) => ConvCond b (Tagged '[n] [a]) where
  convCond = convCondF (proxy# :: Proxy# b)

-- instance ConvCond b (Tagged '[n] a) => ConvCond b (Tagged '[n] (Maybe a)) where
--   convCond = convCondF (proxy# :: Proxy# b)

convCondF :: (Foldable f, ConvCond b (Tagged '[n] a))
          => Proxy# b -> Tagged '[n] (f a) -> ConvCondMonad (T.Text, [FieldDB b])
convCondF (_::Proxy# b) (x :: Tagged '[n] (f a))
  = fmap  ( bimap ( (\t -> if T.null t then t else formatS "({})" $ Only t)
                  . T.intercalate " OR "
                  )
                  concat
          . unzip
          )
  . mapM (convCond @b @(Tagged '[n] a) . Tagged) . toList . untag
  $ x

instance (ConvCond b (Tagged '[n1] x), ConvCond b (Tagged (n2 ': ns) y))
      => ConvCond b (Tagged (n1 ': n2 ': ns) (x,y)) where
  convCond (Tagged (x,y))
    = (\(s1,v1) (s2,v2)
        -> if T.null s1 then (s2,v2)
            else if T.null s2 then (s1,v1)
              else (formatS "{} AND {}" (s1,s2), v1 ++ v2))
    <$> convCond @b @(Tagged '[n1] x) (Tagged x)
    <*> convCond @b @(Tagged (n2 ': ns) y) (Tagged y)

-- UndecidableInstances
instance ConvCond b (CondRec t (Grec v)) => ConvCond b (Condition t v) where
  convCond = \case
    And c1 c2 -> bi "AND" c1 c2
    Or  c1 c2 -> bi "OR" c1 c2
    Not c     -> (\(t,vs) -> if T.null t then ("0=1",[])
                                else (formatS "NOT ({})" (Only t), vs) )
              <$> convCond @b c
    Rec tr    -> convCond @b tr
   where
    bi (s::T.Text) c1 c2
      = (\(s1,v1) (s2,v2)
          -> if T.null s1 then ifnull (s2,v2)
              else if T.null s2 then ifnull (s1,v1)
                else (formatS "({}) {} ({})" (s1,s,s2), v1 ++ v2))
        <$> convCond @b c1 <*> convCond @b c2
      where
        ifnull (s',v')
          | s == "OR" = mempty
          | otherwise = (s',v')
