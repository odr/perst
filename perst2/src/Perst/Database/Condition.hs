{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE UndecidableInstances      #-}
module Perst.Database.Condition where

import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Reader       (ReaderT, ask, local,
                                                   runReaderT)
import           Control.Monad.Trans.State.Strict (State, evalState, get,
                                                   modify)
import           Data.Aeson                       (FromJSON, ToJSON)
import           Data.Bifunctor                   (bimap, first, second)
import           Data.Foldable                    (toList)
import           Data.Singletons.Prelude          (Sing, SingI (..), fromSing)
import           Data.Singletons.Prelude.Maybe    (FromJust)
import           Data.String                      (IsString (..))
import           Data.Tagged                      (Tagged (..), untag)
import qualified Data.Text                        as T
import           Data.Text.Format                 (Only (..))
import           GHC.Generics                     (Generic)
import           GHC.Prim                         (Proxy#, proxy#)
import           GHC.TypeLits                     (KnownSymbol, Symbol,
                                                   symbolVal')

import           Data.Type.GrecTree
import           Perst.Database.DataDef           (GetToRefByName, RefCols,
                                                   RefFrom, SchRefs, Schema,
                                                   formatS)
import           Perst.Database.DbOption          (DbOption (..))
import           Perst.Types                      (PChilds)

data Condition (sch :: Schema) (t :: Symbol) v
  = And (Condition sch t v) (Condition sch t v)
  | Or  (Condition sch t v) (Condition sch t v)
  | Not (Condition sch t v)
  | Rec (CondRec   sch t v)
  deriving Generic

deriving instance Show (CondRec sch t v) => Show (Condition sch t v)

instance Monoid (CondRec sch t v) => Monoid (Condition sch t v) where
  mempty = Rec mempty
  mappend c1 c2 = And c1 c2

type family CondRec sch t v where
  CondRec sch t (Tagged (Leaf s) (PChilds v)) =
    Tagged (Leaf s) (CondChild sch (FromJust (GetToRefByName t s (SchRefs sch))) v)
  CondRec sch t (Tagged (Leaf s) (Tagged s1 v)) =
    Tagged (Leaf s) (Tagged s1 (CondRec sch t v))
  CondRec sch t (Tagged (Leaf s) v) = Tagged (Leaf s) [CondVal v]
  CondRec sch t (Tagged (Node l x r) (vl,vr)) =
    Tagged (Node l x r) ( Untag (CondRec sch t (Tagged l vl))
                        , Untag (CondRec sch t (Tagged r vr))
                        )

data CondVal v  = CvEq v
                | CvGe v
                | CvLe v
                | CvNull
                | CvLike v
                | CvNot (CondVal v)
                deriving (Eq,Show,Functor,Generic)
instance FromJSON v => FromJSON (CondVal v)
instance ToJSON   v => ToJSON   (CondVal v)

-- [(Symbol,Symbol)] is needed to get references info for childs
data CondChild sch ref v = CsExists (Condition sch (RefFrom ref) v)
                         | CsNotExists (Condition sch (RefFrom ref) v)
  deriving Generic

instance FromJSON (Condition sch (RefFrom ref) v)
      => FromJSON (CondChild sch ref v)
instance ToJSON   (Condition sch (RefFrom ref) v)
      => ToJSON   (CondChild sch ref v)

-- номер таблицы родителя (номер дочерней таблицы, номер параметра)
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
      CvNot n -> first (formatS "NOT ({})" . Only) <$> convCondVal pb name0 n
   where
    bi (op :: T.Text) v = withPar
      $ \p -> (formatS "{} {} {}" (name, op, p), Just $ convert v)

-- Есть проблема с циклическими ссылками.
-- На уровне типов не понять, что дочерний список пустой.
-- Будет бесконечный type-checking.
-- Надо либо не поддерживать циклических структур вообще,
-- либо ограничить вложенность каким-то количеством уровней (как?)
convCondChild :: ( DbOption b, SingI (RefCols ref)
                 , ConvCond b (Condition sch ch v), RefFrom ref ~ ch
                 )
              => Proxy# b -> T.Text -> CondChild sch ref v
              -> ConvCondMonad (T.Text, [FieldDB b])
convCondChild (_::Proxy# b) name (cs :: CondChild sch ref v)
  = ask >>= \pnum -> lift (modify (first (+1)) >> fst <$> get) >>= go pnum
 where
  go pnum cnum = case cs of
    CsExists cond    -> sub "" cond
    CsNotExists cond -> sub "NOT " cond
   where
    ref = T.intercalate " AND "
        $ map (\(ch,pr) -> formatS "t{}.{} = t{}.{}" (cnum,ch,pnum,pr))
        $ fromSing (sing :: Sing (RefCols ref))
    sub (s :: T.Text)
      = fmap (first (\c ->
          formatS "{}EXISTS (SELECT 1 FROM {} t{} WHERE {}{}{})"
            (s, name, cnum, ref, if T.null c then (""::T.Text) else " AND ", c)
        ))
      . local (const cnum)
      . convCond @b

instance (KnownSymbol n, DbOption b, Convert v (FieldDB b))
      => ConvCond b (Tagged (Leaf n) (CondVal v)) where
  convCond  = fmap (second toList)
            . convCondVal (proxy# :: Proxy# b)
                          (fromString $ symbolVal' (proxy# :: Proxy# n))
            . untag

instance ( KnownSymbol n, DbOption b, ConvCond b (Condition sch ch v)
         , SingI (RefCols ref), RefFrom ref ~ ch
         )
      => ConvCond b (Tagged (Leaf n) (CondChild sch ref v)) where
  convCond  = convCondChild (proxy# :: Proxy# b)
                            (fromString $ symbolVal' (proxy# :: Proxy# n))
            . untag

instance ConvCond b (Tagged (Leaf n) a) => ConvCond b (Tagged (Leaf n) [a]) where
  convCond
    = fmap  ( bimap ( (\t -> if T.null t then t else formatS "({})" $ Only t)
                    . T.intercalate " OR "
                    )
                    concat
            . unzip
            )
    . mapM (convCond @b @(Tagged (Leaf n) a) . Tagged) . untag

instance (ConvCond b (Tagged l vl), ConvCond b (Tagged r vr))
      => ConvCond b (Tagged (Node l x r) (vl,vr)) where
  convCond (Tagged (vl,vr))
    = (\(s1,v1) (s2,v2)
        -> if T.null s1 then (s2,v2)
            else if T.null s2 then (s1,v1)
              else (formatS "{} AND {}" (s1,s2), v1 ++ v2))
    <$> convCond @b @(Tagged l vl) (Tagged vl)
    <*> convCond @b @(Tagged r vr) (Tagged vr)

instance ConvCond b (CondRec sch t v) => ConvCond b (Condition sch t v) where
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
