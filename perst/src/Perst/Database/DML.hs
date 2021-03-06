{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
module Perst.Database.DML
  ( module DML, DMLCons, RecCons, DML(..)
  )
  where

import           Data.Bifunctor            (second)
import           Data.Singletons.Prelude   (SingI)
import           GHC.Prim                  (Proxy#, proxy#)

import           Data.Type.Grec            (ConvFromGrec (..),
                                            ConvGrecInfo (..), ConvToGrec (..),
                                            Convert (..), FieldNamesConvGrec,
                                            Grec (..), GrecWith (..),
                                            GrecWithout (..))
import           Perst.Database.Condition  (Condition, ConvCond)
import           Perst.Database.DataDef    (DataDefInfo (..), DdKey)
import           Perst.Database.DbOption   (DbOption (..), MonadCons,
                                            SessionMonad)

import           Perst.Database.DML.Delete as DML
import           Perst.Database.DML.Insert as DML
import           Perst.Database.DML.Select as DML
import           Perst.Database.DML.Update as DML
import           Perst.Database.TreeDef    (TdData)

type RecCons b r = (ConvFromGrec r [FieldDB b], ConvGrecInfo r)
type DMLCons b t r
  = ( DbOption b, DataDefInfo t, RecCons b r
    , SingI (DdKey t) -- for insert
    , ConvToGrec [FieldDB b] r -- for select
    , Eq (FieldDB b) -- for updateDiff
    )

gwok :: Proxy# k -> a -> GrecWithout (FieldNamesConvGrec k) a
gwok _ = GWO

class DMLCons b t (Grec r) => DML b t r where

  insertMany  :: ( MonadCons m, Traversable f
                 , RecCons b k
                 , SingI (FieldNamesConvGrec k)
                 )
              => f (k,r) -> SessionMonad b m (Maybe (f (GenKey b)))
  insertMany (fkr :: f (k,r))
    = insertManyDef (proxy# :: Proxy# '(b,t))
    $ fmap (second $ gwok (proxy# :: Proxy# k) . Grec) fkr

  deleteMany  :: (MonadCons m, Traversable f, RecCons b k)
              => f k -> SessionMonad b m ()
  deleteMany  = deleteManyDef (proxy# :: Proxy# '(b,t))

  -- k - key; k1 - addition to rec
  updateMany :: (MonadCons m, Traversable f
                , RecCons b k, SingI (FieldNamesConvGrec k)
                , RecCons b k1, SingI (FieldNamesConvGrec k1)
                )
              => f (k,(k1,r)) -> SessionMonad b m ()
  updateMany (fkkr :: f (k,(k1,r)))
    = updateManyDef (proxy# :: Proxy# '(b,t))
    $ fmap (second $ gwok (proxy# :: Proxy# k) . second Grec) fkkr

  updateDiffMany :: (MonadCons m
                    , RecCons b k, SingI (FieldNamesConvGrec k)
                    , RecCons b k1, SingI (FieldNamesConvGrec k1)
                    )
                  => [(k,(k1,r),(k1,r))] -> SessionMonad b m ()
  updateDiffMany (krrs :: [(k,(k1,r),(k1,r))])
    = updateDiffManyDef (proxy# :: Proxy# '(b,t))
    $ fmap (\(k, r1, r2) -> (k, gwokgrek (second Grec r1)
                              , gwokgrek (second Grec r2))) krrs
   where
    gwokgrek = gwok (proxy# :: Proxy# k)

  selectMany :: (MonadCons m, Traversable f, RecCons b k
                , Convert [FieldDB b] ([FieldDB b], r1)
                , ConvGrecInfo r1
                )
              => Proxy# r1 -> f k -> SessionMonad b m (f [(r1,r)])
  selectMany (_ :: Proxy# r1)
    = fmap (fmap (map (second unGrec)))
    . selectManyDef (proxy# :: Proxy# '(b,t,(r1,Grec r)))
  -- insertMany' :: (MonadCons m, Traversable f)
  --             => f r -> SessionMonad b m (Maybe (f (GenKey b)))
  -- insertMany' = insertMany @b @t @r . fmap ((),)
  --
  selectCond :: (ConvCond b (Condition tree r)
                , MonadCons m, (TdData tree ~ t)
                , Convert [FieldDB b] ([FieldDB b], r1)
                , ConvGrecInfo r1
                )
             => Proxy# r1 -> Condition tree r -> SessionMonad b m [(r1,r)]
  selectCond (_ :: Proxy# r1)
    = fmap (map (second unGrec)) . selectCondDef (proxy# :: Proxy# '(b,r1))
