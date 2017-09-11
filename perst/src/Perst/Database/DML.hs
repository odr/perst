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
                                            GrecWith (..), GrecWithout (..))
import           Perst.Database.DataDef    (DataDefInfo (..), DataKey)
import           Perst.Database.DbOption   (DbOption (..), MonadCons,
                                            SessionMonad)

import           Perst.Database.DML.Delete as DML
import           Perst.Database.DML.Insert as DML
import           Perst.Database.DML.Select as DML
import           Perst.Database.DML.Update as DML

type RecCons b r = (ConvFromGrec r [FieldDB b], ConvGrecInfo r)
type DMLCons b t r
  = ( DbOption b, DataDefInfo t, RecCons b r
    , SingI (DataKey t) -- for insert
    , ConvToGrec [FieldDB b] r -- for select
    , Eq (FieldDB b) -- for updateDiff
    )

gwok :: Proxy# k -> a -> GrecWithout (FieldNamesConvGrec k) a
gwok _ = GWO

class DMLCons b t r => DML b t r where

  insertMany  :: ( MonadCons m, Traversable f
                 , RecCons b k
                 , SingI (FieldNamesConvGrec k)
                 )
              => f (k,r) -> SessionMonad b m (Maybe (f (GenKey b)))
  insertMany (fkr :: f (k,r))
    = insertManyDef (proxy# :: Proxy# b) (proxy# :: Proxy# t)
    $ fmap (second $ gwok (proxy# :: Proxy# k)) fkr

  deleteMany  :: (MonadCons m, Traversable f, RecCons b k)
              => f k -> SessionMonad b m ()
  deleteMany  = deleteManyDef (proxy# :: Proxy# b) (proxy# :: Proxy# t)

  -- k - key; k1 - addition to rec
  updateMany :: (MonadCons m, Traversable f
                , RecCons b k, SingI (FieldNamesConvGrec k)
                , RecCons b k1, SingI (FieldNamesConvGrec k1)
                )
              => f (k,(k1,r)) -> SessionMonad b m ()
  updateMany (fkkr :: f (k,(k1,r)))
    = updateManyDef (proxy# :: Proxy# b) (proxy# :: Proxy# t)
    $ fmap (second $ gwok (proxy# :: Proxy# k)) fkkr

  updateDiffMany :: (MonadCons m
                    , RecCons b k, SingI (FieldNamesConvGrec k)
                    , RecCons b k1, SingI (FieldNamesConvGrec k1)
                    )
                  => [(k,(k1,r),(k1,r))] -> SessionMonad b m ()
  updateDiffMany (krrs :: [(k,(k1,r),(k1,r))])
    = updateDiffManyDef (proxy# :: Proxy# b) (proxy# :: Proxy# t)
    $ fmap (\(k, r1, r2) -> (k, gwokgrek r1, gwokgrek r2)) krrs
   where
    gwokgrek = gwok (proxy# :: Proxy# k)

  selectMany :: (MonadCons m, Traversable f, RecCons b k
                , Convert [FieldDB b] ([FieldDB b], r1)
                , ConvGrecInfo r1
                )
              => Proxy# r1 -> f k -> SessionMonad b m (f [(r1,r)])
  selectMany (_ :: Proxy# r1) = selectManyDef (proxy# :: Proxy# b) (proxy# :: Proxy# t)
                                              (proxy# :: Proxy# (r1,r))

  insertMany' :: (MonadCons m, Traversable f)
              => f r -> SessionMonad b m (Maybe (f (GenKey b)))
  insertMany' = insertMany @b @t @r . fmap ((),)
