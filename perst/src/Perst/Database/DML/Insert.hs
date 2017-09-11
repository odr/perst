{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE TypeApplications #-}
module Perst.Database.DML.Insert where

import           Control.Monad           (when)
import           Control.Monad.Catch     (finally)
import           Data.List               ((\\))
import           Data.Singletons.Prelude (SingI)
import qualified Data.Text               as T
import           GHC.Prim                (Proxy#, proxy#)

import           Data.Type.Grec          as Grec (ConvFromGrec (..),
                                                  ConvGrecInfo (..),
                                                  GrecWithout (..))
import           Perst.Database.DataDef  (DataDef, DataDefInfo (..), DataKey,
                                          WithoutKey, formatS)
import           Perst.Database.DbOption (DbOption (..), MonadCons,
                                          SessionMonad)

type InsTextCons b t r = (DbOption b, ConvGrecInfo r, DataDefInfo t)

type InsCons m b t r
  = ( InsTextCons b t r, MonadCons m, ConvFromGrec r [FieldDB b]
    , SingI (DataKey t)
    )

type InsManyCons m f b t r = ( Traversable f, InsCons m b t r)

insertTextDef :: InsTextCons b t r
        => Proxy# b -> Proxy# t -> Proxy# r -> T.Text
insertTextDef (_ :: Proxy# b) (_ :: Proxy# t) (_ :: Proxy# r)
  = formatS "INSERT INTO {}({}) VALUES({})"
  ( tableName @t
  , T.intercalate "," fns
  , T.intercalate "," $ zipWith (const $ paramName @b) fns [0..]
  )
 where
  fns = (Grec.fieldNames @r) \\ if autoIns @t then primaryKey @t else []

insertManyDef  ::  InsManyCons m f b t r
    => Proxy# b -> Proxy# t -> f r -> SessionMonad b m (Maybe (f (GenKey b)))
insertManyDef (pb :: Proxy# b) (pt :: Proxy# t) (rs :: f r) = do
  when ai $ preRunInAuto @b
  (cmd :: PrepCmd b) <- prepareCommand @b
                      $ insertTextDef pb pt (proxy# :: Proxy# r)
  finally ( fmap sequenceA
          $ mapM (\r -> do
                  runPrepared @b cmd r
                  if ai
                    then Just <$> getLastKey @b
                    else return Nothing
              ) $ fmap (
                    if ai
                      then convFromGrec @(WithoutKey t r) . GWO
                      else convFromGrec
                  ) rs
          )
          (finalizePrepared @b cmd)
 where
  ai = autoIns @t
