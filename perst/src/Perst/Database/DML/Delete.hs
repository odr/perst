{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}
module Perst.Database.DML.Delete where

--
import           Control.Monad.Catch     (finally)
-- import           Data.Singletons.Prelude (Sing, SingI (..))
import qualified Data.Text               as T
import           GHC.Prim                (Proxy#, proxy#)

import           Data.Type.Grec          as Grec (ConvFromGrec (..),
                                                  ConvGrecInfo (..),
                                                  GrecWith (..))
import           Perst.Database.DataDef  (DataDef, DataDefInfo (..), WithKey,
                                          formatS)
import           Perst.Database.DbOption (DbOption (..), MonadCons,
                                          SessionMonad)

-- * DELETE
type DelTextCons b t k = (DbOption b, ConvGrecInfo k, DataDefInfo t)

type DelCons m b t k = (MonadCons m, DelTextCons b t k, ConvFromGrec k [FieldDB b])

type DelManyCons m f b t k = (Traversable f, DelCons m b t k)

-- class DeleteByKey b t k where
--   deleteByKeyMany :: (MonadCons m, Traversable f) => f k -> SessionMonad b m ()
--   default deleteByKeyMany :: DelManyCons m f b t k  => f k -> SessionMonad b m ()
--   deleteByKeyMany = deleteByKeyManyDef (proxy# :: Proxy# b) (proxy# :: Proxy# t)
--
--   deleteByKey :: MonadCons m => k -> SessionMonad b m ()
--   deleteByKey = deleteByKeyMany @b @t . (:[])

deleteTextDef :: DelTextCons b t k => Proxy# b -> Proxy# t -> Proxy# k -> T.Text
deleteTextDef (_ :: Proxy# b) (_ :: Proxy# t) (_ :: Proxy# k)
  = formatS "DELETE FROM {} WHERE {}"
    ( tableName @t
    , T.intercalate " AND "
        $ zipWith (\n s -> formatS "{} = {}" (s, paramName @b n)) [0..]
        $ Grec.fieldNames @k
    )

deleteManyDef :: DelManyCons m f b t k
              => Proxy# b -> Proxy# t -> f k -> SessionMonad b m ()
deleteManyDef (pb :: Proxy# b) pt (ks :: f k) = do
  cmd <- prepareCommand @b $ deleteTextDef pb pt (proxy# :: Proxy# k)
  finally (mapM_ (runPrepared @b cmd . convFromGrec) ks)
          (finalizePrepared @b cmd)

-- deleteByKeyManySafe :: (Traversable f, DelByKeyConstr m b t k)
--                     => Sing t -> f k -> SessionMonad b m ()
-- deleteByKeyManySafe = deleteByKeyMany

-- class DeleteByKey b t (WithKey t r) => DeleteByPK b t r where
--   deleteByPKMany :: (MonadCons m, Traversable f) => f r -> SessionMonad b m ()
--   deleteByPKMany = deleteByKeyMany @b @t . fmap (\r -> GW r :: WithKey t r)
--
--   deleteByPK  :: MonadCons m => r -> SessionMonad b m ()
--   deleteByPK = deleteByPKMany @b @t . (:[])
--
-- instance DeleteByKey b t (WithKey t r) => DeleteByPK b t r
--
-- class DeleteByPK b t (Grec r) => DeleteByPKR b t r where
--   deleteByPKManyR :: (MonadCons m, Traversable f) => f r -> SessionMonad b m ()
--   deleteByPKManyR = deleteByPKMany @b @t . fmap Grec
--
--   deleteByPKR :: MonadCons m => r -> SessionMonad b m ()
--   deleteByPKR = deleteByPK @b @t . Grec
--
-- instance DeleteByPK b t (Grec r) => DeleteByPKR b t r
