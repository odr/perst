{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}
module Perst.Database.DML.Select where

import           Control.Monad.Catch       (finally)
import qualified Data.Text                 as T
import           GHC.Prim                  (Proxy#, proxy#)

import           Data.Type.Grec            as Grec (ConvFromGrec (..),
                                                    ConvGrecInfo (..),
                                                    ConvToGrec (..), Grec (..),
                                                    GrecWith (..))
import           Perst.Database.DataDef    (WithKey, formatS, tableName)
import           Perst.Database.DbOption   (DbOption (..), MonadCons,
                                            SessionMonad)
import           Perst.Database.DML.Update (UpdTextCons)


type SelCons b t r k
  = ( UpdTextCons b t r k, ConvToGrec [FieldDB b] r, ConvFromGrec k [FieldDB b])

type SelManyCons m f b t r k
  = ( MonadCons m, Traversable f, SelCons b t r k)


class SelectByKey b t r k where
  selectMany :: (MonadCons m, Traversable f) => f k -> SessionMonad b m (f [r])
  default selectMany :: SelManyCons m f b t r k
                     => f k -> SessionMonad b m (f [r])
  selectMany = selectManyDef (proxy# :: Proxy# b) (proxy# :: Proxy# t)
                             (proxy# :: Proxy# r)

-- instance SelCons b t (r1,r2) k
--     => SelectByKey b t (r1,r2) k where
--   selectMany = selectManyDef (proxy# :: Proxy# b) (proxy# :: Proxy# t)
--                              (proxy# :: Proxy# (r1,r2))

selectText :: UpdTextCons b t r k
           => Proxy# b -> Proxy# t -> Proxy# r -> Proxy# k -> T.Text
selectText (_ :: Proxy# b) (_ :: Proxy# t) (_ :: Proxy# r) (_ :: Proxy# k)
  = formatS "SELECT {} FROM {} WHERE {}"
    ( T.intercalate "," $ Grec.fieldNames @r
    , tableName @t
    , T.intercalate " AND "
        $ zipWith (\n s -> formatS "{} = {}" (s, paramName @b n))
                  [0..] $ Grec.fieldNames @k
    )


selectManyDef :: SelManyCons m f b t r k
            => Proxy# b -> Proxy# t -> Proxy# r -> f k -> SessionMonad b m (f [r])
selectManyDef (pb :: Proxy# b) (pt :: Proxy# t) (pr :: Proxy# r) (ks :: f k)
  = do
    cmd <- prepareCommand @b $ selectText pb pt pr (proxy# :: Proxy# k)
    finally ( fmap (fmap convToGrec)
              <$> mapM (runSelect @b cmd) (fmap convFromGrec ks)
            )
            (finalizePrepared @b cmd)
--
-- selectManySafe :: (SelConstr m b t r k, Traversable f)
--                => Sing t -> Proxy r -> f k -> SessionMonad b m (f [r])
-- selectManySafe = selectMany

class SelectByKey b t (Grec r) k => SelectByKeyR b t r k where
  selectManyR :: (MonadCons m, Traversable f)
              => f k -> SessionMonad b m (f [r])
  selectManyR = fmap (fmap $ map unGrec) . selectMany @b @t @(Grec r)

instance SelectByKey b t (Grec r) k => SelectByKeyR b t r k
