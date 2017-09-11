{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE TypeApplications #-}
module Perst.Database.DML.Select where

import           Control.Monad.Catch       (finally)
import qualified Data.Text                 as T
import           GHC.Prim                  (Proxy#, proxy#)

import           Data.Type.Grec            as Grec (ConvFromGrec (..),
                                                    ConvGrecInfo (..),
                                                    ConvToGrec (..),
                                                    GrecWith (..))
import           Perst.Database.DataDef    (WithKey, formatS, tableName)
import           Perst.Database.DbOption   (DbOption (..), MonadCons,
                                            SessionMonad)
import           Perst.Database.DML.Update (UpdTextCons)


type SelCons b t r k
  = ( UpdTextCons b t r k, ConvToGrec [FieldDB b] r, ConvFromGrec k [FieldDB b])

type SelManyCons m f b t r k
  = ( MonadCons m, Traversable f, SelCons b t r k)

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
