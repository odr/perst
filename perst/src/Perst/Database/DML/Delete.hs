{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE TypeApplications #-}
module Perst.Database.DML.Delete where

import           Control.Monad.Catch     (finally)
import           Data.Text               (Text, intercalate)
import           GHC.Prim                (Proxy#, proxy#)

import           Data.Type.Grec          as Grec (ConvFromGrec (..),
                                                  ConvGrecInfo (..))
import           Perst.Database.DataDef  (DataDef, DataDefInfo (..), formatS)
import           Perst.Database.DbOption (DbOption (..), MonadCons,
                                          SessionMonad)

type DelTextCons b t k = (DbOption b, ConvGrecInfo k, DataDefInfo t)

type DelCons m b t k = (MonadCons m, DelTextCons b t k, ConvFromGrec k [FieldDB b])

type DelManyCons m f b t k = (Traversable f, DelCons m b t k)

deleteTextDef :: DelTextCons b t k => Proxy# '(b,t,k) -> Text
deleteTextDef (_ :: Proxy# '(b,t,k))
  = formatS "DELETE FROM {} WHERE {}"
    ( tableName @t
    , intercalate " AND "
        $ zipWith (\n s -> formatS "{} = {}" (s, paramName @b n)) [0..]
        $ Grec.fieldNames @k
    )

deleteManyDef :: DelManyCons m f b t k
              => Proxy# '(b,t) -> f k -> SessionMonad b m ()
deleteManyDef (_ :: Proxy# '(b,t)) (ks :: f k)
  | null ks = return ()
  | otherwise = do
    cmd <- prepareCommand @b $ deleteTextDef (proxy# :: Proxy# '(b,t,k))
    finally (mapM_ (runPrepared @b cmd . convFromGrec) ks)
            (finalizePrepared @b cmd)
