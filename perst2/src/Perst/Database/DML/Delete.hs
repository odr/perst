{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE TypeApplications #-}
module Perst.Database.DML.Delete where

import           Control.Monad.Catch     (finally)
import           Data.Text               (Text, intercalate)
import           GHC.Prim                (Proxy#, proxy#)

import           Data.Type.GrecTree      (ConsNames, ConvNames (..),
                                          Convert (..))
import           Perst.Database.DataDef  (DataDefInfo (..), formatS)
import           Perst.Database.DbOption (DbOption (..), MonadCons,
                                          SessionMonad)
import           Perst.Types             (NoLstFld)

type DelTextCons b t k = (DbOption b, DataDefInfo t, ConsNames NoLstFld k)

type DelCons b t k = (DelTextCons b t k, Convert k [FieldDB b])

deleteTextDef :: DelTextCons b t k => Proxy# '(b,t,k) -> Text
deleteTextDef (_ :: Proxy# '(b,t,k))
  = formatS "DELETE FROM {} WHERE {}"
    ( tableName @t
    , intercalate " AND "
        $ zipWith (\n s -> formatS "{} = {}" (s, paramName @b n)) [0..]
        $ fldNames @NoLstFld @k
    )

deleteManyDef :: (MonadCons m, Traversable f, DelCons b t k)
              => Proxy# '(b,t) -> f k -> SessionMonad b m ()
deleteManyDef (_ :: Proxy# '(b,t)) (ks :: f k)
  | null ks = return ()
  | otherwise = do
    cmd <- prepareCommand @b $ deleteTextDef (proxy# :: Proxy# '(b,t,k))
    finally (mapM_ (runPrepared @b cmd . convert) ks)
            (finalizePrepared @b cmd)
