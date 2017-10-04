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
import           Perst.Database.Condition  (Condition, ConvCond (..),
                                            runConvCond)
import           Perst.Database.DataDef    (DataDefInfo, WithKey, formatS,
                                            tableName)
import           Perst.Database.DbOption   (DbOption (..), MonadCons,
                                            SessionMonad)
import           Perst.Database.DML.Update (UpdTextCons)
import           Perst.Database.TreeDef    (TdData)


type SelCons b t r k
  = ( UpdTextCons b t r k, ConvToGrec [FieldDB b] r, ConvFromGrec k [FieldDB b])

type SelManyCons m f b t r k
  = ( MonadCons m, Traversable f, SelCons b t r k)

selectText :: UpdTextCons b t r k
           => Proxy# b -> Proxy# t -> Proxy# r -> Proxy# k -> T.Text
selectText (pb :: Proxy# b) pt pr (_ :: Proxy# k)
  = selectText' pb pt pr
  $ T.intercalate " AND "
  $ zipWith (\n s -> formatS "{} = {}" (s, paramName @b n))
            [0..] $ Grec.fieldNames @k

selectManyDef :: SelManyCons m f b t r k
            => Proxy# b -> Proxy# t -> Proxy# r -> f k -> SessionMonad b m (f [r])
selectManyDef (pb :: Proxy# b) (pt :: Proxy# t) (pr :: Proxy# r) (ks :: f k)
  = do
    cmd <- prepareCommand @b $ selectText pb pt pr (proxy# :: Proxy# k)
    finally ( fmap (fmap convToGrec)
              <$> mapM (runSelect @b cmd) (fmap convFromGrec ks)
            )
            (finalizePrepared @b cmd)

selectText' :: (DbOption b, ConvGrecInfo r, DataDefInfo t)
            => Proxy# b -> Proxy# t -> Proxy# r -> T.Text -> T.Text
selectText' (_ :: Proxy# b) (_ :: Proxy# t) (_ :: Proxy# r) whereCond
  = formatS "SELECT {} FROM {} t0 WHERE {}"
    ( T.intercalate "," $ Grec.fieldNames @r
    , tableName @t
    , whereCond
    )

selectCondDef
  :: ( DbOption b, ConvGrecInfo r, DataDefInfo (TdData t)
     , ConvToGrec [FieldDB b] r
     , ConvCond b (Condition t r)
     , MonadCons m
     )
  => Proxy# b -> Condition t r -> SessionMonad b m [r]
selectCondDef (pb :: Proxy# b) (c :: Condition t r) = do
  cmd <- prepareCommand @b $ selectText' pb (proxy# :: Proxy# (TdData t)) (proxy# :: Proxy# r) txt
  finally (map convToGrec <$> runSelect @b cmd ps) (finalizePrepared @b cmd)
 where
  (txt,ps) = runConvCond (convCond @b c)
