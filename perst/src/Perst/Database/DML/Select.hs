{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE TypeApplications #-}
module Perst.Database.DML.Select where

import           Control.Monad.Catch       (finally)
-- import           Data.Singletons.Prelude   (Snd)
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

selectText :: UpdTextCons b t r k => Proxy# '(b,t,r,k) -> T.Text
selectText (_ :: Proxy# '(b,t,r,k))
  = selectText' (proxy# :: Proxy# '(t,r))
  $ T.intercalate " AND "
  $ zipWith (\n s -> formatS "{} = {}" (s, paramName @b n))
            [0..] $ fieldNames @k

selectManyDef :: SelManyCons m f b t r k
            => Proxy# '(b,t,r) -> f k -> SessionMonad b m (f [r])
selectManyDef (_ :: Proxy# '(b,t,r)) (ks :: f k)
  = do
    cmd <- prepareCommand @b $ selectText (proxy# :: Proxy# '(b,t,r,k))
    finally ( fmap (fmap convToGrec)
              <$> mapM (runSelect @b cmd) (fmap convFromGrec ks)
            )
            (finalizePrepared @b cmd)

selectText' :: (ConvGrecInfo r, DataDefInfo t)
            => Proxy# '(t,r) -> T.Text -> T.Text
selectText' (_ :: Proxy# '(t,r)) whereCond
  = formatS "SELECT {} FROM {} t0 WHERE {}"
    ( T.intercalate "," $ fieldNames @r
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
  cmd <- prepareCommand @b $ selectText' (proxy# :: Proxy# '(TdData t,r)) txt
  finally (map convToGrec <$> runSelect @b cmd ps) (finalizePrepared @b cmd)
 where
  (txt,ps) = runConvCond (convCond @b c)
