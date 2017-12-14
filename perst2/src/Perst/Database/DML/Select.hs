{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE TypeApplications #-}
module Perst.Database.DML.Select where

import           Control.Monad.Catch      (finally)
import           Data.Monoid              (Monoid (..))
import qualified Data.Text                as T
import           GHC.Prim                 (Proxy#, proxy#)

import           Data.Type.GrecTree       (ConsNames, ConvNames (..),
                                           Convert (..), convNoRest)
import           Perst.Database.Condition (CondRec, Condition, ConvCond (..),
                                           runConvCond)
import           Perst.Database.DataDef   (DataDefInfo, formatS, tableName)
import           Perst.Database.DbOption  (DbOption (..), MonadCons,
                                           SessionMonad)
import           Perst.Database.TreeDef   (TdData)
import           Perst.Types              (NoLstFld)


type SelCons0 b t r =
  ( DbOption b, DataDefInfo t, Convert r [FieldDB b], ConsNames NoLstFld r
  , Convert [FieldDB b] (r,[FieldDB b])
  )

type SelCons b t r k =
  ( SelCons0 b t r, Convert k [FieldDB b], ConsNames NoLstFld k)

type SelCondCons b t r = (SelCons0 b (TdData t) r, ConvCond b (CondRec t r))

selectText :: ( DbOption b, DataDefInfo t
              , ConsNames NoLstFld r, ConsNames NoLstFld k
              )
           => Proxy# '(b,t,r,k) -> T.Text
selectText (_ :: Proxy# '(b,t,r,k))
  = selectText' (proxy# :: Proxy# '(t,r))
  $ T.intercalate " AND "
  $ zipWith (\n s -> formatS "{} = {}" (s, paramName @b n))
            [0..] $ fldNames @NoLstFld @k

selectManyDef :: (MonadCons m, Traversable f, SelCons b t r k)
              => Proxy# '(b,t,r) -> f k -> SessionMonad b m (f [r])
selectManyDef (_ :: Proxy# '(b,t,r)) (ks :: f k)
  | null ks   = return $ const [] <$> ks
  | otherwise = do
    cmd <- prepareCommand @b $ selectText (proxy# :: Proxy# '(b,t,r,k))
    finally ( fmap (fmap convNoRest)
              <$> mapM (runSelect @b cmd) (fmap convert ks)
            )
            (finalizePrepared @b cmd)

selectText' :: (DataDefInfo t, ConsNames NoLstFld r)
            => Proxy# '(t,r) -> T.Text -> T.Text
selectText' (_ :: Proxy# '(t,r)) whereCond
  = formatS "SELECT {} FROM {} t0 {}"
    ( T.intercalate "," $ fldNames @NoLstFld @r
    , tableName @t
    , if T.null whereCond then "" else ("WHERE " `mappend` whereCond)
    )

-- type TN r = Tagged () r
selectCondDef :: (SelCondCons b t r, MonadCons m)
              => Proxy# b -> Condition t r -> SessionMonad b m [r]
selectCondDef (_ :: Proxy# b) (c :: Condition t r) = do
  cmd <- prepareCommand @b $ selectText' (proxy# :: Proxy# '(TdData t,r)) txt
  finally (map convNoRest <$> runSelect @b cmd ps) (finalizePrepared @b cmd)
 where
  (txt,ps) = runConvCond (convCond @b c)
