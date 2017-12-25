{-# LANGUAGE MagicHash        #-}
-- {-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType       #-}
module Perst.Database.DML.Select where

import           Control.Monad.Catch      (finally)
import           Data.Monoid              (Monoid (..))
import           Data.Singletons.Prelude  (SingI)
import qualified Data.Text                as T
import           GHC.Prim                 (Proxy#, proxy#)

import           Data.Type.GrecTree       (ConsNames, ConvNames (..),
                                           Convert (..), convNoRest)
import           Perst.Database.Condition (CondRec, Condition, ConvCond (..),
                                           runConvCond)
import           Perst.Database.DataDef   (DataStruct, DataStructInfo, DdName,
                                           DsInfo, GetDS, GetDataStruct,
                                           formatS, tableName)
import           Perst.Database.DbOption  (DbOption (..), MonadCons,
                                           SessionMonad)
-- import           Perst.Database.TreeDef   (TdData)
import           Perst.Types              (NoLstFld)


type SelCons0 b t r =
  ( DbOption b, SingI (DsInfo t), Convert r [FieldDB b], ConsNames NoLstFld r
  , Convert [FieldDB b] (r,[FieldDB b])
  )

type SelCons b t r k =
  ( SelCons0 b t r, Convert k [FieldDB b], ConsNames NoLstFld k)

type SelCondCons b sch t r = (SelCons0 b t r, ConvCond b (CondRec sch (DdName t) r))

selectText :: ( DbOption b, DataStructInfo t
              , ConsNames NoLstFld r, ConsNames NoLstFld k
              )
           => Proxy# '(b,(t::DataStruct),r,k) -> T.Text
selectText (_ :: Proxy# '(b,t,r,k))
  = selectText' (proxy# :: Proxy# '(t,r))
  $ T.intercalate " AND "
  $ zipWith (\n s -> formatS "{} = {}" (s, paramName @b n))
            [0..] $ fldNames @NoLstFld @k

selectManyDef :: (MonadCons m, Traversable f, SelCons b t r k)
              => Proxy# '(b,(t::DataStruct),r) -> f k -> SessionMonad b m (f [r])
selectManyDef (_ :: Proxy# '(b,t,r)) (ks :: f k)
  | null ks   = return $ const [] <$> ks
  | otherwise = do
    cmd <- prepareCommand @b $ selectText (proxy# :: Proxy# '(b,t,r,k))
    finally ( fmap (fmap convNoRest)
              <$> mapM (runSelect @b cmd) (fmap convert ks)
            )
            (finalizePrepared @b cmd)

selectText' :: (SingI (DsInfo t), ConsNames NoLstFld r)
            => Proxy# ('((t::DataStruct),r)) -> T.Text -> T.Text
selectText' (_ :: Proxy# '(t,r)) whereCond
  = formatS "SELECT {} FROM {} t0 {}"
    ( T.intercalate "," $ fldNames @NoLstFld @r
    , tableName @t
    , if T.null whereCond then "" else ("WHERE " `mappend` whereCond)
    )

selectCondDef :: ( SelCondCons b sch ds r, MonadCons m
                 , GetDataStruct t sch ~ Just ds
                 , t ~ DdName ds
                 , SingI (DsInfo ds)
                 )
              => Proxy# b -> Condition sch t r -> SessionMonad b m [r]
selectCondDef (_ :: Proxy# b) (c :: Condition sch t r) = do
  cmd <- prepareCommand @b $ selectText' (proxy# :: Proxy# '(GetDS t sch,r)) txt
  finally (map convNoRest <$> runSelect @b cmd ps) (finalizePrepared @b cmd)
 where
  (txt,ps) = runConvCond (convCond @b c)
