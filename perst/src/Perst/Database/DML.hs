module Perst.Database.DML
  (
  -- * INSERT
    insertText
  , insertMany
  , insert
  , insertManyAuto
  , insertAuto

  -- * UPDATE
  , updateByKeyText
  , updateByKeyMany
  , updateByKey
  , updateByPKMany
  , updateByPK

  -- * DELETE
  , deleteByKeyText
  , deleteByKeyMany
  , deleteByPKMany
  , deleteByPK

  -- * SELECT
  , selectText
  , selectMany'
  , selectMany
  )
  where

import           Control.Monad.Catch        (MonadMask, finally)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.Trans.Reader (ask)
import           Data.Bifunctor             (bimap)
import           Data.List                  (intercalate, (\\))
import           Data.Proxy                 (Proxy (..))
import           Data.Text.Format           (format)
import qualified Data.Text.Lazy             as TL
import           Data.Type.Grec             (ConvFromGrec (..), ConvToGrec (..),
                                             GrecWith (..), GrecWithout (..))
import           Perst.Database.Constraints (DelByKeyConstr, DelConstr,
                                             InsAutoConstr, InsConstr,
                                             RecConstr, SelConstr,
                                             UpdByKeyConstr, UpdConstr)
import           Perst.Database.DataDef     (DdKey, fieldNames', fieldNamesT,
                                             primaryKey, tableName)
import           Perst.Database.DbOption    (DbOption (..), DbOptionConstr,
                                             SessionMonad)

insertText :: RecConstr b t r
            => Proxy b -> Proxy t -> Proxy r -> Bool -> TL.Text
insertText pb pt pr withPK
  = format "INSERT INTO {}({}) VALUES({})"
    ( tableName pt
    , intercalate "," fns
    , TL.intercalate "," $ zipWith (const $ paramName pb) fns [0..]
    )
 where
  fns = fieldNames' pr \\ if withPK then [] else primaryKey pt

insertMany  :: (MonadIO m, MonadMask m, InsConstr b t r)
            => Proxy t -> [r] -> SessionMonad b m ()
insertMany pt (rs :: [r]) = do
  (pb :: Proxy b, _) <- ask
  (cmd :: PrepCmd b) <- prepareCommand $ insertText pb pt (Proxy :: Proxy r) True
  finally  (mapM_ (runPrepared cmd . convFromGrec) rs)
                  (finalizePrepared cmd)

insert :: (MonadIO m, MonadMask m, InsConstr b t r)
            => Proxy t -> r -> SessionMonad b m ()
insert pt r = insertMany pt [r]

insertManyAuto :: ( MonadIO m, MonadMask m, InsAutoConstr b t r
                  ) => Proxy t -> [r] -> SessionMonad b m [GenKey b]
insertManyAuto (pt :: Proxy t) (rs :: [r]) = do
  (pb :: Proxy b, _) <- ask
  preRunInAuto
  (cmd :: PrepCmd b) <- prepareCommand $ insertText pb pt (Proxy :: Proxy r) False
  finally  (mapM ((>> getLastKey)
                      . runPrepared cmd
                      . convFromGrec
                      . (GWO :: r -> GrecWithout (DdKey t) r)
                      ) rs)
                  (finalizePrepared cmd)

insertAuto :: ( MonadIO m, MonadMask m, InsAutoConstr b t r
              ) => Proxy t -> r -> SessionMonad b m [GenKey b]
insertAuto pt (r :: r) = insertManyAuto pt [r]

-- * UPDATE

updateByKeyText :: UpdByKeyConstr b t r k
    => Proxy b -> Proxy t -> Proxy r -> Proxy (k :: *) -> TL.Text
updateByKeyText pb pt pr pk
  = format "UPDATE {} SET {} WHERE {}" (tableName pt, rs, ks)
 where
  (ks, rs)
    = bimap interSnd interSnd
    $ splitAt (length keyNames)
    $ zipWith (\n s -> (s, format "{} = {}" (s, paramName pb n))) [0..]
    $ keyNames ++ fieldNames' pr
   where
    keyNames = fieldNames' pk
    interSnd = TL.intercalate "," . map snd

updateByKeyMany  :: (MonadIO m, MonadMask m, UpdByKeyConstr b t r k)
                => Proxy t -> [(k,r)] -> SessionMonad b m ()
updateByKeyMany (pt :: Proxy t) (rs :: [(k,r)]) = do
  (pb :: Proxy b, _) <- ask
  (cmd :: PrepCmd b) <- prepareCommand
                      $ updateByKeyText pb pt (Proxy :: Proxy r) (Proxy :: Proxy k)
  finally (mapM_ (runPrepared cmd . uncurry (++) . bimap convFromGrec convFromGrec) rs)
          (finalizePrepared cmd)

updateByKey :: (MonadIO m, MonadMask m, UpdByKeyConstr b t r k)
            => Proxy t -> (k, r) -> SessionMonad b m ()
updateByKey pt = updateByKeyMany pt . (:[])

updateByPKMany :: ( MonadIO m, MonadMask m
                  , UpdByKeyConstr b t (GrecWithout (DdKey t) r) (GrecWith (DdKey t) r)
                  ) => Proxy t -> [r] -> SessionMonad b m ()
updateByPKMany (pt :: Proxy t) (rs :: [r])
    = updateByKeyMany pt
    $ map (\r -> (GW r :: GrecWith (DdKey t) r, GWO r :: GrecWithout (DdKey t) r)) rs

updateByPK :: ( MonadIO m, MonadMask m
              , UpdByKeyConstr b t (GrecWithout (DdKey t) r) (GrecWith (DdKey t) r)
              ) => Proxy t -> r -> SessionMonad b m ()
updateByPK pt = updateByPKMany pt . (:[])

-- * DELETE

deleteByKeyText :: DelByKeyConstr b t k
    => Proxy b -> Proxy t -> Proxy (k :: *) -> TL.Text
deleteByKeyText pb pt pk
  = format "DELETE FROM {} WHERE {}"
    ( tableName pt
    , TL.intercalate ","
        $ zipWith (\n s -> format "{} = {}" (s, paramName pb n)) [0..]
        $ fieldNames' pk
    )

deleteByKeyMany :: (MonadIO m, MonadMask m, DelByKeyConstr b t k)
                => Proxy t -> [k] -> SessionMonad b m ()
deleteByKeyMany pt (ks :: [k]) = do
  (pb :: Proxy b, _) <- ask
  (cmd :: PrepCmd b) <- prepareCommand
                      $ deleteByKeyText pb pt (Proxy :: Proxy k)
  finally (mapM_ (runPrepared cmd . convFromGrec) ks) (finalizePrepared cmd)

deleteByKey :: (MonadIO m, MonadMask m, DelByKeyConstr b t k)
            => Proxy t -> k -> SessionMonad b m ()
deleteByKey pt = deleteByKeyMany pt . (:[])

deleteByPKMany :: ( MonadIO m, MonadMask m, DelByKeyConstr b t (GrecWith (DdKey t) r)
                  ) => Proxy t -> [r] -> SessionMonad b m ()
deleteByPKMany (pt :: Proxy t) (rs :: [r])
    = deleteByKeyMany pt
    $ map (\r -> GW r :: GrecWith (DdKey t) r) rs

deleteByPK :: ( MonadIO m, MonadMask m, DelByKeyConstr b t (GrecWith (DdKey t) r)
              ) => Proxy t -> r -> SessionMonad b m ()
deleteByPK pt = deleteByPKMany pt . (:[])

-- * SELECT

selectText' :: DbOptionConstr b t
  => Proxy (b :: *) -> Proxy t -> [TL.Text] -> [TL.Text] -> TL.Text
selectText' pb pt fldNames keyNames
  = format "SELECT {} FROM {} WHERE {}"
    ( TL.intercalate "," fldNames
    , tableName pt
    , TL.intercalate ","
        $ zipWith (\n s -> format "{} = {}" (s, paramName pb n))
                  [0..] keyNames
    )
selectText :: SelConstr b t r k
  => Proxy (b :: *) -> Proxy t -> Proxy (r :: *) -> Proxy (k :: *) -> TL.Text
selectText pb pt pr pk = selectText' pb pt (fieldNamesT pr) (fieldNamesT pk)

selectMany' :: (MonadIO m, MonadMask m, DbOptionConstr b t)
            => Proxy t -> [TL.Text] -> [TL.Text] -> [[FieldDB b]]
            -> SessionMonad b m [[[FieldDB b]]]
selectMany' (pt :: Proxy t) fldNames keyNames keys = do
  (pb :: Proxy b, _) <- ask
  (cmd :: PrepCmd b) <- prepareCommand $ selectText' pb pt fldNames keyNames
  finally (mapM (runSelect cmd) keys) (finalizePrepared cmd)

selectMany :: (MonadIO m, MonadMask m, SelConstr b t r k)
                => Proxy t -> Proxy r -> [k] -> SessionMonad b m [[r]]
selectMany (pt :: Proxy t) (pr :: Proxy r) (ks :: [k])
  = map (map convToGrec)
  <$> selectMany' pt
                  (fieldNamesT pr)
                  (fieldNamesT (Proxy :: Proxy k))
                  (map convFromGrec ks)
