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
  )
  where

import           Control.Arrow              ((***))
import           Control.Monad.Catch        (MonadMask, finally)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.Trans.Reader (ask)
import           Data.List                  (intercalate, partition, (\\))
import           Data.Maybe                 (fromMaybe)
import           Data.Proxy                 (Proxy (..))
import           Data.Text.Format           (format)
import           Data.Text.Lazy             (Text)
import qualified Data.Text.Lazy             as TL
import           Data.Type.Grec
import           Perst.Database.Types

insertText :: RecConstr b t r
            => Proxy b -> Proxy t -> Proxy r -> Bool -> Text
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
                      . delPos (fromMaybe [] $ posKey pt)
                      . convFromGrec
                      ) rs)
                  (finalizePrepared cmd)
 where
--  delPos :: [Integer] -> [a] -> [a]
  delPos ns = map snd . filter ((`notElem` ns) . fst) . zip [0..]

insertAuto :: ( MonadIO m, MonadMask m, InsAutoConstr b t r
              ) => Proxy t -> r -> SessionMonad b m [GenKey b]
insertAuto pt (r :: r) = insertManyAuto pt [r]

-- * UPDATE

updateByKeyText :: UpdByKeyConstr b t r k
    => Proxy b -> Proxy t -> Proxy r -> Proxy (k :: *) -> Text
updateByKeyText pb pt pr pk
  = format "UPDATE {} SET {} WHERE {}"
    ( tableName pt
    , rs
    , ks
    )
 where
  (ks, rs)
    = interSnd *** interSnd
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
  finally (mapM_ (runPrepared cmd . uncurry (++) . (convFromGrec *** convFromGrec)) rs)
          (finalizePrepared cmd)

updateByKey :: (MonadIO m, MonadMask m, UpdByKeyConstr b t r k)
            => Proxy t -> (k, r) -> SessionMonad b m ()
updateByKey pt = updateByKeyMany pt . (:[])

updateByPKMany :: ( MonadIO m, MonadMask m
                  , UpdByKeyConstr b t (GrecWithout (KeyDef t) r) (GrecWith (KeyDef t) r)
                  ) => Proxy t -> [r] -> SessionMonad b m ()
updateByPKMany (pt :: Proxy t) (rs :: [r])
    = updateByKeyMany pt
    $ map (\r -> (GW r :: GrecWith (KeyDef t) r, GWO r :: GrecWithout (KeyDef t) r)) rs


updateByPK :: ( MonadIO m, MonadMask m
              , UpdByKeyConstr b t (GrecWithout (KeyDef t) r) (GrecWith (KeyDef t) r)
              ) => Proxy t -> r -> SessionMonad b m ()
updateByPK pt = updateByPKMany pt . (:[])

-- * DELETE

deleteByKeyText :: DelByKeyConstr b t k
    => Proxy b -> Proxy t -> Proxy (k :: *) -> Text
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

deleteByPKMany :: ( MonadIO m, MonadMask m, DelByKeyConstr b t (GrecWith (KeyDef t) r)
                  ) => Proxy t -> [r] -> SessionMonad b m ()
deleteByPKMany (pt :: Proxy t) (rs :: [r])
    = deleteByKeyMany pt
    $ map (\r -> GW r :: GrecWith (KeyDef t) r) rs

deleteByPK :: ( MonadIO m, MonadMask m, DelByKeyConstr b t (GrecWith (KeyDef t) r)
              ) => Proxy t -> r -> SessionMonad b m ()
deleteByPK pt = deleteByPKMany pt . (:[])