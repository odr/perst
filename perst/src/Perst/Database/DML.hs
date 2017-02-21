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

insertText :: (TabConstrB b t, RecConstr b t r)
            => Proxy b -> Proxy t -> Proxy r -> Bool -> Text
insertText pb pt pr withPK
  = format "INSERT INTO {}({}) VALUES({})"
    ( tableName pt
    , intercalate "," fns
    , TL.intercalate "," $ zipWith (const $ paramName pb) fns [0..]
    )
 where
  fns = fieldNames' pr \\ if withPK then [] else primaryKey pt

insertMany  :: (MonadIO m, MonadMask m, TabConstrB b t, InsConstr b t r)
            => Proxy t -> [r] -> SessionMonad b m ()
insertMany pt (rs :: [r]) = do
  (pb :: Proxy b, _) <- ask
  (cmd :: PrepCmd b) <- prepareCommand $ insertText pb pt (Proxy :: Proxy r) True
  finally  (mapM_ (runPrepared cmd . convFromGrec) rs)
                  (finalizePrepared cmd)

insert :: (MonadIO m, MonadMask m, TabConstrB b t, InsConstr b t r)
            => Proxy t -> r -> SessionMonad b m ()
insert pt r = insertMany pt [r]

insertManyAuto :: ( MonadIO m, MonadMask m, DBOption b
                  , TabConstrB b t, InsAutoConstr b t r
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

insertAuto :: ( MonadIO m, MonadMask m, TabConstrB b t, InsAutoConstr b t r
              ) => Proxy t -> r -> SessionMonad b m [GenKey b]
insertAuto pt (r :: r) = insertManyAuto pt [r]

-- * UPDATE

updateByKeyText :: (TabConstrB b t, UpdByKeyConstr b t r k)
    => Proxy b -> Proxy t -> Proxy r -> Proxy k ->Text
updateByKeyText pb pt pr pk
  = format "UPDATE {} SET {} WHERE {}"
    ( tableName pt
    , rs
    , pks
    )
 where
  (pks, rs)
    = interSnd *** interSnd
    $ partition ((`elem` getSymbols pk) . fst)
    $ zipWith (\n s -> (s, format "{} = {}" (s, paramName pb n))) [0..]
    $ fieldNames' pr
   where
    interSnd = TL.intercalate "," . map snd

updateByKeyMany  :: (MonadIO m, MonadMask m, TabConstrB b t, UpdByKeyConstr b t r k)
                => Proxy t -> Proxy k -> [r] -> SessionMonad b m ()
updateByKeyMany (pt :: Proxy t) (pk :: Proxy k) (rs :: [r]) = do
  (pb :: Proxy b, _) <- ask
  (cmd :: PrepCmd b) <- prepareCommand $ updateByKeyText pb pt (Proxy :: Proxy r) pk
  finally  (mapM_ (runPrepared cmd . convFromGrec) rs)
                  (finalizePrepared cmd)

updateByKey :: (MonadIO m, MonadMask m, TabConstrB b t, UpdByKeyConstr b t r k)
            => Proxy t -> Proxy k -> r -> SessionMonad b m ()
updateByKey pt pk = updateByKeyMany pt pk . (:[])

updateByPKMany  :: (MonadIO m, MonadMask m, TabConstrB b t, UpdByKeyConstr b t r (KeyDef t))
                => Proxy t -> [r] -> SessionMonad b m ()
updateByPKMany (pt :: Proxy t) = updateByKeyMany pt (Proxy :: Proxy (KeyDef t))

updateByPK  :: (MonadIO m, MonadMask m, TabConstrB b t, UpdByKeyConstr b t r (KeyDef t))
            => Proxy t -> r -> SessionMonad b m ()
updateByPK pt = updateByPKMany pt . (:[])

-- updateWithKeyMany  :: (MonadIO m, MonadMask m, TabConstrB b t, UpdByKeyConstr b t r k)
--                 => Proxy t -> Proxy k -> [(ListRep k, r)] -> SessionMonad b m ()
-- updateWithKeyMany (pt :: Proxy t) (pk :: Proxy k) (rs :: [(ListRep k, r)]) = do
