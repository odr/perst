module Perst.Database.DML
  (
  -- * INSERT
    insertText
  , insertMany, insertManyR
  , insert, insertR
  , insertManyAuto, insertManyAutoR
  , insertAuto, insertAutoR

  -- * UPDATE
  , updateByKeyText
  , updateByKeyMany, updateByKeyManyR
  , updateByKey, updateByKeyR
  , updateByPKMany, updateByPKManyR
  , updateByPK, updateByPKR

  -- * DELETE
  , deleteByKeyText
  , deleteByKeyMany
  , deleteByPKMany, deleteByPKManyR
  , deleteByPK, deleteByPKR

  -- * SELECT
  , selectText
  , selectMany, selectManyR

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
import           Data.Traversable           (Traversable (..))
import           Data.Type.Grec             (ConvFromGrec (..), ConvToGrec (..),
                                             Grec (..), GrecWith (..),
                                             GrecWithout (..))
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
insertText pb pt pr withPK  -- = insertText' pb pt . map TL.pack . fieldNames'
  = format "INSERT INTO {}({}) VALUES({})"
  ( tableName pt
  , TL.intercalate "," fns
  , TL.intercalate "," $ zipWith (const $ paramName pb) fns [0..]
  )
 where
  fns = map TL.pack (fieldNames' pr)
      \\ if withPK then [] else map TL.pack (primaryKey pt)

insertMany  :: ( MonadIO m, MonadMask m, Traversable f
               , InsConstr b t r
               )
            => Proxy t -> f r -> SessionMonad b m ()
insertMany pt (rs :: f r) = do
  (pb :: Proxy b, _) <- ask
  (cmd :: PrepCmd b) <- prepareCommand $ insertText pb pt (Proxy :: Proxy r) True
  finally  (mapM_ (runPrepared cmd) $ fmap convFromGrec rs)
                  (finalizePrepared cmd)

insertManyR  :: ( MonadIO m, MonadMask m, Traversable f
                , InsConstr b t (Grec r)
                )
            => Proxy t -> f r -> SessionMonad b m ()
insertManyR pt = insertMany pt . fmap Grec

insert :: (MonadIO m, MonadMask m, InsConstr b t r)
            => Proxy t -> r -> SessionMonad b m ()
insert pt r = insertMany pt [r]

insertR :: (MonadIO m, MonadMask m, InsConstr b t (Grec r))
            => Proxy t -> r -> SessionMonad b m ()
insertR pt = insert pt . Grec

insertManyAuto :: ( MonadIO m, MonadMask m, Traversable f
                  , InsAutoConstr b t r
                  ) => Proxy t -> f r -> SessionMonad b m (f (GenKey b))
insertManyAuto (pt :: Proxy t) (rs :: f r) = do
  (pb :: Proxy b, _) <- ask
  preRunInAuto
  (cmd :: PrepCmd b) <- prepareCommand $ insertText pb pt (Proxy :: Proxy r) False
  finally (mapM ((>> getLastKey) . runPrepared cmd)
                $ fmap (convFromGrec . (GWO :: r -> GrecWithout (DdKey t) r)) rs
          )
          (finalizePrepared cmd)

insertManyAutoR :: ( MonadIO m, MonadMask m, Traversable f
                   , InsAutoConstr b t (Grec r)
                   ) => Proxy t -> f r -> SessionMonad b m (f (GenKey b))
insertManyAutoR pt = insertManyAuto pt . fmap Grec

insertAuto :: ( MonadIO m, MonadMask m, InsAutoConstr b t r
              ) => Proxy t -> r -> SessionMonad b m [GenKey b]
insertAuto pt (r :: r) = insertManyAuto pt [r]

insertAutoR :: ( MonadIO m, MonadMask m, InsAutoConstr b t (Grec r)
              ) => Proxy t -> r -> SessionMonad b m [GenKey b]
insertAutoR pt = insertAuto pt . Grec

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
  finally (mapM_ ( runPrepared cmd . uncurry (++)
                 . bimap convFromGrec convFromGrec
                 ) rs)
          (finalizePrepared cmd)

updateByKeyManyR  :: (MonadIO m, MonadMask m, UpdByKeyConstr b t (Grec r) k)
                => Proxy t -> [(k,r)] -> SessionMonad b m ()
updateByKeyManyR pt = updateByKeyMany pt . map (fmap Grec)

updateByKey :: (MonadIO m, MonadMask m, UpdByKeyConstr b t r k)
            => Proxy t -> (k, r) -> SessionMonad b m ()
updateByKey pt = updateByKeyMany pt . (:[])

updateByKeyR  :: (MonadIO m, MonadMask m, UpdByKeyConstr b t (Grec r) k)
                => Proxy t -> (k,r) -> SessionMonad b m ()
updateByKeyR pt = updateByKey pt . fmap Grec

updateByPKMany
  ::  ( MonadIO m, MonadMask m
      , UpdByKeyConstr b t (GrecWithout (DdKey t) r) (GrecWith (DdKey t) r)
      ) => Proxy t -> [r] -> SessionMonad b m ()
updateByPKMany (pt :: Proxy t) (rs :: [r])
    = updateByKeyMany pt
    $ map (\r -> ( GW r :: GrecWith (DdKey t) r
                 , GWO r :: GrecWithout (DdKey t) r)
                 ) rs

updateByPKManyR
  :: ( MonadIO m, MonadMask m
     , UpdByKeyConstr b t (GrecWithout (DdKey t) (Grec r))
                          (GrecWith (DdKey t) (Grec r))
     ) => Proxy t -> [r] -> SessionMonad b m ()
updateByPKManyR pt = updateByPKMany pt . map Grec

updateByPK
  ::  ( MonadIO m, MonadMask m
      , UpdByKeyConstr b t (GrecWithout (DdKey t) r) (GrecWith (DdKey t) r)
      ) => Proxy t -> r -> SessionMonad b m ()
updateByPK pt = updateByPKMany pt . (:[])

updateByPKR
  ::  ( MonadIO m, MonadMask m
      , UpdByKeyConstr b t (GrecWithout (DdKey t) (Grec r))
                           (GrecWith (DdKey t) (Grec r))
      ) => Proxy t -> r -> SessionMonad b m ()
updateByPKR pt = updateByPK pt . Grec

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

deleteByPKMany
  ::  ( MonadIO m, MonadMask m, DelByKeyConstr b t (GrecWith (DdKey t) r)
      ) => Proxy t -> [r] -> SessionMonad b m ()
deleteByPKMany (pt :: Proxy t) (rs :: [r])
    = deleteByKeyMany pt
    $ map (\r -> GW r :: GrecWith (DdKey t) r) rs

deleteByPKManyR
  :: ( MonadIO m, MonadMask m, DelByKeyConstr b t (GrecWith (DdKey t) (Grec r))
     ) => Proxy t -> [r] -> SessionMonad b m ()
deleteByPKManyR pt = deleteByPKMany pt . map Grec

deleteByPK
  ::  ( MonadIO m, MonadMask m, DelByKeyConstr b t (GrecWith (DdKey t) r)
      ) => Proxy t -> r -> SessionMonad b m ()
deleteByPK pt = deleteByPKMany pt . (:[])

deleteByPKR
  ::  ( MonadIO m, MonadMask m, DelByKeyConstr b t (GrecWith (DdKey t) (Grec r))
      ) => Proxy t -> r -> SessionMonad b m ()
deleteByPKR pt = deleteByPK pt . Grec

-- * SELECT

selectText :: SelConstr b t r k
  => Proxy (b :: *) -> Proxy t -> Proxy (r :: *) -> Proxy (k :: *) -> TL.Text
selectText pb pt pr pk
  = format "SELECT {} FROM {} WHERE {}"
    ( TL.intercalate "," $ fieldNamesT pr
    , tableName pt
    , TL.intercalate ","
        $ zipWith (\n s -> format "{} = {}" (s, paramName pb n))
                  [0..] $ fieldNamesT pk
    )

selectMany :: (MonadIO m, MonadMask m, SelConstr b t r k, Traversable f)
            => Proxy t -> Proxy r -> f k -> SessionMonad b m (f [r])
selectMany (pt :: Proxy t) (pr :: Proxy r) (ks :: f k)
  = do
    (pb :: Proxy b, _) <- ask
    (cmd :: PrepCmd b) <- prepareCommand $ selectText pb pt pr (Proxy :: Proxy k)
    finally (fmap (fmap convToGrec) <$> mapM (runSelect cmd) (fmap convFromGrec ks))
            (finalizePrepared cmd)

selectManyR :: (MonadIO m, MonadMask m, SelConstr b t (Grec r) k, Traversable f)
            => Proxy t -> Proxy r -> f k -> SessionMonad b m (f [r])
selectManyR pt (pr :: Proxy r)
  = fmap (fmap $ map unGrec) . selectMany pt (Proxy :: Proxy (Grec r))
