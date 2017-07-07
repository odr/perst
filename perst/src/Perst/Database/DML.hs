module Perst.Database.DML
  (
  -- * INSERT
    insertText
  , insertMany, insertManyR
  , insert, insertR
  -- , insertManyAuto, insertManyAutoR
  -- , insertAuto, insertAutoR

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

import           Control.Monad              (when)
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
                                             InsConstr, RecConstr, SelConstr,
                                             UpdByKeyConstr, UpdConstr)
import           Perst.Database.DataDef     (DdAutoIns, DdKey, fieldNames',
                                             fieldNamesT, primaryKey, showProxy,
                                             tableName)
import           Perst.Database.DbOption    (DbOption (..), DbOptionConstr,
                                             SessionMonad)

insertText :: InsConstr m b t r
            => Proxy b -> Proxy t -> Proxy r -> SessionMonad b m TL.Text
insertText pb (pt :: Proxy t) pr
  = return $ format "INSERT INTO {}({}) VALUES({})"
  ( tableName pt
  , TL.intercalate "," fns
  , TL.intercalate "," $ zipWith (const $ paramName pb) fns [0..]
  )
 where
  autoIns = showProxy (Proxy :: Proxy (DdAutoIns t))
  fns = map TL.pack (fieldNames' pr)
      \\ if autoIns then map TL.pack (primaryKey pt) else []

insertMany  :: (Traversable f, InsConstr m b t r)
            => Proxy t -> f r -> SessionMonad b m (Maybe (f (GenKey b)))
insertMany (pt :: Proxy t) (rs :: f r) = do
  (pb :: Proxy b, _) <- ask
  when autoIns preRunInAuto
  (cmd :: PrepCmd b) <- insertText pb pt pr >>= prepareCommand
  finally (fmap sequenceA
              $ mapM (\r -> do
                      runPrepared cmd r
                      if autoIns
                        then Just <$> getLastKey
                        else return Nothing
                  ) $ fmap (
                        if autoIns
                          then convFromGrec . (GWO :: r -> GrecWithout (DdKey t) r)
                          else convFromGrec
                      ) rs
          )
          (finalizePrepared cmd)
 where
  pr = Proxy :: Proxy r
  autoIns = showProxy (Proxy :: Proxy (DdAutoIns t))

insertManyR  :: (Traversable f, InsConstr m b t (Grec r))
            => Proxy t -> f r -> SessionMonad b m (Maybe (f (GenKey b)))
insertManyR pt = insertMany pt . fmap Grec

insert  :: InsConstr m b t r
        => Proxy t -> r -> SessionMonad b m (Maybe (GenKey b))
insert pt r = fmap head <$> insertMany pt [r]

insertR :: InsConstr m b t (Grec r)
        => Proxy t -> r -> SessionMonad b m (Maybe (GenKey b))
insertR pt = insert pt . Grec

-- * UPDATE

updateByKeyText :: UpdByKeyConstr m b t r k
    => Proxy b -> Proxy t -> Proxy r -> Proxy (k :: *) -> SessionMonad b m TL.Text
updateByKeyText pb pt pr pk
  = return $ format "UPDATE {} SET {} WHERE {}" (tableName pt, rs, ks)
 where
  (ks, rs)
    = bimap interSnd interSnd
    $ splitAt (length keyNames)
    $ zipWith (\n s -> (s, format "{} = {}" (s, paramName pb n))) [0..]
    $ keyNames ++ fieldNames' pr
   where
    keyNames = fieldNames' pk
    interSnd = TL.intercalate "," . map snd

updateByKeyMany :: UpdByKeyConstr m b t r k
                => Proxy t -> [(k,r)] -> SessionMonad b m ()
updateByKeyMany (pt :: Proxy t) (rs :: [(k,r)]) = do
  (pb :: Proxy b, _) <- ask
  (cmd :: PrepCmd b) <- updateByKeyText pb pt pr pk
                    >>= prepareCommand
  finally (mapM_ ( runPrepared cmd . uncurry (++)
                 . bimap convFromGrec convFromGrec
                 ) rs)
          (finalizePrepared cmd)
 where
  (pr,pk) = (Proxy :: Proxy r, Proxy :: Proxy k)


updateByKeyManyR  :: UpdByKeyConstr m b t (Grec r) k
                  => Proxy t -> [(k,r)] -> SessionMonad b m ()
updateByKeyManyR pt = updateByKeyMany pt . map (fmap Grec)

updateByKey :: UpdByKeyConstr m b t r k
            => Proxy t -> (k, r) -> SessionMonad b m ()
updateByKey pt = updateByKeyMany pt . (:[])

updateByKeyR  :: UpdByKeyConstr m b t (Grec r) k
              => Proxy t -> (k,r) -> SessionMonad b m ()
updateByKeyR pt = updateByKey pt . fmap Grec

updateByPKMany
  :: UpdByKeyConstr m b t (GrecWithout (DdKey t) r) (GrecWith (DdKey t) r)
  => Proxy t -> [r] -> SessionMonad b m ()
updateByPKMany (pt :: Proxy t) (rs :: [r])
    = updateByKeyMany pt
    $ map (\r -> ( GW  r :: GrecWith    (DdKey t) r
                 , GWO r :: GrecWithout (DdKey t) r)
                 ) rs

updateByPKManyR  :: ( UpdByKeyConstr m b t (GrecWithout (DdKey t) (Grec r))
                                           (GrecWith    (DdKey t) (Grec r))
                    )
  => Proxy t -> [r] -> SessionMonad b m ()
updateByPKManyR pt = updateByPKMany pt . fmap Grec

updateByPK
  :: UpdByKeyConstr m b t (GrecWithout (DdKey t) r) (GrecWith (DdKey t) r)
  => Proxy t -> r -> SessionMonad b m ()
updateByPK pt = updateByPKMany pt . (:[])

updateByPKR
  :: UpdByKeyConstr m b t (GrecWithout (DdKey t) (Grec r))
                          (GrecWith    (DdKey t) (Grec r))
  => Proxy t -> r -> SessionMonad b m ()
updateByPKR pt = updateByPK pt . Grec

-- * DELETE

deleteByKeyText :: DelByKeyConstr m b t k
    => Proxy b -> Proxy t -> Proxy (k :: *) -> SessionMonad b m TL.Text
deleteByKeyText pb pt pk
  = return $ format "DELETE FROM {} WHERE {}"
    ( tableName pt
    , TL.intercalate ","
        $ zipWith (\n s -> format "{} = {}" (s, paramName pb n)) [0..]
        $ fieldNames' pk
    )

deleteByKeyMany :: (Traversable f,  DelByKeyConstr m b t k)
                => Proxy t -> f k -> SessionMonad b m ()
deleteByKeyMany pt (ks :: f k) = do
  (pb :: Proxy b, _) <- ask
  (cmd :: PrepCmd b) <- deleteByKeyText pb pt (Proxy :: Proxy k)
                    >>= prepareCommand
  finally (mapM_ (runPrepared cmd . convFromGrec) ks) (finalizePrepared cmd)

deleteByKey :: DelByKeyConstr m b t k => Proxy t -> k -> SessionMonad b m ()
deleteByKey pt = deleteByKeyMany pt . (:[])

deleteByPKMany  :: (Traversable f, DelByKeyConstr m b t (GrecWith (DdKey t) r))
                => Proxy t -> f r -> SessionMonad b m ()
deleteByPKMany (pt :: Proxy t) (rs :: f r)
    = deleteByKeyMany pt
    $ fmap (\r -> GW r :: GrecWith (DdKey t) r) rs

deleteByPKManyR ::  ( Traversable f
                    , DelByKeyConstr m b t (GrecWith (DdKey t) (Grec r))
                    )
                => Proxy t -> f r -> SessionMonad b m ()
deleteByPKManyR pt = deleteByPKMany pt . fmap Grec

deleteByPK  :: DelByKeyConstr m b t (GrecWith (DdKey t) r)
            => Proxy t -> r -> SessionMonad b m ()
deleteByPK pt = deleteByPKMany pt . (:[])

deleteByPKR :: DelByKeyConstr m b t (GrecWith (DdKey t) (Grec r))
            => Proxy t -> r -> SessionMonad b m ()
deleteByPKR pt = deleteByPK pt . Grec

-- * SELECT

selectText  :: SelConstr m b t r k
            => Proxy (b :: *) -> Proxy t -> Proxy (r :: *) -> Proxy (k :: *)
            -> SessionMonad b m TL.Text
selectText pb pt pr pk
  = return $ format "SELECT {} FROM {} WHERE {}"
    ( TL.intercalate "," $ fieldNamesT pr
    , tableName pt
    , TL.intercalate ","
        $ zipWith (\n s -> format "{} = {}" (s, paramName pb n))
                  [0..] $ fieldNamesT pk
    )

selectMany :: (SelConstr m b t r k, Traversable f)
            => Proxy t -> Proxy r -> f k -> SessionMonad b m (f [r])
selectMany (pt :: Proxy t) (pr :: Proxy r) (ks :: f k)
  = do
    (pb :: Proxy b, _) <- ask
    (cmd :: PrepCmd b) <- selectText pb pt pr (Proxy :: Proxy k)
                      >>= prepareCommand
    finally ( fmap (fmap convToGrec)
              <$> mapM (runSelect cmd) (fmap convFromGrec ks)
            )
            (finalizePrepared cmd)

selectManyR :: (SelConstr m b t (Grec r) k, Traversable f)
            => Proxy t -> Proxy r -> f k -> SessionMonad b m (f [r])
selectManyR pt (pr :: Proxy r)
  = fmap (fmap $ map unGrec) . selectMany pt (Proxy :: Proxy (Grec r))
