{-# LANGUAGE TypeInType #-}
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
  -- * Diff UPDATE
  , updateByKeyDiffMany, updateByPKDiffMany
  -- , updateByKeyDiffMany, updateByKeyDiffManyR
  -- , updateByPKDiffMany, updateByPKDiffManyR

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
import           Data.Bifunctor             (bimap, first, second)
import           Data.Kind
import           Data.List                  (intercalate, (\\))
import qualified Data.Map                   as M
import           Data.Maybe                 (catMaybes)
import           Data.Proxy                 (Proxy (..))
import           Data.Singletons.Prelude
import           Data.Text.Format           (format)
import qualified Data.Text.Lazy             as TL
import           Data.Traversable           (Traversable (..))
import           Data.Type.Grec             (ConvFromGrec (..), ConvToGrec (..),
                                             Grec (..), GrecF, GrecWith (..),
                                             GrecWithout (..), grec)
import           Perst.Database.Constraints (DelByKeyConstr, DelConstr,
                                             InsConstr, RecConstr, SelConstr,
                                             UpdByKeyConstr, UpdByKeyDiffConstr,
                                             UpdConstr)
import           Perst.Database.DataDef     (DataDef, DdAutoIns, DdKey, WithKey,
                                             WithoutKey, autoIns, fieldNames',
                                             fieldNamesT, primaryKey, sDdKey,
                                             showProxy, tableName)
import           Perst.Database.DbOption    (DbOption (..), DbOptionConstr,
                                             SessionMonad)

insertText :: InsConstr m b t r
            => Proxy b -> Sing t -> Proxy r -> SessionMonad b m TL.Text
insertText pb (pt :: Sing t) pr
  = return $ format "INSERT INTO {}({}) VALUES({})"
  ( tableName pt
  , TL.intercalate "," fns
  , TL.intercalate "," $ zipWith (const $ paramName pb) fns [0..]
  )
 where
  -- autoIns = showProxy (Proxy :: Proxy (DdAutoIns t))
  fns = map TL.pack (fieldNames' pr)
      \\ if autoIns pt then map TL.pack (primaryKey pt) else []

insertMany  :: (Traversable f, InsConstr m b t r)
            => Sing (t :: DataDef) -> f r -> SessionMonad b m (Maybe (f (GenKey b)))
insertMany (pt :: Sing t) (rs :: f r) = do
  (pb :: Proxy b, _) <- ask
  when ai preRunInAuto
  (cmd :: PrepCmd b) <- insertText pb pt pr >>= prepareCommand
  finally (fmap sequenceA
              $ mapM (\r -> do
                      runPrepared cmd r
                      if ai
                        then Just <$> getLastKey
                        else return Nothing
                  ) $ fmap (
                        if ai
                          then convFromGrec . (GWO :: r -> GrecWithout (DdKey t) r)
                          else convFromGrec
                      ) rs
          )
          (finalizePrepared cmd)
 where
  pr = Proxy :: Proxy r
  ai = autoIns pt
  -- autoIns = showProxy (Proxy :: Proxy (DdAutoIns t))

insertManyR  :: (Traversable f, InsConstr m b t (GrecF r))
            => Sing t -> f r -> SessionMonad b m (Maybe (f (GenKey b)))
insertManyR pt = insertMany pt . fmap grec

insert  :: InsConstr m b t r
        => Sing t -> r -> SessionMonad b m (Maybe (GenKey b))
insert pt r = fmap head <$> insertMany pt [r]

insertR :: InsConstr m b t (GrecF r)
        => Sing t -> r -> SessionMonad b m (Maybe (GenKey b))
insertR pt = insert pt . grec

-- * UPDATE

updateByKeyText :: UpdByKeyConstr m b t r k
    => Proxy b -> Sing t -> Proxy r -> Proxy k -> SessionMonad b m TL.Text
updateByKeyText pb pt pr pk
  = return $ format "UPDATE {} SET {} WHERE {}" (tableName pt, rs, ks)
 where
  (ks, rs)
    = bimap (interSnd " AND ") (interSnd ",")
    $ splitAt (length keyNames)
    $ zipWith (\n s -> (s, format "{} = {}" (s, paramName pb n))) [0..]
    $ keyNames ++ fieldNames' pr
   where
    keyNames = fieldNames' pk
    interSnd s = TL.intercalate s . map snd

--
updateByKeyDiffText :: UpdByKeyDiffConstr m b t r k
                    => Proxy b -> Sing t -> k -> r -> r
                    -> SessionMonad b m (TL.Text, [FieldDB b])
updateByKeyDiffText (pb :: Proxy b) pt (k :: k) old (new :: r)
  = return (format "UPDATE {} SET {} WHERE {}" (tableName pt, rs, ks), vrs++vks)
 where
  old' = convFromGrec old :: [FieldDB b]
  new' = convFromGrec new
  fns  = fieldNames' (Proxy :: Proxy r)
  kns  = fieldNames' (Proxy :: Proxy k)
  k' = convFromGrec k
  (rs,vrs) = first (TL.intercalate ",")
          $ unzip
          $ catMaybes
          $ zipWith (\(o, n, fn) num ->
                      if o == n
                        then Nothing
                        else Just (format "{} = {}" (fn, paramName pb num), n)
                  ) (zip3 old' new' fns) [0..]
  (ks,vks)
      = first (TL.intercalate " AND ")
      $ unzip
      $ zipWith3 (\vk fn num -> (format "{} = {}" (fn, paramName pb num), vk))
                k' kns [length vrs..]

--
updateByKeyDiffTextMany :: (UpdByKeyDiffConstr m b t r k)
                        => Proxy b -> Sing t -> [(k,r,r)]
                        -> SessionMonad b m (M.Map TL.Text [[FieldDB b]])
updateByKeyDiffTextMany pb pt
  = fmap (M.fromListWith mappend)
  . mapM (fmap (second (:[])) . (\(k,o,n) -> updateByKeyDiffText pb pt k o n))

--
updateByKeyMany :: (UpdByKeyConstr m b t r k, Traversable f)
                => Sing t -> f (k,r)  -> SessionMonad b m ()
updateByKeyMany (pt :: Sing t) (rs :: f (k,r)) = do
  (pb :: Proxy b, _) <- ask
  (cmd :: PrepCmd b) <- updateByKeyText pb pt pr pk
                    >>= prepareCommand
  finally (mapM_ ( runPrepared cmd . uncurry (++)
                 . bimap convFromGrec convFromGrec
                 ) rs)
          (finalizePrepared cmd)
 where
  (pr,pk) = (Proxy :: Proxy r, Proxy :: Proxy k)

--
updateByKeyDiffMany :: (UpdByKeyDiffConstr m b t r k)
                => Sing t -> [(k,r,r)] -> SessionMonad b m ()
updateByKeyDiffMany (pt :: Sing t) (rs :: [(k,r,r)]) = do
  (pb :: Proxy b, _) <- ask
  mp <- M.toList <$> updateByKeyDiffTextMany pb pt rs
  mapM_ (\(t,ps) -> do
      (cmd :: PrepCmd b) <- prepareCommand t
      finally (mapM_ (runPrepared cmd) ps)
              (finalizePrepared cmd)

    ) mp


updateByKeyManyR  :: (UpdByKeyConstr m b t (GrecF r) k, Traversable f)
                  => Sing t -> f (k,r) -> SessionMonad b m ()
updateByKeyManyR pt = updateByKeyMany pt . fmap (fmap grec)

updateByKey :: UpdByKeyConstr m b t r k
            => Sing t -> (k, r) -> SessionMonad b m ()
updateByKey pt = updateByKeyMany pt . (:[])

updateByKeyR  :: UpdByKeyConstr m b t (GrecF r) k
              => Sing t -> (k,r) -> SessionMonad b m ()
updateByKeyR pt = updateByKey pt . fmap grec

updateByPKMany  ::  ( UpdByKeyConstr m b t (WithoutKey t r) (WithKey t r)
                    , Traversable f
                    )
                => Sing t -> f r -> SessionMonad b m ()
updateByPKMany (pt :: Sing t) (rs :: f r)
    = updateByKeyMany pt
    $ fmap (\r -> (GW r, GWO r) :: (WithKey t r, WithoutKey t r)) rs
--
updateByPKDiffMany :: (UpdByKeyDiffConstr m b t (WithoutKey t r) (WithKey t r))
                    => Sing t -> [(r,r)] -> SessionMonad b m ()
updateByPKDiffMany (pt :: Sing t) (rs :: [(r,r)])
    = updateByKeyDiffMany pt
    $ fmap (\(o,n) -> (GW  o, GWO o, GWO n)
                   :: (WithKey t r, WithoutKey t r, WithoutKey t r)
           ) rs

updateByPKManyR :: ( UpdByKeyConstr m b t (WithoutKey t (GrecF r))
                                          (WithKey t (GrecF r))
                   , Traversable f
                   )
                   => Sing t -> f r -> SessionMonad b m ()
updateByPKManyR pt = updateByPKMany pt . fmap grec

updateByPK  :: UpdByKeyConstr m b t (WithoutKey t r) (WithKey t r)
            => Sing t -> r -> SessionMonad b m ()
updateByPK pt = updateByPKMany pt . (:[])

updateByPKR :: UpdByKeyConstr m b t (WithoutKey t (GrecF r)) (WithKey t (GrecF r))
            => Sing t -> r -> SessionMonad b m ()
updateByPKR pt = updateByPK pt . grec

-- * DELETE

deleteByKeyText :: DelByKeyConstr m b t k
    => Proxy b -> Sing t -> Proxy (k :: *) -> SessionMonad b m TL.Text
deleteByKeyText pb pt pk
  = return $ format "DELETE FROM {} WHERE {}"
    ( tableName pt
    , TL.intercalate " AND "
        $ zipWith (\n s -> format "{} = {}" (s, paramName pb n)) [0..]
        $ fieldNames' pk
    )

deleteByKeyMany :: (Traversable f,  DelByKeyConstr m b t k)
                => Sing t -> f k -> SessionMonad b m ()
deleteByKeyMany pt (ks :: f k) = do
  (pb :: Proxy b, _) <- ask
  (cmd :: PrepCmd b) <- deleteByKeyText pb pt (Proxy :: Proxy k)
                    >>= prepareCommand
  finally (mapM_ (runPrepared cmd . convFromGrec) ks) (finalizePrepared cmd)

deleteByKey :: DelByKeyConstr m b t k => Sing t -> k -> SessionMonad b m ()
deleteByKey pt = deleteByKeyMany pt . (:[])

deleteByPKMany  :: (Traversable f, DelByKeyConstr m b t (GrecWith (DdKey t) r))
                => Sing t -> f r -> SessionMonad b m ()
deleteByPKMany (pt :: Sing t) (rs :: f r)
    = deleteByKeyMany pt
    $ fmap (\r -> GW r :: GrecWith (DdKey t) r) rs

deleteByPKManyR ::  ( Traversable f
                    , DelByKeyConstr m b t (GrecWith (DdKey t) (GrecF r))
                    )
                    => Sing t -> f r -> SessionMonad b m ()
deleteByPKManyR pt = deleteByPKMany pt . fmap grec

deleteByPK  :: DelByKeyConstr m b t (GrecWith (DdKey t) r)
            => Sing t -> r -> SessionMonad b m ()
deleteByPK pt = deleteByPKMany pt . (:[])

deleteByPKR :: DelByKeyConstr m b t (GrecWith (DdKey t) (GrecF r))
            => Sing t -> r -> SessionMonad b m ()
deleteByPKR pt = deleteByPK pt . grec

-- * SELECT

selectText  :: SelConstr m b t r k
            => Proxy (b :: *) -> Sing t -> Proxy (r :: *) -> Proxy (k :: *)
            -> SessionMonad b m TL.Text
selectText pb pt pr pk
  = return $ format "SELECT {} FROM {} WHERE {}"
    ( TL.intercalate "," $ fieldNamesT pr
    , tableName pt
    , TL.intercalate " AND "
        $ zipWith (\n s -> format "{} = {}" (s, paramName pb n))
                  [0..] $ fieldNamesT pk
    )

selectMany :: (SelConstr m b t r k, Traversable f)
            => Sing t -> Proxy r -> f k -> SessionMonad b m (f [r])
selectMany (pt :: Sing t) (pr :: Proxy r) (ks :: f k)
  = do
    (pb :: Proxy b, _) <- ask
    (cmd :: PrepCmd b) <- selectText pb pt pr (Proxy :: Proxy k)
                      >>= prepareCommand
    finally ( fmap (fmap convToGrec)
              <$> mapM (runSelect cmd) (fmap convFromGrec ks)
            )
            (finalizePrepared cmd)

selectManyR :: (SelConstr m b t (GrecF r) k, Traversable f)
            => Sing t -> Proxy r -> f k -> SessionMonad b m (f [r])
selectManyR pt (pr :: Proxy r)
  = fmap (fmap $ map unGrec) . selectMany pt (Proxy :: Proxy (GrecF r))
