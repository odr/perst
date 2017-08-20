{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}
module Perst.Database.DML.Update where

import           Control.Monad.Catch     (finally)
import           Data.Bifunctor          (bimap, first, second)
import qualified Data.Map                as M
import           Data.Maybe              (catMaybes)
-- import           Data.Proxy              (Proxy (..))
-- import           Data.Singletons.Prelude (Sing, SingI (..))
import qualified Data.Text               as T
import           GHC.Prim                (Proxy#, proxy#)

import           Data.Type.Grec          as Grec (ConvFromGrec (..),
                                                  ConvGrecInfo (..), Grec (..),
                                                  GrecWith (..),
                                                  GrecWithout (..))
import           Perst.Database.DataDef  (DataDef, DataDefInfo (..), WithKey,
                                          WithoutKey, formatS)
import           Perst.Database.DbOption (DbOption (..), MonadCons,
                                          SessionMonad)

--
-- * UPDATE

type UpdTextCons b t r k
  = (DbOption b, ConvGrecInfo r, ConvGrecInfo k, DataDefInfo t)

type UpdCons m b t r k
  = ( MonadCons m, UpdTextCons b t r k
    , ConvFromGrec k [FieldDB b], ConvFromGrec r [FieldDB b]
    )

type UpdManyCons m f b t r k = ( UpdCons m b t r k, Traversable f)

--
-- class UpdateByKey b t r k where
--
--   updateByKeyMany :: (MonadCons m, Traversable f)
--                   => f (k,r)  -> SessionMonad b m ()
--   default updateByKeyMany :: UpdManyCons m f b t r k
--                   => f (k,r)  -> SessionMonad b m ()
--   updateByKeyMany = updateByKeyManyDef (proxy# :: Proxy# b) (proxy# :: Proxy# t)
--
--   updateByKey :: MonadCons m => (k, r) -> SessionMonad b m ()
--   updateByKey = updateByKeyMany @b @t . (:[])

updateTextDef :: UpdTextCons b t r k
                => Proxy# b -> Proxy# t -> Proxy# r -> Proxy# k
                -> T.Text
updateTextDef (_ :: Proxy# b) (_ :: Proxy# t) (_ :: Proxy# r) (_ :: Proxy# k)
  = formatS "UPDATE {} SET {} WHERE {}" (tableName @t, rs, ks)
 where
  (ks, rs)
    = bimap (interSnd " AND ") (interSnd ",")
    $ splitAt (length keyNames)
    $ zipWith (\n s -> (s, formatS "{} = {}" (s, paramName @b n))) [0..]
    $ keyNames ++ Grec.fieldNames @r
   where
    keyNames = Grec.fieldNames @k
    interSnd s = T.intercalate s . map snd

--
updateManyDef :: UpdManyCons m f b t r k
              => Proxy# b -> Proxy# t -> f (k,r)  -> SessionMonad b m ()
updateManyDef (pb :: Proxy# b) (pt :: Proxy# t) (rs :: f (k,r)) = do
  cmd <- prepareCommand @b
      $ updateTextDef pb pt (proxy# :: Proxy# r) (proxy# :: Proxy# k)
  finally (mapM_ ( runPrepared @b cmd . uncurry (++)
                 . bimap convFromGrec convFromGrec
                 ) rs)
          (finalizePrepared @b cmd)

-- updateByKeyManySafe :: (UpdByKeyConstr m b t r k, Traversable f)
--                     => Sing t -> f (k,r)  -> SessionMonad b m ()
-- updateByKeyManySafe = updateByKeyMany

type UpdDiffTextCons b t r k = ( UpdTextCons b t r k, ConvFromGrec r [FieldDB b]
                               , ConvFromGrec k [FieldDB b], Eq (FieldDB b)
                               )

type UpdDiffManyCons m b t r k = (MonadCons m, UpdDiffTextCons b t r k)

-- class UpdateByKeyDiff b t r k where
--   updateByKeyDiffMany :: MonadCons m => [(k,r,r)] -> SessionMonad b m ()
--   default updateByKeyDiffMany :: UpdDiffManyCons m b t r k
--                               => [(k,r,r)] -> SessionMonad b m ()
--   updateByKeyDiffMany = updateByKeyDiffManyDef (proxy# :: Proxy# b)
--                                                (proxy# :: Proxy# t)

updateDiffTextDef :: UpdDiffTextCons b t r k
                  => Proxy# b -> Proxy# t -> k -> r -> r
                  -> (T.Text, [FieldDB b])
updateDiffTextDef (_ :: Proxy# b) (_ :: Proxy# t) (k :: k) old (new :: r) =
  (formatS "UPDATE {} SET {} WHERE {}" (tableName @t, rs, ks), vrs++vks)
 where
  old' = convFromGrec old :: [FieldDB b]
  new' = convFromGrec new
  fns  = Grec.fieldNames @r
  kns  = Grec.fieldNames @k
  k' = convFromGrec k
  (rs,vrs) = first (T.intercalate ",")
          $ unzip
          $ catMaybes
          $ zipWith (\(o, n, fn) num ->
                      if o == n
                        then Nothing
                        else Just (formatS "{} = {}" (fn, paramName @b num), n)
                  ) (zip3 old' new' fns) [0..]
  (ks,vks)
      = first (T.intercalate " AND ")
      $ unzip
      $ zipWith3 (\vk fn num -> (formatS "{} = {}" (fn, paramName @b num), vk))
                k' kns [length vrs..]

--
updateDiffTextManyDef  :: UpdDiffTextCons b t r k
                       => Proxy# b -> Proxy# t -> [(k,r,r)]
                       -> M.Map T.Text [[FieldDB b]]
updateDiffTextManyDef pb pt
  = M.fromListWith mappend
  . map (second (:[]) . (\(k,o,n) -> updateDiffTextDef pb pt k o n))

--
updateDiffManyDef :: UpdDiffManyCons m b t r k
          => Proxy# b -> Proxy# t -> [(k,r,r)] -> SessionMonad b m ()
updateDiffManyDef (pb :: Proxy# b) (pt :: Proxy# t) (rs :: [(k,r,r)]) = do
  mapM_ (\(t,ps) -> do
      (cmd :: PrepCmd b) <- prepareCommand @b t
      finally (mapM_ (runPrepared @b cmd) ps)
              (finalizePrepared @b cmd)
    ) $ M.toList $ updateDiffTextManyDef pb pt rs

-- updateByKeyDiffManySafe :: (UpdByKeyDiffConstr m b t r k)
--                 => Sing t -> [(k,r,r)] -> SessionMonad b m ()
-- updateByKeyDiffManySafe = updateByKeyDiffMany

-- class UpdateByKey b t (Grec r) k => UpdateByKeyR b t r k where
--   updateByKeyManyR  :: (MonadCons m, Traversable f)
--                     => f (k,r) -> SessionMonad b m ()
--   updateByKeyManyR = updateByKeyMany @b @t . fmap (fmap Grec)
--
--   updateByKeyR :: MonadCons m => (k,r) -> SessionMonad b m ()
--   updateByKeyR = updateByKey @b @t . fmap Grec
--
-- instance UpdateByKey b t (Grec r) k => UpdateByKeyR b t r k
-- --
-- class UpdateByKey b t (WithoutKey t r) (WithKey t r) => UpdateByPK b t r where
--   updateByPKMany :: (MonadCons m, Traversable f) => f r -> SessionMonad b m ()
--   updateByPKMany
--       = updateByKeyMany @b @t
--       . fmap (\r -> (GW r, GWO r) :: (WithKey t r, WithoutKey t r))
--
--   updateByPK  :: MonadCons m => r -> SessionMonad b m ()
--   updateByPK = updateByPKMany @b @t . (:[])
--
-- instance UpdateByKey b t (WithoutKey t r) (WithKey t r) => UpdateByPK b t r
--
-- class UpdateByKeyDiff b t (WithoutKey t r) (WithKey t r)
--     => UpdateByPKDiff b t r where
--   updateByPKDiffMany :: MonadCons m => [(r,r)] -> SessionMonad b m ()
--   updateByPKDiffMany (rs :: [(r,r)])
--       = updateByKeyDiffMany @b @t
--       $ fmap (\(o,n) -> (GW  o, GWO o, GWO n)
--                      :: (WithKey t r, WithoutKey t r, WithoutKey t r)
--              ) rs
--
-- instance UpdateByKeyDiff b t (WithoutKey t r) (WithKey t r)
--         => UpdateByPKDiff b t r
--
-- class UpdateByPK b t (Grec r) => UpdateByPKR b t r where
--   updateByPKManyR :: (MonadCons m, Traversable f) => f r -> SessionMonad b m ()
--   updateByPKManyR = updateByPKMany @b @t . fmap Grec
--
--   updateByPKR :: MonadCons m => r -> SessionMonad b m ()
--   updateByPKR = updateByPK @b @t . Grec
--
-- instance UpdateByPK b t (Grec r) => UpdateByPKR b t r
