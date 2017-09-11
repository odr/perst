{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE TypeApplications #-}
module Perst.Database.DML.Update where

import           Control.Monad.Catch     (finally)
import           Data.Bifunctor          (bimap, first, second)
import qualified Data.Map                as M
import           Data.Maybe              (catMaybes)
import qualified Data.Text               as T
import           GHC.Prim                (Proxy#, proxy#)

import           Data.Type.Grec          as Grec (ConvFromGrec (..),
                                                  ConvGrecInfo (..),
                                                  GrecWith (..),
                                                  GrecWithout (..))
import           Perst.Database.DataDef  (DataDef, DataDefInfo (..), WithKey,
                                          WithoutKey, formatS)
import           Perst.Database.DbOption (DbOption (..), MonadCons,
                                          SessionMonad)

type UpdTextCons b t r k
  = (DbOption b, ConvGrecInfo r, ConvGrecInfo k, DataDefInfo t)

type UpdDiffTextCons0 b t r k = ( UpdTextCons b t r k
                               , ConvFromGrec r [FieldDB b]
                               , ConvFromGrec k [FieldDB b]
                               )

type UpdDiffTextCons b t r k = (UpdDiffTextCons0 b t r k, Eq (FieldDB b))

type UpdDiffManyCons m b t r k = (MonadCons m, UpdDiffTextCons b t r k)

type UpdManyCons m f b t r k = ( MonadCons m
                               , UpdDiffTextCons0 b t r k
                               , Traversable f
                               )

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

updateManyDef :: UpdManyCons m f b t r k
              => Proxy# b -> Proxy# t -> f (k,r)  -> SessionMonad b m ()
updateManyDef (pb :: Proxy# b) (pt :: Proxy# t) (rs :: f (k,r)) = do
  cmd <- prepareCommand @b
      $ updateTextDef pb pt (proxy# :: Proxy# r) (proxy# :: Proxy# k)
  finally (mapM_ ( runPrepared @b cmd . uncurry (++)
                 . bimap convFromGrec convFromGrec
                 ) rs)
          (finalizePrepared @b cmd)

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

updateDiffTextManyDef  :: UpdDiffTextCons b t r k
                       => Proxy# b -> Proxy# t -> [(k,r,r)]
                       -> M.Map T.Text [[FieldDB b]]
updateDiffTextManyDef pb pt
  = M.fromListWith mappend
  . map (second (:[]) . (\(k,o,n) -> updateDiffTextDef pb pt k o n))

updateDiffManyDef :: UpdDiffManyCons m b t r k
          => Proxy# b -> Proxy# t -> [(k,r,r)] -> SessionMonad b m ()
updateDiffManyDef (pb :: Proxy# b) (pt :: Proxy# t) (rs :: [(k,r,r)]) = do
  mapM_ (\(t,ps) -> do
      (cmd :: PrepCmd b) <- prepareCommand @b t
      finally (mapM_ (runPrepared @b cmd) ps)
              (finalizePrepared @b cmd)
    ) $ M.toList $ updateDiffTextManyDef pb pt rs
