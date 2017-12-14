{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE TypeApplications #-}
module Perst.Database.DML.Update where

import           Control.Monad           (when)
import           Control.Monad.Catch     (finally)
import           Data.Bifunctor          (bimap, first, second)
import qualified Data.Map                as M
import           Data.Maybe              (mapMaybe)
import qualified Data.Text               as T
import           GHC.Prim                (Proxy#, proxy#)

import           Data.Type.GrecTree      (ConsNames, ConvNames (..),
                                          Convert (..))
import           Perst.Database.DataDef  (DataDefInfo (..), formatS)
import           Perst.Database.DbOption (DbOption (..), MonadCons,
                                          SessionMonad)
import           Perst.Types             (NoLstFld)

type UpdCons b t r k =
  ( DbOption b, Convert r [FieldDB b], Convert k [FieldDB b]
  , ConsNames NoLstFld k, ConsNames NoLstFld r, DataDefInfo t
  )

type UpdDiffCons b t r k = (UpdCons b t r k, Convert r [FieldDB b], Eq (FieldDB b))

updateTextDef :: UpdCons b t r k => Proxy# '(b,t,r,k) -> T.Text
updateTextDef (_ :: Proxy# '(b,t,r,k))
  = formatS "UPDATE {} SET {} WHERE {}" (tableName @t, rs, ks)
 where
  (ks, rs)
    = bimap (interSnd " AND ") (interSnd ",")
    $ splitAt (length keyNames)
    $ zipWith (\n s -> (s, formatS "{} = {}" (s, paramName @b n))) [0..]
    $ keyNames ++ fldNames @NoLstFld @r
   where
    keyNames = fldNames @NoLstFld @k
    interSnd s = T.intercalate s . map snd

updateManyDef :: (UpdCons b t r k, MonadCons m, Traversable f)
              => Proxy# '(b,t) -> f (k,r)  -> SessionMonad b m ()
updateManyDef (_ :: Proxy# '(b,t)) (rs :: f (k,r))
  | null rs = return ()
  | otherwise = do
    cmd <- prepareCommand @b $ updateTextDef (proxy# :: Proxy# '(b,t,r,k))
    finally (mapM_ ( runPrepared @b cmd
                   . uncurry (++)
                   . bimap convert convert
                   ) rs)
            (finalizePrepared @b cmd)

updateDiffTextDef :: UpdDiffCons b t r k
                  => Proxy# '(b,t) -> k -> r -> r -> Maybe (T.Text, [FieldDB b])
updateDiffTextDef (_ :: Proxy# '(b,t)) (k :: k) old (new :: r)
  | null vrs  = Nothing
  | otherwise = Just (formatS "UPDATE {} SET {} WHERE {}" (tableName @t, rs, ks), vrs ++ vks)
 where
  old' = convert old :: [FieldDB b]
  new' = convert new
  fns  = fldNames @NoLstFld @r
  kns  = fldNames @NoLstFld @k
  k' = convert k
  (rs,vrs) = first (T.intercalate ",")
          $ unzip
          $ zipWith (\num (_, n, fn) -> (formatS "{} = {}" (fn, paramName @b num), n)) [0..]
          $ filter (\(o, n, _) -> o /= n)
          $ zip3 old' new' fns
  (ks,vks)
      = first (T.intercalate " AND ")
      $ unzip
      $ zipWith3 (\vk fn num -> (formatS "{} = {}" (fn, paramName @b num), vk))
                k' kns [length vrs..]

updateDiffTextManyDef :: UpdDiffCons b t r k
                    => Proxy# '(b,t) -> [(k,r,r)] -> M.Map T.Text [[FieldDB b]]
updateDiffTextManyDef pbt
  = M.fromListWith mappend
  . mapMaybe (fmap (second (:[])) . (\(k,o,n) -> updateDiffTextDef pbt k o n))

updateDiffManyDef :: (UpdDiffCons b t r k, MonadCons m)
                  => Proxy# '(b,t) -> [(k,r,r)] -> SessionMonad b m ()
updateDiffManyDef (pbt :: Proxy# '(b,t)) (rs :: [(k,r,r)]) = do
  mapM_ (\(t,ps) -> when (not $ null ps) $ do
      (cmd :: PrepCmd b) <- prepareCommand @b t
      finally (mapM_ (runPrepared @b cmd) ps) (finalizePrepared @b cmd)
    ) $ M.toList $ updateDiffTextManyDef pbt rs
