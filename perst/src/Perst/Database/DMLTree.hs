{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
module Perst.Database.DMLTree where

import           Control.Applicative        (ZipList (..))
import           GHC.Prim                   (Proxy#, proxy#)

import           Data.Type.Grec             (Grec (..))
import           Perst.Database.Condition   (Condition)
import           Perst.Database.DbOption    (MonadCons, SessionMonad)
import           Perst.Database.Tree.Delete (DelTreeCons, deleteTreeManyDef)
import           Perst.Database.Tree.Insert (InsTreeCons, insertTreeManyDef)
import           Perst.Database.Tree.Select (SelTreeCond, SelTreeCons,
                                             selectTreeCondDef,
                                             selectTreeManyDef)
import           Perst.Database.Tree.Update (UpdTreeCons, updateTreeManyDef)


class UpdTreeCons b t () r => DMLTree b t r where
  selectTreeMany :: (MonadCons m, SelTreeCons b t k r)
                 => [k] -> SessionMonad b m [[r]]
  selectTreeMany = fmap getZipList
                 . selectTreeManyDef (proxy# :: Proxy# b) (proxy# :: Proxy# t)
                                     (proxy# :: Proxy# r)
                 . ZipList

  selectTreeCond :: (MonadCons m, SelTreeCond b t r)
                 => Condition t (Grec r) -> SessionMonad b m [r]
  selectTreeCond = selectTreeCondDef (proxy# :: Proxy# b)

  insertTreeMany :: MonadCons m => [r] -> SessionMonad b m [r]
  insertTreeMany = fmap (getZipList . fmap snd)
                 . insertTreeManyDef (proxy# :: Proxy# b) (proxy# :: Proxy# t)
                 . ZipList . map ((),)

  deleteTreeMany :: MonadCons m => [r] -> SessionMonad b m ()
  deleteTreeMany = deleteTreeManyDef (proxy# :: Proxy# b) (proxy# :: Proxy# t)
                 . ZipList . map ((),)

  updateTreeMany :: MonadCons m => [r] -> [r] -> SessionMonad b m ()
  updateTreeMany olds news
    = updateTreeManyDef (proxy# :: Proxy# b) (proxy# :: Proxy# t)
                        (map ((),) olds) (map ((),) news)
