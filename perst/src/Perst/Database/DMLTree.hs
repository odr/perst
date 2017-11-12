{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}
module Perst.Database.DMLTree where

import           Control.Applicative        (ZipList (..))
import           GHC.Prim                   (Proxy#, proxy#)

-- import           Data.Type.Grec             (Grec (..))
import           Perst.Database.Condition   (Condition)
import           Perst.Database.DbOption    (MonadCons, SessionMonad)
-- import           Perst.Database.DML         (DML)
import           Perst.Database.Tree.Delete (DelTreeCons, deleteTreeManyDef)
import           Perst.Database.Tree.Insert (InsTreeCons, insertTreeManyDef)
import           Perst.Database.Tree.Select (SelTreeCond, SelTreeCons,
                                             selectTreeCondDef,
                                             selectTreeManyDef)
import           Perst.Database.Tree.Update (UpdTreeCons, updateTreeManyDef)
import           Perst.Database.TreeDef     (TreeDef' (..))


class UpdTreeCons b t () r => DMLTree b t r where
  selectTreeMany :: (MonadCons m, SelTreeCons b t k r)
                 => [k] -> SessionMonad b m [[r]]
  selectTreeMany = fmap getZipList
                 . selectTreeManyDef (proxy# :: Proxy# '(b,t,r))
                 . ZipList

  selectTreeCond :: (MonadCons m, SelTreeCond b t r)
      => Condition t r -> SessionMonad b m [r]
  selectTreeCond = selectTreeCondDef (proxy# :: Proxy# b)

  insertTreeMany :: MonadCons m => [r] -> SessionMonad b m [r]
  insertTreeMany = fmap (getZipList . fmap snd)
                 . insertTreeManyDef (proxy# :: Proxy# '(b,t))
                 . ZipList . map ((),)

  deleteTreeMany :: MonadCons m => [r] -> SessionMonad b m ()
  deleteTreeMany = deleteTreeManyDef (proxy# :: Proxy# '(b,t))
                 . ZipList . map ((),)

  updateTreeMany :: MonadCons m => [r] -> [r] -> SessionMonad b m ()
  updateTreeMany olds news = updateTreeManyDef (proxy# :: Proxy# '(b,t))
                                               (map ((),) olds) (map ((),) news)

-- instance DML b t r => DMLTree b (TreeDefC t '[]) r where
--   selectTreeMany = fmap (fmap (map snd)) . selectMany (proxy# :: Proxy# ())
  -- selectTreeCond =

-- instance UpdTreeCons b (TreeDefC t '[]) () r => DMLTree b (TreeDefC t '[]) r
