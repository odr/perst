{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}
module Perst.Database.Tree.Delete where

import           Control.Applicative     (ZipList (..))
import           Data.Functor.Compose    (Compose (..))
import           Data.Type.Grec          (Grec (..), GrecLens (..),
                                          GrecWith (..))
import           Lens.Micro.Extras       (view)

-- import           Perst.Database.Constraints (DelByKeyConstr)
import           Perst.Database.DataDef  (DdKey)
import           Perst.Database.DbOption (MonadCons, SessionMonad)
import           Perst.Database.DML      (DeleteByKey (..))
import           Perst.Database.Tree.Def (AppCons, FieldByName, GrecChilds,
                                          TdData, TopPK, TreeDef)

type DeleteTreeConstraint b t r =
  ( DeleteByKey b (TdData t) (TopPK t r)
  , DeleteChilds b (GrecChilds t r) r
  )

class DeleteTree b t r where
  deleteTreeManyF :: (MonadCons m, AppCons f) => f r -> SessionMonad b m ()
  default deleteTreeManyF :: (MonadCons m, AppCons f, DeleteTreeConstraint b t r)
                          => f r -> SessionMonad b m ()
  deleteTreeManyF s = do
    deleteChilds @b @(GrecChilds t r) s
    deleteByKeyMany @b @(TdData t) $ fmap (GW :: r -> TopPK t r) s

  deleteTreeMany  :: MonadCons m => [r] -> SessionMonad b m ()
  deleteTreeMany = deleteTreeManyF @b @t . ZipList

class DeleteChilds b chs r where
  deleteChilds  :: (MonadCons m , AppCons f) => f r -> SessionMonad b m ()

instance DeleteChilds b '[] r where
  deleteChilds _ = return ()

type DelChildCons b s td rs chs r
  = DelChildCons' b s td rs chs r (FieldByName s r)
type DelChildCons' b s td rs chs r fld =
  ( DeleteTree b td (Grec fld)
  , GrecLens s [fld] r
  , DeleteChilds b chs r
  )

instance  DelChildCons b s td rs chs r
          => DeleteChilds b ('(s,'(td,rs)) ': chs) r where
  deleteChilds (rs :: f r) = do
    deleteTreeManyF @b @td rc
    deleteChilds @b @chs rs
   where
    rc :: Compose f ZipList (Grec (FieldByName s r))
    rc = Compose $ (ZipList . fmap Grec . view (grecLens @s)) <$> rs

class DeleteTree b t (Grec r) => DeleteTreeR b t r where
  deleteTreeManyR :: MonadCons m => [r] -> SessionMonad b m ()
  deleteTreeManyR = deleteTreeMany @b @t . fmap Grec
