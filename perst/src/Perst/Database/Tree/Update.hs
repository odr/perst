{-# LANGUAGE ExistentialQuantification #-}
-- {-# LANGUAGE RankNTypes                #-}
-- {-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE UndecidableInstances      #-}

module Perst.Database.Tree.Update where

import           Control.Applicative        (ZipList (..))
import           Data.Bifunctor             (first)
import           Data.Maybe                 (fromJust, isJust, isNothing)
import           Data.Proxy                 (Proxy (..))

import           Data.Type.Grec             (Grec (..), GrecWith (..))

import           Perst.Database.Constraints
import           Perst.Database.DataDef
import           Perst.Database.DbOption    (SessionMonad)
import           Perst.Database.DML
import           Perst.Database.Tree.Def
import           Perst.Database.Tree.Insert
import           Perst.Database.Tree.Select



type UpdateTreeConstraint m f b t r =
  ( Applicative f, Traversable f, Monoid (f (Maybe r, r))
  , UpdConstr m b (TdData t) r (GrecWith (DdKey (TdData t)) r)
  , SelConstr m b (TdData t) r (GrecWith (DdKey (TdData t)) r)
  , InsertTreeConstraint m f b t r
  -- , UpdateChilds m f b (DdAutoIns (TdData t))
  --               (DdKey (TdData t)) (GrecChilds t r) r
  )

updateTreeManyR :: UpdateTreeConstraint m ZipList b t (Grec r)
                => Proxy (t :: TreeDef) -> [r] -> SessionMonad b m ()
updateTreeManyR pt = updateTreeMany pt . fmap Grec

updateTreeMany  :: UpdateTreeConstraint m ZipList b t r
                => Proxy (t :: TreeDef) -> [r] -> SessionMonad b m ()
updateTreeMany pt = updateTreeMany' pt . ZipList

updateTreeMany' :: UpdateTreeConstraint m f b t r
                => Proxy (t :: TreeDef) -> f r -> SessionMonad b m ()
updateTreeMany' (pt :: Proxy t) (rs :: f r) = do
  rs' <- selectMany ptd (Proxy :: Proxy r) (fmap getPk rs)

  let crs = classify <$> rs <*> rs'
  insertTreeMany' pt $ snd <$> filter' (isNothing . fst) crs
  onlyUpdateTree pt $ first fromJust <$> filter' (isJust    . fst) crs
 where
  ptd = Proxy :: Proxy (TdData t)
  getPk = GW :: r -> GrecWith (DdKey (TdData t)) r
  classify r []   = (Nothing , r)
  classify r [r'] = (Just r', r)
  filter' :: Monoid (f a) => (a -> Bool) -> f a -> f a
  filter' p = foldMap (\a -> if p a then pure a else mempty)

onlyUpdateTree :: Proxy (t :: TreeDef) -> f (r, r) -> SessionMonad b m ()
onlyUpdateTree = undefined
