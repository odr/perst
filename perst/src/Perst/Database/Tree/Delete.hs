{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}
module Perst.Database.Tree.Delete where

import           Control.Applicative     (ZipList (..))
import           Data.Bifunctor          (second)
import           Data.Functor.Compose    (Compose (..))
import           Data.Singletons.Prelude (SingI)
import           Data.Tagged             (Tagged (..))
import           GHC.Prim                (Proxy#, proxy#)
import           Lens.Micro.Extras       (view)

import           Data.Type.Grec          (FieldNamesConvGrec, Fsts, Grec (..),
                                          GrecLens (..), GrecWith (..), IsSub,
                                          NamesGrecLens (..), Snds)
import           Perst.Database.DbOption (MonadCons, SessionMonad)
import           Perst.Database.DML      (DML (..), RecCons)
import           Perst.Database.TreeDef  (AppCons, FieldByName, GrecChilds,
                                          RecParent, TdData, TopPK, TreeDef)

type DelTreeCons b t k r =
  ( DML b (TdData t) r
  , RecCons b (TopPK t (k,Grec r))
  , DeleteChilds b (GrecChilds t (k, Grec r)) k r
  )

deleteTreeManyDef :: (MonadCons m , AppCons f, DelTreeCons b t k r)
                  => Proxy# '(b,t) -> f (k,r) -> SessionMonad b m ()
deleteTreeManyDef (_ :: Proxy# '(b,t)) (rs :: f (k,r)) = do
  deleteChilds @b @(GrecChilds t (k, Grec r)) rs
  deleteMany @b @(TdData t) @r
          $ fmap ( (GW . second Grec :: (k,r) -> TopPK t (k,Grec r)) ) rs

class DeleteChilds b chs k r where
  deleteChilds  :: (MonadCons m , AppCons f) => f (k,r) -> SessionMonad b m ()

instance DeleteChilds b '[] k r where
  deleteChilds _ = return ()

type DelChildCons b s td rs chs k r =
  ( DelTreeCons b td (Tagged (Fsts rs) (RecParent k (Grec r) rs))
                     (FieldByName s (k, Grec r))
  , GrecLens s [FieldByName s (k, Grec r)] (k, Grec r)
  , DeleteChilds b chs k r
  , NamesGrecLens (Snds rs) (RecParent k (Grec r) rs) (k, Grec r)
  )

instance  DelChildCons b s td rs chs k r
          => DeleteChilds b ('(s,'(td,rs)) ': chs) k r where
  deleteChilds rs = do
    deleteTreeManyDef(proxy# :: Proxy# '(b,td)) $ Compose $ delRec <$> rs
    deleteChilds @b @chs rs
   where
    delRec :: (k,r) -> ZipList ( Tagged (Fsts rs) (RecParent k (Grec r) rs)
                               , FieldByName s (k, Grec r)
                               )
    delRec (k,r) = (Tagged (namesGrecGet @(Snds rs) (k, Grec r)),)
                 <$> ZipList (view (grecLens @s) (k, Grec r))
