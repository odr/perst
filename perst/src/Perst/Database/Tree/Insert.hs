{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}
module Perst.Database.Tree.Insert where

import           Control.Applicative     (ZipList (..), liftA2, liftA3)
import           Data.Bifunctor          (bimap, first, second)
import           Data.Functor.Compose    (Compose (..))
import           Data.Maybe              (fromMaybe)
import           Data.Singletons.Prelude (SingI)
import           Data.Tagged             (Tagged (..))
import           GHC.Prim                (Proxy#, proxy#)
import           Lens.Micro              ((&), (.~))
import           Lens.Micro.Extras       (view)

import           Data.Type.Grec          (FieldNamesConvGrec, Fsts, Grec (..),
                                          GrecLens (..), NamesGrecLens (..),
                                          Snds)
import           Perst.Database.DataDef  (DataAutoIns)
import           Perst.Database.DbOption (GenKey, MonadCons, SessionMonad)
import           Perst.Database.DML      (DML (..), RecCons)
import           Perst.Database.TreeDef  (AppCons, FieldByName, GrecChilds,
                                          RecParent, TdData, TopKey, TreeDef)

type InsTreeCons b t k r =
  ( DML b (TdData t) r
  , RecCons b k
  , SingI (FieldNamesConvGrec k)
  , InsertChilds b (DataAutoIns (TdData t))
                  (GrecChilds t (k, Grec r)) (TopKey t) k r
  )

insertTreeManyDef :: (MonadCons m , AppCons f, InsTreeCons b t k r)
                => Proxy# '(b,t) -> f (k,r) -> SessionMonad b m (f (k,r))
insertTreeManyDef (_ :: Proxy# '(b,t)) (rs :: f (k,r)) = do
  mbk <- fmap (fmap tagKey) <$> insertMany @b @(TdData t) rs
  insertChilds @b @(DataAutoIns(TdData t)) @(GrecChilds t (k, Grec r)) mbk rs
 where
  tagKey :: x -> Tagged (TopKey t) x
  tagKey = Tagged

instance InsertChilds b ai '[] pk k r where
  insertChilds _ = return

type InsChildCons b s td rs k r =
  ( InsTreeCons b td (Tagged (Fsts rs) (RecParent k (Grec r) rs))
                     (FieldByName s (k, Grec r))
  , RecCons b k
  , GrecLens s [FieldByName s (k, Grec r)] (k, Grec r)
  , NamesGrecLens (Snds rs) (RecParent k (Grec r) rs) (k, Grec r)
  )
type InsChildConsF b pk s td rs chs k r =
  ( InsChildCons b s td rs k r
  , InsertChilds b 'False chs pk k r
  )

type InsChildConsT b pk s td rs chs k r =
  ( InsChildCons b s td rs (k, Tagged pk (GenKey b)) r
  , InsertChilds b 'True chs pk k r
  )

class InsertChilds b ai chs pk k r where
  insertChilds  ::  ( MonadCons m , AppCons f )
                => Maybe (f (Tagged pk (GenKey b))) -> f (k,r)
                -> SessionMonad b m (f (k,r))

instance InsChildConsF b pk s td rs chs k r
      => InsertChilds b False ( '(s, '(td,rs)) ': chs) pk k r where
  insertChilds mbk rs = do
    rs' <- fmap ( liftA2 (\(k,r) r' -> second unGrec
                                    $ (k, Grec r) & grecLens @s .~ r') rs
                . fmap (map snd . getZipList)
                . getCompose
                )
        $ insertTreeManyDef (proxy# :: Proxy# '(b,td))
        $ Compose $ newRec <$> rs

    insertChilds @b @'False @chs mbk rs'
   where
    newRec :: (k,r) -> ZipList ( Tagged (Fsts rs) (RecParent k (Grec r) rs)
                               , FieldByName s (k, Grec r)
                               )
    newRec (k,r) = (Tagged (namesGrecGet @(Snds rs) (k, Grec r)),)
                <$> ZipList (view (grecLens @s) (k, Grec r))


instance InsChildConsT b pk s td rs chs k r
      => InsertChilds b True ( '(s, '(td,rs)) ': chs) pk k r where
  insertChilds mbk rs = do
    rs' <- fmap ( fmap (first fst)
                . liftA3 (\(k,r) tpk r'
                            -> second unGrec
                            $ ((k,tpk), Grec r) & grecLens @s .~ r') rs ks
                . fmap (map snd . getZipList)
                . getCompose
                )
        $ insertTreeManyDef (proxy# :: Proxy# '(b,td))
        $ Compose $ newRec <$> ks <*> rs

    insertChilds @b @'True @chs mbk rs'
   where
    newRec :: Tagged pk (GenKey b) -> (k,r)
          -> ZipList  ( Tagged (Fsts rs) (RecParent (k, Tagged pk (GenKey b)) (Grec r) rs)
                      , FieldByName s ((k, Tagged pk (GenKey b)), Grec r)
                      )
    newRec tpk (k,r) = (Tagged (namesGrecGet @(Snds rs) ((k,tpk), Grec r)),)
                  <$> ZipList (view (grecLens @s) ((k,tpk), Grec r))
    ks = fromMaybe (error $ "There is no key value (Nothing) in insertChilds"
                  ++ " but parent has AutoIns flag") mbk
