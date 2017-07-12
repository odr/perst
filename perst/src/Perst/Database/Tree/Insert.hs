{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE UndecidableInstances      #-}
module Perst.Database.Tree.Insert where

import           Control.Applicative           (ZipList (..), liftA2)
import           Control.Monad.Catch           (MonadMask)
import           Control.Monad.IO.Class        (MonadIO (..))
import           Data.Functor.Compose          (Compose (..))
import           Data.Singletons.Prelude
import           Data.Singletons.Prelude.Maybe (FromMaybeSym0)
import           Data.Singletons.TH            (promoteOnly)
import           Data.Tagged                   (Tagged (..))
-- import           GHC.TypeLits               (Symbol)
import           Data.Maybe                    (fromMaybe)
import           Lens.Micro                    ((&), (.~))
import           Lens.Micro.Extras             (view)

import           Data.Type.Grec                (FieldsGrec, FieldsGrecSym0,
                                                Grec (..), GrecLens (..),
                                                GrecWithout (..), ListToPairs,
                                                NamesGrecLens (..), Submap,
                                                SubmapSym0)
import           Perst.Database.Constraints    (InsConstr)
import           Perst.Database.DataDef        (DdAutoIns, DdKey)
import           Perst.Database.DbOption       (GenKey, SessionMonad)
import           Perst.Database.DML            (insertMany)
import           Perst.Database.Tree.Def       (FieldByName, GrecChilds, TdData,
                                                TopKey, TreeDef)
-- import           Perst.Lens                    (NamesGrecLens (..))

type InsertTreeConstraint m f b t r =
  ( Applicative f, Traversable f
  , InsConstr m b (TdData t) r
  , InsertChilds m f b (DdAutoIns (TdData t)) (TopKey t) (GrecChilds t r) r
  )

insertTreeManyR :: InsertTreeConstraint m ZipList b t (Grec r)
                => Proxy (t :: TreeDef) -> [r] -> SessionMonad b m [r]
insertTreeManyR pt = fmap (fmap unGrec) . insertTreeMany pt . fmap Grec

insertTreeMany  :: InsertTreeConstraint m ZipList b t r
                => Proxy (t :: TreeDef) -> [r] -> SessionMonad b m [r]
insertTreeMany pt = fmap getZipList . insertTreeMany' pt . ZipList

insertTreeMany' :: InsertTreeConstraint m f b t r
                => Proxy (t :: TreeDef) -> f r -> SessionMonad b m (f r)
insertTreeMany' (_ :: Proxy t) (rs :: f r) = do
  mbk <- fmap (fmap tagKey) <$> insertMany ptd rs
  insertChilds pai ptc mbk rs
 where
  ptd = Proxy :: Proxy (TdData t)
  ptc = Proxy :: Proxy (GrecChilds t r)
  pai = Proxy :: Proxy (DdAutoIns (TdData t))
  tagKey :: x -> Tagged (TopKey t) x
  tagKey = Tagged

class InsertChilds m f b (ai :: Bool) pk chs r where
  insertChilds  :: Proxy ai
                -> Proxy chs
                -> Maybe (f (Tagged (pk :: [Symbol]) (GenKey b)))
                -> f r
                -> SessionMonad b m (f r)

instance Monad m => InsertChilds m f b ai pk '[] r where
  insertChilds _ _ _ = return

promoteOnly [d|
  getParentTypes rs r = fromMaybe
    (error "Invalid parent fields in InsertChilds!")
    $ submap (map snd rs) $ fieldsGrec r
  |]

type Fsts rs = Map FstSym0 rs
type Snds rs = Map SndSym0 rs
type AddParent s rs r =
  ( Tagged (Fsts rs) (RecParent r rs)
  , GrecWithout (Fsts rs) (Grec (FieldByName s r))
  )
instance  ( InsertTreeConstraint
              m (Compose f ZipList) b td
              (AddParent s rs r)
          , GrecLens s [FieldByName s r] r
          , InsertChilds m f b False pk chs r
          , Applicative f
          , NamesGrecLens (Snds rs) (RecParent r rs) r
          ) => InsertChilds m f b False pk ( '(s, '(td,rs)) ': chs) r where
  insertChilds pai _ mbk rs = do
    rs' <- fmap ( liftA2 (\r r' -> r & grecLens fn .~ r') rs
                . fmap getZipList
                . getCompose
                . fmap (unGrec . unGWO . snd)
                )
        $ insertTreeMany' (Proxy :: Proxy td)
        $ Compose $ newRec <$> rs

    insertChilds pai (Proxy :: Proxy chs) mbk rs'
   where
    fn = Proxy :: Proxy s
    newRec :: r -> ZipList (AddParent s rs r)
    newRec r = (\r' -> (tr, GWO (Grec r'))) <$> ZipList (view (grecLens fn) r)
     where
      tr = Tagged (namesGrecGet (Proxy :: Proxy (Snds rs)) r)

type RecParent r rs = ListToPairs (GetParentTypes rs r)
type RecAutoIns b pk r = (Tagged pk (GenKey b), GrecWithout pk (Grec r))
type RecParentAutoIns b pk r rs = RecParent (RecAutoIns b pk r) rs
type AddParentAutoIns b pk s rs r =
  ( Tagged (Fsts rs) (RecParentAutoIns b pk r rs)
  , GrecWithout (Fsts rs) (Grec (FieldByName s r))
  )

instance  ( InsertTreeConstraint
              m (Compose f ZipList) b td
              (AddParentAutoIns b pk s rs r)
          , GrecLens s [FieldByName s r] r
          , InsertChilds m f b True pk chs r
          , Applicative f
          , NamesGrecLens (Snds rs)
                          (RecParentAutoIns b pk r rs)
                          (RecAutoIns b pk r)
          ) => InsertChilds m f b True pk ( '(s, '(td,rs)) ': chs) r where
  insertChilds pai _ mbk rs = do
    rs' <- fmap ( liftA2 (\r r' -> r & grecLens fn .~ r') rs
                . fmap getZipList
                . getCompose
                . fmap (unGrec . unGWO . snd)
                )
        $ insertTreeMany' (Proxy :: Proxy td)
        $ Compose $ newRec <$> ks <*> rs

    insertChilds pai (Proxy :: Proxy chs) mbk rs'
   where
    fn = Proxy :: Proxy s
    newRec :: Tagged pk (GenKey b) -> r -> ZipList (AddParentAutoIns b pk s rs r)
    newRec k r = (\r' -> (tr, GWO (Grec r')))
            <$> ZipList (view (grecLens fn) r)
     where
      tr = Tagged (namesGrecGet (Proxy :: Proxy (Snds rs))
                  (k, GWO (Grec r) :: GrecWithout pk (Grec r)))
    ks = fromMaybe (error $ "There is no key value (Nothing) in insertChilds"
                  ++ " but parent has AutoIns flag") mbk
