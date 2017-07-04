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
import           Lens.Micro                    ((&), (.~))
import           Lens.Micro.Extras             (view)

import           Data.Type.Grec                (FieldsGrec, FieldsGrecSym0,
                                                Grec (..), GrecWithout (..),
                                                LensedConstraint, ListToPairs,
                                                nlens)
import           Perst.Database.Constraints    (InsConstr)
import           Perst.Database.DataDef        (DdAutoIns, DdKey)
import           Perst.Database.DbOption       (GenKey, SessionMonad)
import           Perst.Database.DML            (insertMany)
import           Perst.Database.Tree.Def       (FieldByName, GrecChilds, TdData,
                                                TreeDef)
import           Perst.Lens                    (NamesLens (..))
import           Perst.Types                   (Submap, SubmapSym0)

type InsertTreeConstraint m f b t r =
  ( Applicative f, Traversable f
  , InsConstr m b (TdData t) r
  , InsertChilds m f b (DdAutoIns (TdData t))
                (DdKey (TdData t)) (GrecChilds t r) r
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
  tagKey :: x -> Tagged (DdKey (TdData t)) x
  tagKey = Tagged

class InsertChilds m f b (ai :: Bool) pk chs r where
  insertChilds  :: Proxy ai
                -> Proxy chs
                -> Maybe (f (Tagged pk (GenKey b)))
                -> f r
                -> SessionMonad b m (f r)

instance Monad m => InsertChilds m f b ai pk '[] r where
  insertChilds _ _ _ = return

promoteOnly [d|
  getParentTypes rs r = fromMaybe
    (error "Invalid parent fields in InsertChilds!")
    $ submap (map snd rs) $ fieldsGrec r
  |]

instance  ( InsertTreeConstraint
              m (Compose f ZipList) b td
              ( Tagged (Map FstSym0 rs) (ListToPairs (GetParentTypes rs r))
              , GrecWithout (Map FstSym0 rs) (Grec (FieldByName s r))
              )
          , LensedConstraint r s [FieldByName s r]
          , InsertChilds m f b False pk chs r
          , Applicative f
          , NamesLens (Map FstSym0 rs) (ListToPairs (GetParentTypes rs r)) r
          ) => InsertChilds m f b False pk ( '(s, '(td,rs)) ': chs) r where
  insertChilds pai _ mbk rs = do
    rs' <- fmap ( liftA2 (\r r' -> r & nlens fn .~ r') rs
                . fmap getZipList
                . getCompose
                . fmap (unGrec . unGWO . snd)
                )
        $ insertTreeMany' (Proxy :: Proxy td)
        $ Compose $ newRec <$> rs

    insertChilds pai (Proxy :: Proxy chs) mbk rs'
   where
    fn = Proxy :: Proxy s
    newRec :: r -> ZipList
                ( Tagged (Map FstSym0 rs) (ListToPairs (GetParentTypes rs r))
                , GrecWithout (Map FstSym0 rs) (Grec (FieldByName s r))
                )
    newRec r = (\r' -> (tr, GWO (Grec r'))) <$> ZipList (view (nlens fn) r)
     where
      tr = Tagged (namesGet (Proxy :: Proxy (Map FstSym0 rs)) r)

-- instance  ( InsertTreeConstraint
--               m (Compose f ZipList) b td
--               ( Tagged (Map FstSym0 rs)
--                    (ListToPairs (GetParentTypes rs
--                                   (Tagged pk (GenKey b), GrecWithout pk r)))
--               , GrecWithout (Map FstSym0 rs) (Grec (FieldByName s r))
--               )
--           , LensedConstraint r s [FieldByName s r]
--           , InsertChilds m f b True pk chs r
--           , Applicative f
--           , NamesLens (Map FstSym0 rs)
--                       (ListToPairs (GetParentTypes rs r
--                           (Tagged pk (GenKey b), GrecWithout pk r)))
--                       (Tagged pk (GenKey b), GrecWithout pk r)
--           ) => InsertChilds m f b True pk ( '(s, '(td,rs)) ': chs) r where
--   insertChilds pai _ mbk rs = do
--     rs' <- fmap ( liftA2 (\r r' -> r & nlens fn .~ r') rs
--                 . fmap getZipList
--                 . getCompose
--                 . fmap (unGrec . unGWO . snd)
--                 )
--         $ insertTreeMany' (Proxy :: Proxy td)
--         $ Compose $ newRec <$> rs
--
--     insertChilds pai (Proxy :: Proxy chs) mbk rs'
--    where
--     fn = FieldName :: FieldName s
--     newRec :: r -> ZipList
--                 ( Tagged (Map FstSym0 rs) (ListToPairs (GetParentTypes rs r))
--                 , GrecWithout (Map FstSym0 rs) (Grec (FieldByName s r))
--                 )
--     newRec r = (\r' -> (tr, GWO (Grec r'))) <$> ZipList (view (nlens fn) r)
--      where
--       tr = Tagged (namesGet (Proxy :: Proxy (Map FstSym0 rs)) r)
--     k = fromMaybe (error "There is no key value (Nothing) in insertChilds "
--                   ++ "but parent has AutoIns flag") mbk
