{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}
module Perst.Database.Tree.Insert where

import           Control.Applicative        (ZipList (..), liftA2)
import           Control.Monad.Catch        (MonadMask)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Data.Functor.Compose       (Compose (..))
import           Data.Singletons.Prelude    (Proxy(..))
import           Lens.Micro                 ((&), (.~))
import           Lens.Micro.Extras          (view)

import           Data.Type.Grec             (FieldName (..), Grec (..),
                                             LensedConstraint, nlens)
import           Perst.Database.Constraints (InsConstr)
import           Perst.Database.DbOption    (SessionMonad)
import           Perst.Database.DML         (insertManyR)
import           Perst.Database.Tree.Def    (FieldByName, GrecChilds, TdData,
                                             TreeDef)

type InsertTreeConstraint m f b t r =
  ( MonadIO m, MonadMask m, Applicative f, Traversable f
  , InsConstr b (TdData t) (Grec r)
  , InsertChilds m f b (GrecChilds t r) r
  )

insertTreeMany :: InsertTreeConstraint m ZipList b t r
  => Proxy (t :: TreeDef) -> [r] -> SessionMonad b m [r]
insertTreeMany pt = fmap getZipList . insertTreeMany' pt . ZipList

insertTreeMany' :: InsertTreeConstraint m f b t r
  => Proxy (t :: TreeDef) -> f r -> SessionMonad b m (f r)
insertTreeMany' (_ :: Proxy t) (rs :: f r) = do
  insertManyR ptd rs
  insertChilds ptc rs
 where
  ptd = Proxy :: Proxy (TdData t)
  ptc = Proxy :: Proxy (GrecChilds t r)


class InsertChilds m f b chs r where
  insertChilds :: Proxy chs -> f r -> SessionMonad b m (f r)

instance Monad m => InsertChilds m f b '[] r where
  insertChilds _ = return

instance  ( InsertTreeConstraint m (Compose f ZipList) b td (FieldByName s r)
          , LensedConstraint r s [FieldByName s r]
          , InsertChilds m f b chs r
          , Applicative f
          ) => InsertChilds m f b ('(s,'(td,rs)) ': chs) r where
  insertChilds _ rs = do
    rs' <- fmap ( liftA2 (\r r' -> r & nlens fn .~ r') rs
                . fmap getZipList
                . getCompose
                )
        $ insertTreeMany' (Proxy :: Proxy td)
        $ Compose $ (ZipList . view (nlens fn)) <$> rs
    insertChilds (Proxy :: Proxy chs) rs'
   where
    ptd = Proxy :: Proxy td
    fn = FieldName :: FieldName s
