{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE UndecidableInstances      #-}
module Perst.Database.Tree.Delete where

import           Control.Applicative        (ZipList (..))
import           Data.Functor.Compose       (Compose (..))
import           Data.Proxy                 (Proxy (..))
import           Data.Type.Grec             (GrecF, GrecLens (..),
                                             GrecWith (..), grec)
import           Lens.Micro.Extras          (view)
import           Perst.Database.Constraints (DelByKeyConstr)
import           Perst.Database.DataDef     (DdKey)
import           Perst.Database.DbOption    (SessionMonad)
import           Perst.Database.DML         (deleteByKeyMany)
import           Perst.Database.Tree.Def    (FieldByName, GrecChilds, TdData,
                                             TopPK, TreeDef)

type DeleteTreeConstraint m f b t r =
  ( Applicative f, Traversable f
  , DelByKeyConstr m b (TdData t) (TopPK t r)
  , DeleteChilds m f b (GrecChilds t r) r
  )

deleteTreeManyR :: DeleteTreeConstraint m ZipList b t (GrecF r)
                => Proxy (t :: TreeDef) -> [r] -> SessionMonad b m ()
deleteTreeManyR (pt :: Proxy t) = deleteTreeMany pt . fmap grec
 -- where
 --  getKey = GW . Grec :: r -> GrecWith (DdKey (TdData t)) (Grec r)

deleteTreeMany  :: DeleteTreeConstraint m ZipList b t r
                => Proxy (t :: TreeDef) -> [r] -> SessionMonad b m ()
deleteTreeMany pt = deleteTreeMany' pt . ZipList

deleteTreeMany' :: DeleteTreeConstraint m f b t r
                => Proxy (t :: TreeDef) -> f r -> SessionMonad b m ()
deleteTreeMany' (_ :: Proxy t) (s :: f r) = do
  deleteChilds pc s
  deleteByKeyMany pd $ fmap getKey s
 where
  pc = Proxy :: Proxy (GrecChilds t r)
  pd = Proxy :: Proxy (TdData t)
  getKey = GW :: r -> TopPK t r

class DeleteChilds m f b chs r where
  deleteChilds  :: Proxy chs -> f r -> SessionMonad b m ()

instance Monad m => DeleteChilds m f b '[] r where
  deleteChilds _ _ = return ()

instance  ( DeleteTreeConstraint m (Compose f ZipList) b td (GrecF (FieldByName s r))
          , GrecLens s [FieldByName s r] r
          , Applicative f
          , DeleteChilds m f b chs r
          )
          => DeleteChilds m f b ('(s,'(td,rs)) ': chs) r where
  deleteChilds _ rs = do
    deleteTreeMany' (Proxy :: Proxy td) rc
    deleteChilds (Proxy :: Proxy chs) rs
   where
    rc :: Compose f ZipList (GrecF (FieldByName s r))
    rc = Compose $ (ZipList . fmap grec . view (grecLens (Proxy :: Proxy s))) <$> rs
