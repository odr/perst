{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}
module Perst.Database.Tree.Select where -- (selectTreeMany, SelectChilds()) where

import           Control.Applicative        (ZipList (..), liftA2)
import           Control.Monad.Catch        (MonadMask)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Data.Bifunctor             (second)
import           Data.Functor.Compose       (Compose (..))
import           Data.Singletons.Prelude    (FstSym0, Map, Proxy (..), Sing,
                                             SingI (..), Symbol)
import           Data.Tagged                (Tagged (..), retag)
import           Data.Traversable           (Traversable (..))
import           Lens.Micro                 ((.~))

import           Data.Type.Grec             (Grec (..), GrecLens (..),
                                             GrecWith (..))
import           Perst.Database.Constraints (SelConstr)
import           Perst.Database.DbOption    (SessionMonad)
import           Perst.Database.DML         (selectMany)
import           Perst.Database.Tree.Def    (ChildByParents, FieldByName,
                                             GrecChilds, TaggedAllParentKeys,
                                             TdData, TreeDef)

type SelectTreeConstraint m f b t r k =
  ( Applicative f, Traversable f
  , SelConstr m b (TdData t) (TaggedAllParentKeys t, Grec r) k
  , SelectChilds m (Compose f ZipList) b (GrecChilds t (Grec r)) (TaggedAllParentKeys t) r
  , SingI (TdData t)
  )

selectTreeMany :: SelectTreeConstraint m ZipList b t r k
  => Proxy t -> Proxy r -> [k] -> SessionMonad b m [[r]]
selectTreeMany pt pr  = fmap getZipList . selectTreeMany' pt pr . ZipList

selectTreeMany' :: SelectTreeConstraint m f b t r k
  => Proxy t -> Proxy r -> f k -> SessionMonad b m (f [r])
selectTreeMany' (_ :: Proxy t) (_ :: Proxy r) ks
  = Compose . fmap (ZipList . map (second unGrec)) <$> selectMany ptd pkr ks
  >>= fmap (fmap getZipList . getCompose . fmap snd) . selectChilds ptc
 where
  ptd = sing :: Sing (TdData t)
  pkr = Proxy :: Proxy (TaggedAllParentKeys t, Grec r)
  ptc = Proxy :: Proxy (GrecChilds t (Grec r))

class SelectChilds m f b
          (chs :: [(Symbol, (TreeDef, [(Symbol,Symbol)]))])
          ks r where
  selectChilds :: Proxy chs -> f (ks,r) -> SessionMonad b m (f (ks,r))

instance Monad m => SelectChilds m f b '[] ks r where
  selectChilds _ = return

instance  ( SelectTreeConstraint m f b td (FieldByName s (Grec r))
              (GrecWith (Map FstSym0 rs) (Tagged (ChildByParents rs nk) vk))
          , GrecLens s [FieldByName s (Grec r)] (Grec r)
          , SelectChilds m f b chs (Tagged nk vk) r
          )
    => SelectChilds m f b ('(s,'(td,rs)) ': chs) (Tagged nk vk) r where
  selectChilds _ compKR
    = liftA2 updRec compKR <$> selectTreeMany' ptd ptr newkey
    >>= selectChilds (Proxy :: Proxy chs)
   where
    ptd = Proxy :: Proxy td
    ptr = Proxy :: Proxy (FieldByName s (Grec r))
    newkey = fmap
      ( (GW :: Tagged (ChildByParents rs nk) vk
            -> GrecWith (Map FstSym0 rs) (Tagged (ChildByParents rs nk) vk))
      . (retag :: Tagged nk vk -> Tagged (ChildByParents rs nk) vk)
      . fst
      ) compKR
    updRec :: (Tagged nk vk, r) -> [FieldByName s (Grec r)] -> (Tagged nk vk, r)
    updRec k' r' = second (unGrec . (grecLens (Proxy :: Proxy s) .~ r') . Grec) k'
