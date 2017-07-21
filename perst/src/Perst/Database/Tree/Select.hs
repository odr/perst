{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}
module Perst.Database.Tree.Select where -- (selectTreeMany, SelectChilds()) where

import           Control.Applicative        (ZipList (..), liftA2)
import           Control.Monad.Catch        (MonadMask)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Data.Bifunctor             (second)
import           Data.Functor.Compose       (Compose (..))
import           Data.Singletons.Prelude    (Fst, FstSym0, Head, Map,
                                             Proxy (..), Snd, Symbol)
import           Data.Tagged                (Tagged (..), retag)
import           Data.Traversable           (Traversable (..))
import           Lens.Micro                 ((.~))

import           Data.Type.Grec             (Grec (..), GrecF, GrecLens (..),
                                             GrecWith (..), grec)
import           Perst.Database.Constraints (SelConstr)
import           Perst.Database.DbOption    (SessionMonad)
import           Perst.Database.DML         (selectMany)
import           Perst.Database.Tree.Def    (ChildByParents, FieldByName,
                                             GrecChilds, TaggedAllParentKeys,
                                             TdData, TreeDef)

type SelectTreeConstraint m f b t r k =
  ( Applicative f, Traversable f
  , SelectTreeConstraint' m f b t r (GrecF r) k
  )
type SelectTreeConstraint' m f b t r gr k =
  ( SelConstr m b (TdData t) (TaggedAllParentKeys t, gr) k
  , SelectChilds m (Compose f ZipList) b (GrecChilds t gr) (TaggedAllParentKeys t) r
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
  ptd = Proxy :: Proxy (TdData t)
  pkr = Proxy :: Proxy (TaggedAllParentKeys t, GrecF r)
  ptc = Proxy :: Proxy (GrecChilds t (GrecF r))

class SelectChilds m f b
          (chs :: [(Symbol, (TreeDef, [(Symbol,Symbol)]))])
          ks r where
  selectChilds :: Proxy chs -> f (ks,r) -> SessionMonad b m (f (ks,r))

instance Monad m => SelectChilds m f b '[] ks r where
  selectChilds _ = return

type SelectChildsConstraint m f b chs nk vk r =
  SelectChildsConstraint' m f b chs nk vk (GrecF r) (Fst (Head chs))

type SelectChildsConstraint' m f b chs nk vk r s =
  SelectChildsConstraint'' m f b chs nk vk r s (FieldByName s r) (Snd (Snd (Head chs)))

type SelectChildsConstraint'' m f b chs nk vk r s r' rs =
  ( SelectTreeConstraint m f b (Fst (Snd (Head chs))) r'
          (GrecWith (Map FstSym0 rs) (Tagged (ChildByParents rs nk) vk))
  , GrecLens s [r'] r
  )

instance  ( SelectChildsConstraint m f b ('(s,'(td,rs)) ': chs) nk vk r
          , SelectChilds m f b chs (Tagged nk vk) r
          )
    => SelectChilds m f b ('(s,'(td,rs)) ': chs) (Tagged nk vk) r where
  selectChilds _ compKR
    = liftA2 updRec compKR <$> selectTreeMany' ptd ptr newkey
    >>= selectChilds (Proxy :: Proxy chs)
   where
    ptd = Proxy :: Proxy td
    ptr = Proxy :: Proxy (FieldByName s (GrecF r ))
    newkey = fmap
      ( (GW :: Tagged (ChildByParents rs nk) vk
            -> GrecWith (Map FstSym0 rs) (Tagged (ChildByParents rs nk) vk))
      . (retag :: Tagged nk vk -> Tagged (ChildByParents rs nk) vk)
      . fst
      ) compKR
    updRec :: (Tagged nk vk, r) -> [FieldByName s (GrecF r)] -> (Tagged nk vk, r)
    updRec k' r' = second (unGrec . (grecLens (Proxy :: Proxy s) .~ r') . grec) k'
