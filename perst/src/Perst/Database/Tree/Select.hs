{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}
module Perst.Database.Tree.Select where

import           Control.Applicative     (ZipList (..), liftA2)
import           Data.Bifunctor          (second)
import           Data.Functor.Compose    (Compose (..))
-- import           Data.Proxy              (Proxy (..))
import           Data.Tagged             (Tagged (..), retag)
import           Data.Traversable        (Traversable (..))
import           GHC.Prim                (Proxy#, proxy#)
import           GHC.TypeLits            (Symbol)
import           Lens.Micro              ((.~))

import           Data.Type.Grec          (Grec (..), GrecLens (..),
                                          GrecWith (..))
-- import           Perst.Database.Constraints (SelConstr)
import           Perst.Database.DbOption (MonadCons, SessionMonad)
import           Perst.Database.DML      (SelectByKey (..))
import           Perst.Database.Tree.Def (AppCons, ChildByParents, FieldByName,
                                          GrecChilds, TaggedAllParentKeys,
                                          TdData, TreeDef)
import           Perst.Types             (Fsts)

type {- family SelectTreeConstraint m f b t r k where -}
  SelectTreeConstraint m f b t r k =
    ( AppCons f, MonadCons m
    , SelectTreeConstraint' b t r (Grec r) k (TaggedAllParentKeys t)
    )

type {- family SelectTreeConstraint' b t r gr k tapk where -}
  SelectTreeConstraint' b t r gr k tapk =
    ( SelectByKey b (TdData t) (tapk, gr) k
    , SelectTreeConstraint'' b r tapk (GrecChilds t gr)
    )

type {- family SelectTreeConstraint'' b r tapk gc where -}
  SelectTreeConstraint'' b r tapk gc =
    ( SelectChilds b gc tapk r
    -- , SingI gc
    )

class SelectTree b t r k where
  selectTreeMany :: (MonadCons m , AppCons f) => f k -> SessionMonad b m (f [r])
  default selectTreeMany :: SelectTreeConstraint m f b t r k
                          => f k -> SessionMonad b m (f [r])
  selectTreeMany = selectTreeManyDef (proxy# :: Proxy# b) (proxy# :: Proxy# t)
                                     (proxy# :: Proxy# r)

  selectTreeManyL :: MonadCons m => [k] -> SessionMonad b m [[r]]
  selectTreeManyL = fmap getZipList . selectTreeMany @b @t @r . ZipList


selectTreeManyDef :: SelectTreeConstraint m f b t r k
  => Proxy# b -> Proxy# t -> Proxy# r -> f k -> SessionMonad b m (f [r])
selectTreeManyDef (_ :: Proxy# b) (_ :: Proxy# t) (_ :: Proxy# r) ks = do
  ps <- Compose . fmap (ZipList . map (second unGrec))
    <$> selectMany @b @(TdData t) @(TaggedAllParentKeys t, Grec r) ks
  fmap (fmap getZipList . getCompose . fmap snd)
    $ selectChilds @b @(GrecChilds t (Grec r)) ps

 -- where
  -- ptd = sTdData st
  -- pkr = Proxy :: Proxy (TaggedAllParentKeys t, GrecF r)
  -- ptc = sing :: Sing (GrecChilds t (GrecF r))

class SelectChilds b (chs :: [(Symbol, (TreeDef, [(Symbol,Symbol)]))]) ks r where
  selectChilds :: (MonadCons m, AppCons f) => f (ks,r) -> SessionMonad b m (f (ks,r))

instance SelectChilds b '[] ks r where
  selectChilds = return

type {- family SelectChildsConstraint b s td rs nk vk r where -}
  SelectChildsConstraint b s td rs nk vk r =
    SelectChildsConstraint' b s td rs nk vk (Grec r)

type {- family SelectChildsConstraint' b s td ts nk vk r where -}
  SelectChildsConstraint' b s td rs nk vk r =
    SelectChildsConstraint'' b s td rs nk vk r (FieldByName s r)


type {- family SelectChildsConstraint'' b s td rs nk vk r r' where -}
  SelectChildsConstraint'' b s td rs nk vk r r'  =
    ( SelectTree b td r'
        (GrecWith (Fsts rs) (Tagged (ChildByParents rs nk) vk))
    , GrecLens s [r'] r
    )

instance  ( SelectChildsConstraint b s td rs nk vk r
          , SelectChilds b chs (Tagged nk vk) r
          )
    => SelectChilds b ('(s,'(td,rs)) ': chs) (Tagged nk vk) r where
  selectChilds compKR = do
    r <- liftA2 updRec compKR
        <$> selectTreeMany @b @td @(FieldByName s (Grec r)) newkey
    selectChilds @b @chs r
   where
    -- ptr = Proxy :: Proxy (FieldByName s (GrecF r ))
    newkey = fmap
      ( (GW :: Tagged (ChildByParents rs nk) vk
            -> GrecWith (Fsts rs) (Tagged (ChildByParents rs nk) vk))
      . (retag :: Tagged nk vk -> Tagged (ChildByParents rs nk) vk)
      . fst
      ) compKR
    updRec :: (Tagged nk vk, r) -> [FieldByName s (Grec r)] -> (Tagged nk vk, r)
    updRec k' r' = second (unGrec . (grecLens @s .~ r') . Grec) k'
