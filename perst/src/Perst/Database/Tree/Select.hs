{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}
module Perst.Database.Tree.Select where

import           Control.Applicative     (ZipList (..), liftA2)
import           Data.Functor.Compose    (Compose (..))
import           Data.Tagged             (Tagged (..), retag)
import           GHC.Prim                (Proxy#, proxy#)
import           GHC.TypeLits            (Symbol)
import           Lens.Micro              ((.~))

import           Data.Type.Grec          (ConvGrecInfo, Convert, GrecLens (..),
                                          GrecWith (..))
import           Perst.Database.DbOption (DbOption (..), MonadCons,
                                          SessionMonad)
import           Perst.Database.DML      (DML (..), RecCons (..))
import           Perst.Database.Tree.Def (AppCons, ChildByParents, FieldByName,
                                          GrecChilds, TaggedAllParentKeys,
                                          TdData, TreeDef)
import           Perst.Types             (Fsts)

type SelTreeCons b t k r =
  ( DML b (TdData t) r
  , RecCons b k
  , SelTreeCons' b t r k (TaggedAllParentKeys t)
  )

type SelTreeCons' b t r k tapk =
  ( Convert [FieldDB b] ([FieldDB b], tapk)
  , ConvGrecInfo tapk
  , SelectChilds b (GrecChilds t r) tapk r
  )

selectTreeManyDef :: (AppCons f, MonadCons m, SelTreeCons b t k r)
                  => Proxy# b -> Proxy# t -> Proxy# r -> f k
                  -> SessionMonad b m (f [r])
selectTreeManyDef (_::Proxy# b) (_::Proxy# t) (_::Proxy# r) (ks::f k) = do
  ps <- Compose . fmap ZipList
    <$> selectMany @b @(TdData t) @r
                  (proxy# :: Proxy# (TaggedAllParentKeys t)) ks
  fmap (fmap getZipList . getCompose)
    $ selectChilds @b @(GrecChilds t r) ps


class SelectChilds b (chs :: [(Symbol, (TreeDef, [(Symbol,Symbol)]))]) k r where
  selectChilds :: (MonadCons m, AppCons f) => f (k,r) -> SessionMonad b m (f r)

instance SelectChilds b '[] k r where
  selectChilds = return . fmap snd

type SelectChildsConstraint b s td rs nk vk r =
    SelectChildsConstraint' b s td rs nk vk r (FieldByName s (r))

type SelectChildsConstraint' b s td rs nk vk r r'  =
  ( SelTreeCons b td
      (GrecWith (Fsts rs) (Tagged (ChildByParents rs nk) vk)) r'
  , GrecLens s [r'] (r)
  )

instance ( SelectChildsConstraint b s td rs nk vk r
         , SelectChilds b chs (Tagged nk vk) r
         )
         => SelectChilds b ('(s,'(td,rs)) ': chs) (Tagged nk vk) r where
  selectChilds compKR = do
    r <- liftA2 updRec compKR
        <$> selectTreeManyDef (proxy# :: Proxy# b) (proxy# :: Proxy# td)
              (proxy# :: Proxy# (FieldByName s r)) newkey
    selectChilds @b @chs r
   where
    newkey = fmap
      ( (GW :: Tagged (ChildByParents rs nk) vk
            -> GrecWith (Fsts rs) (Tagged (ChildByParents rs nk) vk))
      . (retag :: Tagged nk vk -> Tagged (ChildByParents rs nk) vk)
      . fst
      ) compKR
    updRec :: (Tagged nk vk, r) -> [FieldByName s r] -> (Tagged nk vk, r)
    updRec (k,r) rs' = (k, (grecLens @s .~ rs')  r)
