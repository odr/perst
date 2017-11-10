{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}
module Perst.Database.Tree.Select where

import           Control.Applicative      (ZipList (..), liftA2)
import           Data.Functor.Compose     (Compose (..))
import           Data.Tagged              (Tagged (..), retag)
import           GHC.Prim                 (Proxy#, proxy#)
import           GHC.TypeLits             (Symbol)
import           Lens.Micro               ((.~))

import           Data.Type.Grec           (ConvGrecInfo, Convert, Fsts,
                                           Grec (..), GrecLens (..),
                                           GrecWith (..))
import           Perst.Database.Condition (Condition, ConvCond (..))
import           Perst.Database.DbOption  (DbOption (..), MonadCons,
                                           SessionMonad)
import           Perst.Database.DML       (DML (..), RecCons (..))
import           Perst.Database.TreeDef   (AppCons, ChildByParents, FieldByName,
                                           GrecChilds, TaggedAllParentKeys,
                                           TdData, TreeDef)

type SelTreeCons b t k r =
  ( DML b (TdData t) r
  , RecCons b k
  , SelTreeCons' b t r (TaggedAllParentKeys t (Grec r))
  )

type SelTreeCons' b t r tapk =
  ( Convert [FieldDB b] ([FieldDB b], tapk)
  , ConvGrecInfo tapk
  , SelectChilds b (GrecChilds t (Grec r)) tapk r
  )

type SelTreeCond b t r =
  ( DML b (TdData t) r
  , ConvCond b (Condition t r)
  , SelTreeCons' b t r (TaggedAllParentKeys t (Grec r))
  )

selectTreeManyDef :: (AppCons f, MonadCons m, SelTreeCons b t k r)
                  => Proxy# '(b,t,r) -> f k -> SessionMonad b m (f [r])
selectTreeManyDef (_::Proxy# '(b,t,r)) (ks::f k) = do
  ps <- Compose . fmap ZipList
    <$> selectMany @b @(TdData t) @r
                  (proxy# :: Proxy# (TaggedAllParentKeys t (Grec r))) ks
  (fmap getZipList . getCompose)
    <$> selectChilds @b @(GrecChilds t (Grec r)) ps

selectTreeCondDef
  :: (MonadCons m, SelTreeCond b t r)
  => Proxy# b -> Condition t r -> SessionMonad b m [r]
selectTreeCondDef (_::Proxy# b) (c :: Condition t r) = do
  rs <- ZipList
    <$> selectCond @b @(TdData t) @r
                  (proxy# :: Proxy# (TaggedAllParentKeys t (Grec r))) c
  getZipList <$> selectChilds @b @(GrecChilds t (Grec r)) rs

class SelectChilds b (chs :: [(Symbol, (TreeDef, [(Symbol,Symbol)]))]) k r where
  selectChilds :: (MonadCons m, AppCons f) => f (k,r) -> SessionMonad b m (f r)

instance SelectChilds b '[] k r where
  selectChilds = return . fmap snd

type SelectChildsConstraint b s td rs nk vk r =
    SelectChildsConstraint' b s td rs nk vk r (FieldByName s (Grec r))

type SelectChildsConstraint' b s td rs nk vk r r'  =
  ( SelTreeCons b td
      (GrecWith (Fsts rs) (Tagged (ChildByParents rs nk) vk)) r'
  , GrecLens s [r'] (Grec r)
  )

instance ( SelectChildsConstraint b s td rs nk vk r
         , SelectChilds b chs (Tagged nk vk) r
         )
         => SelectChilds b ('(s,'(td,rs)) ': chs) (Tagged nk vk) r where
  selectChilds compKR = do
    r <- liftA2 updRec compKR
        <$> selectTreeManyDef (proxy# :: Proxy# '(b,td,FieldByName s (Grec r)))
                              newkey
    selectChilds @b @chs r
   where
    newkey = fmap
      ( (GW :: Tagged (ChildByParents rs nk) vk
            -> GrecWith (Fsts rs) (Tagged (ChildByParents rs nk) vk))
      . (retag :: Tagged nk vk -> Tagged (ChildByParents rs nk) vk)
      . fst
      ) compKR
    updRec :: (Tagged nk vk, r) -> [FieldByName s (Grec r)] -> (Tagged nk vk, r)
    updRec (k,r) rs' = (k, unGrec $ (grecLens @s .~ rs') $ Grec r)
