{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}
module Perst.Database.Tree.Select where

import           Control.Applicative       (ZipList (..), liftA2)
import           Data.Functor.Compose      (Compose (..))
import           Data.Tagged               (Tagged (..))
import           GHC.Prim                  (Proxy#, proxy#)
import           Lens.Micro                ((&), (.~))

import           Data.Type.GrecTree        (ConvNames (..), Grec (..), LType,
                                            TGetSet (..), TLens' (..))
import           Perst.Database.Condition  (Condition)
import           Perst.Database.DbOption   (MonadCons, SessionMonad)
import           Perst.Database.DML.Select (SelCondCons, SelCons, selectCondDef,
                                            selectManyDef)
import           Perst.Database.TreeDef    (AppCons, MbChild, TdData, TreeDef)
import           Perst.Types               (Fsts, LstFld, PChilds (..), Snds)


type SelTreeCons b t r k
  = (SelCons b (TdData t) r k, SelectChilds b t (FldNames LstFld r) r)
type SelTreeCondCons b t r
  = (SelCondCons b t r, SelectChilds b t (FldNames LstFld r) r)

selectTreeManyDef :: (AppCons f, MonadCons m, SelTreeCons b t r k)
                  => Proxy# '(b,t,r) -> f k -> SessionMonad b m (f [r])
selectTreeManyDef (_::Proxy# '(b,t,r)) (ks::f k) = do
  rs <- Compose . fmap ZipList
      <$> selectManyDef (proxy#::Proxy# '(b,TdData t,r)) ks
  fmap getZipList . getCompose <$> selectChilds @b @t @(FldNames LstFld r) rs

selectTreeCondDef :: (MonadCons m, SelTreeCondCons b t r)
  => Proxy# b -> Condition t r -> SessionMonad b m [r]
selectTreeCondDef (pb::Proxy# b) (c::Condition t r) = do
  rs <- ZipList <$> selectCondDef pb c
  getZipList <$> selectChilds @b @t @(FldNames LstFld r) rs

class SelectChilds b (t::TreeDef) fs r where
  selectChilds :: (MonadCons m, AppCons f) => f r -> SessionMonad b m (f r)

instance SelectChilds b t '[] r where
  selectChilds = return

instance ( MbChild s t ~ Just '(td,ref)
         , LType s r ~ PChilds v
         , Grec v
         , TLens' s r
         , Snds ref ~ pref
         , Fsts ref ~ cref
         , TGetSet pref r, GSType pref r ~ vref
         , Tagged cref vref ~ chfk
         , SelTreeCons b td (GrecTagged v) chfk
         , SelectChilds b t ss r
         )
      => SelectChilds b t (s ': ss) r where
  selectChilds rs = do
    -- undefined
    rs' <- liftA2 (\r cs -> r & tlens' @s .~ PChilds (fromTagged <$> cs)) rs
      <$> selectTreeManyDef (proxy# :: Proxy# '(b,td,(GrecTagged v))) (keyRec <$> rs)
    selectChilds @b @t @ss rs'
    where
      keyRec r = Tagged @cref (tget @pref r)
