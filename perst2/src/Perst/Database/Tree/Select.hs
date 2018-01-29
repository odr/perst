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

import           Data.Type.GrecTree        (ConvNames (..), Grec (..), SingI,
                                            TGetSet (..))
import qualified Perst.Database.Condition  as C
import qualified Perst.Database.Condition2 as C2
import           Perst.Database.DataDef    (DdName, GetDS, GetDataStruct,
                                            GetToRefByName, RefCols, RefFrom,
                                            SchRefs, Schema)
import           Perst.Database.DbOption   (AppCons, MonadCons, SessionMonad)
import           Perst.Database.DML.Select (SelCondCons, SelCons, SelCons0,
                                            selectCond2Def, selectCondDef,
                                            selectManyDef)
-- import           Perst.Database.TreeDef    (AppCons, MbChild, TdData, TreeDef)
import           Perst.Types               (Fsts, LstFld, PChilds (..), Snds)


type SelTreeCons b sch t r k
  = (SelCons b (GetDS t sch) r k, SelectChilds b sch t (FldNames LstFld r) r)
type SelTreeCondCons b sch t r
  = (SelCondCons b sch (GetDS t sch) r, SelectChilds b sch t (FldNames LstFld r) r)
type SelTreeCond2Cons b sch t r
  = (SelCons0 b (GetDS t sch) r, SelectChilds b sch t (FldNames LstFld r) r)

selectTreeManyDef :: (AppCons f, MonadCons m
                     , GetDataStruct t sch ~ Just ds
                     , SelTreeCons b sch t r k
                     )
           => Proxy# '(b,(sch::Schema),t,r) -> f k -> SessionMonad b m (f [r])
selectTreeManyDef (_::Proxy# '(b,sch,t,r)) (ks::f k) = do
  rs <- Compose . fmap ZipList
      <$> selectManyDef (proxy#::Proxy# '(b,GetDS t sch,r)) ks
  fmap getZipList . getCompose <$> selectChilds @b @sch @t @(FldNames LstFld r) rs

selectTreeCondDef :: (MonadCons m, SelTreeCondCons b sch t r
                     , GetDataStruct t sch ~ Just ds
                     , t ~ DdName ds
                     )
  => Proxy# b -> C.Condition (sch::Schema) t r -> SessionMonad b m [r]
selectTreeCondDef (pb::Proxy# b) (c::C.Condition sch t r) = do
  rs <- ZipList <$> selectCondDef pb c
  getZipList <$> selectChilds @b @sch @t @(FldNames LstFld r) rs

selectTreeCond2Def :: (MonadCons m, SelTreeCond2Cons b sch t r
                     , GetDataStruct t sch ~ Just ds
                     , SingI sch
                     )
  => Proxy# '(t,r) -> C2.Cond '(b,(sch::Schema)) a -> SessionMonad b m [r]
selectTreeCond2Def (p::Proxy# '(t,r)) (c::C2.Cond '(b,sch) a) = do
  rs <- ZipList <$> selectCond2Def p c
  getZipList <$> selectChilds @b @sch @t @(FldNames LstFld r) rs

class SelectChilds b (sch :: Schema) t fs r where
  selectChilds :: (MonadCons m, AppCons f) => f r -> SessionMonad b m (f r)

instance SelectChilds b sch t '[] r where
  selectChilds = return

instance ( GetToRefByName t s (SchRefs sch) ~ Just ref
         , GSType '[s] r ~ PChilds v
         , TGetSet '[s] r
         , Grec v
         , RefCols ref ~ rcols
         , Fsts rcols ~ rcols1
         , Snds rcols ~ rcols2
         , TGetSet rcols2 r, GSType rcols2 r ~ vref
         , Tagged rcols1 vref ~ chfk
         , GetDataStruct (RefFrom ref) sch ~ Just ds
         , SelTreeCons b sch (RefFrom ref) (GrecTagged v) chfk
         , SelectChilds b sch t ss r
         )
      => SelectChilds b sch t (s ': ss) r where
  selectChilds rs = do
    -- undefined
    rs' <- liftA2 (\r cs -> tset @'[s] (PChilds $ fromTagged <$> cs) r) rs
      <$> selectTreeManyDef (proxy# :: Proxy# '(b,sch,RefFrom ref,GrecTagged v))
                            (keyRec <$> rs)
    selectChilds @b @sch @t @ss rs'
    where
      keyRec r = Tagged @rcols1 (tget @rcols2 r)
