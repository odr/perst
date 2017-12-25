{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Perst.Database.Tree.Update where

import           Data.Bifunctor             (bimap)
import qualified Data.Map.Strict            as M
import           Data.Maybe                 (catMaybes)
import           Data.Tagged                (Tagged (..))
import           GHC.Prim                   (Proxy#, proxy#)

import           Data.Type.GrecTree         (BTreeFromList, ConvNames (..),
                                             Grec (..), TGetSet (..),
                                             TShowHide (..))
import           Perst.Database.DataDef     (DdKey, GetDS, GetDataStruct,
                                             GetToRefByName, RefCols, RefFrom,
                                             SchRefs)
import           Perst.Database.DbOption    (MonadCons, SessionMonad)
import           Perst.Database.DML.Update  (UpdDiffCons, updateDiffManyDef)
import           Perst.Database.Tree.Delete (DelTreeCons, deleteTreeManyDef)
import           Perst.Database.Tree.Insert (InsTreeCons, insertTreeManyDef)
import           Perst.Types                (Fsts, LstFld, PChilds (..), Snds)

type TopKey sch t = DdKey (GetDS t sch)
type UpdKey sch t r =
  Tagged (BTreeFromList (TopKey sch t)) (GSType (TopKey sch t) r)
type UpdTreeCons b sch t r =
  ( InsTreeCons b sch t r
  , DelTreeCons b sch t r
  , TGetSet (DdKey (GetDS t sch)) r
  , UpdDiffCons b (GetDS t sch) r (UpdKey sch t r)
  , UpdateChilds b sch t (FldNames LstFld r) r
  , Ord (GSType (TopKey sch t) r)
  )

updateTreeManyDef ::
  (MonadCons m, GetDataStruct t sch ~ Just ds, UpdTreeCons b sch t r)
  => Proxy# '(b,sch,t) -> [r] -> [r] -> SessionMonad b m ()
updateTreeManyDef (pbt::Proxy# '(b,sch,t)) (olds::[r]) news = do
  -- undefined
  deleteTreeManyDef pbt $ filter (not . (`M.member` news') . key) olds
  updateDiffManyDef (proxy#::Proxy# '(b,GetDS t sch))
    $ map (\(o,n) -> (Tagged @(BTreeFromList (TopKey sch t)) $ key o, o, n)) us
  updateChilds @b @sch @t @(FldNames LstFld r) us
  _ <- insertTreeManyDef pbt $ filter (not . (`M.member` olds') . key) news
  return ()
 where
  key r = tget @(TopKey sch t) r
  mkMap = M.fromList . fmap (\x -> (key x, x))
  olds' = mkMap olds
  news' = mkMap news
  us  = catMaybes $ map (\o -> sequence (o, M.lookup (key o) news')) olds

class UpdateChilds b sch t fs r where
  updateChilds  :: MonadCons m => [(r,r)] -> SessionMonad b m ()

instance UpdateChilds b sch t '[] r where
  updateChilds _ = return ()

instance ( GetToRefByName t s (SchRefs sch) ~ Just ref
         , GSType '[s] r ~ PChilds v
         , TGetSet '[s] r
         , Grec v, GrecTagged v ~ tv
         , RefCols ref ~ rcols
         , Fsts rcols ~ rcols1
         , Snds rcols ~ rcols2
         , TGetSet rcols2 r, GSType rcols2 r ~ vref
         , Tagged rcols1 vref ~ chfk
         , TShowHide rcols1 tv
         , dtv ~ HiddenType rcols1 tv
         , Grec (chfk, dtv), GrecTagged (chfk, dtv) ~ tchld
         , GetDataStruct (RefFrom ref) sch ~ Just cds
         , UpdTreeCons b sch (RefFrom ref) tchld
         , UpdateChilds b sch t ss r
         )
     => UpdateChilds b sch t (s ': ss) r where
  updateChilds rs = do
    updateTreeManyDef (proxy# :: Proxy# '(b,sch,RefFrom ref)) olds news
    updateChilds @b @sch @t @ss rs
   where
    olds :: [tchld]
    news :: [tchld]
    (olds,news) = bimap concat concat $ unzip $ map (bimap newRec newRec) rs
    newRec r = ( toTagged
               . (Tagged @rcols1 (tget @rcols2 r), )
               . thide @rcols1
               . toTagged
               )
            <$> (unPChilds $ tget @'[s] r)
