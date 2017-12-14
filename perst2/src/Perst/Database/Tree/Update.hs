{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Perst.Database.Tree.Update where

import           Data.Bifunctor             (bimap)
import           Data.Kind                  (Type)
import qualified Data.Map.Strict            as M
import           Data.Maybe                 (catMaybes)
import           Data.Tagged                (Tagged (..))
import           GHC.Prim                   (Proxy#, proxy#)
import           Lens.Micro.Extras          (view)

import           Data.Type.GrecTree         (BTreeFromList, ConvNames (..),
                                             Grec (..), LType, TGetSet (..),
                                             TLens' (..))
import           Perst.Database.DbOption    (MonadCons, SessionMonad)
import           Perst.Database.DML.Update  (UpdDiffCons, updateDiffManyDef)
import           Perst.Database.Tree.Delete (DelTreeCons, deleteTreeManyDef)
import           Perst.Database.Tree.Insert (InsTreeCons, insertTreeManyDef)
import           Perst.Database.TreeDef     (MbChild, TdData, TopKey, TreeDef)
import           Perst.Types                (Fsts, LstFld, PChilds (..), Snds)

type UpdKey t r = Tagged (BTreeFromList (TopKey t)) (GSType (TopKey t) r)
type UpdTreeCons b t r =
  ( InsTreeCons b t r
  , DelTreeCons b t r
  , TGetSet (TopKey t) r
  , UpdDiffCons b (TdData t) r (UpdKey t r)
  , UpdateChilds b t (FldNames LstFld r) r
  , Ord (GSType (TopKey t) r)
  )

updateTreeManyDef :: (MonadCons m, UpdTreeCons b t r)
                => Proxy# ('(b,t) :: (Type,TreeDef)) -> [r] -> [r] -> SessionMonad b m ()
updateTreeManyDef (pbt::Proxy# '(b,t)) (olds::[r]) news = do
  -- undefined
  deleteTreeManyDef pbt $ filter (not . (`M.member` news') . key) olds
  updateDiffManyDef (proxy#::Proxy# '(b,TdData t))
    $ map (\(o,n) -> (Tagged @(BTreeFromList (TopKey t)) $ key o, o, n)) us
  updateChilds @b @t @(FldNames LstFld r) us
  _ <- insertTreeManyDef pbt $ filter (not . (`M.member` olds') . key) news
  return ()
 where
  key r = tget @(TopKey t) r
  mkMap = M.fromList . fmap (\x -> (key x, x))
  olds' = mkMap olds
  news' = mkMap news
  us  = catMaybes $ map (\o -> sequence (o, M.lookup (key o) news')) olds

class UpdateChilds b (t::TreeDef) fs r where
  updateChilds  :: MonadCons m => [(r,r)] -> SessionMonad b m ()

instance UpdateChilds b t '[] r where
  updateChilds _ = return ()

instance ( MbChild s t ~ Just '(td,ref)
         , TLens' s r
         , LType s r ~ PChilds v
         , Grec v, GrecTagged v ~ tv
         , TGetSet (Snds ref) r, GSType (Snds ref) r ~ vref
         , Tagged (Fsts ref) vref ~ chfk
         , Grec (chfk, tv), GrecTagged (chfk, tv) ~ tchld
         , UpdTreeCons b td tchld
         , UpdateChilds b t ss r
         )
     => UpdateChilds b t (s ': ss) r where
  updateChilds rs = do
    updateTreeManyDef (proxy# :: Proxy# '(b,td)) olds news
    updateChilds @b @t @ss rs
   where
    (olds,news) = bimap concat concat $ unzip $ map (bimap newRec newRec) rs
    newRec r = (toTagged . (Tagged @(Fsts ref) (tget @(Snds ref) r), ))
            <$> (map toTagged $ unPChilds $ view (tlens' @s) r)
