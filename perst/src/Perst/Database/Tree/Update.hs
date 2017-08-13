{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

module Perst.Database.Tree.Update where

import           Control.Applicative        (ZipList (..))
import           Data.Bifunctor             (bimap, first)
import qualified Data.Map.Strict            as M
import           Data.Maybe                 (fromJust, isJust)
-- import           Data.Proxy                 (Proxy (..))
import           Data.Tagged                (Tagged (..))
import           GHC.Prim                   (Proxy#, proxy#)
import           Lens.Micro.Extras          (view)

import           Data.Type.Grec             (Grec (..), GrecLens (..),
                                             GrecWith (..), GrecWithout (..),
                                             NamesGrecLens (..), gwPairs)

-- import           Perst.Database.Constraints (DelByKeyConstr, UpdByKeyDiffConstr)
-- import           Perst.Database.DataDef     (DdAutoIns)
import           Perst.Database.DbOption    (MonadCons, SessionMonad)
import           Perst.Database.DML         (UpdateByPKDiff (..))
import           Perst.Database.Tree.Def    (FieldByName, GrecChilds, TdData,
                                             TopKey, TopNotPK, TopPK,
                                             TopPKPairs, TreeDef)
import           Perst.Database.Tree.Delete (DeleteTree (..))
import           Perst.Database.Tree.Insert (AddParent, InsertTree (..),
                                             RecParent)
import           Perst.Types                (Fsts, Snds)

type {- family UpdateTreeConstraint b t r where -}
  UpdateTreeConstraint b t r =
    UpdateTreeConstraint' b t r (TopPKPairs t r) (GrecChilds t r) (TopPK t r) (TopKey t) (TdData t)

type {- family UpdateTreeConstraint' b t r tpkp gc tpk tk d where -}
  UpdateTreeConstraint' b t r tpkp gc tpk tk d =
    ( Ord tpkp
    , InsertTree b t r
    , DeleteTree b t r
    , NamesGrecLens tk tpkp tpk
    , UpdateByPKDiff b d r
    , UpdateChilds b gc r
    )

{-
updateTree doesn't return data because
- there are some difficults in implementation
- anyway better to select data after update because of the
  possibility of db-triggers and so on
Maybe in future we can change it...
-}
class UpdateTree b t r where
  updateTreeMany :: MonadCons m => [r] -> [r] -> SessionMonad b m ()
  default updateTreeMany :: (MonadCons m, UpdateTreeConstraint b t r)
                          => [r] -> [r] -> SessionMonad b m ()
  updateTreeMany = updateTreeManyDef (proxy# :: Proxy# b) (proxy# :: Proxy# t)

class UpdateTree b t (Grec r) => UpdateTreeR b t r where
  updateTreeManyR :: MonadCons m => [r] -> [r] -> SessionMonad b m ()
  updateTreeManyR olds = updateTreeMany @b @t (map Grec olds) . map Grec

instance UpdateTree b t (Grec r) => UpdateTreeR b t r

updateTreeManyDef :: (MonadCons m, UpdateTreeConstraint b t r)
                => Proxy# b -> Proxy# t -> [r] -> [r] -> SessionMonad b m ()
updateTreeManyDef (_ :: Proxy# b) (_ :: Proxy# t) (olds :: [r]) news = do
  deleteTreeMany @b @t $ filter (not . (`M.member` news') . pairs) olds
  updateByPKDiffMany @b @(TdData t) us
  updateChilds @b @(GrecChilds t r) us
  insertTreeMany @b @t $ filter (not . (`M.member` olds') . pairs) news
  return ()
 where
  pairs (n :: r) = gwPairs (GW n :: TopPK t r)
  mkMap = M.fromList . fmap (\r -> (pairs r, r))
  olds' = mkMap olds
  news' = mkMap news
  us  = map (first fromJust)
      $ filter (isJust . fst)
      $ fmap (\n -> (M.lookup (pairs n) olds', n)) news


class UpdateChilds b chs r where
  updateChilds  :: MonadCons m => [(r,r)] -> SessionMonad b m ()

instance UpdateChilds b '[] r where
  updateChilds _ = return ()

type {- family UpdChildCons b s td rs chs r where -}
  UpdChildCons b s td rs chs r
    = UpdChildCons' b s td rs chs r (RecParent r rs) (FieldByName s r)

type {- family UpdChildCons' b s td rs chs r rp fld where -}
  UpdChildCons' b s td rs chs r rp fld =
    ( UpdateTree b td (AddParent (Fsts rs) rp fld)
    , GrecLens s [fld] r
    , UpdateChilds b chs r
    , NamesGrecLens (Snds rs) rp r
    )
instance UpdChildCons b s td rs chs r
      => UpdateChilds b ('(s,'(td,rs)) ': chs) r where
  updateChilds rs = do
    updateTreeMany @b @td olds news
    updateChilds @b @chs rs
   where
    (olds,news) = bimap concat concat $ unzip $ map (bimap newRec newRec) rs
    --
    newRec :: r -> [AddParent (Fsts rs) (RecParent r rs) (FieldByName s r)]
    newRec r = (\r' -> (tr, GWO (Grec r'))) <$> view (grecLens @s) r
     where
      tr = Tagged (namesGrecGet @(Snds rs) r)
