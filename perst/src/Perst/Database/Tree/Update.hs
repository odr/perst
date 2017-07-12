{-# LANGUAGE ExistentialQuantification #-}
-- {-# LANGUAGE RankNTypes                #-}
-- {-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE UndecidableInstances      #-}

module Perst.Database.Tree.Update where

import           Control.Applicative        (ZipList (..))
import           Data.Bifunctor             (bimap, first)
import qualified Data.Map.Strict            as M
import           Data.Maybe                 (fromJust, isJust)
import           Data.Proxy                 (Proxy (..))
import           Data.Tagged                (Tagged (..))
import           Lens.Micro.Extras          (view)

import           Data.Type.Grec             (Grec (..), GrecLens (..),
                                             GrecWith (..), GrecWithout (..),
                                             NamesGrecLens (..), gwPairs)

import           Perst.Database.Constraints (UpdByKeyDiffConstr)
import           Perst.Database.DbOption    (SessionMonad)
import           Perst.Database.DML         (updateByPKDiffMany)
import           Perst.Database.Tree.Def    (FieldByName, GrecChilds, TdData,
                                             TopKey, TopNotPK, TopPK,
                                             TopPKPairs, TreeDef)
import           Perst.Database.Tree.Delete (DeleteTreeConstraint,
                                             deleteTreeMany)
import           Perst.Database.Tree.Insert (AddParent, InsertTreeConstraint,
                                             RecParent, Snds, insertTreeMany)

type UpdateTreeConstraint m b t r =
  ( InsertTreeConstraint m ZipList b t r
  , DeleteTreeConstraint m ZipList b t r
  , Ord (TopPKPairs t r)
  , NamesGrecLens (TopKey t) (TopPKPairs t r) (TopPK t r)
  , UpdByKeyDiffConstr m b (TdData t) (TopNotPK t r) (TopPK t r)
  , UpdateChilds m b (GrecChilds t r) r
  )

{-
updateTree doesn't return data because
- there are some difficults in implementation
- anyway better to select data after update because of the possible db-triggers and so on
Maybe in future we can change it...
-}


updateTreeManyR :: (UpdateTreeConstraint m b t (Grec r))
                => Proxy (t :: TreeDef) -> [r] -> [r] -> SessionMonad b m ()
updateTreeManyR pt olds = updateTreeMany pt (map Grec olds)
                        . map Grec

-- updateTreeMany return list of inserted record with keys
updateTreeMany :: (UpdateTreeConstraint m b t r)
                => Proxy (t :: TreeDef) -> [r] -> [r] -> SessionMonad b m ()
updateTreeMany (pt :: Proxy t) (olds :: [r]) (news :: [r]) = do
  deleteTreeMany pt $ filter (not . (`M.member` news') . pairs) olds
  updateByPKDiffMany (Proxy :: Proxy (TdData t)) us
  updateChilds (Proxy :: Proxy (GrecChilds t r)) us
  insertTreeMany pt $ filter (not . (`M.member` olds') . pairs) news
  return ()
 where
  pairs (n :: r) = gwPairs (GW n :: TopPK t r)
  mkMap = M.fromList . fmap (\r -> (pairs r, r))
  olds' = mkMap olds
  news' = mkMap news
  us  = map (first fromJust)
      $ filter (isJust . fst)
      $ fmap (\n -> (M.lookup (pairs n) olds', n)) news


class UpdateChilds m b chs r where
  updateChilds  :: Proxy chs -> [(r,r)] -> SessionMonad b m ()

instance Monad m => UpdateChilds m b '[] r where
  updateChilds _ _ = return ()

instance  ( UpdateTreeConstraint m b td (AddParent s rs r)
          , GrecLens s [FieldByName s r] r
          , UpdateChilds m b chs r
          , NamesGrecLens (Snds rs) (RecParent r rs) r
          )
          => UpdateChilds m b ('(s,'(td,rs)) ': chs) r where
  updateChilds _ rs = do
    updateTreeMany (Proxy :: Proxy td) olds news
    updateChilds (Proxy :: Proxy chs) rs
   where
    (olds,news) = bimap concat concat $ unzip $ map (bimap newRec newRec) rs
    --
    fn = Proxy :: Proxy s
    newRec :: r -> [AddParent s rs r]
    newRec r = (\r' -> (tr, GWO (Grec r'))) <$> view (grecLens fn) r
     where
      tr = Tagged (namesGrecGet (Proxy :: Proxy (Snds rs)) r)
