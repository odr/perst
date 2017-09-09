{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

module Perst.Database.Tree.Update where

import           Data.Bifunctor             (bimap)
import qualified Data.Map.Strict            as M
import           Data.Maybe                 (catMaybes)
import           Data.Singletons.Prelude    (SingI)
import           Data.Tagged                (Tagged (..))
import           GHC.Prim                   (Proxy#, proxy#)
import           Lens.Micro.Extras          (view)

import           Data.Type.Grec             (FieldNamesConvGrec, FieldNamesGrec,
                                             GrecLens (..), GrecWith (..),
                                             GrecWithout (..),
                                             NamesGrecLens (..), gwPairs,
                                             gwTPairs)
import           Perst.Database.DbOption    (MonadCons, SessionMonad)
import           Perst.Database.DML         (DML (..), RecCons)
import           Perst.Database.Tree.Def    (FieldByName, GrecChilds, KwoR,
                                             Pair, RecParent', TdData, TopKey,
                                             TopPK, TopPKPairs, TreeDef)
import           Perst.Database.Tree.Delete (DelTreeCons, deleteTreeManyDef)
import           Perst.Database.Tree.Insert (InsTreeCons, insertTreeManyDef)
import           Perst.Types                (Fsts, Snds)

type UpdTreeCons b t k r =
    ( InsTreeCons b t k r
    , DelTreeCons b t k r
    , UpdTreeCons' b k r (TopPKPairs t (Pair k r)) (TopPK t (Pair k r))
                          (TopKey t)
    -- , UpdateChilds b (GrecChilds t (Pair k r)) k r
    )

type UpdTreeCons' b k r tpkp tpk tk =
    ( Ord tpkp
    , NamesGrecLens tk tpkp (Pair k r)
    , SingI (FieldNamesConvGrec (Tagged tk tpkp))
    , RecCons b (Tagged tk tpkp)
    )

updateTreeManyDef :: (MonadCons m, UpdTreeCons b t k r)
                => Proxy# b -> Proxy# t -> [(k,r)] -> [(k,r)] -> SessionMonad b m ()
updateTreeManyDef (pb :: Proxy# b) (pt :: Proxy# t) (olds :: [(k,r)]) news = do
  deleteTreeManyDef pb pt $ filter (not . (`M.member` news') . pairs) olds
  updateDiffMany @b @(TdData t) @r $ map (\(o,n) -> (pairsT o, o, n)) us
  -- updateChilds @b @(GrecChilds t (Pair k r)) us
  insertTreeManyDef pb pt $ filter (not . (`M.member` olds') . pairs) news
  return ()
 where
  pairs  ((k,r)::(k,r))
    = gwPairs  (GW (GWO k :: KwoR k r, r) :: TopPK t (Pair k r))
  pairsT ((k,r)::(k,r))
    = gwTPairs (GW (GWO k :: KwoR k r, r) :: TopPK t (Pair k r))
  mkMap = M.fromList . fmap (\x -> (pairs x, x))
  olds' = mkMap olds
  news' = mkMap news
  us  = catMaybes $ map (\o -> sequence (o, M.lookup (pairs o) news')) olds

class UpdateChilds b chs k r where
  updateChilds  :: MonadCons m => [((k,r),(k,r))] -> SessionMonad b m ()

instance UpdateChilds b '[] k r where
  updateChilds _ = return ()

type UpdChildCons b s td rs chs k r =
  ( UpdChildCons' b s td rs chs (RecParent' k r rs) (FieldByName s (Pair k r))
                               (Pair k r)
  , UpdateChilds b chs k r
  )

type UpdChildCons' b s td rs chs rp fld p =
  ( UpdTreeCons b td (Tagged (Fsts rs) rp) fld
  , GrecLens s [fld] p
  , NamesGrecLens (Snds rs) rp p
  )

instance UpdChildCons b s td rs chs k r
      => UpdateChilds b ('(s,'(td,rs)) ': chs) k r where
  updateChilds rs = do
    updateTreeManyDef (proxy# :: Proxy# b) (proxy# :: Proxy# td) olds news
    updateChilds @b @chs rs
   where
    (olds,news) = bimap concat concat $ unzip $ map (bimap newRec newRec) rs
    --
    newRec :: (k,r) -> [ ( Tagged (Fsts rs) (RecParent' k r rs)
                         , FieldByName s (Pair k r)
                         )
                       ]
    newRec (k,r)
      = (Tagged (namesGrecGet @(Snds rs) (GWO k :: KwoR k r, r)),)
      <$> view (grecLens @s) (GWO k :: KwoR k r, r)
