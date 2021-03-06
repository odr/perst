{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

module Perst.Database.Tree.Update where

import           Data.Bifunctor               (bimap)
import qualified Data.Map.Strict              as M
import           Data.Maybe                   (catMaybes)
import           Data.Singletons.Prelude      (SingI)
import           Data.Singletons.Prelude.List
import           Data.Tagged                  (Tagged (..))
import           GHC.Prim                     (Proxy#, proxy#)
import           Lens.Micro.Extras            (view)

import           Data.Type.Grec               (FieldNamesConvGrec,
                                               FieldNamesGrec, Fsts,
                                               GWSPairs (..), GWSTagged,
                                               Grec (..), GrecLens (..),
                                               GrecWith (..),
                                               GrecWithSecond (..),
                                               GrecWithout (..),
                                               NamesGrecLens (..), Snds,
                                               gwTagged, gwsTagged)
import           Perst.Database.DbOption      (MonadCons, SessionMonad)
import           Perst.Database.DML           (DML (..), RecCons)
import           Perst.Database.Tree.Delete   (DelTreeCons, deleteTreeManyDef)
import           Perst.Database.Tree.Insert   (InsTreeCons, insertTreeManyDef)
import           Perst.Database.TreeDef       (FieldByName, GrecChilds, Pair,
                                               TdData, TopKey, TopPK,
                                               TopPKPairs, TopPKTagged, TreeDef)

type UpdTreeCons b t k r =
    ( InsTreeCons b t k r
    , DelTreeCons b t k r
    , UpdTreeCons' b k r (TopPKPairs t (Pair k r))
                         (TopPKTagged t (Pair k r)) (TopKey t)
    , UpdateChilds b (GrecChilds t (Pair k r)) k r
    )

type UpdTreeCons' b k r tpkp tpk tk =
    ( Ord tpk
    , SingI (FieldNamesConvGrec tpk)
    , RecCons b tpk
    , NamesGrecLens tk tpkp (Pair k r)
    )

updateTreeManyDef :: (MonadCons m, UpdTreeCons b t k r)
                => Proxy# '(b,t) -> [(k,r)] -> [(k,r)] -> SessionMonad b m ()
updateTreeManyDef (pbt :: Proxy# '(b,t)) (olds :: [(k,r)]) news = do
  deleteTreeManyDef pbt $ filter (not . (`M.member` news') . pairs) olds
  updateDiffMany @b @(TdData t) @r $ map (\(o,n) -> (pairs o, o, n)) us
  updateChilds @b @(GrecChilds t (Pair k r)) us
  insertTreeManyDef pbt $ filter (not . (`M.member` olds') . pairs) news
  return ()
 where
  pairs  ((k,r)::(k,r)) = gwTagged (GW (GWO k, Grec r) :: TopPK t (Pair k r))
  mkMap = M.fromList . fmap (\x -> (pairs x, x))
  olds' = mkMap olds
  news' = mkMap news
  us  = catMaybes $ map (\o -> sequence (o, M.lookup (pairs o) news')) olds

class UpdateChilds b chs k r where
  updateChilds  :: MonadCons m => [((k,r),(k,r))] -> SessionMonad b m ()

instance UpdateChilds b '[] k r where
  updateChilds _ = return ()

type UpdChildCons b s td rs chs k r =
  ( UpdChildCons' b s td rs (FieldByName s (Pair k r)) (Pair k r)
  , UpdateChilds b chs k r
  )

type UpdChildCons' b s td rs fld p =
  ( UpdTreeCons b td (GWSTagged rs p) fld
  , GrecLens s [fld] p
  , NamesGrecLens (Snds rs) (GWSPairs rs p) p
  )

instance UpdChildCons b s td rs chs k r
      => UpdateChilds b ('(s,'(td,rs)) ': chs) k r where
  updateChilds rs = do
    updateTreeManyDef (proxy# :: Proxy# '(b,td)) olds news
    updateChilds @b @chs rs
   where
    (olds,news) = bimap concat concat $ unzip $ map (bimap newRec newRec) rs

    newRec :: (k,r) -> [ ( GWSTagged rs (Pair k r)
                         , FieldByName s (Pair k r)
                         )
                       ]
    newRec (k,r) = (gwsTagged (GWS (GWO k, Grec r) :: GrecWithSecond rs (Pair k r)),)
                <$> view (grecLens @s) ((GWO k, Grec r) :: Pair k r)
