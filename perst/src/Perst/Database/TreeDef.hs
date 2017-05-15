{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# OPTIONS_GHC -fprint-explicit-kinds #-}

module Perst.Database.TreeDef
    ( TreeDef'(..), TreeDef --, TdData, TdChilds, ParentKeyNames
    , selectTreeMany, insertTreeMany
    )
    where

import           Control.Applicative        (ZipList (..))
import           Control.Arrow              ((&&&))
import           Control.Monad              (liftM2)
import           Control.Monad.Catch        (MonadMask)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Data.Bifunctor             (second)
import           Data.Kind                  (Type)
import           Data.List                  (foldl')
import           Data.Singletons.Prelude
import           Data.Singletons.TH
import qualified Data.Text.Lazy             as TL

import           Data.Type.Grec             (ConvFromGrec (..), ConvToGrec (..),
                                             ConvTree (..), Convert (..),
                                             FieldsTree, Grec (..), TreeChilds,
                                             TreeRec, TreeT, TreeT' (..))

import           Perst.Database.Constraints (DelConstr)
import           Perst.Database.DataDef     (DataDef', fieldNamesT, showProxy)
import           Perst.Database.DbOption    (DbOptionConstr, FieldDB,
                                             SessionMonad)
import           Perst.Database.DML         (insertMany', selectMany')

singletons [d|
  data TreeDef' s t = TreeDefC (DataDef' s t) [(TreeDef' s t, [(s,s)])]

  tdData    (TreeDefC d _) = d
  tdChilds  (TreeDefC _ c) = c

  parentKeyNames t = map (map fst . snd) $ tdChilds t
  |]

type TreeDef = TreeDef' Symbol Type

{-
data TreeT' s t = TreeT { treeRec     :: [(s,t)]
                        , treeChilds  :: [(s, TreeT' s t)]
                        }
type TreeT = TreeT' Symbol Type
type FieldsTree a = GFieldsTree (Rep a) -- :: TreeT

data ConvTree a = ConvTree [a] [[ConvTree a]] deriving (Eq, Show)

selectMany' :: (MonadIO m, MonadMask m, DelConstr b t k)
            => Proxy t -> [TL.Text] -> [k] -> SessionMonad b m [[[FieldDB b]]]
-}

getProxies :: SingI (Map FstSym0 (TreeRec r))
  => Proxy (t :: TreeDef) -> Proxy (r :: TreeT) ->
  (Proxy (TreeChilds r), Proxy (TdChilds t), Proxy (TdData t), [TL.Text])
getProxies (pt :: Proxy t) (pr :: Proxy r) = (Proxy, Proxy, Proxy
    , map TL.pack $ showProxy (Proxy :: Proxy (Map FstSym0 (TreeRec r))))

selectTreeMany
  :: (MonadIO m, MonadMask m
     , Convert (ConvTree (FieldDB b)) (Grec r)
     , SingI (Map FstSym0 (TreeRec (FieldsTree r)))
     , DelConstr b (TdData t) k
     , ProcessChilds b (TdChilds t) (TreeChilds (FieldsTree r))
     , SingI (ParentKeyNames t)
     , Show (FieldDB b)
     )
  => Proxy (t :: TreeDef) -> Proxy (r :: Type) -> [k] -> SessionMonad b m [[r]]
selectTreeMany (pt :: Proxy t) (pr :: Proxy r) (ks :: [k])
  = map (map (unGrec . convert))
  <$> selectTreeMany' pt
                      (Proxy :: Proxy (FieldsTree r))
                      (fieldNamesT (Proxy :: Proxy k))
                      (map convFromGrec ks)

selectTreeMany' :: ( MonadIO m, MonadMask m , DbOptionConstr b (TdData t)
                   , SingI (Map FstSym0 (TreeRec r))
                   , SingI (ParentKeyNames t)
                   , ProcessChilds b (TdChilds t) (TreeChilds r)
                   , Show (FieldDB b)
                   )
                => Proxy (t :: TreeDef) -> Proxy (r :: TreeT)
                -> [TL.Text] -> [[FieldDB b]]
                -> SessionMonad b m [[ConvTree (FieldDB b)]]
selectTreeMany' (pt :: Proxy t) (pr :: Proxy r) keyNames keys
  = selectMany' ptd fldNames keyNames keys
      >>= mapM (mapM $ fmap (uncurry ConvTree)
                     . sequence
                     . second (selectChilds ptc prc)
                     . recAndKeys)
 where
  (prc,ptc,ptd,recNames) = getProxies pt pr
  parentKeyNames  = map (map TL.pack)
                  $ showProxy (Proxy :: Proxy (ParentKeyNames t))
  fldNames = recNames `mappend` mconcat parentKeyNames
  recLen = length recNames
  parentKeyLens = map length parentKeyNames
  recAndKeys rs = (r1, splitByCnt rs1 parentKeyLens)
   where
    (r1,rs1) = splitAt recLen rs
  splitByCnt xs = reverse . fst
      . foldl'(\(res, rest) l ->
          let (rn, rsn) = splitAt l rest in (rn : res, rsn)
        ) ([], xs)

insertTreeMany
  ::  (MonadIO m, MonadMask m , DbOptionConstr b (TdData t)
      , SingI (Map FstSym0 (TreeRec (FieldsTree r)))
      , ProcessChilds b (TdChilds t) (TreeChilds (FieldsTree r))
      , Convert (Grec r) (ConvTree (FieldDB b))
      )
  => Proxy (t :: TreeDef) -> [r] -> SessionMonad b m ()
insertTreeMany (pt :: Proxy t) (rs :: [r])
  = insertTreeMany' pt (Proxy :: Proxy (FieldsTree r)) (map (convert . Grec) rs)

insertTreeMany'
  :: ( MonadIO m, MonadMask m , DbOptionConstr b (TdData t)
     , SingI (Map FstSym0 (TreeRec r))
     , ProcessChilds b (TdChilds t) (TreeChilds r)
     )
  => Proxy (t :: TreeDef) -> Proxy (r :: TreeT) -> [ConvTree (FieldDB b)]
  -> SessionMonad b m ()
insertTreeMany' (pt :: Proxy t) (pr :: Proxy r) cts = do
  insertMany' ptd recNames $ map ctRec cts
  insertChilds ptc prc $ concatMap ctChilds cts
 where
  (prc,ptc,ptd,recNames) = getProxies pt pr

class ProcessChilds b (t :: [(TreeDef, [(Symbol,Symbol)])])
                      (r :: [(Symbol, TreeT)]) where
  selectChilds :: (MonadIO m, MonadMask m)
                => Proxy t -> Proxy r -> [[FieldDB b]]
                -> SessionMonad b m [[ConvTree (FieldDB b)]]
  insertChilds :: (MonadIO m, MonadMask m)
               => Proxy t -> Proxy r -> [[ConvTree (FieldDB b)]]
               -> SessionMonad b m ()

instance ProcessChilds b '[] '[] where
  selectChilds _ _ _ = return []
  insertChilds _ _ _ = return ()

instance (ProcessChilds b ts rs, DbOptionConstr b (TdData t1)
    , SingI (ParentKeyNames t1), SingI (Map FstSym0 (TreeRec r2))
    , ProcessChilds b (TdChilds t1) (TreeChilds r2)
    , SingI (Map SndSym0 t2)
    , Show (FieldDB b)
    )
    => ProcessChilds b ('(t1,t2) ': ts) ('(r1,r2) ': rs)
 where
  selectChilds _ _ ks
    = liftM2 (:)
        (concat <$> selectTreeMany' (Proxy :: Proxy t1)
                        (Proxy :: Proxy r2) keyNames (take 1 ks))
        (selectChilds (Proxy :: Proxy ts) (Proxy :: Proxy rs) (tail ks))
   where
    keyNames = map TL.pack $ showProxy (Proxy :: Proxy (Map SndSym0 t2))

  insertChilds _ _ ctss = do
    insertTreeMany' (Proxy :: Proxy t1) (Proxy :: Proxy r2) (head ctss)
    insertChilds    (Proxy :: Proxy ts) (Proxy :: Proxy rs) (tail ctss)
