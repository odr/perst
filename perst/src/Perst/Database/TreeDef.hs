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

import           Control.Applicative           (ZipList (..))
import           Control.Arrow                 ((&&&))
import           Control.Monad                 (liftM2)
import           Control.Monad.Catch           (MonadMask)
import           Control.Monad.IO.Class        (MonadIO (..))
import           Data.Bifunctor                (second)
import           Data.Kind                     (Type)
import           Data.List                     (foldl')
import           Data.Maybe                    (fromMaybe)
import           Data.Singletons.Prelude
import           Data.Singletons.Prelude.List
import           Data.Singletons.Prelude.Maybe
import           Data.Singletons.TH
import qualified Data.Text.Lazy                as TL

import           Data.Type.Grec
-- (ConvFromGrec (..),
--                                                ConvToGrec (..), ConvTree (..),
--                                                Convert (..), FieldsTree,
--                                                Grec (..), TreeChilds, TreeRec,
--                                                TreeT, TreeT' (..))

import           Perst.Database.Constraints    (DelConstr)
import           Perst.Database.DataDef
-- (DataDef', fieldNamesT, showProxy)
import           Perst.Database.DbOption       (DbOptionConstr, FieldDB,
                                                SessionMonad)
import           Perst.Database.DML            (insertMany', selectMany')
import           Perst.Types

singletons [d|
  data TreeDef' s t = TreeDefC (DataDef' s t) [(s, (TreeDef' s t, [(s,s)]))]

  tdData    (TreeDefC d _) = d
  tdChilds  (TreeDefC _ c) = c

  parentKeyNames t = map (map fst . snd . snd) $ tdChilds t

  bindMb mv f = case mv of
    Nothing -> Nothing
    Just v  -> f v

  |]

promoteOnly [d|
  checkTree' :: (Eq s) => TreeDef' s t -> Bool
  checkTree' t
    = all (\(_,(a,b)) -> isSub (map fst b) (ddFlds $ tdData t)
                      && checkTree' a
          ) $ tdChilds t

  -- checkChilds' :: Eq s
  --   => ([(s,t)] -> [(s,t)] -> c) -> TreeDef' s t -> TreeT' s t -> Maybe [c]
  -- checkChilds' f (TreeDefC tr tc) (TreeTC rr rc)
  --   = foldr (\(s,treet) mcs
  --           -> bindMb mcs
  --           $ \cs  -> bindMb (lookup s tc)
  --           $ \tp  -> bindMb (checkChilds' f (fst tp) treet)
  --           $ \cs' -> Just (cs ++ cs')
  --       ) (Just [f (ddRec tr) rr]) rc

  checkChilds' :: Eq s
    => ([(s,t)] -> [(s,t)] -> c) -> TreeDef' s t -> TreeT' s t -> [c]
  checkChilds' f (TreeDefC tr tc) (TreeTC rr rc)
    = foldr (\(s,treet) cs ->
            let tp = fromMaybe
                      (error "Record's field name doesn't fit any name of childs in tree definition")
                      (lookup s tc) in
              checkChilds' f (fst tp) treet ++ cs
        ) [f (ddRec tr) rr] rc

  eqName :: Eq s => [(s, (TreeDef' s t, [(s,s)]))] -> [(s, TreeT' s t)] -> Bool
  eqName _ [] = True
  eqName [] _ = error "Record child not in the list of references"
  eqName ((st,_) : _) ((sr,_) : _) = st == sr

  |]
checkChilds' :: Eq s
  => ([(s,t)] -> [(s,t)] -> c) -> TreeDef' s t -> TreeT' s t -> [c]
checkChilds' f (TreeDefC tr tc) (TreeTC rr rc)
  = foldr (\(s,treet) cs ->
          let tp = fromMaybe
                    (error "Record's field name doesn't fit any name of childs in tree definition")
                    (lookup s tc) in
            checkChilds' f (fst tp) treet ++ cs
      ) [f (ddRec tr) rr] rc

type TreeDef = TreeDef' Symbol Type

type CheckTree a = CheckTree' a ~ True
type CheckChilds a b = FromConsList (CheckChilds' ContainSym0 a b)

{-
data TreeT' s t = TreeTC [(s,t)] [(s, TreeT' s t)]
type TreeT = TreeT' Symbol Type
type FieldsTree a = GFieldsTree (Rep a) -- :: TreeT

data ConvTree a = ConvTree [a] [[ConvTree a]] deriving (Eq, Show)

selectMany' :: (MonadIO m, MonadMask m, DelConstr b t k)
            => Proxy t -> [TL.Text] -> [k] -> SessionMonad b m [[[FieldDB b]]]
-}

getProxies :: SingI (Map FstSym0 (TreeRec r))
  => Proxy (t :: TreeDef) -> Proxy (r :: TreeT) ->
  (Proxy (TreeChilds r), Proxy (TdChilds t), Proxy (TdData t)
  , Proxy (EqName (TdChilds t) (TreeChilds r))
  , [TL.Text])
getProxies (pt :: Proxy t) (pr :: Proxy r) = (Proxy, Proxy, Proxy, Proxy
    , map TL.pack $ showProxy (Proxy :: Proxy (Map FstSym0 (TreeRec r))))

type family SelTreeConstr b t r where
  SelTreeConstr b t r = ( SingI (Map FstSym0 (TreeRec r))
                        , ProcessChilds b (TdChilds t) (TreeChilds r)
                                  (EqName (TdChilds t) (TreeChilds r))
                        , CheckTree t
                        , CheckChilds t r
                        , SingI (ParentKeyNames t)
                        , Show (FieldDB b)
                        )

selectTreeMany
  :: (MonadIO m, MonadMask m, Convert (ConvTree (FieldDB b)) (Grec r)
     , DelConstr b (TdData t) k, SelTreeConstr b t (FieldsTree r)
     )
  => Proxy (t :: TreeDef) -> Proxy (r :: Type) -> [k] -> SessionMonad b m [[r]]
selectTreeMany (pt :: Proxy t) (pr :: Proxy r) (ks :: [k])
  = map (map (unGrec . convert))
  <$> selectTreeMany' pt
                      (Proxy :: Proxy (FieldsTree r))
                      (fieldNamesT (Proxy :: Proxy k))
                      (map convFromGrec ks)

selectTreeMany'
    :: ( MonadIO m, MonadMask m
       , DbOptionConstr b (TdData t), SelTreeConstr b t r
       )
    => Proxy (t :: TreeDef) -> Proxy (r :: TreeT) -> [TL.Text] -> [[FieldDB b]]
    -> SessionMonad b m [[ConvTree (FieldDB b)]]
selectTreeMany' (pt :: Proxy t) (pr :: Proxy r) keyNames keys
  = selectMany' ptd fldNames keyNames keys
      >>= mapM (mapM $ fmap (uncurry ConvTree)
                     . sequence
                     . second (selectChilds peq ptc prc)
                     . recAndKeys)
 where
  (prc,ptc,ptd,peq,recNames) = getProxies pt pr
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
                (EqName (TdChilds t) (TreeChilds (FieldsTree r)))
      , Convert (Grec r) (ConvTree (FieldDB b))
      )
  => Proxy (t :: TreeDef) -> [r] -> SessionMonad b m ()
insertTreeMany (pt :: Proxy t) (rs :: [r])
  = insertTreeMany' pt (Proxy :: Proxy (FieldsTree r)) (map (convert . Grec) rs)

insertTreeMany'
  :: ( MonadIO m, MonadMask m , DbOptionConstr b (TdData t)
     , SingI (Map FstSym0 (TreeRec r))
     , ProcessChilds b (TdChilds t) (TreeChilds r)
               (EqName (TdChilds t) (TreeChilds r))
     )
  => Proxy (t :: TreeDef) -> Proxy (r :: TreeT) -> [ConvTree (FieldDB b)]
  -> SessionMonad b m ()
insertTreeMany' (pt :: Proxy t) (pr :: Proxy r) cts = do
  insertMany' ptd recNames $ map ctRec cts
  insertChilds peq ptc prc $ concatMap ctChilds cts
 where
  (prc,ptc,ptd,peq,recNames) = getProxies pt pr

class ProcessChilds b (t :: [(Symbol, (TreeDef, [(Symbol,Symbol)]))])
                      (r :: [(Symbol, TreeT)]) (eq :: Bool) where
  selectChilds :: (MonadIO m, MonadMask m)
                => Proxy eq -> Proxy t -> Proxy r -> [[FieldDB b]]
                -> SessionMonad b m [[ConvTree (FieldDB b)]]
  insertChilds :: (MonadIO m, MonadMask m)
               => Proxy eq -> Proxy t -> Proxy r -> [[ConvTree (FieldDB b)]]
               -> SessionMonad b m ()

instance ProcessChilds b t '[] 'True where
  selectChilds _ _ _ _ = return []
  insertChilds _ _ _ _ = return ()

instance (ProcessChilds b ts rs (EqName ts rs), DbOptionConstr b (TdData t1)
    , SelTreeConstr b t1 r2
    , SingI (Map SndSym0 t2)
    , Show (FieldDB b)
    )
    => ProcessChilds b ('(s,'(t1,t2)) ': ts) ('(s,r2) ': rs) 'True
 where
  selectChilds _ _ _ ks
    = liftM2 (:)
        (concat <$> selectTreeMany' (Proxy :: Proxy t1)
                        (Proxy :: Proxy r2) keyNames (take 1 ks))
        (selectChilds (Proxy :: Proxy (EqName ts rs)) (Proxy :: Proxy ts)
                      (Proxy :: Proxy rs) (tail ks))
   where
    keyNames = map TL.pack $ showProxy (Proxy :: Proxy (Map SndSym0 t2))

  insertChilds _ _ _ ctss = do
    insertTreeMany' (Proxy :: Proxy t1) (Proxy :: Proxy r2) (head ctss)
    insertChilds    (Proxy :: Proxy (EqName ts rs)) (Proxy :: Proxy ts)
                    (Proxy :: Proxy rs) (tail ctss)

instance (ProcessChilds b ts rs (EqName ts rs)
    -- , DbOptionConstr b (TdData t1)
    -- , SelTreeConstr b t1 r2
    -- , SingI (Map SndSym0 t2)
    -- , Show (FieldDB b)
    )
    => ProcessChilds b (t ': ts) rs 'False
 where
  selectChilds _ _ = selectChilds (Proxy :: Proxy (EqName ts rs)) (Proxy :: Proxy ts)
  insertChilds _ _ = insertChilds (Proxy :: Proxy (EqName ts rs)) (Proxy :: Proxy ts)
