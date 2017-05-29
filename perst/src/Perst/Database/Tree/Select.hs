{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}
module Perst.Database.Tree.Select where

import           Control.Monad              (liftM2)
import           Control.Monad.Catch        (MonadMask)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Data.Bifunctor             (second)
import           Data.Kind                  (Type)
import           Data.List                  (foldl')
import           Data.Singletons.Prelude
import qualified Data.Text.Lazy             as TL

import           Data.Type.Grec
import           Perst.Database.Constraints (DelConstr)
import           Perst.Database.DataDef     (fieldNamesT, showProxy)
import           Perst.Database.DbOption    (DbOptionConstr, FieldDB,
                                             SessionMonad)
import           Perst.Database.DML         (selectMany')
import           Perst.Database.Tree.Def

type family SelTreeConstr b t r where
  SelTreeConstr b t r = ( SingI (Map FstSym0 (TreeRec r))
                        , SelectChilds b (TdChilds t) (TreeChilds r)
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


class SelectChilds b (t :: [(Symbol, (TreeDef, [(Symbol,Symbol)]))])
                      (r :: [(Symbol, TreeT)]) (eq :: Bool) where
  selectChilds :: (MonadIO m, MonadMask m)
                => Proxy eq -> Proxy t -> Proxy r -> [[FieldDB b]]
                -> SessionMonad b m [[ConvTree (FieldDB b)]]

instance SelectChilds b t '[] 'True where
  selectChilds _ _ _ _ = return []

instance (SelectChilds b ts rs (EqName ts rs), DbOptionConstr b (TdData t1)
    , SelTreeConstr b t1 r2
    , SingI (Map SndSym0 t2)
    , Show (FieldDB b)
    )
    => SelectChilds b ('(s,'(t1,t2)) ': ts) ('(s,r2) ': rs) 'True
 where
  selectChilds _ _ _ ks
    = liftM2 (:)
        (concat <$> selectTreeMany' (Proxy :: Proxy t1)
                        (Proxy :: Proxy r2) keyNames (take 1 ks))
        (selectChilds (Proxy :: Proxy (EqName ts rs)) (Proxy :: Proxy ts)
                      (Proxy :: Proxy rs) (tail ks))
   where
    keyNames = map TL.pack $ showProxy (Proxy :: Proxy (Map SndSym0 t2))

instance (SelectChilds b ts rs (EqName ts rs))
    => SelectChilds b (t ': ts) rs 'False
 where
  selectChilds _ _ = selectChilds (Proxy :: Proxy (EqName ts rs)) (Proxy :: Proxy ts)
