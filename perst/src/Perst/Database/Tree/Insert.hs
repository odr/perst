{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}
module Perst.Database.Tree.Insert where

import           Control.Applicative        (ZipList (..), liftA2)
import           Control.Monad.Catch        (MonadMask)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Data.Functor.Compose       (Compose (..))
import           Data.Singletons.Prelude
import           Lens.Micro                 ((&), (.~))
import           Lens.Micro.Extras          (view)

import           Data.Type.Grec
import           Perst.Database.Constraints (InsConstr)
import           Perst.Database.DbOption    (SessionMonad)
import           Perst.Database.DML         (insertManyR)
import           Perst.Database.Tree.Def    (FieldByName, GrecChilds, TdData,
                                             TreeDef)

type InsertTreeConstraint m f b t r =
  ( MonadIO m, MonadMask m, Applicative f, Traversable f
  , InsConstr b (TdData t) (Grec r)
  , InsertChilds m f b (GrecChilds t r) r
  )

insertTreeMany :: InsertTreeConstraint m ZipList b t r
  => Proxy (t :: TreeDef) -> [r] -> SessionMonad b m [r]
insertTreeMany pt = fmap getZipList . insertTreeMany' pt . ZipList

insertTreeMany' :: InsertTreeConstraint m f b t r
  => Proxy (t :: TreeDef) -> f r -> SessionMonad b m (f r)
insertTreeMany' (_ :: Proxy t) (rs :: f r) = do
  insertManyR ptd rs
  insertChilds ptc rs
 where
  ptd = Proxy :: Proxy (TdData t)
  ptc = Proxy :: Proxy (GrecChilds t r)


class InsertChilds m f b chs r where
  insertChilds :: Proxy chs -> f r -> SessionMonad b m (f r)

instance Monad m => InsertChilds m f b '[] r where
  insertChilds _ = return


instance  ( InsertTreeConstraint m (Compose f ZipList) b td (FieldByName s r)
          , LensedConstraint r s [FieldByName s r]
          , InsertChilds m f b chs r
          , Applicative f
          ) => InsertChilds m f b ('(s,'(td,rs)) ': chs) r where
  insertChilds _ rs = do
    rs' <- fmap ( liftA2 (\r r' -> r & nlens fn .~ r') rs
                . fmap getZipList
                . getCompose
                )
        $ insertTreeMany' (Proxy :: Proxy td)
        $ Compose $ (ZipList . view (nlens fn)) <$> rs
    insertChilds (Proxy :: Proxy chs) rs'
   where
    ptd = Proxy :: Proxy td
    fn = FieldName :: FieldName s
    -- lns = nlens (FieldName :: FieldName s)
    -- ptr = Proxy :: Proxy (FieldByName s r)

{-
type family InsTreeConstr b t r where
  InsTreeConstr b t r = ( SingI (Map FstSym0 (TreeRec r))
                        , InsertChilds b (TdChilds t) (TreeChilds r)
                                  (EqName (TdChilds t) (TreeChilds r))
                        , CheckTree t
                        , CheckChilds t r
                        , SingI (ParentKeyNames t)
                        , Show (FieldDB b)
                        )

insertTreeMany
  ::  (MonadIO m, MonadMask m , DbOptionConstr b (TdData t)
      , SingI (Map FstSym0 (TreeRec (FieldsTree r)))
      , InsertChilds b (TdChilds t) (TreeChilds (FieldsTree r))
                (EqName (TdChilds t) (TreeChilds (FieldsTree r)))
      , Convert (Grec r) (ConvTree (FieldDB b))
      )
  => Proxy (t :: TreeDef) -> [r] -> SessionMonad b m ()
insertTreeMany (pt :: Proxy t) (rs :: [r])
  = insertTreeMany' pt (Proxy :: Proxy (FieldsTree r)) (map (convert . Grec) rs)

insertTreeMany'
  :: ( MonadIO m, MonadMask m , DbOptionConstr b (TdData t)
     , SingI (Map FstSym0 (TreeRec r))
     , InsertChilds b (TdChilds t) (TreeChilds r)
               (EqName (TdChilds t) (TreeChilds r))
     )
  => Proxy (t :: TreeDef) -> Proxy (r :: TreeT) -> [ConvTree (FieldDB b)]
  -> SessionMonad b m ()
insertTreeMany' (pt :: Proxy t) (pr :: Proxy r) cts = do
  insertMany' ptd recNames $ map ctRec cts
  insertChilds peq ptc prc $ concatMap ctChilds cts
 where
  (prc,ptc,ptd,peq,recNames) = getProxies pt pr

class InsertChilds b (t :: [(Symbol, (TreeDef, [(Symbol,Symbol)]))])
                      (r :: [(Symbol, TreeT)]) (eq :: Bool) where
  insertChilds :: (MonadIO m, MonadMask m)
               => Proxy eq -> Proxy t -> Proxy r -> [[ConvTree (FieldDB b)]]
               -> SessionMonad b m ()

instance InsertChilds b t '[] 'True where
  insertChilds _ _ _ _ = return ()

instance (InsertChilds b ts rs (EqName ts rs), DbOptionConstr b (TdData t1)
    , InsTreeConstr b t1 r2
    , SingI (Map SndSym0 t2)
    , Show (FieldDB b)
    )
    => InsertChilds b ('(s,'(t1,t2)) ': ts) ('(s,r2) ': rs) 'True
 where
  insertChilds _ _ _ ctss = do
    insertTreeMany' (Proxy :: Proxy t1) (Proxy :: Proxy r2) (head ctss)
    insertChilds    (Proxy :: Proxy (EqName ts rs)) (Proxy :: Proxy ts)
                    (Proxy :: Proxy rs) (tail ctss)

instance (InsertChilds b ts rs (EqName ts rs))
    => InsertChilds b (t ': ts) rs 'False
 where
  insertChilds _ _ = insertChilds (Proxy :: Proxy (EqName ts rs)) (Proxy :: Proxy ts)
-}
