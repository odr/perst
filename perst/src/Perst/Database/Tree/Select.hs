{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}
module Perst.Database.Tree.Select (selectTreeMany) where

import           Control.Applicative        (ZipList (..), liftA2)
import           Control.Monad.Catch        (MonadMask)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Data.Bifunctor             (second)
import           Data.Functor.Compose       (Compose (..))
import           Data.Singletons.Prelude    (FstSym0, Map, Proxy (..), Symbol)
import           Data.Tagged                (Tagged (..), retag)
import           Data.Traversable           (Traversable (..))
import           Lens.Micro                 ((.~))

import           Data.Type.Grec             (FieldName (..), Grec (..),
                                             GrecWith (..), LensedConstraint,
                                             nlens)
import           Perst.Database.Constraints (SelConstr)
import           Perst.Database.DbOption    (SessionMonad)
import           Perst.Database.DML         (selectMany)
import           Perst.Database.Tree.Def    (ChildByParents, FieldByName,
                                             GrecChilds, TaggedAllParentKeys,
                                             TdData)

type SelectTreeConstraint m f b t r k =
  ( MonadIO m, MonadMask m, Applicative f, Traversable f
  , SelConstr b (TdData t) (TaggedAllParentKeys t, Grec r) k
  , SelectChilds m (Compose f ZipList) b (GrecChilds t r) (TaggedAllParentKeys t) r
  )

selectTreeMany :: SelectTreeConstraint m ZipList b t r k
  => Proxy t -> Proxy r -> [k] -> SessionMonad b m [[r]]
selectTreeMany pt pr  = fmap getZipList . selectTreeMany' pt pr . ZipList

selectTreeMany' :: SelectTreeConstraint m f b t r k
  => Proxy t -> Proxy r -> f k -> SessionMonad b m (f [r])
selectTreeMany' (_ :: Proxy t) (_ :: Proxy r) ks
  = Compose . fmap (ZipList . map (second unGrec)) <$> selectMany ptd pkr ks
  >>= fmap (fmap getZipList . getCompose . fmap snd) . selectChilds ptc
 where
  ptd = Proxy :: Proxy (TdData t)
  pkr = Proxy :: Proxy (TaggedAllParentKeys t, Grec r)
  ptc = Proxy :: Proxy (GrecChilds t r)

class SelectChilds m f b chs ks r where
  selectChilds :: Proxy chs -> f (ks,r) -> SessionMonad b m (f (ks,r))

instance Monad m => SelectChilds m f b '[] ks r where
  selectChilds _ = return

instance  ( SelectTreeConstraint m f b td (FieldByName s r)
              (GrecWith (Map FstSym0 rs) (Tagged (ChildByParents rs nk) vk))
          , LensedConstraint r s [FieldByName s r]
          , SelectChilds m f b chs (Tagged nk vk) r
          )
    => SelectChilds m f b ('(s,'(td,rs)) ': chs) (Tagged nk vk) r where
  selectChilds _ compKR
    = liftA2 updRec compKR <$> selectTreeMany' ptd ptr newkey
    >>= selectChilds (Proxy :: Proxy chs)
   where
    ptd = Proxy :: Proxy td
    ptr = Proxy :: Proxy (FieldByName s r)
    newkey = fmap
      ( (GW :: Tagged (ChildByParents rs nk) vk
            -> GrecWith (Map FstSym0 rs) (Tagged (ChildByParents rs nk) vk))
      . (retag :: Tagged nk vk -> Tagged (ChildByParents rs nk) vk)
      . fst
      ) compKR
    updRec k' r' = second (nlens (FieldName :: FieldName s) .~ r') k'







  -- -- f :: SelConstr b (TdData (Fst (Child s t))) tt k
  -- --   => Proxy ('(s, tt) :: (Symbol, Type)) -> (tt :: Type) -> SessionMonad b m [[tt]]
  -- f (_ :: Proxy ('(s,tt) :: (Symbol, Type))) r
  --   = selectTreeMany
  --       (Proxy :: Proxy (Fst (Child s t)))
  --       (Proxy :: Proxy tt)
  --       []

-- f :: c a => (c a => a -> b) -> a -> b
-- f :: (a -> b) -> a -> b
-- f g a = g a
--
-- f1 :: Show a => a -> String
-- f1 = show
-- test = f f1 (5::Int) :: String
--
-- class SelectChilds b (t :: [(Symbol, (TreeDef, [(Symbol,Symbol)]))]) r where
--   selectChilds :: (MonadIO m, MonadMask m) => Proxy t -> r -> SessionMonad b m r



{-
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
  recLen = length recN
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
-}
