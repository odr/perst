{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE UndecidableInstances      #-}
module Perst.Database.Tree.Insert where

import           Control.Applicative           (ZipList (..), liftA2)
import           Data.Functor.Compose          (Compose (..))
import           Data.Maybe                    (fromMaybe)
import           Data.Singletons.Prelude
import           Data.Singletons.Prelude.Maybe (FromMaybeSym0)
import           Data.Singletons.TH            (promoteOnly)
import           Data.Tagged                   (Tagged (..))
import           GHC.Prim                      (Proxy#, proxy#)
import           Lens.Micro                    ((&), (.~))
import           Lens.Micro.Extras             (view)

import           Data.Type.Grec                (FieldsGrec, FieldsGrecSym0,
                                                Grec (..), GrecLens (..),
                                                GrecWith (..), GrecWithout (..),
                                                ListToPairs, NamesGrecLens (..),
                                                Submap, SubmapSym0)
-- import           Perst.Database.Constraints    (InsConstr)
import           Perst.Database.DataDef        (DataAutoIns, DataKey)
import           Perst.Database.DbOption       (GenKey, MonadCons, SessionMonad)
import           Perst.Database.DML            (Insert (..))
import           Perst.Database.Tree.Def       (AppCons, FieldByName,
                                                GrecChilds, TdData, TopKey,
                                                TopKey, TreeDef)
import           Perst.Types                   (Fsts, Snds)

type InsertTreeConstraint' b t r =
  ( Insert b (TdData t) r
  , InsertChilds b (DataAutoIns (TdData t)) (GrecChilds t r) (TopKey t) r
  )

type {- family InsertTreeConstraint m f b t r where -}
  InsertTreeConstraint m f b t r =
    ( AppCons f
    , MonadCons m
    , InsertTreeConstraint' b t r
    )

class InsertTree b t r where
  insertTreeManyF :: (MonadCons m , AppCons f) => f r -> SessionMonad b m (f r)
  default insertTreeManyF :: InsertTreeConstraint m f b t r
                          => f r -> SessionMonad b m (f r)
  insertTreeManyF = insertTreeManyDef (proxy# :: Proxy# b) (proxy# :: Proxy# t)

  insertTreeMany  :: MonadCons m => [r] -> SessionMonad b m [r]
  insertTreeMany = fmap getZipList . insertTreeManyF @b @t . ZipList

instance InsertTreeConstraint' b t (r1,r2) => InsertTree b t (r1,r2)
instance InsertTreeConstraint' b t (Tagged ns r) => InsertTree b t (Tagged ns r)
instance InsertTreeConstraint' b t (GrecWith ns r) => InsertTree b t (GrecWith ns r)
instance InsertTreeConstraint' b t (GrecWithout ns r) => InsertTree b t (GrecWithout ns r)


class InsertTree b t (Grec r) => InsertTreeR b t r where
  insertTreeManyR :: MonadCons m => [r] -> SessionMonad b m [r]
  insertTreeManyR = fmap (fmap unGrec) . insertTreeMany @b @t . fmap Grec

instance InsertTree b t (Grec r) => InsertTreeR b t r


insertTreeManyDef :: InsertTreeConstraint m f b t r
                => Proxy# b -> Proxy# t -> f r -> SessionMonad b m (f r)
insertTreeManyDef (_ :: Proxy# b) (_ :: Proxy# t) (rs :: f r) = do
  mbk <- fmap (fmap tagKey) <$> insertMany @b @(TdData t) rs
  insertChilds @b @(DataAutoIns(TdData t)) @(GrecChilds t r) mbk rs
 where
  tagKey :: x -> Tagged (TopKey t) x
  tagKey = Tagged

class InsertChilds b (ai :: Bool)
                  (chs :: [(Symbol,(TreeDef,[(Symbol,Symbol)]))]) pk r where
  insertChilds  :: (MonadCons m , AppCons f)
                => Maybe (f (Tagged (pk :: [Symbol]) (GenKey b))) -> f r
                -> SessionMonad b m (f r)

instance InsertChilds b ai '[] pk r where
  insertChilds _ = return

promoteOnly [d|
  getParentTypes rs r = fromMaybe
    (error "Invalid parent fields in InsertChilds!")
    $ submap (map snd rs) $ fieldsGrec r
  |]

type family InsChildConsF b pk s td rs chs r where
  InsChildConsF b pk s td rs chs r
    = InsChildConsF' b pk s td rs chs r (RecParent r rs) (FieldByName s r) (Fsts rs)

type family InsChildConsF' b pk s td rs chs r rp fld frs where
  InsChildConsF' b pk s td rs chs r rp fld frs =
    ( InsertTree b td (AddParent frs rp fld)
    , GrecLens s [fld] r
    , InsertChilds b 'False chs pk r
    , NamesGrecLens (Snds rs) rp r
    )

type family AddParent frs rp fld where
  AddParent frs rp fld = (Tagged frs rp, GrecWithout frs (Grec fld))

instance InsChildConsF b pk s td rs chs r
      => InsertChilds b False ( '(s, '(td,rs)) ': chs) pk r where
  insertChilds {-sai (SCons (STuple2 sname (STuple2 std srs)) schs) -}mbk rs = do
    rs' <- fmap ( liftA2 (\r r' -> r & grecLens @s .~ r') rs
                . fmap getZipList
                . getCompose
                . fmap (unGrec . unGWO . snd)
                )
        $ insertTreeManyF @b @td
        $ Compose $ newRec <$> rs

    insertChilds @b @'False @chs mbk rs'
   where
    newRec :: r -> ZipList (AddParent (Fsts rs) (RecParent r rs) (FieldByName s r))
    newRec r = (\r' -> (tr, GWO (Grec r'))) <$> ZipList (view (grecLens @s) r)
     where
      tr = Tagged (namesGrecGet @(Snds rs) r)

type family RecParent r rs where
  RecParent r rs = ListToPairs (GetParentTypes rs r)
type family RecAutoIns b pk r where
  RecAutoIns b pk r = (Tagged pk (GenKey b), GrecWithout pk (Grec r))

type family InsChildConsT b pk s td rs chs r where
  InsChildConsT b pk s td rs chs r
    = InsChildConsT' b pk s td rs chs r (Fsts rs) (FieldByName s r) (RecAutoIns b pk r)

type family InsChildConsT' b pk s td rs chs r frs fld rai where
  InsChildConsT' b pk s td rs chs r frs fld rai
    = InsChildConsT'' b pk s td rs chs r frs fld rai (RecParent rai rs)

type family InsChildConsT'' b pk s td rs chs r frs fld rai rpai where
  InsChildConsT'' b pk s td rs chs r frs fld rai rpai =
    ( InsertTree b td (AddParentAutoIns frs rpai fld)
    , GrecLens s [fld] r
    , InsertChilds b True chs pk r
    , NamesGrecLens (Snds rs) rpai rai
    )
type family AddParentAutoIns frs rpai fld where
  AddParentAutoIns frs rpai fld = (Tagged frs rpai, GrecWithout frs (Grec fld))

instance InsChildConsT b pk s td rs chs r
      => InsertChilds b True ( '(s, '(td,rs)) ': chs) pk r where
  insertChilds {-sai (SCons (STuple2 sname (STuple2 std srs)) schs) -}mbk rs = do
    rs' <- fmap ( liftA2 (\r r' -> r & grecLens @s .~ r') rs
                . fmap getZipList
                . getCompose
                . fmap (unGrec . unGWO . snd)
                )
        $ insertTreeManyF @b @td
        $ Compose $ newRec <$> ks <*> rs

    insertChilds @b @'True @chs mbk rs'
   where
    newRec :: Tagged pk (GenKey b) -> r
          -> ZipList  ( AddParentAutoIns
                          (Fsts rs)
                          (RecParent (RecAutoIns b pk r) rs)
                          (FieldByName s r)
                      )
    newRec k r = (\r' -> (tr, GWO (Grec r')))
            <$> ZipList (view (grecLens @s) r)
     where
      tr = Tagged (namesGrecGet @(Snds rs)
                  (k, GWO (Grec r) :: GrecWithout pk (Grec r)))
    ks = fromMaybe (error $ "There is no key value (Nothing) in insertChilds"
                  ++ " but parent has AutoIns flag") mbk
