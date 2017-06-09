{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE UndecidableInstances      #-}

module Perst.Database.Tree.Def where

import           Data.Kind                     (Type)
import           Data.Singletons.Prelude
import           Data.Singletons.Prelude.List  (FindSym0, LookupSym0, NubSym0)
import           Data.Singletons.Prelude.Maybe (FromMaybeSym0)
import           Data.Singletons.TH            (promoteOnly, singletons)
-- import           GHC.TypeLits                  (ErrorMessage (..),
--                                                 TypeError (..))
-- import qualified Data.Text.Lazy                as TL

import           Data.Type.Grec                (FieldNamesNotConvGrec, Fields,
                                                Grec (..), InternalType,
                                                ListToTaggedPairs)
import           Perst.Database.DataDef        (DataDef', DdFldsSym0, DdRecSym0)
import           Perst.Types                   (IsSubSym0, Submap2, Submap2Sym0)

{-
data TreeT' s t = TreeTC [(s,t)] [(s, TreeT' s t)]
type TreeT = TreeT' Symbol Type
type FieldsTree a = GFieldsTree (Rep a) -- :: TreeT

data ConvTree a = ConvTree [a] [[ConvTree a]] deriving (Eq, Show)

selectMany' :: (MonadIO m, MonadMask m, DelConstr b t k)
            => Proxy t -> [TL.Text] -> [k] -> SessionMonad b m [[[FieldDB b]]]
-}

singletons [d|
  data TreeDef' s t = TreeDefC (DataDef' s t) [(s, (TreeDef' s t, [(s,s)]))]

  tdData    (TreeDefC d _) = d
  tdChilds  (TreeDefC _ c) = c

  parentKeyNames t = map (map fst . snd . snd) $ tdChilds t
  |]
promoteOnly [d|
  allParentKeyNames :: Eq s => TreeDef' s t -> [s]
  allParentKeyNames t = nub $ concatMap (map snd . snd . snd) $ tdChilds t

  allParentKeys t = fromMaybe
      (error "Some keys in the definition of relation are not in the description of table row")
      $ submap2 (allParentKeyNames t) $ ddRec $ tdData t

  child s t = fromMaybe
      (error "There is no right children")
      $ lookup s $ tdChilds t
  childKeys s = map fst . snd . child s
  parentKeys s = map snd . snd . child s

  childByParents rs ss
      = map (\s' -> fromMaybe "_u_n_u_s_e_d_" $ lookup s' ck) ss
    where
      ck = map (\(a,b) -> (b,a)) rs

  fieldByName' s xs = fromMaybe (error "Can't lookup child field")
                    $ lookup s xs

  grecChilds' t ss  = fromMaybe (error "Can't submap childs!")
                    $ submap2 ss (tdChilds t)
  |]

promoteOnly [d|
  checkTree' :: (Eq s) => TreeDef' s t -> Bool
  checkTree' t
    = all (\(_,(a,b)) -> isSub (map fst b) (ddFlds $ tdData t)
                      && checkTree' a
          ) $ tdChilds t
  |]

type GrecChilds t r = GrecChilds' t (FieldNamesNotConvGrec (Grec r))

type TreeDef = TreeDef' Symbol Type

type CheckTree a = CheckTree' a ~ True

type MbFieldByName s r = Lookup s (Fields r)

type FieldByName s r = InternalType (FieldByName' s (Fields r))

type TaggedAllParentKeys t = ListToTaggedPairs (AllParentKeys t)
-- type CheckChilds a b = FromConsList (CheckChilds' ContainSym0 a b)
--
-- getProxies :: SingI (Map FstSym0 (TreeRec r))
--   => Proxy (t :: TreeDef) -> Proxy (r :: TreeT) ->
--   (Proxy (TreeChilds r), Proxy (TdChilds t), Proxy (TdData t)
--   , Proxy (EqName (TdChilds t) (TreeChilds r))
--   , [TL.Text])
-- getProxies (pt :: Proxy t) (pr :: Proxy r) = (Proxy, Proxy, Proxy, Proxy
--     , map TL.pack $ showProxy (Proxy :: Proxy (Map FstSym0 (TreeRec r))))
