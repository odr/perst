{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE UndecidableInstances      #-}

module Perst.Database.TreeDef where

import           Data.Kind                     (Type)
-- import           Data.List                     (nub)
import           Data.Maybe                    (fromMaybe)
import           Data.Singletons.Prelude
import           Data.Singletons.Prelude.List  (FindSym0, LookupSym0, NubBySym0,
                                                NubSym0)
import           Data.Singletons.Prelude.Maybe (FromMaybeSym0, sFromMaybe)
import           Data.Singletons.TH            (promoteOnly, singletons)

import           Data.Type.Grec                (FieldNamesGrec,
                                                FieldNamesNotConvGrec,
                                                FieldsGrec, FieldsGrecSym0,
                                                FieldsSym0, GWPairs, GWTagged,
                                                Grec (..), GrecWith,
                                                GrecWithout, InternalType,
                                                IsSubSym0, ListToPairs,
                                                ListToTaggedPairs, Submap2,
                                                Submap2Sym0, SubmapSym0, isSub,
                                                submap2)
import           Perst.Database.DataDef        (DataDef', DataKey, DdInfo)

type AppCons f = (Applicative f, Traversable f)

singletons [d|
  data TreeDef' s t = TreeDefC
    { tdData :: ([(s,t)], DataDef' s)
    , tdChilds :: [(s, (TreeDef' s t, [(s,s)]))]
    }
  |]

type TreeDef = TreeDef' Symbol Type

promoteOnly [d|
  child :: Eq s => s -> TreeDef' s t -> (TreeDef' s t, [(s,s)])
  child s t = case lookup s $ tdChilds t of
    Nothing -> (error "There is no right children")
    Just x  -> x

  childKeys :: Eq s => s -> TreeDef' s t -> [s]
  childKeys s = map fst . snd . child s

  parentKeys :: Eq s => s -> TreeDef' s t -> [s]
  parentKeys s = map snd . snd . child s

  fieldByName' :: Eq s => s -> [(s,a)] -> a
  fieldByName' s xs = case lookup s xs of
    Nothing -> error "Can't lookup field by name"
    Just x  -> x

  grecChilds' :: Eq s => TreeDef' s t -> [s] -> [(s, (TreeDef' s t, [(s,s)]))]
  grecChilds' t ss  = case submap2 ss (tdChilds t) of
    Nothing -> error "GrecChilds': Can't submap childs!"
    Just x  -> x

  childByParents rs ss = map (\s' -> fromMaybe "_u_n_u_s_e_d_" $ lookup s' ck) ss
    where
      ck = map (\(a,b) -> (b,a)) rs

  allParentKeys :: Eq s => TreeDef' s t -> [(s,t)]
  allParentKeys (TreeDefC d ch)
    = case submap2 (nub $ concatMap (map snd . snd . snd) ch) $ fst d of
        Nothing -> error "Some keys in the definition of relation are not in the description of table row"
        Just x -> x

  getParentTypes :: [(Symbol,Symbol)] -> Type -> [Type]
  getParentTypes rs r = fromMaybe
    (error "Invalid parent fields in InsertChilds!")
    $ submap (map snd rs) $ nubBy (\a b -> fst a == fst b) $ fieldsGrec r

  |]

type GrecChilds t r = GrecChilds' t (FieldNamesNotConvGrec r)

type MbFieldByName s r = Lookup s (FieldsGrec r)

type FieldByName s r = InternalType (FieldByName' s (FieldsGrec r))

type TaggedAllParentKeys t = ListToTaggedPairs (AllParentKeys t)

type TopKey t         = DataKey (TdData t)
type TopPK t r        = GrecWith (TopKey t) r
type TopNotPK t r     = GrecWithout (TopKey t) r
type TopPKPairs t r   = GWPairs (TopKey t) r
type TopPKTagged t r  = GWTagged (TopKey t) r

type KwoR k r = GrecWithout (FieldNamesGrec r) k
type Pair k r = (KwoR k r, r)

type RecParent k r rs = ListToPairs (GetParentTypes rs (k, r))
type RecParent' k r rs = ListToPairs (GetParentTypes rs (Pair k r))
