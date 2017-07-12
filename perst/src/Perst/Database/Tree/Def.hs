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

import           Data.Type.Grec                (FieldNamesNotConvGrec,
                                                FieldsGrec, GWPairs, Grec (..),
                                                GrecWith, GrecWithout,
                                                InternalType, IsSubSym0,
                                                ListToTaggedPairs, Submap2,
                                                Submap2Sym0)
import           Perst.Database.DataDef        (DataDef', DdFldsSym0, DdKey,
                                                DdRecSym0)

singletons [d|
  data TreeDef' s t = TreeDefC (DataDef' s t) [(s, (TreeDef' s t, [(s,s)]))]

  |]
promoteOnly [d|
  tdData    (TreeDefC d _) = d
  tdChilds  (TreeDefC _ c) = c

  parentKeyNames t = map (map fst . snd . snd) $ tdChilds t

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

  childByParents rs ss = map (\s' -> fromMaybe "_u_n_u_s_e_d_" $ lookup s' ck) ss
    where
      ck = map (\(a,b) -> (b,a)) rs

  fieldByName' s xs = fromMaybe (error "Can't lookup child field")
                    $ lookup s xs

  grecChilds' t ss  = fromMaybe (error "GrecChilds': Can't submap childs!")
                    $ submap2 ss (tdChilds t)
  |]

promoteOnly [d|
  checkTree' :: (Eq s) => TreeDef' s t -> Bool
  checkTree' t
    = all (\(_,(a,b)) -> isSub (map fst b) (ddFlds $ tdData t)
                      && checkTree' a
          ) $ tdChilds t
  |]

type GrecChilds t r = GrecChilds' t (FieldNamesNotConvGrec r)

type TreeDef = TreeDef' Symbol Type

type CheckTree a = CheckTree' a ~ True

type MbFieldByName s r = Lookup s (FieldsGrec r)

type FieldByName s r = InternalType (FieldByName' s (FieldsGrec r))

type TaggedAllParentKeys t = ListToTaggedPairs (AllParentKeys t)

type TopKey t         = DdKey (TdData t)
type TopPK t r        = GrecWith (TopKey t) r
type TopPKPairs t r   = GWPairs (TopKey t) r
