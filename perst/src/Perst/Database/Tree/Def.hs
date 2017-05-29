{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE UndecidableInstances      #-}

module Perst.Database.Tree.Def where

import           Data.Kind                     (Type)
import           Data.Singletons.Prelude
import           Data.Singletons.Prelude.List
import           Data.Singletons.Prelude.Maybe
import           Data.Singletons.TH
import qualified Data.Text.Lazy                as TL

import           Data.Type.Grec
import           Perst.Database.DataDef
import           Perst.Types

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
  checkTree' :: (Eq s) => TreeDef' s t -> Bool
  checkTree' t
    = all (\(_,(a,b)) -> isSub (map fst b) (ddFlds $ tdData t)
                      && checkTree' a
          ) $ tdChilds t

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

type TreeDef = TreeDef' Symbol Type

type CheckTree a = CheckTree' a ~ True
type CheckChilds a b = FromConsList (CheckChilds' ContainSym0 a b)

getProxies :: SingI (Map FstSym0 (TreeRec r))
  => Proxy (t :: TreeDef) -> Proxy (r :: TreeT) ->
  (Proxy (TreeChilds r), Proxy (TdChilds t), Proxy (TdData t)
  , Proxy (EqName (TdChilds t) (TreeChilds r))
  , [TL.Text])
getProxies (pt :: Proxy t) (pr :: Proxy r) = (Proxy, Proxy, Proxy, Proxy
    , map TL.pack $ showProxy (Proxy :: Proxy (Map FstSym0 (TreeRec r))))
