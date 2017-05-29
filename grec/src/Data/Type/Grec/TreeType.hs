{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE UndecidableInstances      #-}
module Data.Type.Grec.TreeType -- ( TreeT, FieldsTree, TreeT'(..))
  where

import           Data.Kind               (Type)
import           Data.Singletons.Prelude
import           Data.Singletons.TH      (singletons)
import           GHC.Generics
import           GHC.TypeLits            (ErrorMessage (..), Symbol, TypeError)

singletons [d|
  data TreeT' s t = TreeTC [(s,t)] [(s, TreeT' s t)]
  treeRec     (TreeTC r _) = r
  treeChilds  (TreeTC _ c) = c
  appendTreeT (TreeTC a1 b1) (TreeTC a2 b2) = TreeTC (a1 ++ a2) (b1 ++ b2)
  |]
type TreeT = TreeT' Symbol Type

type FieldsTree a = GFieldsTree (Rep a)
type family GFieldsTree (a :: k1) :: TreeT where
  GFieldsTree (S1 (MetaSel ('Just s) _ _ _) (Rec0 String))
      = 'TreeTC '[ '(s, String)] '[]
  GFieldsTree (S1 (MetaSel ('Just s) _ _ _) (Rec0 [v]))
      = 'TreeTC '[] '[ '(s,GFieldsTree (Rep v))]
  GFieldsTree (S1 (MetaSel ('Just s) _ _ _) (Rec0 v))
      = 'TreeTC '[ '(s, v)] '[]
  GFieldsTree (C1 _ s) = GFieldsTree s
  GFieldsTree (a :*: b) = AppendTreeT (GFieldsTree a) (GFieldsTree b)
  -- data
  GFieldsTree (D1 (MetaData _ _ _ 'False) (C1 _ s)) = GFieldsTree s
  -- newtype
  GFieldsTree (D1 (MetaData _ _ _ 'True) (C1 _ (S1 _ (Rec0 dt)))) = GFieldsTree (Rep dt)
  GFieldsTree a = TypeError (
      Text "GFieldsTree is supported only for Representation of record with one constructor"
      :$$: Text "and at least one field or newtype for such record"
      :$$: Text "Checked type is " :<>: ShowType a
      )

singletons [d|
  data TTree' s t = TTreeC { tRec :: [(s,t)], tChilds :: [(s,t)] }
  appendTTree (TTreeC a1 b1) (TTreeC a2 b2) = TTreeC (a1 ++ a2) (b1 ++ b2)
  |]

type TTree = TTree' Symbol Type

type FieldsTTree a = GFieldsTTree (Rep a)
type family GFieldsTTree (a :: k1) :: TTree where
  GFieldsTTree (S1 (MetaSel ('Just s) _ _ _) (Rec0 String))
      = 'TTreeC '[ '(s, String)] '[]
  GFieldsTTree (S1 (MetaSel ('Just s) _ _ _) (Rec0 [v]))
      = 'TTreeC '[] '[ '(s,v)]
  GFieldsTTree (S1 (MetaSel ('Just s) _ _ _) (Rec0 v))
      = 'TTreeC '[ '(s, v)] '[]
  GFieldsTTree (C1 _ s) = GFieldsTTree s
  GFieldsTTree (a :*: b) = AppendTTree (GFieldsTTree a) (GFieldsTTree b)
  -- data
  GFieldsTTree (D1 (MetaData _ _ _ 'False) (C1 _ s)) = GFieldsTTree s
  -- newtype
  GFieldsTTree (D1 (MetaData _ _ _ 'True) (C1 _ (S1 _ (Rec0 dt)))) = GFieldsTTree (Rep dt)
  GFieldsTTree a = TypeError (
      Text "GFieldsTTree is supported only for Representation of record with one constructor"
      :$$: Text "and at least one field or newtype for such record"
      :$$: Text "Checked type is " :<>: ShowType a
      )
