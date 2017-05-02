{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE UndecidableInstances      #-}

module Perst.Database.DataDef
  ( DelCons(..)
  -- * Table definition
  , DataDef'(..), DataDef
  , TableD, ViewD
  , DdName, DdRec, DdFlds, DdKey, DdUniq, DdUpd, AllKeys, DdFrgn, DdData
  , DdNameSym0, DdRecSym0, DdKeySym0, DdUniqSym0, DdUpdSym0, DdFrgnSym0
  , IsProj
  , DataDefConstr
  -- * Good functions to take table info in runtime
  , tableName, fieldNames
  , fieldNames'
  , primaryKey, uniqKeys, foreignKeys
  -- * Some other stuffs
  , Subrec
  )
  where

import           Data.Kind                     (Constraint, Type)
import           Data.Proxy                    (Proxy (..))
import           Data.Singletons.Prelude
import           Data.Singletons.Prelude.List
import           Data.Singletons.Prelude.Maybe (FromJust)
import           Data.Singletons.TH            (genDefunSymbols, promoteOnly,
                                                singletons, singletonsOnly)
import           Data.Tagged                   (Tagged)
import           Data.Type.Grec                (FieldNamesGrec, FieldsGrec,
                                                ListToPairs, Typ)
import           GHC.Generics                  (Generic)
import           GHC.TypeLits                  (ErrorMessage (..), KnownSymbol,
                                                TypeError)
import           Perst.Types                   (AllIsNub, AllIsSub, IsNub,
                                                IsSub, IsSubSym0, Submap, FkIsNub, CheckFK)

singletons [d|
  data DelCons = DcRestrict | DcCascade | DcSetNull
    deriving (Show, Eq, Ord)
  -- fk :: s -> DelCons -> ([(s,s)],(s,DelCons))
  -- fk s dc= ([(s,s)],(s,dc))
  |]

type FK s = ([(s,s)],(s,DelCons))

singletons [d|
  -- data DelCons = DcRestrict | DcCascade | DcSetNull
  --   deriving (Show, Eq, Ord)

  data DataDef' s t
    = TableDef
      { ddn :: s -- Symbol
      , ddr :: [(s,t)] -- [(Symbol,Type)]
      , ddfn :: [s] -- [Symbol]
      , ddk :: [s] -- [Symbol]
      , ddu :: [[s]] --[[Symbol]]
      , ddf :: [FK s]  -- [([(s,s)],(s,DeleteConstraint))] --[FK]
      }
    | ViewDef
      { ddn   :: s -- Symbol
      , ddr   :: [(s,t)] -- [(Symbol,Type)]
      , ddfn  :: [s]   -- [Symbol]
      , ddSql :: Maybe s -- Maybe Symbol
      , ddup  :: [s] -- [Symbol]
      , ddk   :: [s] -- [Symbol]
      , ddu   :: [[s]] -- [[Symbol]]
      , ddf   :: [FK s] -- [([(s,s)],(s,DeleteConstraint))] -- [FK]
      }
    | ProjDef
      { ddr   :: [(s,t)]
      , ddfn  :: [s]
      , ddd   :: DataDef' s t
      }
    -- | TreeDef
    --   { ddData    :: DataDef' s t
    --   , ddChilds  :: [(DataDef' s t, [(s,s)])]
    --   , ddrChilds :: [(s,t)]
    --   , ddfChilds :: [s]
    --   }

  ddName  (TableDef n _ _ _ _ _)     = n
  ddName  (ViewDef  n _ _ _ _ _ _ _) = n
  ddName  (ProjDef  _ _ dd)          = ddName dd
  ddRec   (TableDef _ r _ _ _ _)     = r
  ddRec   (ViewDef  _ r _ _ _ _ _ _) = r
  ddRec   (ProjDef  r _ _)           = r
  ddFlds  (TableDef _ _ fn _ _ _)     = fn
  ddFlds  (ViewDef  _ _ fn _ _ _ _ _) = fn
  ddFlds  (ProjDef  _ fn _)           = fn
  ddUpd   (TableDef _ _ fn _ _ _)    = fn
  ddUpd   (ViewDef  _ _ _ _ u _ _ _) = u
  ddUpd   (ProjDef  _ fn _)          = fn
  ddKey   (TableDef _ _ _ p _ _)     = p
  ddKey   (ViewDef  _ _ _ _ _ p _ _) = p
  ddKey   (ProjDef  _ fn dd)         = ddKey dd
  -- ddUniq :: Eq s => DataDef' s t -> [[s]]
  ddData  (ProjDef _ _ dd) = dd
  |]
promoteOnly [d|
  -- isTable TableDef {} = True
  -- isTable _           = False
  isProj ProjDef {} = True
  isProj _          = False
  |]
promoteOnly [d|
  ddUniq  (TableDef _ _ _ _ u _)     = u
  ddUniq  (ViewDef  _ _ _ _ _ _ u _) = u
  ddUniq  (ProjDef  _ fn dd)         = filter (`isSub` fn) (ddUniq dd)
  ddFrgn  (TableDef _ _ _ _ _ f)     = f
  ddFrgn  (ViewDef  _ _ _ _ _ _ _ f) = f
  ddFrgn  (ProjDef  _ fn dd)         = filter ((`isSub` fn) . map fst . fst) (ddFrgn dd)
  allKeys (TableDef _ _ _ p u _)     = p : u
  allKeys (ViewDef  _ _ _ _ _ p u _) = p : u
  allKeys (ProjDef  _ fn dd)         = filter (`isSub` fn) (AllKeys dd)
  |]

type DataDef = DataDef' Symbol Type

type TableD v p u = TableD' (Typ v) (FieldsGrec v) p u

type family TableD' n r p u where
  TableD' n r p u = TableDef n r (Map FstSym0 r) p u

type ViewD v s upd p u = ViewD' (Typ v) (FieldsGrec v) s upd p u

type family ViewD' n r s upd p u where
  ViewD' n r s upd p u = ViewDef n r (Map FstSym0 r) s upd p u

type ProjD v dd = ProjD' (FieldsGrec v) dd

type family ProjD' r dd where
  ProjD' r dd = ProjDef r (Map FstSym0 r) dd

type DataDefConstr d =
  ( IsNub (AllKeys d) ~ True
  , AllIsNub (AllKeys d) ~ True
  , AllIsSub (AllKeys d) (DdFlds d) ~ True
  , FkIsNub (DdFrgn d) ~ True
  , CheckFK (DdFrgn d) (DdFlds d) ~ True
  , SingI (DdFlds d), SingI (DdKey d)
  , SingI (DdUniq d), SingI (DdFrgn d)
  , KnownSymbol (DdName d)
  , If (IsProj d)
      (Submap (DdFlds d) (DdRec (DdData d)) ~ Just (Map SndSym0 (DdRec d)))
      (()::Constraint)
  )

type family Subrec t ns where
  Subrec t ns = Tagged ns (ListToPairs (FromJust (Submap ns (DdRec t))))

genDefunSymbols [''TableD, ''ViewD, ''Subrec]


tableName :: KnownSymbol (DdName t) => Proxy t -> String
tableName (_ :: Proxy t) = fromSing (sing :: Sing (DdName t))

fieldNames :: SingI (DdFlds t) => Proxy (t :: DataDef) -> [String]
fieldNames (_ :: Proxy t) = fromSing (sing :: Sing (DdFlds t))

fieldNames' :: SingI (FieldNamesGrec r) => Proxy r -> [String]
fieldNames' (_ :: Proxy r) = fromSing (sing :: Sing (FieldNamesGrec r))

primaryKey :: SingI (DdKey t) => Proxy (t :: DataDef) -> [String]
primaryKey (_ :: Proxy t) = fromSing (sing :: Sing (DdKey t))

uniqKeys :: SingI (DdUniq t) => Proxy (t :: DataDef) -> [[String]]
uniqKeys (_ :: Proxy t) = fromSing (sing :: Sing (DdUniq t))

foreignKeys :: SingI (DdFrgn t)
            => Proxy (t :: DataDef) -> [([(String, String)], (String, DelCons))]
foreignKeys (_ :: Proxy t) = fromSing (sing :: Sing (DdFrgn t))
