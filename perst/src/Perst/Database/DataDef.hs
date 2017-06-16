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
  , DdName, DdRec, DdFlds, DdKey, DdUniq, DdUpd, AllKeys, DdFrgn, DdAutoIns -- , DdData
  , DdNameSym0, DdRecSym0, DdKeySym0, DdUniqSym0, DdUpdSym0, DdFrgnSym0, DdFldsSym0
  -- , IsProj
  , DataDefConstr
  , ddFlds, sDdFlds, ddRec, sDdRec
  -- * Good functions to take table info in runtime
  , tableName, fieldNames
  , fieldNames', fieldNamesT
  , primaryKey, uniqKeys, foreignKeys
  -- * Some other stuffs
  , Subrec
  , showProxy
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
import qualified Data.Text.Lazy                as TL
import           Data.Type.Grec                (FieldNamesConvGrec, FieldsGrec,
                                                Grec, ListToPairs, Typ)
import           GHC.Generics                  (Generic)
import           GHC.TypeLits                  (ErrorMessage (..), KnownSymbol,
                                                TypeError)
import           Perst.Types                   (AllIsNub, AllIsSub, CheckFK,
                                                FkIsNub, IsNub, IsSub,
                                                IsSubSym0, Submap)

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
      { ddn  :: s
      , ddr  :: [(s,t)]
      , ddfn :: [s]
      , ddk  :: [s]
      , ddu  :: [[s]]
      , ddf  :: [FK s]
      , ddai :: Bool
      }
    | ViewDef
      { ddn   :: s
      , ddr   :: [(s,t)]
      , ddfn  :: [s]
      , ddSql :: Maybe s
      , ddup  :: [s]
      , ddk   :: [s]
      , ddu   :: [[s]]
      , ddf   :: [FK s]
      }
    --  ProjDef
    --   { ddr   :: [(s,t)]
    --   , ddfn  :: [s]
    --   , ddd   :: DataDef' s t
    --   }

  ddName  (TableDef n _ _ _ _ _ _)   = n
  ddName  (ViewDef  n _ _ _ _ _ _ _) = n
  -- ddName  (ProjDef  _ _ dd)          = ddName dd
  ddRec   (TableDef _ r _ _ _ _ _)   = r
  ddRec   (ViewDef  _ r _ _ _ _ _ _) = r
  -- ddRec   (ProjDef  r _ _)           = r
  ddFlds  (TableDef _ _ fn _ _ _ _)   = fn
  ddFlds  (ViewDef  _ _ fn _ _ _ _ _) = fn
  -- ddFlds  (ProjDef  _ fn _)           = fn
  ddUpd   (TableDef _ _ fn _ _ _ _)  = fn
  ddUpd   (ViewDef  _ _ _ _ u _ _ _) = u
  -- ddUpd   (ProjDef  _ fn _)          = fn
  ddKey   (TableDef _ _ _ p _ _ _)   = p
  ddKey   (ViewDef  _ _ _ _ _ p _ _) = p
  -- ddKey   (ProjDef  _ fn dd)         = ddKey dd
  -- ddData  (ProjDef _ _ dd)          = dd
  ddAutoIns   (TableDef _ _ _ _ _ _ ai)  = ai

  |]
-- promoteOnly [d|
--   isProj ProjDef {} = True
--   isProj _          = False
--   |]
promoteOnly [d|
  ddUniq  (TableDef _ _ _ _ u _ _)   = u
  ddUniq  (ViewDef  _ _ _ _ _ _ u _) = u
  -- ddUniq  (ProjDef  _ fn dd)         = filter (`isSub` fn) (ddUniq dd)
  ddFrgn  (TableDef _ _ _ _ _ f _)   = f
  ddFrgn  (ViewDef  _ _ _ _ _ _ _ f) = f
  -- ddFrgn  (ProjDef  _ fn dd)         = filter ((`isSub` fn) . map fst . fst) (ddFrgn dd)
  allKeys (TableDef _ _ _ p u _ _)   = p : u
  allKeys (ViewDef  _ _ _ _ _ p u _) = p : u
  -- allKeys (ProjDef  _ fn dd)         = filter (`isSub` fn) (AllKeys dd)
  |]

type DataDef = DataDef' Symbol Type

type TableD v = TableD' (Typ v) (FieldsGrec (Grec v))

type family TableD' n r where
  TableD' n r = TableDef n r (Map FstSym0 r)

type ViewD v = ViewD' (Typ v) (FieldsGrec (Grec v))

type family ViewD' n r where
  ViewD' n r = ViewDef n r (Map FstSym0 r)

-- type ProjD v dd = ProjD' (FieldsGrec (Grec v)) dd

-- type family ProjD' r dd where
--   ProjD' r dd = ProjDef r (Map FstSym0 r) dd

type DataDefConstr d =
  ( IsNub (AllKeys d) ~ True
  , AllIsNub (AllKeys d) ~ True
  , AllIsSub (AllKeys d) (DdFlds d) ~ True
  , FkIsNub (DdFrgn d) ~ True
  , CheckFK (DdFrgn d) (DdFlds d) ~ True
  , SingI (DdFlds d), SingI (DdKey d)
  , SingI (DdUniq d), SingI (DdFrgn d)
  , KnownSymbol (DdName d)
  -- , If (IsProj d)
  --     (Submap (DdFlds d) (DdRec (DdData d)) ~ Just (Map SndSym0 (DdRec d)))
  --     (()::Constraint)
  )

type family Subrec t ns where
  Subrec t ns = Tagged ns (ListToPairs (FromJust (Submap ns (DdRec t))))

genDefunSymbols [''TableD, ''ViewD, ''Subrec]


tableName :: KnownSymbol (DdName t) => Proxy t -> String
tableName (_ :: Proxy t) = fromSing (sing :: Sing (DdName t))

fieldNames :: SingI (DdFlds t) => Proxy (t :: DataDef) -> [String]
fieldNames (_ :: Proxy t) = fromSing (sing :: Sing (DdFlds t))

fieldNames' :: SingI (FieldNamesConvGrec r) => Proxy r -> [String]
fieldNames' (_ :: Proxy r) = fromSing (sing :: Sing (FieldNamesConvGrec r))

fieldNamesT :: SingI (FieldNamesConvGrec r) => Proxy r -> [TL.Text]
fieldNamesT = map TL.pack . fieldNames'

primaryKey :: SingI (DdKey t) => Proxy (t :: DataDef) -> [String]
primaryKey (_ :: Proxy t) = fromSing (sing :: Sing (DdKey t))

uniqKeys :: SingI (DdUniq t) => Proxy (t :: DataDef) -> [[String]]
uniqKeys (_ :: Proxy t) = fromSing (sing :: Sing (DdUniq t))

foreignKeys :: SingI (DdFrgn t)
            => Proxy (t :: DataDef) -> [([(String, String)], (String, DelCons))]
foreignKeys (_ :: Proxy t) = fromSing (sing :: Sing (DdFrgn t))

showProxy :: (SingI t, SingKind k) => Proxy (t :: k) -> DemoteRep k
showProxy (_ :: Proxy t) = fromSing (sing :: Sing t)
