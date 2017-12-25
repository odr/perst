{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE UndecidableInstances      #-}

module Perst.Database.DataDef
  -- ( DelCons(..)
  -- -- * Table definition
  -- , DataDef'(..), DataDef, DataDef''(..)
  -- , TableD, ViewD, DataD -- , DataDFC(..)
  -- , DdName, DdRec, DdFlds, DdKey, DdUniq, DdUpd, AllKeys, DdFrgn, DdAutoIns -- , DdData
  -- , DdNameSym0, DdRecSym0, DdKeySym0, DdUniqSym0, DdUpdSym0, DdFrgnSym0, DdFldsSym0
  -- , Dd, Ddf
  -- , sDdName, sDdRec, sDdFlds, sDdKey, sDdUniq, sDdUpd, sAllKeys, sDdFrgn, sDdAutoIns -- , DdData
  -- , sDd, sDdf
  -- , CheckInsMandatory, KeyType
  -- -- , DataDefConstr
  -- , ddFlds, ddRec
  -- -- * Good functions to take table info in runtime
  -- -- , DataStructInfo(..)
  -- , tableName, fieldNames, primaryKey, uniqKeys, foreignKeys, autoIns, viewText
  -- -- , fieldNames', fieldNamesT
  -- -- * Some other stuffs
  -- , Subrec
  -- , showProxy
  -- , WithKey, WithoutKey
  -- , formatS
  -- )
  where

import           Data.Kind                     (Type)
import           Data.Proxy                    (Proxy (..))
import           Data.Singletons.Prelude
import           Data.Singletons.Prelude.List  (FilterSym0, FindSym0)
import           Data.Singletons.Prelude.Maybe (FromJust, FromJustSym0)
import           Data.Singletons.TH            (promoteOnly, singletons)
import           Data.Text                     (Text)
import           Data.Text.Format              (Format, format)
import           Data.Text.Format.Params       (Params)
import qualified Data.Text.Lazy                as TL
-- import           Data.Type.Grec                (GrecWith, GrecWithout)

singletons [d|
  data DelCons = DcRestrict | DcCascade | DcSetNull
    deriving (Show, Eq, Ord)

  data DataInfo s
    = TableInfo
      { diName    :: s
      , diKey     :: [s]
      , diUniq    :: [[s]]
      , diAutoIns :: Bool
      }
    | ViewInfo
      { diName    :: s
      , diSql     :: Maybe s
      , diUpd     :: [s]
      , diKey     :: [s]
      , diUniq    :: [[s]]
      , diAutoIns :: Bool
      } deriving Show

  data DataStruct' s t = DataStructC { dsInfo  :: DataInfo s
                                     , dsRec   :: t
                                     } deriving Show

  data Ref' s = RefC { refName :: s
                     , refFrom :: s
                     , refTo :: s
                     , refCols :: [(s,s)]
                     , refDelCons :: DelCons
                     } deriving Show

  data Schema' s t = SchemaC { schDS :: [DataStruct' s t]
                             , schRefs :: [Ref' s]
                             } deriving Show

  |]

promoteOnly [d|
  getFromRefs :: DataStruct' s t -> [Ref' s] -> [Ref' s]
  getFromRefs ds rs = filter (\r -> refFrom r == n) rs
    where n = diName $ dsInfo ds

  getToRefs :: DataStruct' s t -> [Ref' s] -> [Ref' s]
  getToRefs ds rs = filter (\r -> refTo r == n) rs
    where n = diName $ dsInfo ds

  getFromRefByName :: s -> s -> [Ref' s] -> Maybe (Ref' s)
  getFromRefByName from ref refs =
    find (\r -> refFrom r == from && refName r == ref) refs

  getToRefByName :: s -> s -> [Ref' s] -> Maybe (Ref' s)
  getToRefByName to ref refs =
    find (\r -> refTo r == to && refName r == ref) refs

  getDataStruct :: s -> Schema' s t -> Maybe (DataStruct' s t)
  getDataStruct s = find ((s ==) . diName . dsInfo) . schDS

  getDataStructName :: s -> Schema' s t -> s
  getDataStructName s = diName . dsInfo . fromJust . find ((s ==) . diName . dsInfo) . schDS

  |]

-- deriving instance Show s => Show (DataInfo s)
-- deriving instance Show s => Show (FK s)
-- deriving instance Show s => Show (DataDef' s)

type DataStruct = DataStruct' Symbol Type
type Ref = Ref' Symbol
type Schema = Schema' Symbol Type

type DdKey t = DiKey (DsInfo t)
type DdAutoIns t = DiAutoIns (DsInfo t)
type DdName t = DiName (DsInfo t)

type GetDS t sch = FromJust (GetDataStruct t sch)
-- type DataKey t = DdKey (Snd t)

-- type WithKey t r = GrecWith (DdKey t) r
-- type WithoutKey t r = GrecWithout (DdKey t) r

-- type DataAutoIns t = DiAutoIns (DdInfo t)

class SingI (DsInfo t) => DataStructInfo (t :: DataStruct) where
  singDataInfo :: Sing (DsInfo t)
  singDataInfo = sing

  dataInfo :: DataInfo Text
  dataInfo = fromSing (singDataInfo @t)

  -- dataInfo :: DataInfo Text
  -- dataInfo = ddInfo (dataDef @t)

  isTable :: Bool
  isTable = case dataInfo @t of { TableInfo {} -> True; _ -> False }

  isView :: Bool
  isView = case dataInfo @t of { ViewInfo {} -> True; _ -> False }

  primaryKey :: [Text]
  primaryKey = diKey (dataInfo @t)
  --
  uniqKeys :: [[Text]]
  uniqKeys = diUniq (dataInfo @t)

  -- foreignKeys :: [FK Text]
  -- foreignKeys = ddFKs (dataDef @t)

  autoIns :: Bool
  autoIns = diAutoIns (dataInfo @t)

  viewText :: Maybe Text
  viewText | isView @t = diSql (dataInfo @t)
           | otherwise = Nothing

  tableName :: Text
  tableName = diName (dataInfo @t) -- fromSing (sing :: Sing (Typ (Fst t)))

  -- fieldNames :: [Text]
  -- fieldNames = fromSing (sing :: Sing (Fsts (Fst t)))

instance SingI (DsInfo t) => DataStructInfo t

showProxy :: (SingI t, SingKind k) => Proxy (t :: k) -> Demote k
showProxy (_ :: Proxy t) = fromSing (sing :: Sing t)

formatS :: Params ps => Format -> ps -> Text
formatS f = TL.toStrict . format f
