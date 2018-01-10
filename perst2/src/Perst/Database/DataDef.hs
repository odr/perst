{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE LambdaCase                #-}
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
import           Data.List                     (find)
import           Data.Proxy                    (Proxy (..))
import           Data.Singletons.Prelude
import           Data.Singletons.Prelude.List  (FilterSym0, FindSym0, sFilter,
                                                sFind)
import           Data.Singletons.Prelude.Maybe (FromJust, FromJustSym0)
import           Data.Singletons.TH            (promoteOnly, singletons)
import           Data.Singletons.TypeRepStar   ()
import           Data.Text                     (Text)
import           Data.Text.Format              (Format, format)
import           Data.Text.Format.Params       (Params)
import qualified Data.Text.Lazy                as TL

import           Data.Type.GrecTree            (singToList)

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

singletons [d|
  getDiName :: DataInfo p -> p
  getDiName (TableInfo {diName = n}) = n
  getDiName (ViewInfo {diName = n})  = n

  getDsName :: DataStruct' s t -> s
  getDsName (DataStructC {dsInfo = di}) = getDiName di

  getRefName :: Ref' s -> s
  getRefName (RefC {refName = rn}) = rn

  getRefFrom :: Ref' s -> s
  getRefFrom (RefC {refFrom = rf}) = rf

  getRefTo :: Ref' s -> s
  getRefTo (RefC {refTo = rf}) = rf

  getRefCols :: Ref' s -> [(s,s)]
  getRefCols (RefC {refCols = rc}) = rc

  getFromRefs :: Eq s => DataStruct' s t -> [Ref' s] -> [Ref' s]
  getFromRefs ds rs = filter ((== getDsName ds) . getRefFrom) rs

  getToRefs :: Eq s => DataStruct' s t -> [Ref' s] -> [Ref' s]
  getToRefs ds rs = filter (\RefC { refTo = rf } -> rf == getDsName ds) rs

  getFromRefByName :: Eq s => s -> s -> [Ref' s] -> Maybe (Ref' s)
  getFromRefByName from ref refs =
    find (\r -> getRefFrom r == from && getRefName r == ref) refs

  getToRefByName :: Eq s => s -> s -> [Ref' s] -> Maybe (Ref' s)
  getToRefByName to ref refs =
    find (\r -> getRefTo r == to && getRefName r == ref) refs

  getDataStructs :: Schema' s t -> [DataStruct' s t]
  getDataStructs (SchemaC {schDS = ds}) = ds

  getRefs :: Schema' s t -> [Ref' s]
  getRefs (SchemaC {schRefs = rs}) = rs

  getDataStruct :: Eq s => s -> Schema' s t -> Maybe (DataStruct' s t)
  getDataStruct s = find ((s ==) . getDsName) . getDataStructs

  -- getDataStructName :: Eq s => s -> Schema' s t -> s
  -- getDataStructName s = getDsName . fromJust . getDataStruct

  getRef :: Eq s => s -> Schema' s t -> Maybe (Ref' s)
  getRef r = find ((r==) . getRefName) . getRefs
 |]

-- promoteOnly [d|
--
--
--
--   |]

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

  isTable :: Bool
  isTable = case dataInfo @t of { TableInfo {} -> True; _ -> False }

  isView :: Bool
  isView = case dataInfo @t of { ViewInfo {} -> True; _ -> False }

  primaryKey :: [Text]
  primaryKey = diKey (dataInfo @t)
  --
  uniqKeys :: [[Text]]
  uniqKeys = diUniq (dataInfo @t)

  autoIns :: Bool
  autoIns = diAutoIns (dataInfo @t)

  viewText :: Maybe Text
  viewText | isView @t = diSql (dataInfo @t)
           | otherwise = Nothing

  tableName :: Text
  tableName = diName (dataInfo @t)

instance SingI (DsInfo t) => DataStructInfo t

showProxy :: (SingI t, SingKind k) => Proxy (t :: k) -> Demote k
showProxy (_ :: Proxy t) = fromSing (sing :: Sing t)

formatS :: Params ps => Format -> ps -> Text
formatS f = TL.toStrict . format f

findSomeSing :: (forall x. Sing (x::k) -> Bool) -> Sing (xs :: [k])
             -> Maybe (SomeSing k)
findSomeSing f = find (\(SomeSing x) -> f x) . singToList

refByName :: Text -> Sing (sch::Schema) -> Maybe (SomeSing Ref)
refByName t = \case
  SSchemaC { sSchRefs = sr } -> find ((t ==) . getRefName') $ singToList sr

singDataStructs :: Sing sch -> Sing (SchDS sch)
singDataStructs = \case SSchemaC { sSchDS = sds } -> sds

singRefs :: Sing sch -> Sing (SchRefs sch)
singRefs = \case SSchemaC { sSchRefs = refs } -> refs

-- singDataStruct :: Sing s -> Sing sch -> Sing (GetDataStruct)
-- singDataStruct = sFind () . singDataStructs

-- singDataStruct :: Sing s -> Sing sch -> Sing

-- getRefName :: Sing (ref :: Ref) -> Text
-- getRefName = fromSing . sGetRefName
--
getRefName' :: SomeSing Ref -> Text
getRefName' (SomeSing x) = fromSing $ sGetRefName x

dsByName :: Text -> Sing (sch::Schema) -> Maybe (SomeSing DataStruct)
dsByName t = \case
  SSchemaC { sSchDS = sds } -> findSomeSing (\ds -> t == fromSing (sGetDsName ds)) sds

-- getDsName :: Sing (ds :: DataStruct) -> Text
-- getDsName = \case
--   SDataStructC { sDsInfo = dsi } -> case dsi of
--     STableInfo { sDiName = n } -> fromSing n
--     SViewInfo  { sDiName = n } -> fromSing n
--
getDsName' :: SomeSing DataStruct -> Text
getDsName' (SomeSing x) = fromSing $ sGetDsName x

-- data SomeType where
--   SomeType :: forall t. Proxy t -> SomeType

dsChildByRefName :: Text -> Sing (sch::Schema) -> Maybe (SomeSing DataStruct)
dsChildByRefName t ssch
  = refByName t ssch >>= \case
      SomeSing ref -> case ref of
          SRefC { sRefFrom = rfrom } -> dsByName (fromSing rfrom) ssch
