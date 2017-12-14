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
  -- -- , DataDefInfo(..)
  -- , tableName, fieldNames, primaryKey, uniqKeys, foreignKeys, autoIns, viewText
  -- -- , fieldNames', fieldNamesT
  -- -- * Some other stuffs
  -- , Subrec
  -- , showProxy
  -- , WithKey, WithoutKey
  -- , formatS
  -- )
  where

import           Data.Proxy              (Proxy (..))
import           Data.Singletons.Prelude
import           Data.Singletons.TH      (singletons)
import           Data.Text               (Text)
import           Data.Text.Format        (Format, format)
import           Data.Text.Format.Params (Params)
import qualified Data.Text.Lazy          as TL
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
      }

  data FK s = FKC
    { fkRefTab  :: s
    , fkDelCons :: DelCons
    , fkRefs    :: [(s,s)]
    }

  data DataDef' s = DataDefC
    { ddInfo :: DataInfo s
    , ddFKs :: [FK s]
    }

  |]

deriving instance Show s => Show (DataInfo s)
deriving instance Show s => Show (FK s)
deriving instance Show s => Show (DataDef' s)

type DataDef = DataDef' Symbol

type DdKey t = DiKey (DdInfo t)
type DdAutoIns t = DiAutoIns (DdInfo t)
-- type DataKey t = DdKey (Snd t)

-- type WithKey t r = GrecWith (DdKey t) r
-- type WithoutKey t r = GrecWithout (DdKey t) r

-- type DataAutoIns t = DiAutoIns (DdInfo t)

class SingI t => DataDefInfo (t :: DataDef) where
  singDataDef :: Sing t
  singDataDef = sing

  dataDef :: DataDef' Text
  dataDef = fromSing (singDataDef @t)

  dataInfo :: DataInfo Text
  dataInfo = ddInfo (dataDef @t)

  isTable :: Bool
  isTable = case dataInfo @t of { TableInfo {} -> True; _ -> False }

  isView :: Bool
  isView = case dataInfo @t of { ViewInfo {} -> True; _ -> False }

  primaryKey :: [Text]
  primaryKey = diKey (dataInfo @t)
  --
  uniqKeys :: [[Text]]
  uniqKeys = diUniq (dataInfo @t)

  foreignKeys :: [FK Text]
  foreignKeys = ddFKs (dataDef @t)

  autoIns :: Bool
  autoIns = diAutoIns (dataInfo @t)

  viewText :: Maybe Text
  viewText | isView @t = diSql (dataInfo @t)
           | otherwise = Nothing

  tableName :: Text
  tableName = diName (dataInfo @t) -- fromSing (sing :: Sing (Typ (Fst t)))

  -- fieldNames :: [Text]
  -- fieldNames = fromSing (sing :: Sing (Fsts (Fst t)))

instance SingI t => DataDefInfo t

showProxy :: (SingI t, SingKind k) => Proxy (t :: k) -> Demote k
showProxy (_ :: Proxy t) = fromSing (sing :: Sing t)

formatS :: Params ps => Format -> ps -> Text
formatS f = TL.toStrict . format f
