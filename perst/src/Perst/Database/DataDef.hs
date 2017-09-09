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

import           Data.Kind                     (Constraint, Type)
import           Data.List                     (nub)
import           Data.Proxy                    (Proxy (..))
import           Data.Singletons.Prelude
import           Data.Singletons.Prelude.List
import           Data.Singletons.Prelude.Maybe (FromJust)
import           Data.Singletons.TH            (genDefunSymbols, promoteOnly,
                                                singletons, singletonsOnly)
import           Data.Tagged                   (Tagged)
import           Data.Text                     (Text)
import           Data.Text.Format              (Format, format)
import           Data.Text.Format.Params       (Params)
import qualified Data.Text.Lazy                as TL
import           Data.Type.Grec                (AllIsSub, FieldNamesConvGrec,
                                                FieldsGrec, Grec, GrecWith,
                                                GrecWithout, IsSub, IsSubSym0,
                                                ListToPairs, Submap, SubmapSym0,
                                                Typ)
import           GHC.Generics                  (Generic)
import           GHC.TypeLits                  (ErrorMessage (..), KnownSymbol,
                                                TypeError)
import           Perst.Types

singletons [d|
  data DelCons = DcRestrict | DcCascade | DcSetNull
    deriving (Show, Eq, Ord)

  data DataInfo s
    -- = TableInfo
    --   { diName    :: s
    --   , diFields  :: [(s,t)]
    --   , diKey     :: [s]
    --   , diUniq    :: [[s]]
    --   , diAutoIns :: Bool
    --   }
    -- | ViewInfo
    --   { diName    :: s
    --   , diFields   :: [(s,t)]
    --   , diSql     :: Maybe s
    --   , diUpd     :: [s]
    --   , diKey     :: [s]
    --   , diUniq    :: [[s]]
    --   , diAutoIns :: Bool
    --   }
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
type DataKey t = DdKey (Snd t)
type WithKey t r = GrecWith (DataKey t) r
type WithoutKey t r = GrecWithout (DataKey t) r

type DataAutoIns t = DiAutoIns (DdInfo (Snd t))

class (SingI (Snd t), SingI (Map FstSym0 (Fst t)))
    => DataDefInfo (t :: ([(Symbol,Type)], DataDef)) where
  singDataDef :: Sing (Snd t)
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

  fieldNames :: [Text]
  fieldNames = fromSing (sing :: Sing (Map FstSym0 (Fst t)))

instance (SingI (Snd t), SingI (Map FstSym0 (Fst t))) => DataDefInfo t

showProxy :: (SingI t, SingKind k) => Proxy (t :: k) -> Demote k
showProxy (_ :: Proxy t) = fromSing (sing :: Sing t)

formatS :: Params ps => Format -> ps -> Text
formatS f = TL.toStrict . format f
