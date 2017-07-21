{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE UndecidableInstances      #-}

module Perst.Database.DataDef
  ( DelCons(..)
  -- * Table definition
  , DataDef'(..), DataDef, DataDef''(..)
  , TableD, ViewD, DataD -- , DataDFC(..)
  , DdName, DdRec, DdFlds, DdKey, DdUniq, DdUpd, AllKeys, DdFrgn, DdAutoIns -- , DdData
  , DdNameSym0, DdRecSym0, DdKeySym0, DdUniqSym0, DdUpdSym0, DdFrgnSym0, DdFldsSym0
  , Dd, Ddf
  , sDdName, sDdRec, sDdFlds, sDdKey, sDdUniq, sDdUpd, sAllKeys, sDdFrgn, sDdAutoIns -- , DdData
  , sDd, sDdf
  , CheckInsMandatory, KeyType
  -- , DataDefConstr
  -- , ddFlds, ddRec
  -- * Good functions to take table info in runtime
  , tableName, fieldNames
  , fieldNames', fieldNamesT
  , primaryKey, uniqKeys, foreignKeys, autoIns
  -- * Some other stuffs
  , Subrec
  , showProxy
  , WithKey, WithoutKey
  )
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
import qualified Data.Text.Lazy                as TL
import           Data.Type.Grec                (AllIsSub, FieldNamesConvGrec,
                                                FieldsGrec, GrecF, GrecWith,
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
  |]

type FK s = ([(s,s)],(s,DelCons))

singletons [d|
  -- data DelCons = DcRestrict | DcCascade | DcSetNull
  --   deriving (Show, Eq, Ord)

  data DataDef'' s t
    = TableDef
      { ddn  :: s
      , ddr  :: [(s,t)]
      , ddfn :: [s]
      , ddk  :: [s]
      , ddu  :: [[s]]
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
      , ddai :: Bool
      }

  data DataDef' s t = DataDefC { dd :: DataDef'' s t, ddf :: [FK s] }

  ddName'    (TableDef n _ _ _ _ _ )     = n
  ddName'    (ViewDef  n _ _ _ _ _  _ _) = n
  ddRec'     (TableDef _ r _ _ _ _ )     = r
  ddRec'     (ViewDef  _ r _ _ _ _  _ _) = r
  ddFlds'    (TableDef _ _ fn _ _ __)     = fn
  ddFlds'    (ViewDef  _ _ fn _ _ __ _ _) = fn
  ddUpd'     (TableDef _ _ fn _ _ _)     = fn
  ddUpd'     (ViewDef  _ _ _ _ u _  _ _) = u
  ddKey'     (TableDef _ _ _ p _ _ )     = p
  ddKey'     (ViewDef  _ _ _ _ _ p  _ _) = p
  ddAutoIns' (TableDef _ _ _ _  _ ai)     = ai
  ddAutoIns' (ViewDef  _ _ _ _  _ _ _ ai) = ai
  ddUniq'    (TableDef _ _ _ _ u _)     = u
  ddUniq'    (ViewDef  _ _ _ _ _ _ u _) = u
  ddName     (DataDefC d _) = ddName' d
  ddRec      (DataDefC d _) = ddRec' d
  ddFlds     (DataDefC d _) = ddFlds' d
  ddUpd      (DataDefC d _) = ddUpd' d
  ddKey      (DataDefC d _) = ddKey' d
  ddAutoIns  (DataDefC d _) = ddAutoIns' d
  ddUniq     (DataDefC d _)   = ddUniq' d
  ddFrgn     (DataDefC _ f)     = f
  allKeys d = ddKey d : ddUniq d
  isNub :: Eq a => [a] -> Bool
  isNub xs = xs == nub xs
  allIsNub :: Eq a => [[a]] -> Bool
  allIsNub = all isNub
  checkFK :: Eq a => [(c,[(a,b)],d)] -> [a] -> Bool
  checkFK fks rs = all (\(_,fs,_) -> all (\(f,_) -> f `elem` rs) fs) fks
  mkRef (a,b,c) = (b,(ddName' a,c))
  mkRefs xs = map mkRef xs
  pIsNub :: (Eq a, Eq b) => ([a], [b]) -> Bool
  pIsNub (as,bs) = isNub as && isNub bs
  fkIsNub :: (Eq a, Eq b) => [(c,[(a,b)],d)] -> Bool
  fkIsNub = all (pIsNub . unzip) . map (\(x,y,z) -> y)
  |]

promoteOnly [d|
  keyType :: Eq s => DataDef' s t -> Maybe [t]
  keyType t = submap (ddKey t) (ddRec t)

  -- checkInsMandatory :: t -> []
  checkInsMandatory fIsNull t fnr
    = isSub ( mandatoryFields fIsNull (ddRec t)
            \\ if ddAutoIns t then ddKey t else []
            ) fnr
  |]

type DataDef = DataDef' Symbol Type

type TableD v pk uk ai = TableD' (Typ v) (FieldsGrec (GrecF v)) pk uk ai

-- class TableDC n r pk uk ai where
--   type TableDC
-- class TableDC (n:: Symbol) (r::[(Symbol, Type)]) (pk :: [Symbol]) (uk :: [[Symbol]]) (ai::Bool)  where
--   type TableD' n r pk uk ai :: DataDef'' Symbol Type

-- instance  ( IsNub (pk ': uk) ~ True
--           , AllIsNub (pk ': uk) ~ True
--           , AllIsSub (pk ': uk) (Map FstSym0 r) ~ True
--           , SingI r, SingI pk, SingI uk, KnownSymbol n
--           )
--       => TableDC n r pk uk ai where
type TableD' n r pk uk ai = TableDef n r (Map FstSym0 r) pk uk ai

-- class DataDFC (d :: DataDef'' Symbol Type)
--               (refs :: [(DataDef'' Symbol Type, [(Symbol, Symbol)], DelCons)])
--  where
--   type DataD d refs :: DataDef

genDefunSymbols [''Typ]

-- instance  ( FkIsNub refs ~ True
--           , CheckFK refs (DdFlds' d) ~ True
--           )
--       => DataDFC d refs where
type DataD d refs = DataDefC d (MkRefs refs)

-- View is not interesting right now... Rewrite later!
type ViewD v = ViewD' (Typ v) (FieldsGrec (GrecF v))

type family ViewD' n r where
  ViewD' n r = ViewDef n r (Map FstSym0 r)

-- type DataDefConstr d =
--   ( SingI (DdFlds d), SingI (DdKey d)
--   , SingI (DdUniq d), SingI (DdFrgn d)
--   , KnownSymbol (DdName d)
--   )

type family Subrec t ns where
  Subrec t ns = Tagged ns (ListToPairs (FromJust (Submap ns (DdRec t))))

genDefunSymbols [''TableD, ''ViewD, ''Subrec]


tableName :: Sing (t :: DataDef) -> String
tableName = fromSing . sDdName

fieldNames :: Sing (t :: DataDef) -> [String]
fieldNames = fromSing . sDdFlds

fieldNames' :: SingI (FieldNamesConvGrec r) => Proxy r -> [String]
fieldNames' (_ :: Proxy r) = fromSing (sing :: Sing (FieldNamesConvGrec r))

fieldNamesT :: SingI (FieldNamesConvGrec r) => Proxy r -> [TL.Text]
fieldNamesT = map TL.pack . fieldNames'

primaryKey :: Sing (t :: DataDef) -> [String]
primaryKey = fromSing . sDdKey

uniqKeys :: Sing (t :: DataDef) -> [[String]]
uniqKeys = fromSing . sDdUniq

foreignKeys :: Sing (t :: DataDef) -> [([(String, String)], (String, DelCons))]
foreignKeys = fromSing . sDdFrgn

autoIns :: Sing (t::DataDef) -> Bool
autoIns = fromSing . sDdAutoIns

showProxy :: (SingI t, SingKind k) => Proxy (t :: k) -> DemoteRep k
showProxy (_ :: Proxy t) = fromSing (sing :: Sing t)

type WithKey t r = GrecWith (DdKey t) r
type WithoutKey t r = GrecWithout (DdKey t) r
