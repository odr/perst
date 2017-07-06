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
  , TableD, ViewD, DataDFC(..)
  , DdName, DdRec, DdFlds, DdKey, DdUniq, DdUpd, AllKeys, DdFrgn, DdAutoIns -- , DdData
  , DdNameSym0, DdRecSym0, DdKeySym0, DdUniqSym0, DdUpdSym0, DdFrgnSym0, DdFldsSym0
  , Dd, Ddf
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
import           Perst.Types                   (AllIsSub, IsSub, IsSubSym0,
                                                Submap)

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

  ddName'  (TableDef n _ _ _ _ _ )     = n
  ddName'  (ViewDef  n _ _ _ _ _  _ _) = n
  ddRec'   (TableDef _ r _ _ _ _ )     = r
  ddRec'   (ViewDef  _ r _ _ _ _  _ _) = r
  ddFlds'  (TableDef _ _ fn _ _ __)     = fn
  ddFlds'  (ViewDef  _ _ fn _ _ __ _ _) = fn
  ddUpd'   (TableDef _ _ fn _ _ _)     = fn
  ddUpd'   (ViewDef  _ _ _ _ u _  _ _) = u
  ddKey'   (TableDef _ _ _ p _ _ )     = p
  ddKey'   (ViewDef  _ _ _ _ _ p  _ _) = p
  ddAutoIns'   (TableDef _ _ _ _  _ ai)     = ai
  ddAutoIns'   (ViewDef  _ _ _ _  _ _ _ ai) = ai
  ddName    (DataDefC d _) = ddName' d
  ddRec     (DataDefC d _) = ddRec' d
  ddFlds    (DataDefC d _) = ddFlds' d
  ddUpd     (DataDefC d _) = ddUpd' d
  ddKey     (DataDefC d _) = ddKey' d
  ddAutoIns (DataDefC d _) = ddAutoIns' d

  |]
-- promoteOnly [d|
--   isProj ProjDef {} = True
--   isProj _          = False
--   |]
promoteOnly [d|
  ddUniq'  (TableDef _ _ _ _ u _)     = u
  ddUniq'  (ViewDef  _ _ _ _ _ _ u _) = u
  ddUniq  (DataDefC d _)         = DdUniq' d
  ddFrgn  (DataDefC _ f)     = f
  allKeys d = ddKey d : ddUniq d
  |]

promoteOnly [d|
  mkRefs :: [(a,b,c)] -> [(b,(a',c))]
  mkRefs = map (\(a,b,c) -> (b,(ddName' a,c)))

  isNub :: Eq a => [a] -> Bool
  isNub xs = xs == nub xs

  allIsNub :: Eq a => [[a]] -> Bool
  allIsNub = all isNub

  fkIsNub :: (Eq a, Eq b) => [(c,[(a,b)],d)] -> Bool
  fkIsNub = all (pIsNub . unzip) . map (\(x,y,z) -> y)
    where
      pIsNub (as,bs) = isNub as && isNub bs

  checkFK :: Eq a => [(c,[(a,b)],d)] -> [a] -> Bool
  checkFK fks rs = all (\(_,fs,_) -> all (\(f,_) -> f `elem` rs) fs) fks

  |]

type DataDef = DataDef' Symbol Type

type TableD v pk uk ai = TableD' (Typ v) (FieldsGrec (Grec v)) pk uk ai

-- class TableDC n r pk uk ai where
--   type TableDC
class TableDC (n:: Symbol) (r::[(Symbol, Type)]) (pk :: [Symbol]) (uk :: [[Symbol]]) (ai::Bool)  where
  type TableD' n r pk uk ai :: DataDef'' Symbol Type

instance  ( IsNub (pk ': uk) ~ True
          , AllIsNub (pk ': uk) ~ True
          , AllIsSub (pk ': uk) (Map FstSym0 r) ~ True
          , SingI r, SingI pk, SingI uk, KnownSymbol n
          )
      => TableDC n r pk uk ai where
  type TableD' n r pk uk ai = TableDef n r (Map FstSym0 r) pk uk ai

class DataDFC (d :: DataDef'' Symbol Type)
              (refs :: [(DataDef'' Symbol Type, [(Symbol, Symbol)], DelCons)])
 where
  type DataD d refs :: DataDef

genDefunSymbols [''Typ]

instance  ( FkIsNub refs ~ True
          , CheckFK refs (DdFlds' d) ~ True
          )
      => DataDFC d refs where
  type DataD d refs = DataDefC d (MkRefs refs)

-- View is not interesting right now... Rewrite later!
type ViewD v = ViewD' (Typ v) (FieldsGrec (Grec v))

type family ViewD' n r where
  ViewD' n r = ViewDef n r (Map FstSym0 r)

type DataDefConstr d =
  ( SingI (DdFlds d), SingI (DdKey d)
  , SingI (DdUniq d), SingI (DdFrgn d)
  , KnownSymbol (DdName d)
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
