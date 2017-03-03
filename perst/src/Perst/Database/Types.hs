{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE UndecidableInstances      #-}
module Perst.Database.Types
  (
  -- * Table definition

  DeleteConstraint(..)
  , DataDef(..), TableD, ViewD
  , DdName, DdRec, DdKey, DdUniq, DdFrgn, DdUpd

  -- * Backend definition

  , DBOption(..)

  -- * Singletons type machinery

  , Nullable, NullableSym0, NullableSym1
  , DbTypeName, DbTypeNameSym0, DbTypeNameSym1, DbTypeNameSym2

  -- * Good functions to take table info in runtime

  , tableName, fieldNames, dbTypeNames
  , fieldNames', dbTypeNames'
  , getSymbols, primaryKey, uniqKeys, foreignKeys

  -- * Utilities

  , runCommand

  -- * Some other stuffs

  , SessionMonad
  , Subrec
  , FieldNames
  , FieldTypes
  )
 where

import           Control.Monad.Catch           (MonadCatch)
import           Control.Monad.IO.Class        (MonadIO)
import           Control.Monad.Trans.Reader    (ReaderT)
import           Data.Kind                     (Type)
import           Data.Proxy                    (Proxy (..))
import           Data.Singletons.Prelude
import           Data.Singletons.Prelude.Maybe (FromJust)
import           Data.Singletons.TH
import           Data.Tagged                   (Tagged)
import           Data.Text.Lazy                (Text)
import           Data.Type.Grec
import           GHC.Generics                  (Generic)
import           GHC.Prim                      (Proxy#, proxy#)
-- import           GHC.TypeLits                  (ErrorMessage (..), TypeError)
import           GHC.TypeLits                  (KnownSymbol, SomeSymbol (..),
                                                Symbol (..), symbolVal')
import           Perst.Types                   (BackTypes, Submap)

singletons [d|
  data DeleteConstraint = DCCascade
                        | DCRestrict
                        | DCSetNull
      deriving (Show, Eq, Ord)
  |]

promote [d|
  data DataDef
    = TableDef
      { ddn  :: Symbol
      -- , ddVal   :: Type
      , ddr   :: [(Symbol,Type)]
      , ddk   :: [Symbol]
      , ddu  :: [[Symbol]]
      , ddf  :: [([(Symbol,Symbol)],(Symbol,DeleteConstraint))]
      }
    | ViewDef
      { ddn  :: Symbol
      -- , ddVal   :: Type
      , ddr   :: [(Symbol,Type)]
      , ddSql   :: Maybe Symbol
      , ddup   :: [Symbol]
      , ddk   :: [Symbol]
      , ddu  :: [[Symbol]]
      , ddf    :: [([(Symbol,Symbol)],(Symbol,DeleteConstraint))]
      }
    | JoinByKey
      -- { ddVal   :: Type
      { ddr   :: [(Symbol,Type)]
      , ddList  :: [DataDef]
      }
  ddUpd :: DataDef -> [Symbol]
  ddUpd (TableDef _ r _ _ _)    = map fst r
  ddUpd (ViewDef _ _ _ u _ _ _) = u
  ddName (TableDef n _ _ _ _)    = n
  ddName (ViewDef n _ _ _ _ _ _) = n
  ddRec (TableDef _ r _ _ _)    = r
  ddRec (ViewDef _ r _ _ _ _ _) = r
  ddRec (JoinByKey r _)         = r
  ddKey (TableDef _ _ p _ _)    = p
  ddKey (ViewDef _ _ _ _ p _ _) = p
  ddUniq (TableDef _ _ _ u _)    = u
  ddUniq (ViewDef _ _ _ _ _ u _) = u
  ddFrgn (TableDef _ _ _ _ f)    = f
  ddFrgn (ViewDef _ _ _ _ _ _ f) = f
  |]

type TableD v p u f = TableDef (Typ v) (FieldsGrec v) p u f
type ViewD v s upd p u f = ViewDef (Typ v) (FieldsGrec v) s upd p u f

-- type DdRec (dd :: DataDef)= FieldsGrec (DdVal dd)
{-
data DataDef
  = TableDef
    { rec :: Type
    , pk  :: [Symbol]
    , uk  :: [[Symbol]]
    , fk  :: [([(Symbol,Symbol)],(Symbol,DeleteConstraint))]
    }
  | ViewDef
    { rec :: Type
    , sql :: Symbol
    , upd :: [Symbol]
    , pk  :: [Symbol]
    , uk  :: [[Symbol]]
    , fk  :: [([(Symbol,Symbol)],(Symbol,DeleteConstraint))]
    }
  | JoinByKey
    { rec :: Type
    , dds :: [DataDef]
    }
--  |]

class TableLike (a::DataDef) where
  type TabName    a :: Symbol
  type RecordDef  a :: [(Symbol,Type)]
  type Record     a :: Type
  type Updatable  a :: [Symbol]
  type KeyDef     a :: [Symbol]
  type UniqDef    a :: [[Symbol]]
  -- | Foreign keys: [ [(referencing_field, referenced_field)], (table_name, RefType)]
  type FKDef      a :: [([(Symbol,Symbol)],(Symbol,DeleteConstraint))]

instance TableLike  (TableDef r p u f :: DataDef) where
  type TabName    (TableDef r p u f) = Typ r
  type RecordDef  (TableDef r p u f) = Fields r
  type Record     (TableDef r p u f) = r
  type Updatable  (TableDef r p u f) = Map FstSym0 (Fields r)
  type KeyDef     (TableDef r p u f) = p
  type UniqDef    (TableDef r p u f) = u
  type FKDef      (TableDef r p u f) = f

instance TableLike (ViewDef r s upd p u f :: DataDef) where
  type TabName    (ViewDef r sql upd p u f) = Typ r
  type RecordDef  (ViewDef r sql upd p u f) = Fields r
  type Record     (ViewDef r sql upd p u f) = r
  type Updatable  (ViewDef r sql upd p u f) = upd
  type KeyDef     (ViewDef r sql upd p u f) = p
  type UniqDef    (ViewDef r sql upd p u f) = u
  type FKDef      (ViewDef r sql upd p u f) = f

-- data RecordDefSym0 (l0 :: TyFun DataDef [(Symbol,Type)] ) where
--   RecordDefSym0KindInference ::
--     forall (l0 :: TyFun DataDef [(Symbol,Type)] ) (arg0 :: DataDef).
--       KindOf (Apply RecordDefSym0 arg0) ~ KindOf (RecordDefSym1 arg0)
--     => RecordDefSym0 l0
-- type instance Apply RecordDefSym0 l0 = RecordDefSym1 l0
-- type RecordDefSym1 a = RecordDef a
--
instance TableLike (JoinByKey r dds :: DataDef) where
  type TabName    (JoinByKey r dds) = Typ r
  type RecordDef  (JoinByKey r dds) = Fields r -- ConcatMap RecordDefSym0 dds
  type Record     (JoinByKey r dds) = r
  type Updatable  (JoinByKey r dds) = '[]
  type KeyDef     (JoinByKey r dds) = '[]
  type UniqDef    (JoinByKey r dds) = '[]
  type FKDef      (JoinByKey r dds) = '[]
-}

type FieldNames t = Map FstSym0 (DdRec t)
-- type FieldNames' r = FieldNamesGrec r
type FieldTypes t = Map SndSym0 (DdRec t)
-- type FieldTypes' r = FieldTypesGrec r

type Subrec t ns = Tagged ns (ListToPairs (FromJust (Submap ns (DdRec t))))

type family Nullable a :: (Type, Bool) where
  Nullable (Maybe x) = x ::: True
  Nullable x = x ::: False
data NullableSym0 (l0 :: TyFun Type (Type, Bool) ) where
  NullableSym0KindInference ::
    forall (l0 :: TyFun Type (Type, Bool) ) (arg0 :: Type).
      KindOf (Apply NullableSym0 arg0) ~ KindOf (NullableSym1 arg0)
    => NullableSym0 l0
type instance Apply NullableSym0 l0 = NullableSym1 l0
type NullableSym1 a = Nullable a

tableName :: KnownSymbol (DdName t) => Proxy t -> String
tableName (_ :: Proxy t) = fromSing (sing :: Sing (DdName t))

fieldNames :: SingI (FieldNames t) => Proxy t -> [String]
fieldNames (_ :: Proxy t) = fromSing (sing :: Sing (FieldNames t))

fieldNames' :: SingI (FieldNamesGrec r) => Proxy r -> [String]
fieldNames' (_ :: Proxy r) = fromSing (sing :: Sing (FieldNamesGrec r))

dbTypeNames :: SingI (BackTypes b NullableSym0 DbTypeNameSym0 (DdRec t))
            => Proxy (b :: Type) -> Proxy t -> [(String, Bool)]
dbTypeNames (_ :: Proxy b) (_ :: Proxy t)
  = fromSing (sing :: Sing (BackTypes b NullableSym0 DbTypeNameSym0 (DdRec t)))

dbTypeNames' :: SingI (BackTypes b NullableSym0 DbTypeNameSym0 (FieldsGrec r))
            => Proxy (b :: Type) -> Proxy r -> [(String, Bool)]
dbTypeNames' (_ :: Proxy b) (_ :: Proxy r)
  = fromSing (sing :: Sing (BackTypes b NullableSym0 DbTypeNameSym0 (FieldsGrec r)))

primaryKey :: SingI (DdKey t) => Proxy t -> [String]
primaryKey (_ :: Proxy t) = getSymbols (Proxy :: Proxy (DdKey t))

-- posKey :: SingI (PosKey t) => Proxy t -> Maybe [Integer]
-- posKey (_ :: Proxy t) = fromSing (sing :: Sing (PosKey t))

uniqKeys :: SingI (DdUniq t) => Proxy t -> [[String]]
uniqKeys (_ :: Proxy t) = fromSing (sing :: Sing (DdUniq t))

getSymbols  :: SingI k => Proxy (k::[Symbol]) -> [String]
getSymbols (_ :: Proxy k) = fromSing (sing :: Sing k)

foreignKeys :: SingI (DdFrgn t)
            => Proxy t -> [([(String, String)], (String, DeleteConstraint))]
foreignKeys (_ :: Proxy t) = fromSing (sing :: Sing (DdFrgn t))

-- | Options for backend
class DBOption (back :: Type) where
  type FieldDB back       :: Type
  type Conn back          :: Type
  type SessionParams back :: Type
  type PrepCmd back       :: Type
  type GenKey back        :: Type -- set to () if generation is impossible
  paramName :: Proxy back -> Int -> Text -- ^ How to create param name (like "?1") from param num

  afterCreateTableText :: Proxy back -> Text
  afterCreateTableText _ = ""

  deleteConstraintText :: Proxy back -> DeleteConstraint -> Text
  deleteConstraintText _ DCRestrict = "ON DELETE RESTRICT"
  deleteConstraintText _ DCCascade  = "ON DELETE CASCADE"
  deleteConstraintText _ DCSetNull  = "ON DELETE SET NULL"

  runSession :: (MonadIO m, MonadCatch m)
          => Proxy back -> SessionParams back -> SessionMonad back m a -> m a
  prepareCommand :: MonadIO m => Text -> SessionMonad back m (PrepCmd back)
  -- | Executed before runPrepared in insertAuto operation.
  --   We can get there new key and put it into monad.
  preRunInAuto :: MonadIO m => SessionMonad back m ()
  runPrepared :: MonadIO m
              => PrepCmd back -> [FieldDB back] -> SessionMonad back m ()
  runSelect :: MonadIO m => PrepCmd back -> [FieldDB back]
                         -> SessionMonad back m [[FieldDB back]]
  finalizePrepared :: MonadIO m => PrepCmd back -> SessionMonad back m ()
  getLastKey :: MonadIO m => SessionMonad back m (GenKey back)
  execCommand :: MonadIO m => Text -> SessionMonad back m ()

runCommand :: (DBOption back, MonadIO m)
            => Text -> [FieldDB back] -> SessionMonad back m ()
runCommand sql pars = do
  cmd <- prepareCommand sql
  runPrepared cmd pars


type family DbTypeName (b::Type) (a::Type) :: Symbol
data DbTypeNameSym0 (l0 :: TyFun Type (TyFun Type Symbol -> Type)) where
  DbTypeNameSym0KindInference ::
    forall (l0 :: TyFun Type (TyFun Type Symbol -> Type)) (arg0 :: Type).
      KindOf (Apply DbTypeNameSym0 arg0) ~ KindOf (DbTypeNameSym1 arg0)
    => DbTypeNameSym0 l0
type instance Apply DbTypeNameSym0 l0 = DbTypeNameSym1 l0
data DbTypeNameSym1 (l0 :: Type) (l1 :: TyFun Type Symbol) where
  DbTypeNameSym1KindInference ::
    forall (l0 :: Type) (l1 :: TyFun Type Symbol) (arg0 :: Type).
      KindOf (Apply (DbTypeNameSym1 l0) arg0) ~ KindOf (DbTypeNameSym2 l0 arg0)
    => DbTypeNameSym1 l0 l1
type instance Apply (DbTypeNameSym1 l0) l1 = DbTypeNameSym2 l0 l1
type DbTypeNameSym2 a b = DbTypeName a b

type SessionMonad b m = ReaderT (Proxy b, Conn b) m
