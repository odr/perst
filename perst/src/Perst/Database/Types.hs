{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE UndecidableInstances      #-}
module Perst.Database.Types
  (
  -- * table definition

  DeleteConstraint(..)
  , DataDef(..)
  , TableLike(..)

  -- * backend definition

  , DBOption(..)

  -- * Constraints required for DDL and DML operations

  , TableConstraint
  , TabConstr
  , TabConstrB
  , InsConstr

  -- * Non-select Command returns

  -- , CommandType(..)
  -- , ReturnType
  -- , pInsert, pUpdate, pDelete, pDDL

  -- * Singletons type machinery

  , Nullable, NullableSym0, NullableSym1
  , DbTypeName, DbTypeNameSym0, DbTypeNameSym1, DbTypeNameSym2

  -- * Good functions to take table info in runtime

  , tableName, fieldNames, dbTypeNames, primaryKey, uniqKeys, foreignKeys
  , fieldNames', dbTypeNames'

  -- * Utilities

  , runCommand

  -- * other stuffs

  , SessionMonad
  )
 where

import           Control.Monad.Catch        (MonadCatch)
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.Kind                  (Type)
import           Data.Proxy                 (Proxy (..))
import           Data.Singletons.Prelude
import           Data.Singletons.TH         (singletons)
import           Data.Text.Lazy             (Text)
import           Data.Type.Grec
import           GHC.Exts                   (Constraint)
import           GHC.Generics               (Generic)
import           GHC.Prim                   (Proxy#, proxy#)
import           GHC.TypeLits               (KnownSymbol, SomeSymbol (..),
                                             Symbol (..), symbolVal')
import           Perst.Types                (AllIsNub, AllSubFst, BackTypes,
                                             CheckFK, FkIsNub, IsNub, IsSubFst,
                                             MandatoryFields, Subset)

singletons [d|
  data DeleteConstraint = DCCascade
                        | DCRestrict
                        | DCSetNull
      deriving (Show, Eq, Ord)
  |]

data DataDef
  = TableDef
    { rec :: Type
    , pk  :: [Symbol]
    , uk  :: [[Symbol]]
    , fk  :: [([(Symbol,Symbol)],(Symbol,DeleteConstraint))]
    }

class TableLike (a::k) where
  type TabName    (a :: k) :: Symbol
  type KeyDef     (a :: k) :: [Symbol]
  type RecordDef  (a :: k) :: [(Symbol,Type)]
  type Record     (a :: k) :: Type
  type UniqDef    (a :: k) :: [[Symbol]]
  -- | Foreign keys: [ [(referencing_field, referenced_field)], (table_name, RefType)]
  type FKDef      (a :: k) :: [([(Symbol,Symbol)],(Symbol,DeleteConstraint))]

instance TableLike  (TableDef r p u f :: DataDef) where
  type TabName    (TableDef r p u f) = Typ r
  type RecordDef  (TableDef r p u f) = Fields r
  type Record     (TableDef r p u f) = r
  type KeyDef     (TableDef r p u f) = p
  type UniqDef    (TableDef r p u f) = u
  type FKDef      (TableDef r p u f) = f

type TableConstraint n r p u f
  = ( IsSubFst p r ~ True, AllSubFst u r ~ True, CheckFK f r ~ True
    , IsNub p ~ True, AllIsNub u ~ True, FkIsNub f ~ True
    )

type TabConstr (t :: DataDef) =
  ( TableLike t
  , TableConstraint (TabName t) (RecordDef t) (KeyDef t) (UniqDef t) (FKDef t)
  , KnownSymbol (TabName t)
  , SingI (Map FstSym0 (RecordDef t))
  , SingI (KeyDef t)
  , SingI (UniqDef t)
  , SingI (FKDef t)
  )
type TabConstrB (b :: Type) (t::DataDef) =
  ( TabConstr t
  , SingI (BackTypes b NullableSym0 DbTypeNameSym0 (RecordDef t))
  )

type Mandatory t = MandatoryFields NullableSym0 (RecordDef t)

type InsConstr (b :: Type) (t :: DataDef) (r :: Type) =
  ( TableLike t
  , DBOption b
  , FromGrecConstr r (FieldDB b)
  -- , Subset (Fields r) (RecordDef t) ~ True
  , Subset (Mandatory t) (Map FstSym0 (Fields r)) ~ True
  , SingI (Map FstSym0 (Fields r))
  )

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

tableName :: KnownSymbol (TabName t) => Proxy t -> String
tableName (_ :: Proxy t) = fromSing (sing :: Sing (TabName t))

fieldNames :: SingI (Map FstSym0 (RecordDef t)) => Proxy t -> [String]
fieldNames (_ :: Proxy t) = fromSing (sing :: Sing (Map FstSym0 (RecordDef t)))

fieldNames' :: SingI (Map FstSym0 (Fields r)) => Proxy r -> [String]
fieldNames' (_ :: Proxy r) = fromSing (sing :: Sing (Map FstSym0 (Fields r)))

dbTypeNames :: SingI (BackTypes b NullableSym0 DbTypeNameSym0 (RecordDef t))
            => Proxy (b :: Type) -> Proxy t -> [(String, Bool)]
dbTypeNames (_ :: Proxy b) (_ :: Proxy t)
  = fromSing (sing :: Sing (BackTypes b NullableSym0 DbTypeNameSym0 (RecordDef t)))

dbTypeNames' :: SingI (BackTypes b NullableSym0 DbTypeNameSym0 (Fields r))
            => Proxy (b :: Type) -> Proxy r -> [(String, Bool)]
dbTypeNames' (_ :: Proxy b) (_ :: Proxy r)
  = fromSing (sing :: Sing (BackTypes b NullableSym0 DbTypeNameSym0 (Fields r)))

primaryKey :: SingI (KeyDef t) => Proxy t -> [String]
primaryKey (_ :: Proxy t) = fromSing (sing :: Sing (KeyDef t))

uniqKeys :: SingI (UniqDef t) => Proxy t -> [[String]]
uniqKeys (_ :: Proxy t) = fromSing (sing :: Sing (UniqDef t))

foreignKeys :: SingI (FKDef t)
            => Proxy t -> [([(String, String)], (String, DeleteConstraint))]
foreignKeys (_ :: Proxy t) = fromSing (sing :: Sing (FKDef t))

-- | Options for backend
class DBOption (back :: Type) where
  type FieldDB back       :: Type
  type Conn back          :: Type
  type SessionParams back :: Type
  type PrepCmd back       :: Type
  type GenKey back        :: Type
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
  runPrepared :: MonadIO m
              => PrepCmd back -> [FieldDB back] -> SessionMonad back m ()
  finalizePrepared :: MonadIO m => PrepCmd back -> SessionMonad back m ()
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
