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
  , DataDef(..)
  , TableLike(..)

  -- * Backend definition

  , DBOption(..)

  -- * Constraints required for DDL and DML operations

  , TableConstraint, TabConstr
  , TableConstraintB, TabConstrB
  , RecConstr
  , DDLConstr
  , InsConstr, InsAutoConstr
  , UpdConstr, UpdByKeyConstr
  , DelConstr, DelByKeyConstr
  , SelConstr

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
  )
 where

import           Control.Monad.Catch           (MonadCatch)
import           Control.Monad.IO.Class        (MonadIO)
import           Control.Monad.Trans.Reader    (ReaderT)
import           Data.Kind                     (Type)
import           Data.Proxy                    (Proxy (..))
import           Data.Singletons.Prelude
import           Data.Singletons.Prelude.List  ((:\\))
import           Data.Singletons.Prelude.Maybe (FromJust)
import           Data.Singletons.TH            (singletons)
import           Data.Tagged                   (Tagged)
import           Data.Text.Lazy                (Text)
import           Data.Type.Grec
import           GHC.Exts                      (Constraint)
import           GHC.Generics                  (Generic)
import           GHC.Prim                      (Proxy#, proxy#)
-- import           GHC.TypeLits                  (ErrorMessage (..), TypeError)
import           GHC.TypeLits                  (KnownSymbol, SomeSymbol (..),
                                                Symbol (..), symbolVal')
import           Perst.Types                   (AllIsNub, AllIsSub, BackTypes,
                                                CheckFK, FkIsNub, IsNub, IsSub,
                                                MandatoryFields, PosList,
                                                Submap)

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
  | ViewDef
    { rec :: Type
    , sql :: Symbol
    , upd :: [Symbol]
    , pk  :: [Symbol]
    , uk  :: [[Symbol]]
    , fk  :: [([(Symbol,Symbol)],(Symbol,DeleteConstraint))]
    }

type IsCheck = 'False

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

type FieldNames t = Map FstSym0 (RecordDef t)
type FieldNames' r = FieldNamesGrec r
type FieldTypes t = Map SndSym0 (RecordDef t)
type FieldTypes' r = FieldTypesGrec r

type TableConstraint t fn p u f
  = ( SingI fn, SingI p, SingI u, SingI f
    , KnownSymbol (TabName t)
    )

type TableConstraintB b t rd p u f =
  ( DBOption b
  , TableConstraint t (Map FstSym0 rd) p u f
  , SingI (BackTypes b NullableSym0 DbTypeNameSym0 rd)
  )

type DDLConstraint b t rd fn p u f =
  ( TableConstraintB b t rd p u f
  , CheckIf IsCheck
    ( CheckFK f fn ~ True
    , IsNub p ~ True
    , AllIsNub u ~ True
    , FkIsNub f ~ True
    , IsSub p fn ~ True
    , AllIsSub u fn ~ True
    )
  )

type RecordConstraint b t rd p u f fnr ftr =
  ( TableConstraintB b t rd p u f
  -- record is subrecord from table record
  , CheckIf IsCheck (Submap fnr rd ~ Just ftr)
  , SingI fnr
  )

type InsertConstraint b t rd upd p u f fnr ftr =
  ( RecordConstraint b t rd p u f fnr ftr
  , CheckIf IsCheck
    ( IsSub fnr upd ~ True
    -- inserted record contains all mandatory fields
    , IsSub (Mandatory rd) fnr ~ True
    )
  )

type InsertAutoConstraint b t rd upd p u f fnr ftr =
  ( RecordConstraint b t rd p u f fnr ftr
  , CheckIf IsCheck
    ( IsSub fnr upd ~ True
    -- inserted record contains all mandatory fields except primary key
    , IsSub (Mandatory rd :\\ p) fnr ~ True
    -- primary key is a single field with the same type as generated by backend
    , Submap p rd ~ Just '[GenKey b]
    )
  )

type SelectConstraint b t rd p u f fnr ftr fnk ftk =
  ( RecordConstraint b t rd p u f fnr ftr
  -- key is subrecord from table record
  , CheckIf IsCheck (Submap fnk rd ~ Just ftk)
  , SingI fnk
  )

type UpdateConstraint b t rd upd p u f fnr ftr fnk ftk =
  ( SelectConstraint b t rd p u f fnr ftr fnk ftk
  , CheckIf IsCheck (IsSub fnr upd ~ True)
  )

type UpdateByKeyConstraint b t rd upd p u f fnr ftr fnk ftk =
  ( UpdateConstraint b t rd upd p u f fnr ftr fnk ftk
  -- Perhaps we need Sort all keys to check it without counting of field's order
  -- but it can slow down compile time and profit is not obvious.
  -- I refuse it taking into account that we also can't make case insensitive comparing.
  -- So keys are both case and order sensitive.
  , CheckIf IsCheck (Elem fnk (p ': u) ~ True)
  )

type DeleteByKeyConstraint b t rd p u f fnk ftk =
  ( RecordConstraint b t rd p u f fnk ftk
  , CheckIf IsCheck (Elem fnk (p ': u) ~ True)
  )

type TabConstr (t :: DataDef) =
  ( TableConstraint t (FieldNames t) (KeyDef t) (UniqDef t) (FKDef t)
  )

type TabConstrB (b :: Type) (t::DataDef) =
  ( TableConstraintB b t (RecordDef t) (KeyDef t) (UniqDef t) (FKDef t)
  )

type DDLConstr' b t rd p u f =
  ( DDLConstraint b t rd (Map FstSym0 rd) p u f
  )

type DDLConstr (b :: Type) (t::DataDef) =
  ( DDLConstr' b t (RecordDef t) (KeyDef t) (UniqDef t) (FKDef t)
  )

type Mandatory rd = MandatoryFields NullableSym0 rd

type RecConstr (b :: Type) (t :: DataDef) (r :: Type) =
  ( RecordConstraint b t (RecordDef t) (KeyDef t) (UniqDef t)
                      (FKDef t) (FieldNames' r) (FieldTypes' r)
  )

type InsConstr b t r =
  ( InsertConstraint b t (RecordDef t) (Updatable t) (KeyDef t) (UniqDef t)
                      (FKDef t) (FieldNames' r) (FieldTypes' r)
  , ConvFromGrec r [FieldDB b]
  )

type InsAutoConstr b t r =
  ( InsertAutoConstraint b t (RecordDef t) (Updatable t) (KeyDef t) (UniqDef t)
                      (FKDef t) (FieldNames' r) (FieldTypes' r)
  , ConvFromGrec r [FieldDB b]
  )

type Subrec t ns = Tagged ns (ListToPairs (FromJust (Submap ns (RecordDef t))))

type UpdConstr b t r k =
  ( UpdateConstraint b t (RecordDef t) (Updatable t) (KeyDef t) (UniqDef t)
                      (FKDef t) (FieldNames' r) (FieldTypes' r)
                      (FieldNames' k) (FieldTypes' k)
  , ConvFromGrec r [FieldDB b]
  , ConvFromGrec k [FieldDB b]
  )

type UpdByKeyConstr b t r (k :: Type) =
  ( UpdateByKeyConstraint b t (RecordDef t) (Updatable t) (KeyDef t) (UniqDef t)
                          (FKDef t) (FieldNames' r) (FieldTypes' r)
                          (FieldNames' k) (FieldTypes' k)
  , ConvFromGrec r [FieldDB b]
  , ConvFromGrec k [FieldDB b]
  )

type DelConstr b t k =
  ( RecConstr b t k
  , ConvFromGrec k [FieldDB b]
  )

type DelByKeyConstr b t (k :: Type) =
  ( DeleteByKeyConstraint b t (RecordDef t) (KeyDef t) (UniqDef t)
                          (FKDef t) (FieldNames' k) (FieldTypes' k)
  , ConvFromGrec k [FieldDB b]
  )

type SelConstr b t r (k :: Type) =
  ( SelectConstraint b t (RecordDef t) (KeyDef t) (UniqDef t)
                      (FKDef t) (FieldNames' r) (FieldTypes' r)
                      (FieldNames' k) (FieldTypes' k)
  , ConvToGrec [FieldDB b] r
  , ConvFromGrec k [FieldDB b]
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

fieldNames :: SingI (FieldNames t) => Proxy t -> [String]
fieldNames (_ :: Proxy t) = fromSing (sing :: Sing (FieldNames t))

fieldNames' :: SingI (FieldNames' r) => Proxy r -> [String]
fieldNames' (_ :: Proxy r) = fromSing (sing :: Sing (FieldNames' r))

dbTypeNames :: SingI (BackTypes b NullableSym0 DbTypeNameSym0 (RecordDef t))
            => Proxy (b :: Type) -> Proxy t -> [(String, Bool)]
dbTypeNames (_ :: Proxy b) (_ :: Proxy t)
  = fromSing (sing :: Sing (BackTypes b NullableSym0 DbTypeNameSym0 (RecordDef t)))

dbTypeNames' :: SingI (BackTypes b NullableSym0 DbTypeNameSym0 (Fields r))
            => Proxy (b :: Type) -> Proxy r -> [(String, Bool)]
dbTypeNames' (_ :: Proxy b) (_ :: Proxy r)
  = fromSing (sing :: Sing (BackTypes b NullableSym0 DbTypeNameSym0 (Fields r)))

primaryKey :: SingI (KeyDef t) => Proxy t -> [String]
primaryKey (_ :: Proxy t) = getSymbols (Proxy :: Proxy (KeyDef t))

-- posKey :: SingI (PosKey t) => Proxy t -> Maybe [Integer]
-- posKey (_ :: Proxy t) = fromSing (sing :: Sing (PosKey t))

uniqKeys :: SingI (UniqDef t) => Proxy t -> [[String]]
uniqKeys (_ :: Proxy t) = fromSing (sing :: Sing (UniqDef t))

getSymbols  :: SingI k => Proxy (k::[Symbol]) -> [String]
getSymbols (_ :: Proxy k) = fromSing (sing :: Sing k)

foreignKeys :: SingI (FKDef t)
            => Proxy t -> [([(String, String)], (String, DeleteConstraint))]
foreignKeys (_ :: Proxy t) = fromSing (sing :: Sing (FKDef t))

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

type CheckIf (a :: Bool) (b :: Constraint) = If a b (() :: Constraint)
-- type family CheckIf (a :: Bool) (b :: Constraint) :: Constraint where
--   CheckIf IsCheck False b = ()
--   CheckIf IsCheck True b  = b
