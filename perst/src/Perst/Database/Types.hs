{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE UndecidableInstances      #-}
module Perst.Database.Types
  (
  -- * Table definition

  DeleteConstraint(..)
  , DataDef'(..), DataDef
  -- , DataDef(..)
  , TableD, ViewD
  , DdName, DdRec, DdKey, DdUniq, DdUpd, DdFrgn
  , DdNameSym0, DdRecSym0, DdKeySym0, DdUniqSym0, DdUpdSym0, DdFrgnSym0

  -- * Backend definition

  , DBOption(..)

  -- * Singletons type machinery

  , Nullable, NullableSym0, NullableSym1
  , DbTypeName, DbTypeNameSym0, DbTypeNameSym1, DbTypeNameSym2

  -- * Good functions to take table info in runtime

  , tableName, fieldNames, dbTypeNames
  , fieldNames', dbTypeNames'
  , primaryKey, uniqKeys
  -- , foreignKeys

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
-- import           Data.Semigroup
import           GHC.TypeLits                  (KnownSymbol, SomeSymbol (..),
                                                Symbol (..), symbolVal')
import           Perst.Types                   (BackTypes, Submap)

singletons [d|
  data DeleteConstraint = DCCascade
                        | DCRestrict
                        | DCSetNull
      deriving (Show, Eq, Ord)
  |]

singletons [d|
  data DataDef' s t
    = TableDef
      { ddn :: s -- Symbol
      , ddr :: [(s,t)] -- [(Symbol,Type)]
      , ddk :: [s] -- [Symbol]
      , ddu :: [[s]] --[[Symbol]]
      , ddf :: [([(s,s)],(s,DeleteConstraint))] --[FK]
      }
    | ViewDef
      { ddn   :: s -- Symbol
      , ddr   :: [(s,t)] -- [(Symbol,Type)]
      , ddSql :: Maybe s -- Maybe Symbol
      , ddup  :: [s] -- [Symbol]
      , ddk   :: [s] -- [Symbol]
      , ddu   :: [[s]] -- [[Symbol]]
      , ddf   :: [([(s,s)],(s,DeleteConstraint))] -- [FK]
      }
  -- ddUpd :: DataDef' s t -> [s] -- [Symbol]
  ddUpd (TableDef _ r _ _ _)    = map fst r
  ddUpd (ViewDef _ _ _ u _ _ _) = u
  ddName (TableDef n _ _ _ _)    = n
  ddName (ViewDef n _ _ _ _ _ _) = n
  ddRec (TableDef _ r _ _ _)    = r
  ddRec (ViewDef _ r _ _ _ _ _) = r
  ddKey (TableDef _ _ p _ _)    = p
  ddKey (ViewDef _ _ _ _ p _ _) = p
  ddUniq (TableDef _ _ _ u _)    = u
  ddUniq (ViewDef _ _ _ _ _ u _) = u
  ddFrgn (TableDef _ _ _ _ f)    = f
  ddFrgn (ViewDef _ _ _ _ _ _ f) = f
  |]
singletonsOnly
  [d|
  fieldNames t = map fst $ ddRec t
  fieldTypes t = map snd $ ddRec t
  |]

type DataDef = DataDef' Symbol Type

type family TableD v p u where
  TableD v p u = TableDef (Typ v) (FieldsGrec v) p u
type family ViewD v s upd p u where
  ViewD v s upd p u = ViewDef (Typ v) (FieldsGrec v) s upd p u

type family Subrec t ns where
  Subrec t ns = Tagged ns (ListToPairs (FromJust (Submap ns (DdRec t))))

type family Nullable a :: (Type, Bool) where
  Nullable (Maybe x) = x ::: True
  Nullable x = x ::: False

type family DbTypeName (b::Type) (a::Type) :: Symbol

genDefunSymbols [''TableD, ''ViewD, ''Subrec
                , ''Nullable, ''DbTypeName]

tableName :: KnownSymbol (DdName t) => Proxy t -> String
tableName (_ :: Proxy t) = fromSing (sing :: Sing (DdName t))

fieldNames :: SingI (FieldNames t) => Proxy (t :: DataDef) -> [String]
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

primaryKey :: SingI (DdKey t) => Proxy (t :: DataDef) -> [String]
primaryKey (_ :: Proxy t) = fromSing (sing :: Sing (DdKey t))

-- posKey :: SingI (PosKey t) => Proxy t -> Maybe [Integer]
-- posKey (_ :: Proxy t) = fromSing (sing :: Sing (PosKey t))

uniqKeys :: SingI (DdUniq t) => Proxy (t :: DataDef) -> [[String]]
uniqKeys (_ :: Proxy t) = fromSing (sing :: Sing (DdUniq t))

-- getSymbols  :: SingI k => Proxy (k::[Symbol]) -> [String]
-- getSymbols (_ :: Proxy k) = fromSing (sing :: Sing k)

foreignKeys :: SingI (DdFrgn t)
            => Proxy (t :: DataDef) -> [([(String, String)], (String, DeleteConstraint))]
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

type SessionMonad b m = ReaderT (Proxy b, Conn b) m
