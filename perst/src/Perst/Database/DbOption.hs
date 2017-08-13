{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}
module Perst.Database.DbOption
    (
    -- * Backend definition

      DbOption(..)
    -- , DbOptionConstr

    -- * Singletons type machinery

    , Nullable, NullableSym0, NullableSym1
    , DbTypeName, DbTypeNameSym0, DbTypeNameSym1, DbTypeNameSym2

    -- * Functions to take info in runtime

    -- , dbTypeNames, dbTypeNames'
    , DbTypeNames(..)
    -- , DbRecInfo(..)

    -- * Session

    , SessionMonad
    , MonadCons

    -- * DBFields

    , DBEnum(..)
    ) where

import           Control.Monad.Catch        (MonadCatch, MonadMask)
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.Kind                  (Type)
import           Data.List                  (find)
import           Data.Maybe                 (fromMaybe)
import           Data.Proxy                 (Proxy (..))
import           Data.Singletons.Prelude
import           Data.Singletons.TH         (genDefunSymbols)
import           Data.String                (IsString)
import           Data.Text                  (Text, unpack)
-- import           Data.Text.Lazy             (Text, pack, unpack)
import           GHC.Generics               (Generic)

import           Data.Type.Grec             (Convert (..), FieldsConvGrec)
import           Perst.Database.DataDef
import           Perst.Types                (BackTypes)

type SessionMonad b m = ReaderT (Conn b) m

-- | Options for backend
class DbOption (back :: Type) where
  type FieldDB back       :: Type
  type Conn back          :: Type
  type SessionParams back :: Type
  type PrepCmd back       :: Type
  type GenKey back        :: Type -- set to () if generation is impossible
  paramName :: Int -> Text -- ^ How to create param name (like "?1") from param num

  afterCreateTableText :: Text
  afterCreateTableText = ""

  -- deleteConstraintText :: Proxy back -> DeleteConstraint -> Text
  -- deleteConstraintText _ DCRestrict = "ON DELETE RESTRICT"
  -- deleteConstraintText _ DCCascade  = "ON DELETE CASCADE"
  -- deleteConstraintText _ DCSetNull  = "ON DELETE SET NULL"

  runSession :: (MonadIO m, MonadCatch m)
          => SessionParams back -> SessionMonad back m a -> m a
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
  deleteConstraintText :: DelCons -> Text
  deleteConstraintText DcRestrict = "ON DELETE RESTRICT"
  deleteConstraintText DcCascade  = "ON DELETE CASCADE"
  deleteConstraintText DcSetNull  = "ON DELETE SET NULL"

  runCommand :: MonadIO m => Text -> [FieldDB back] -> SessionMonad back m ()
  runCommand sql pars = do
    cmd <- prepareCommand @back sql
    runPrepared @back cmd pars

type family DbTypeName (b::Type) (a::Type) :: Symbol
type family Nullable a :: (Type, Bool) where
  Nullable (Maybe x) = '(x, True)
  Nullable x = '(x, False)

genDefunSymbols [''DbTypeName, ''Nullable]

type MonadCons m = (MonadIO m, MonadMask m)

class SingI (BackTypes b NullableSym0 DbTypeNameSym0 (FieldsConvGrec t))
      => DbTypeNames b t where
  dbTypeNames :: [(Text, Bool)]
  dbTypeNames = fromSing (sing :: Sing (BackTypes b NullableSym0 DbTypeNameSym0 (FieldsConvGrec t)))

instance SingI (BackTypes b NullableSym0 DbTypeNameSym0 (FieldsConvGrec t))
      => DbTypeNames b t
-- class (SingI (FieldNamesConvGrec (GrecF r)), SingI (Typ r)) => DbRecInfo r where
--
--   tableName :: Text
--   tableName = fromSing (sing :: Sing (Typ r))
--
--   fieldNames :: [Text]
--   fieldNames = fromSing (sing :: Sing (FieldNamesConvGrec (GrecF r)))

-- type DbOptionConstr m b d =
--   ( DbOption b
--   -- , DataDefConstr d
--   , SingI (BackTypes b NullableSym0 DbTypeNameSym0 (FieldsConvGrec d))
--   , MonadCons m
--   )

newtype DBEnum (a :: [Symbol]) = DBEnum { getDBEnum :: Text }
  deriving (Show, Eq, Ord, Generic, IsString)

instance (Convert a Text, SingI ss) => Convert a (DBEnum ss) where
  convert = DBEnum . check . convert
    where
      ss = showProxy (Proxy :: Proxy ss)
      check s
        = fromMaybe (error $ "Invalid value '" ++ unpack s ++ "' for DBEnum" ++ show ss)
        $ find (== s) ss

instance Convert Text a => Convert (DBEnum ss) a where
  convert = convert . getDBEnum
