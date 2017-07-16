{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
module Perst.Database.DbOption
    (
    -- * Backend definition

      DbOption(..)
    , DbOptionConstr

    -- * Singletons type machinery

    , Nullable, NullableSym0, NullableSym1
    , DbTypeName, DbTypeNameSym0, DbTypeNameSym1, DbTypeNameSym2

    -- * Functions to take info in runtime

    , dbTypeNames, dbTypeNames'

    -- * Session

    , SessionMonad
    , runCommand

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
import           Data.Text.Lazy             (Text, pack, unpack)
import           GHC.Generics               (Generic)

import           Data.Type.Grec             (Convert (..), FieldsGrec)
import           Perst.Database.DataDef
import           Perst.Types                (BackTypes)

type SessionMonad b m = ReaderT (Proxy b, Conn b) m

-- | Options for backend
class DbOption (back :: Type) where
  type FieldDB back       :: Type
  type Conn back          :: Type
  type SessionParams back :: Type
  type PrepCmd back       :: Type
  type GenKey back        :: Type -- set to () if generation is impossible
  paramName :: Proxy back -> Int -> Text -- ^ How to create param name (like "?1") from param num

  afterCreateTableText :: Proxy back -> Text
  afterCreateTableText _ = ""

  -- deleteConstraintText :: Proxy back -> DeleteConstraint -> Text
  -- deleteConstraintText _ DCRestrict = "ON DELETE RESTRICT"
  -- deleteConstraintText _ DCCascade  = "ON DELETE CASCADE"
  -- deleteConstraintText _ DCSetNull  = "ON DELETE SET NULL"

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
  deleteConstraintText :: Proxy back -> DelCons -> Text
  deleteConstraintText _ DcRestrict = "ON DELETE RESTRICT"
  deleteConstraintText _ DcCascade  = "ON DELETE CASCADE"
  deleteConstraintText _ DcSetNull  = "ON DELETE SET NULL"

runCommand :: (DbOption back, MonadIO m)
            => Text -> [FieldDB back] -> SessionMonad back m ()
runCommand sql pars = do
  cmd <- prepareCommand sql
  runPrepared cmd pars

type family DbTypeName (b::Type) (a::Type) :: Symbol
type family Nullable a :: (Type, Bool) where
  Nullable (Maybe x) = '(x, True)
  Nullable x = '(x, False)

genDefunSymbols [''DbTypeName, ''Nullable]


dbTypeNames :: SingI (BackTypes b NullableSym0 DbTypeNameSym0 (DdRec t))
            => Proxy (b :: Type) -> Sing t -> [(String, Bool)]
dbTypeNames (_ :: Proxy b) (_ :: Sing t)
  = fromSing (sing :: Sing (BackTypes b NullableSym0 DbTypeNameSym0 (DdRec t)))

dbTypeNames' :: SingI (BackTypes b NullableSym0 DbTypeNameSym0 (FieldsGrec r))
            => Proxy (b :: Type) -> Proxy r -> [(String, Bool)]
dbTypeNames' (_ :: Proxy b) (_ :: Proxy r)
  = fromSing (sing :: Sing (BackTypes b NullableSym0 DbTypeNameSym0 (FieldsGrec r)))

type DbOptionConstr m b d =
  ( DbOption b
  -- , DataDefConstr d
  , SingI (BackTypes b NullableSym0 DbTypeNameSym0 (DdRec d))
  , MonadIO m, MonadMask m
  )

newtype DBEnum (a :: [Symbol]) = DBEnum { getDBEnum :: Text }
  deriving (Show, Eq, Ord, Generic, IsString)

instance (Convert a Text, SingI ss) => Convert a (DBEnum ss) where
  convert = DBEnum . check . convert
    where
      ss = showProxy (Proxy :: Proxy ss)
      check s
        = fromMaybe (error $ "Invalid value '" ++ unpack s ++ "' for DBEnum" ++ show ss)
        $ find (== s) $ map pack ss

instance Convert Text a => Convert (DBEnum ss) a where
  convert = convert . getDBEnum
