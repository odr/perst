{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ConstraintKinds            #-}
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

    -- * Singletons type machinery

    , Nullable
    -- , NullableSym0, NullableSym1
    , DbTypeName
    , DbFieldTypes
    -- , DbTypeNameSym0, DbTypeNameSym1, DbTypeNameSym2

    -- * Functions to take info in runtime

    -- , DbTypeNames(..)

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
import           Data.String                (IsString)
import           Data.Text                  (Text, unpack)
-- import           Data.Text.Lazy             (Text, pack, unpack)
import           GHC.Generics               (Generic)

-- import           Data.Type.Grec             (Convert (..), FieldsConvGrec)
import           Data.Type.GrecTree         (ConvNames (..), Convert (..))
import           Perst.Database.DataDef     (DelCons (..), formatS, showProxy)
import           Perst.Types                (NoLstFld)

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

  condLike :: Text -> Text -> Text
  condLike name par
      = formatS "lower({}) LIKE '%' + lower({}) + '%'" (name, par)

type family DbTypeName (b::Type) (a::Type) :: Symbol
type family Nullable a :: (Type, Bool) where
  Nullable (Maybe x) = '(x, 'True)
  Nullable x = '(x, 'False)

type family DbFldTypes b (a::[*]) :: [(Symbol,Bool)] where
  DbFldTypes b '[] = '[]
  DbFldTypes b (a ': as)
    = (If (Snd (Nullable a)) '(DbTypeName b (Fst (Nullable a)), 'True)
                            '(DbTypeName b a, 'False))
    ': DbFldTypes b as
-- genDefunSymbols [''DbTypeName, ''Nullable]

type DbFieldTypes b a = DbFldTypes b (FldTypes NoLstFld a)

type MonadCons m = (MonadIO m, MonadMask m)

-- class SingI (BackTypes b NullableSym0 DbTypeNameSym0 (FieldsConvGrec t))
--       => DbTypeNames b t where
--   dbTypeNames :: [(Text, Bool)]
--   dbTypeNames = fromSing (sing :: Sing (BackTypes b NullableSym0 DbTypeNameSym0 (FieldsConvGrec t)))
--
-- instance SingI (BackTypes b NullableSym0 DbTypeNameSym0 (FieldsConvGrec t))
--       => DbTypeNames b t

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
