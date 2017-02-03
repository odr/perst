{-# LANGUAGE MagicHash               #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Pers.Database.DDL
    ( DDL(..)
    , FieldDDL(..)
    , RowDDL(..)
    ) where

import           Control.Monad.IO.Class (MonadIO)
import           Data.Proxy             (Proxy (..))
import           Data.Text.Format       (format)
import           Data.Text.Lazy         (Text)
import qualified Data.Text.Lazy         as TL (null)
import           Data.Type.KSet         (Subset)
import           GHC.Prim               (Proxy#, proxy#)
import           GHC.TypeLits           (KnownSymbol, Symbol (..), symbolVal')
import           Pers.Database.Types
import           Pers.Types


class   ( TableLike a
        , Subset (KeyDef a) (FirstL (RecordDef a))
        , Subset (UniqDef a) (FirstL (RecordDef a))
        , CheckFK (RecordDef a) (FKDef a)
        )
        => DDL backend a where
    createTable :: (MonadIO m) => Proxy a -> SessionMonad backend m ()
    dropTable   :: (MonadIO m) => Proxy a -> SessionMonad backend m ()

-- | DDL-type-information and conversion from/to type to/from database type.
--   Database type is a type specified in db-library which
--   present different db-types as a sum-type
class FieldDDL backend (a :: *) where
    typeName    :: Proxy# backend -> Proxy a -> Text -- ^ name of type in database
    nullStr     :: Proxy# backend -> Proxy# a -> Text -- ^ NULL or NOT NULL
    nullStr _ _ = "NOT NULL"
    toDb        :: Proxy# backend -> a -> FieldDB backend -- ^ value to database type
    fromDb      :: Proxy# backend -> FieldDB backend -> Maybe a -- ^ database type to value

class RowDDL backend (a :: [(Symbol,*)]) where
    -- | String to describe a row for table creation
    rowCreate   :: Proxy# backend -> Proxy a
                -> [Text] -> [Text]

instance RowDDL b ('[]) where
    rowCreate _ _   = id

instance (FieldDDL b v, KnownSymbol n, RowDDL b nvs)
    => RowDDL b ('(n, v) ': nvs)
  where
    rowCreate pb (_ :: Proxy ('(n,v) ': nvs))
        = (format "{} {}{}" ( symbolVal' (proxy# :: Proxy# n)
                            , typeName pb (Proxy :: Proxy v)
                            , if TL.null ns then "" else " " `mappend` ns
                            )
        :) . rowCreate pb (Proxy :: Proxy nvs)
      where
        ns = nullStr pb (proxy# :: Proxy# v)
