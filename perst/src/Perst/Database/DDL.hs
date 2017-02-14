{-# LANGUAGE MagicHash               #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Perst.Database.DDL
    ( DDL(..)
    , rowCreate
    , RowCreateConstr
    , HasNull(..)
--    , RowDDL(..)
    ) where

import           Control.Monad.IO.Class  (MonadIO)
import           Data.Kind               (Type)
import           Data.Proxy              (Proxy (..))
import           Data.Singletons.Prelude
-- import           Data.Text.Format        (format)
-- import           Data.Text               (Text)
-- import qualified Data.Text.Lazy          as TL (null)
import           GHC.Prim                (Proxy#, proxy#)
import           GHC.TypeLits            (KnownSymbol, Symbol (..), symbolVal')
import           Perst.Database.Types
import           Perst.Types

class DDL (b :: Backend) (t :: DataDef) where
    ddlCreate :: (MonadIO m) => Proxy t -> SessionMonad b m ()
    ddlDrop   :: (MonadIO m) => Proxy t -> SessionMonad b m ()

-- | DDL-type-information and conversion from/to type to/from database type.
--   Database type is a type specified in db-library which
--   present different db-types as a sum-type
-- class FieldDDL backend (a :: *) where
--     -- typeName    :: Proxy# backend -> Proxy a -> Text -- ^ name of type in database
--     -- nullStr     :: Proxy# backend -> Proxy# a -> Text -- ^ NULL or NOT NULL
--     -- nullStr _ _ = "NOT NULL"
--     toDb        :: Proxy# backend -> a -> FieldDB backend -- ^ value to database type
--     fromDb      :: Proxy# backend -> FieldDB backend -> Maybe a -- ^ database type to value

-- instance FieldDDL b a => KindToStar a Text where
--     k2s _ = typeName (proxy# :: Proxy# b) (Proxy :: Proxy a)

newtype HasNull = HasNull { unHasNull :: Bool }

type RowCreateConstr (b :: Backend) a =
    (SingKind (Backend,Type), DemoteRep (Backend,Type) ~ String)

rowCreate :: RowCreateConstr b a => Sing b -> Sing (a :: [(Symbol,Type)]) -> [String]
rowCreate (_ :: Sing b) (_ :: Sing a)
        = zipWith (\n t -> n ++ " " ++ t) ns ts
    where
        ns  = fromSing (sing :: Sing (Map FstSym0 a))
        ts  = fromSing (sing :: Sing (BackTypes b a))

{-

class RowDDL backend (a :: [(Symbol,*)]) where
    -- | String to describe a row for table creation
    rowCreate   :: Proxy# backend -> Proxy a -> [Text] -> [Text]

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
-}
