{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
module Pers.Database.Types
    ( x
    , RefType(..)
    , DataDef(..)
    , TableLike(..)
    , CheckFK
    , SessionMonad
    , DBOption(..)
    ) where

--import           Bookkeeper
import           Control.Monad.Catch        (MonadCatch)
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.Proxy                 (Proxy (..))
import           Data.Text.Lazy             (Text)
import           Data.Type.KSet             (Subset)
import           GHC.Exts                   (Constraint)
import           GHC.Prim                   (Proxy#, proxy#)
import           GHC.TypeLits               (KnownSymbol, SomeSymbol (..),
                                             Symbol (..), symbolVal')
import           Pers.Types                 (FirstL)

x = 1

data RefType = Parent   -- ^ reference to master data. Delete "Cascade"
             | Ref      -- ^ reference to dictionary.
                        -- Delete "Restricted" or "Set Null" if possible

class GetRefType (r::RefType) where
    getRefType :: Proxy# (r :: RefType) -> RefType
instance GetRefType Parent  where getRefType _ = Parent
instance GetRefType Ref     where getRefType _ = Ref

data DataDef k
    = TableDef
        { name :: Symbol
        , rec  :: [(Symbol, k)]
        , pk   :: [Symbol]
        , uk   :: [[Symbol]]
        , fk   :: [([(Symbol,Symbol)],(Symbol,RefType))]
        }

class TableLike (a::k) where
    type TabName    (a :: k) :: Symbol
    type KeyDef     (a :: k) :: [Symbol]
    type RecordDef  (a :: k) :: [(Symbol,*)]
    type UniqDef    (a :: k) :: [[Symbol]]
    -- | Foreign keys: [ [(referencing_field, referenced_field)], (table_name, RefType)]
    type FKDef      (a :: k) :: [([(Symbol,Symbol)],(Symbol,RefType))]

instance TableLike  (TableDef n rec pk uk fk :: DataDef *) where
    type TabName    (TableDef n rec pk uk fk) = n
    type RecordDef  (TableDef n rec pk uk fk) = rec
    type KeyDef     (TableDef n rec pk uk fk) = pk
    type UniqDef    (TableDef n rec pk uk fk) = uk
    type FKDef      (TableDef n rec pk uk fk) = fk

type family CheckFK (a :: [(k,k2)]) (b :: [([(k,k3)],k4)]) :: Constraint where
    CheckFK a '[] = ()
    CheckFK a ('(bs,b) ': cs) = (Subset (FirstL bs) (FirstL a), CheckFK a cs)

-- | Options for backend
class DBOption back where
    type Conn back
    type FieldDB back
    type SessionParams back
    paramName :: Proxy# back -> Int -> Text -- ^ How to create param name (like "?1") from param num
    runSession :: (MonadIO m, MonadCatch m)
            => Proxy back -> SessionParams back
            -> SessionMonad back m a -> m a

type SessionMonad b m = ReaderT (Proxy b, Conn b) m
