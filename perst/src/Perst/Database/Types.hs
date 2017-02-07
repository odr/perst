{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE UndecidableInstances #-}
module Perst.Database.Types
--     ( RefType(..)
--     , DataDef(..)
-- --    , TableLike(..)
--     , SessionMonad
--     , DBOption(..)
--     , TableConstraint
--     )
    where

--import           Bookkeeper
import           Control.Monad.Catch        (MonadCatch)
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.Proxy                 (Proxy (..))
import           Data.Text.Lazy             (Text)
import           GHC.Exts                   (Constraint)
import           GHC.Prim                   (Proxy#, proxy#)
import           GHC.TypeLits               (KnownSymbol, SomeSymbol (..),
                                             Symbol (..), symbolVal')
import           Perst.Types                (AllIsNub, AllSubFst, CheckFK,
                                             CheckRec, FkIsNub, IsNub, IsSubFst,
                                             KindToStar (..))

data RefType = Parent   -- ^ reference to master data. Delete "Cascade"
             | Ref      -- ^ reference to dictionary.
                        -- Delete "Restricted" or "Set Null" if possible

class GetRefType (r::RefType) where
    getRefType :: Proxy# (r :: RefType) -> RefType
instance GetRefType Parent  where getRefType _ = Parent
instance GetRefType Ref     where getRefType _ = Ref
instance KindToStar Parent RefType where
    k2s _ = Parent
instance KindToStar Ref RefType where
    k2s _ = Ref

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

instance TableLike  (TableDef n r p u f :: DataDef *) where
    type TabName    (TableDef n r p u f) = n
    type RecordDef  (TableDef n r p u f) = r
    type KeyDef     (TableDef n r p u f) = p
    type UniqDef    (TableDef n r p u f) = u
    type FKDef      (TableDef n r p u f) = f

type TableConstraint n r p u f
    =   ( KindToStar n String, KindToStar p [String], KindToStar u [[String]]
        , KindToStar f [([(String, String)], (String, RefType))]
        , IsSubFst p r ~ True, AllSubFst u r ~ True, CheckFK f r ~ True
        , IsNub p ~ True, AllIsNub u ~ True, FkIsNub f ~ True
        , CheckRec r ~ True
        )

type TabConstr (t :: DataDef *) =
    ( TableLike t
    , TableConstraint (TabName t) (RecordDef t) (KeyDef t) (UniqDef t) (FKDef t)
    )

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
