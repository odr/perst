{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE UndecidableInstances      #-}
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
import           Data.Kind                  (Type)
import           Data.Proxy                 (Proxy (..))
import           Data.Singletons.Prelude
import           Data.Singletons.TH         (promote, singletons)
import           Data.Text.Lazy             (Text)
import           GHC.Exts                   (Constraint)
import           GHC.Prim                   (Proxy#, proxy#)
import           GHC.TypeLits               (KnownSymbol, SomeSymbol (..),
                                             Symbol (..), symbolVal')
import           Perst.Types                (AllIsNub, AllSubFst, CheckFK,
                                             CheckRec, FkIsNub, IsNub, IsSubFst)

singletons [d|
    data RefType = Parent   -- ^ reference to master data. Delete "Cascade"
                 | Ref      -- ^ reference to dictionary.
        deriving (Show, Eq, Ord)
                            -- Delete "Restricted" or "Set Null" if possible
    data Backend
    |]
-- promote [d|
--     data Test = C1 {xxx :: (Symbol, Type)}
--     |]
data DataDef
    = TableDef
        { name :: Symbol
        , rec  :: [(Symbol, Type)]
        , pk   :: [Symbol]
        , uk   :: [[Symbol]]
        , fk   :: [([(Symbol,Symbol)],(Symbol,RefType))]
        }


class TableLike (a::k) where
    type TabName    (a :: k) :: Symbol
    type KeyDef     (a :: k) :: [Symbol]
    type RecordDef  (a :: k) :: [(Symbol,Type)]
    type UniqDef    (a :: k) :: [[Symbol]]
    -- | Foreign keys: [ [(referencing_field, referenced_field)], (table_name, RefType)]
    type FKDef      (a :: k) :: [([(Symbol,Symbol)],(Symbol,RefType))]

instance TableLike  (TableDef n r p u f :: DataDef) where
    type TabName    (TableDef n r p u f) = n
    type RecordDef  (TableDef n r p u f) = r
    type KeyDef     (TableDef n r p u f) = p
    type UniqDef    (TableDef n r p u f) = u
    type FKDef      (TableDef n r p u f) = f

type TableConstraint n r p u f
    =   ( IsSubFst p r ~ True, AllSubFst u r ~ True, CheckFK f r ~ True
        , IsNub p ~ True, AllIsNub u ~ True, FkIsNub f ~ True
        , CheckRec r ~ True
        )

type TabConstr (t :: DataDef) =
    ( TableLike t
    , TableConstraint (TabName t) (RecordDef t) (KeyDef t) (UniqDef t) (FKDef t)
    )

-- | Options for backend
class DBOption (back :: Backend) where
    type Conn back          :: Type
    type FieldDB back       :: Type
    type SessionParams back :: Type
    paramName :: Sing back -> Int -> Text -- ^ How to create param name (like "?1") from param num
    runSession :: (MonadIO m, MonadCatch m)
            => Sing back -> SessionParams back -> SessionMonad back m a -> m a

class DBTypes (b :: Backend) (t :: Type) where
    typeName    :: Proxy# b -> Proxy t -> String -- ^ Name of db-type with NOT NULL modifier
    fromDb      :: Proxy# b -> FieldDB b -> t
    toDb        :: Proxy# b -> t -> FieldDB b

type SessionMonad b m = ReaderT (Proxy b, Conn b) m


-- ----------------------
-- data instance Sing (a :: (Symbol, Type)) where
--     STupSymTypeRep :: Typeable (Snd a) => Sing a
--
-- instance Typeable (Snd a) => SingI (a :: (Symbol, Type)) where
--   sing = STupSymTypeRep
-- instance SingKind (Symbol, Type) where
--   type DemoteRep (Symbol, Type) = (String, TypeRep)
--   fromSing (STupSymTypeRep :: Sing (a,b)) = (fromSing a, typeOf (undefined :: b))
--   toSing = undefined
--
-- instance PEq ('Proxy :: Proxy Type) where
--   type (a :: *) :== (b :: *) = a == b
--
-- instance SEq Type where
--   (STypeRep :: Sing a) %:== (STypeRep :: Sing b) =
--     case (eqT :: Maybe (a :~: b)) of
--       Just Refl -> STrue
--       Nothing   -> unsafeCoerce SFalse
--                     -- the Data.Typeable interface isn't strong enough
--                     -- to enable us to define this without unsafeCoerce
--
-- instance SDecide Type where
--   (STypeRep :: Sing a) %~ (STypeRep :: Sing b) =
--     case (eqT :: Maybe (a :~: b)) of
--       Just Refl -> Proved Refl
--       Nothing   -> Disproved (\Refl -> error "Data.Typeable.eqT failed")
--
-- -- TestEquality instance already defined, but we need this one:
-- instance TestCoercion Sing where
--   testCoercion (STypeRep :: Sing a) (STypeRep :: Sing b) =
--     case (eqT :: Maybe (a :~: b)) of
--       Just Refl -> Just Coercion
--       Nothing   -> Nothing

-------------------------
-- data instance Sing (a :: (Symbol, Type)) where
--     STupSymTypeRep :: Typeable (Snd a) => Sing a
-- instance SingKind (Symbol,Type) where
