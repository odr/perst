{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeInType                #-}
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

import           Control.Monad.Catch        (MonadCatch)
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.Kind                  (Type)
import           Data.Proxy                 (Proxy (..))
import           Data.Singletons.Prelude
import           Data.Singletons.TH         (promote, singletons)
import           Data.Text.Lazy             (Text)
import           Data.Type.Grec
import           GHC.Exts                   (Constraint)
import           GHC.Prim                   (Proxy#, proxy#)
import           GHC.TypeLits               (KnownSymbol, SomeSymbol (..),
                                             Symbol (..), symbolVal')
import           Perst.Types                (AllIsNub, AllSubFst, BackTypes,
                                             CheckFK, FkIsNub, IsNub, IsSubFst)

singletons [d|
    data RefType = Parent   -- ^ reference to master data. Delete "Cascade"
                 | Ref      -- ^ reference to dictionary.
        deriving (Show, Eq, Ord)
                            -- Delete "Restricted" or "Set Null" if possible
    |]

data DataDef
    = TableDef
        { rec :: Type
        , pk  :: [Symbol]
        , uk  :: [[Symbol]]
        , fk  :: [([(Symbol,Symbol)],(Symbol,RefType))]
        }

class TableLike (a::k) where
    type TabName    (a :: k) :: Symbol
    type KeyDef     (a :: k) :: [Symbol]
    type RecordDef  (a :: k) :: [(Symbol,Type)]
    type UniqDef    (a :: k) :: [[Symbol]]
    -- | Foreign keys: [ [(referencing_field, referenced_field)], (table_name, RefType)]
    type FKDef      (a :: k) :: [([(Symbol,Symbol)],(Symbol,RefType))]

instance TableLike  (TableDef r p u f :: DataDef) where
    type TabName    (TableDef r p u f) = Cons r
    type RecordDef  (TableDef r p u f) = Fields r
    type KeyDef     (TableDef r p u f) = p
    type UniqDef    (TableDef r p u f) = u
    type FKDef      (TableDef r p u f) = f

type TableConstraint n r p u f
    =   ( IsSubFst p r ~ True, AllSubFst u r ~ True, CheckFK f r ~ True
        , IsNub p ~ True, AllIsNub u ~ True, FkIsNub f ~ True
        )

type TabConstr (t :: DataDef) =
    ( TableLike t
    , TableConstraint (TabName t) (RecordDef t) (KeyDef t) (UniqDef t) (FKDef t)
    , KnownSymbol (TabName t)
    , SingI (Map FstSym0 (RecordDef t))
    , SingI (UniqDef t)
    , SingI (FKDef t)
    )
type TabConstrB (b :: Type) (t::DataDef) =
    ( TabConstr t
    , SingI (BackTypes b DbTypeNameSym0 (RecordDef t))
    )

tableName :: TabConstr t => Proxy t -> String
tableName (_ :: Proxy t) = fromSing (sing :: Sing (TabName t))

fieldNames :: TabConstr t => Proxy t -> [String]
fieldNames (_ :: Proxy t) = fromSing (sing :: Sing (Map FstSym0 (RecordDef t)))

dbTypeNames :: (TabConstrB b t)
            => Proxy (b :: Type) -> Proxy t -> [String]
dbTypeNames (_ :: Proxy b) (_ :: Proxy t)
  = fromSing (sing :: Sing (BackTypes b DbTypeNameSym0 (RecordDef t)))

uniqKeys :: TabConstr t => Proxy t -> [[String]]
uniqKeys (_ :: Proxy t) = fromSing (sing :: Sing (UniqDef t))

foreignKeys :: TabConstr t
            => Proxy t -> [([(String, String)], (String, RefType))]
foreignKeys (_ :: Proxy t) = fromSing (sing :: Sing (FKDef t))

-- | Options for backend
class DBOption (back :: Type) where
  type Conn back          :: Type
  type FieldDB back       :: Type
  type SessionParams back :: Type
  paramName :: Sing back -> Int -> Text -- ^ How to create param name (like "?1") from param num
  runSession :: (MonadIO m, MonadCatch m)
          => Proxy back -> SessionParams back -> SessionMonad back m a -> m a

-- class DBTypes (b :: Backend) (t :: Type) where
--     typeName    :: Proxy# b -> Proxy t -> String -- ^ Name of db-type with NOT NULL modifier
--     fromDb      :: Proxy# b -> FieldDB b -> t
--     toDb        :: Proxy# b -> t -> FieldDB b

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
