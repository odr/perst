{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}
module Perst.Database.Constraints
    -- (
    -- -- * Constraints required for DDL and DML operations
    --
    --   RecConstr
    -- , DDLConstr
    -- , InsConstr, InsAutoConstr
    -- , UpdConstr, UpdByKeyConstr
    -- , DelConstr, DelByKeyConstr
    -- , SelConstr
    -- -- , ddProxy
    --
    -- )
    where

import           Data.Kind                    (Constraint, Type)
import           Data.Singletons.Prelude      (Elem, Error, If, SingI)
import           Data.Singletons.Prelude.List ((:\\))
import           Data.Type.Grec               (ConvFromGrec, ConvToGrec,
                                               FieldNamesConvGrec,
                                               FieldTypesConvGrec, IsSub,
                                               Submap)
import           GHC.TypeLits                 (ErrorMessage (..), KnownSymbol,
                                               SomeSymbol (..), Symbol (..),
                                               TypeError, symbolVal')
import           Perst.Database.DataDef       (AllKeys, CheckInsMandatory,
                                               DataDef, Dd, DdAutoIns, DdFrgn,
                                               DdKey, DdRec, DdRec, DdUpd,
                                               KeyType)
import           Perst.Database.DbOption      (DbOption (..), DbOptionConstr,
                                               NullableSym0)
import           Perst.Types                  (BackTypes, MandatoryFields)

-- type IsCheck = 'True

type DDLConstr m (b :: Type) (t::DataDef) =
  ( DbOptionConstr m b t
  -- , IsProj t ~ False
  )

-- type Mandatory rd = MandatoryFields NullableSym0 rd

type RecConstr m (b :: Type) (t :: DataDef) (r :: Type) =
  ( DbOptionConstr m b t
  , RecConstr' t (FieldNamesConvGrec r) (FieldTypesConvGrec r)
  )

type family RecConstr' t fnr ftr where
  RecConstr' t fnr ftr =
    ( Submap fnr (DdRec t) ~ Just ftr
    , SingI fnr
    )

type InsConstr m b t r =
  ( DbOptionConstr m b t
  , SingI (DdKey t)
  -- , SingI (DdAutoIns t)
  , InsConstr' b t (FieldNamesConvGrec r) (FieldTypesConvGrec r)
  , ConvFromGrec r [FieldDB b]
  )

type family InsConstr' b t fnr ftr where
  InsConstr' b t fnr ftr =
    ( RecConstr' t fnr ftr
    , IsSub fnr (DdUpd t) ~ True
    -- inserted record contains all mandatory fields
    -- , IsSub (Mandatory (DdRec t) :\\ If (DdAutoIns t) (DdKey t) '[]) fnr
    --     ~ True
    , If (CheckInsMandatory NullableSym0 t fnr) (() :: Constraint)
        (TypeError
            ( Text "Inserted record doesn't contain all mandatory fields."
              :$$: Text "Inserted record fields: " :<>: ShowType fnr
              :$$: Text "Mandatory fields: "
                  :<>: ShowType (CheckInsMandatory NullableSym0 t fnr)
              :$$: Text "Table definition: " :<>: ShowType (Dd t)
            ))
    , If (DdAutoIns t) (KeyType t ~ Just '[GenKey b]) (() :: Constraint)
    )

type UpdConstr m b t r k =
  ( RecConstr m b t r
  , UpdConstr' t (FieldNamesConvGrec r) (FieldTypesConvGrec r)
                 (FieldNamesConvGrec k) (FieldTypesConvGrec k)
  , ConvFromGrec r [FieldDB b]
  , ConvFromGrec k [FieldDB b]
  )
type family UpdConstr' t fnr ftr fnk ftk where
  UpdConstr' t fnr ftr fnk ftk =
    ( IsSub fnr (DdUpd t) ~ True
    , Submap fnk (DdRec t) ~ Just ftk
    , SingI fnk
    )

type UpdByKeyConstr m b t r k =
  ( UpdConstr m b t r k
  , Elem (FieldNamesConvGrec k) (AllKeys t) ~ True
  )

type UpdByKeyDiffConstr m b t r k =
  ( UpdByKeyConstr m b t r k
  , Eq (FieldDB b)
  )

type DelConstr m b t k =
  ( RecConstr m b t k
  , ConvFromGrec k [FieldDB b]
  )

type DelByKeyConstr m b t (k :: Type) =
  ( DelConstr m b t k
  , Elem (FieldNamesConvGrec k) (AllKeys t) ~ True
  )

type SelConstr m b t r (k :: Type) =
  ( RecConstr m b t r
  , ConvToGrec [FieldDB b] r
  , ConvFromGrec k [FieldDB b]
  , RecConstr' t (FieldNamesConvGrec k) (FieldTypesConvGrec k)
  )

-- type CheckIf (a :: Bool) (b :: Constraint) = If a b (() :: Constraint)
