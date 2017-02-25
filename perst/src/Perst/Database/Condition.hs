{-# LANGUAGE ExistentialQuantification #-}
module Perst.Database.Condition
    (
    ) where

import           GHC.TypeLits
import           Perst.Database.Types

data Fld = Fld { fldTable :: DataDef, fldName :: Symbol }

data Literal
  = LSym Symbol
  | LNat Nat

data RVal
  = RvFld Fld
  | RvLit Literal
  | RvQuery DataDef

data Condition
  = Equal Fld RVal
  | GT Fld RVal
  | LT Fld RVal
  | GE Fld RVal
  | LE Fld RVal
  | NOT Condition
  | AND Condition Condition
  | OR Condition Condition
  | IN RVal
  | INQ DataDef Condition
  | Exist DataDef Condition
  | NotExist DataDef Condition
