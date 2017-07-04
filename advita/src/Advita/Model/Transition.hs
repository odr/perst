module Advita.Model.Transition where

import           GHC.Generics           (Generic)

import           Perst.Database.DataDef (DataD, DelCons (..), TableD)

import           Advita.Model.Fields    (Ident, VarChar)
import           Advita.Model.Role      (RoleRef, U_role)
import           Advita.Model.State     (TStateTab, W_state)

data W_transition = W_transition
  { id          :: Ident W_transition
  , id_source   :: Ident W_state
  , id_target   :: Ident W_state
  , name        :: VarChar 50
  , description :: Maybe (VarChar 255)
  , constraint  :: Maybe (VarChar 255)
  } deriving (Show, Eq, Generic)

type TTransitionTab = TableD W_transition '["id"] '[] True

type TTransition = DataD TTransitionTab
                        '[ '(TStateTab, '[ '("id_source", "id")], DcRestrict)
                         , '(TStateTab, '[ '("id_target", "id")], DcRestrict)
                         ]

type TransitionRef (x :: DelCons)
      = '(TTransitionTab, '[ '("id_transition", "id")], x)

data U_transition_role = U_transition_role
  { id            :: Ident U_transition_role
  , id_transition :: Ident W_transition
  , id_role       :: Ident U_role
  }

type TTransitionRole = DataD (TableD U_transition_role '["id"] '[] True)
                            '[TransitionRef DcCascade, RoleRef DcCascade]
