module Advita.Model.ContactInfo where
import           GHC.Generics            (Generic)

import           Perst.Database.DataDef  (DataD, DelCons (..), TableD)
import           Perst.Database.DbOption (DBEnum)

import           Advita.Model.Fields     (Ident, VarChar)
import           Advita.Model.User       (U_user, UserRef)
import           Advita.Model.Ward       (Ward, WardRef)

type ContactKind  = DBEnum ["p","h","w"]  -- private, home, work
type ContactType  = DBEnum ["t","m","s"]  -- tel, mail, site

data ContactInfo = ContactInfo
  { id      :: Ident ContactInfo
  , user_id :: Maybe (Ident U_user)
  , ward_id :: Maybe (Ident Ward)
  , typ     :: ContactType
  , kind    :: ContactKind
  , info    :: VarChar 100
  } deriving (Show, Eq, Generic)

type TContactInfo = DataD (TableD ContactInfo '["id"] '[] True)
                         '[ UserRef DcRestrict, WardRef DcRestrict]
