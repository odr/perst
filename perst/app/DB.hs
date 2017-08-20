module DB where
import           Data.Int                (Int64)
import           Data.Text               hiding (map)


import           Data.Type.Grec          (Convert (..))
import           Perst.Database.DbOption (DbOption (..), DbTypeName)

data DB

type instance DbTypeName DB Int64      = "INTEGER"
type instance DbTypeName DB Text       = "TEXT"
type instance DbTypeName DB Double     = "DOUBLE"

data DBData = DBNull | DBText Text | DBInteger Int64 | DBDouble Double
  deriving Eq

instance Convert DBData Int64 where
  convert (DBInteger x) = x

instance Convert Int64 DBData  where
  convert = DBInteger

instance Convert DBData Double where
  convert (DBDouble x) = x

instance Convert Double DBData  where
  convert = DBDouble

instance Convert DBData Text where
  convert (DBText x) = x

instance Convert Text DBData  where
  convert = DBText

instance Convert a DBData => Convert (Maybe a) DBData where
  convert Nothing  = DBNull
  convert (Just a) = convert a

instance Convert DBData a => Convert DBData (Maybe a) where
  convert DBNull = Nothing
  convert x      = Just $ convert x

-- singletonStar [''Text, ''Double, ''Int64, ''Maybe]

instance DbOption DB where
    type SessionParams DB   = Text
    type Conn DB            = DB
    type FieldDB DB         = DBData
    type PrepCmd DB         = DB
    type GenKey DB          = Int64
