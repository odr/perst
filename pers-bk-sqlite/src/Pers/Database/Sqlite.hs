module Pers.Database.Sqlite
    ( Sqlite
    ) where

import           Control.Monad.Catch
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.Trans.Reader (ReaderT (..))
import           Data.ByteString            (ByteString)
import           Data.Int                   (Int64)
import           Data.Proxy                 (Proxy (..))
import           Data.Text                  (Text)
import           Data.Text.Format           (Only (..), format)
import qualified Data.Text.Lazy             as TL
import           Database.SQLite3
import           Pers.Database.DDL          (FieldDDL (..))
import           Pers.Database.Types        (DBOption (..))
import           Prelude                    as P


data Sqlite

instance DBOption Sqlite where
    type Conn Sqlite            = Database
    type SessionParams Sqlite   = Text
    type FieldDB Sqlite         = SQLData
    paramName _                 = format "?{}" . Only
    runSession _ par sm         = do
        liftIO $ P.print "Make Sqlite Connection!"
        conn <- liftIO $ open par
        -- liftIO $ catch (exec conn "PRAGMA foreign_keys = ON;")
        --             (\(_::SomeException) -> return ()) -- for sqlite3
        catch (runReaderT sm (Proxy, conn) <* liftIO (close conn >> P.print "closed!!!"))
                (\(e::SomeException) -> liftIO (close conn >> P.print "closed!!!") >> throwM e)

instance (FieldDDL Sqlite a) => FieldDDL Sqlite (Maybe a) where
    typeName pb (_::Proxy (Maybe a))
                        = typeName pb (Proxy :: Proxy a)
    toDb pb (Just a) = toDb pb a
    toDb _ Nothing   = SQLNull
    fromDb _ SQLNull = Just Nothing
    fromDb pb a      = Just <$> fromDb pb a
    nullStr _ _         = ""

instance FieldDDL Sqlite Int64 where
    typeName _ _            = "INTEGER"
    toDb _                = SQLInteger
    fromDb _ (SQLInteger a) = Just a
    fromDb _ _              = Nothing

instance FieldDDL Sqlite Text where
    typeName _ _            = "TEXT"
    toDb _                = SQLText
    fromDb _ (SQLText a) = Just a
    fromDb _ _           = Nothing

instance FieldDDL Sqlite Double where
    typeName _ _            = "FLOAT"
    toDb _                 = SQLFloat
    fromDb _ (SQLFloat a) = Just a
    fromDb _ _            = Nothing

instance FieldDDL Sqlite ByteString where
    typeName _ _            = "BLOB"
    toDb _                 = SQLBlob
    fromDb _ (SQLBlob a) = Just a
    fromDb _ _           = Nothing
