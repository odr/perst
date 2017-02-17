{-# LANGUAGE UndecidableInstances #-}
module Perst.Database.Sqlite
    ( Sqlite, sqlite
    )
    where

import           Control.Monad.Catch        (SomeException, catch, throwM)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.Trans.Reader (ReaderT (..), ask)
import           Data.ByteString            (ByteString)
import           Data.Int                   (Int64)
import           Data.Proxy                 (Proxy (..))
import           Data.Text                  (Text)
import           Data.Text.Format           (Only (..), format)
import           Data.Text.Lazy             (toStrict)
import           Database.SQLite3           (Database, SQLData, close, exec,
                                             open)
import           Perst.Database.Types       (DBOption (..), DbTypeName)
import           Prelude                    as P

data Sqlite

sqlite :: Proxy Sqlite
sqlite = Proxy

type instance DbTypeName Sqlite Int64      = "INTEGER NOT NULL"
type instance DbTypeName Sqlite Text       = "TEXT NOT NULL"
type instance DbTypeName Sqlite Double     = "FLOAT NOT NULL"
type instance DbTypeName Sqlite ByteString = "BLOB NOT NULL"

type instance DbTypeName Sqlite (Maybe Int64     ) = "INTEGER NULL"
type instance DbTypeName Sqlite (Maybe Text      ) = "TEXT NULL"
type instance DbTypeName Sqlite (Maybe Double    ) = "FLOAT NULL"
type instance DbTypeName Sqlite (Maybe ByteString) = "BLOB NULL"

instance DBOption Sqlite where
    type Conn Sqlite            = Database
    type SessionParams Sqlite   = Text
    type FieldDB Sqlite         = SQLData
    paramName _                 = format "?{}" . Only
    afterCreateTableText _      = "IF NOT EXISTS"
    deleteConstraintText _ _    = ""
    runSession _ par sm         = do
      liftIO $ P.print "Make Sqlite Connection!"
      conn <- liftIO $ open par
      -- liftIO $ catch (exec conn "PRAGMA foreign_keys = ON;")
      --             (\(_::SomeException) -> return ()) -- for sqlite3
      catch (runReaderT sm (Proxy, conn) <* liftIO (close conn >> P.print "closed!!!"))
              (\(e::SomeException) -> liftIO (close conn >> P.print "closed!!!") >> throwM e)
    runCommand cmd = do
      liftIO $ P.print cmd
      ask >>= \(_,conn) -> liftIO (exec conn $ toStrict cmd)
