{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE UndecidableInstances #-}
module Perst.Database.Sqlite
    -- ( Sqlite, sqlite
    -- )
    where

import           Control.Arrow              (first)
import           Control.Monad.Catch
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.Trans.Reader (ReaderT (..), ask)
import           Data.ByteString            (ByteString)
import           Data.Int                   (Int64)
import           Data.List                  (intercalate)
import           Data.Proxy                 (Proxy (..))
import           Data.Text                  (Text)
import           Data.Text.Format           (Only (..), format)
import qualified Data.Text.Lazy             as TL
import           Database.SQLite3
import           GHC.Prim                   (Proxy#, proxy#)
import           GHC.TypeLits               (KnownSymbol)
import           Perst.Database.DDL         (DDL (..), rowCreate)
import           Perst.Database.Types
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
    runSession _ par sm         = do
        liftIO $ P.print "Make Sqlite Connection!"
        conn <- liftIO $ open par
        -- liftIO $ catch (exec conn "PRAGMA foreign_keys = ON;")
        --             (\(_::SomeException) -> return ()) -- for sqlite3
        catch (runReaderT sm (Proxy, conn) <* liftIO (close conn >> P.print "closed!!!"))
                (\(e::SomeException) -> liftIO (close conn >> P.print "closed!!!") >> throwM e)

instance (TabConstrB Sqlite (TableDef r p u f))
        => DDL Sqlite (TableDef r p u f)
  where
    ddlCreate pt
        = runSqliteDDL
            $ format "CREATE TABLE IF NOT EXISTS {} ({}, PRIMARY KEY ({}) {} {})"
                ( tableName pt
                , TL.intercalate ","
                    $ map TL.pack
                    $ rowCreate (Proxy :: Proxy Sqlite) pt
                , intercalate "," $ fieldNames pt
                , foldMap (format ",UNIQUE ({})" . Only . intercalate ",")
                    $ uniqKeys pt
                , foldMap ( format ",FOREIGN KEY ({}) REFERENCES {} ({})"
                          . ((,,) <$> intercalate "," . fst . fst
                                  <*> fst . snd
                                  <*> intercalate "," . snd . fst
                            )
                          . first unzip
                          ) $ foreignKeys pt
                )
    ddlDrop pt
        = runSqliteDDL
            $ format "DROP TABLE {}" $ Only (tableName pt :: String)

runSqliteDDL :: (MonadIO m) => TL.Text -> SessionMonad Sqlite m ()
runSqliteDDL cmd = do
    liftIO $ P.print cmd
    ask >>= \(_,conn) -> liftIO (exec conn $ TL.toStrict cmd)
