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
import           Data.Monoid                ((<>))
import           Data.Proxy                 (Proxy (..))
import           Data.Text                  (Text)
import           Data.Text.Format           (Only (..), format)
import           Data.Text.Lazy             (toStrict)
import           Data.Type.Grec             (Convert (..))
import           Database.SQLite3           (Database, SQLData (..), Statement,
                                             bind, close, exec, finalize,
                                             lastInsertRowId, open, prepare,
                                             reset, step)
import           Perst.Database.Types       (DBOption (..), DbTypeName)

data Sqlite

sqlite :: Proxy Sqlite
sqlite = Proxy

type instance DbTypeName Sqlite Int64      = "INTEGER"
type instance DbTypeName Sqlite Text       = "TEXT"
type instance DbTypeName Sqlite Double     = "FLOAT"
type instance DbTypeName Sqlite ByteString = "BLOB"

instance Convert SQLData Int64 where
  convert (SQLInteger x) = x

instance Convert Int64 SQLData  where
  convert = SQLInteger

instance Convert SQLData Double where
  convert (SQLFloat x) = x

instance Convert Double SQLData  where
  convert = SQLFloat

instance Convert SQLData Text where
  convert (SQLText x) = x

instance Convert Text SQLData  where
  convert = SQLText

instance Convert SQLData ByteString where
  convert (SQLBlob x) = x

instance Convert ByteString SQLData  where
  convert = SQLBlob

instance Convert a SQLData => Convert (Maybe a) SQLData where
  convert Nothing  = SQLNull
  convert (Just a) = convert a

instance Convert SQLData a => Convert SQLData (Maybe a) where
  convert SQLNull = Nothing
  convert x       = Just $ convert x

instance DBOption Sqlite where
    type SessionParams Sqlite   = Text
    type Conn Sqlite            = Database
    type FieldDB Sqlite         = SQLData
    type PrepCmd Sqlite         = Statement
    type GenKey Sqlite          = Int64

    paramName _                 = format "?{}" . Only . (+1)
    afterCreateTableText _      = "IF NOT EXISTS"
    deleteConstraintText _ _    = ""
    runSession _ par sm         = do
      liftIO $ print "Make Sqlite Connection!"
      conn <- liftIO $ open par
      -- liftIO $ catch (exec conn "PRAGMA foreign_keys = ON;")
      --             (\(_::SomeException) -> return ()) -- for sqlite3
      catch (runReaderT sm (Proxy, conn) <* liftIO (close conn >> print "closed!!!"))
              (\(e::SomeException) -> liftIO (close conn >> print "closed!!!") >> throwM e)
    prepareCommand cmd = do
      liftIO $ print $ "prepareCommand: " <> cmd
      ask >>= \(_,conn) -> liftIO (prepare conn $ toStrict cmd)
    preRunInAuto = return ()
    runPrepared stat ps = liftIO $ do
      putStrLn "runPrepared"
      print ps
      reset stat
      bind stat ps
      step stat
      return ()
    finalizePrepared = liftIO . finalize
    getLastKey = ask >>= \(_,conn) -> liftIO (lastInsertRowId conn)
    execCommand cmd = do
      liftIO $ print $ "execCommand: " <> cmd
      ask >>= \(_,conn) -> liftIO (exec conn $ toStrict cmd)
