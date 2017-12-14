{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Perst.Database.Sqlite
    ( Sqlite, SQLData
    )
    where

import           Control.Monad.Catch        (SomeException, catch, throwM)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.Trans.Reader (ReaderT (..), ask)
import           Data.ByteString            (ByteString)
import           Data.Int                   (Int64)
import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import           Data.Text.Format           (Only (..))
import           Data.Type.GrecTree         (AllFld, Convert (..), GPlus,
                                             SConvNames (..), convToMaybe)
import           Database.SQLite3           (Database, SQLData (..), Statement,
                                             StepResult (..), bind, close,
                                             columns, exec, finalize,
                                             lastInsertRowId, open, prepare,
                                             reset, step)

import           Perst.Database.DataDef     (formatS)
import           Perst.Database.DbOption    (DBEnum, DbOption (..), DbTypeName)

data Sqlite

type instance DbTypeName Sqlite Int64      = "INTEGER"
type instance DbTypeName Sqlite Text       = "TEXT"
type instance DbTypeName Sqlite Double     = "FLOAT"
type instance DbTypeName Sqlite ByteString = "BLOB"
type instance DbTypeName Sqlite (DBEnum a) = "TEXT"

instance Convert [SQLData] (Int64,[SQLData]) where
  convert (SQLInteger x : xs) = (x,xs)
  convert x = error $ "Error on convert " ++ show x ++ " to Int64"

instance Convert Int64 [SQLData]  where
  convert = (:[]) . SQLInteger

instance Convert [SQLData] (Double,[SQLData]) where
  convert (SQLFloat x : xs) = (x,xs)
  convert x = error $ "Error on convert " ++ show x ++ " to Double"

instance Convert Double [SQLData]  where
  convert = (:[]) . SQLFloat

instance Convert [SQLData] (Text,[SQLData]) where
  convert (SQLText x : xs) = (x,xs)
  convert x = error $ "Error on convert " ++ show x ++ " to Text"

instance Convert Text [SQLData]  where
  convert = (:[]) . SQLText

instance Convert [SQLData] (ByteString,[SQLData]) where
  convert (SQLBlob x : xs) = (x,xs)
  convert x = error $ "Error on convert " ++ show x ++ " to ByteString"

instance Convert ByteString [SQLData]  where
  convert = (:[]) . SQLBlob

instance Convert (Maybe Int64) [SQLData] where
  convert = maybe [SQLNull] convert
instance Convert (Maybe Double) [SQLData] where
  convert = maybe [SQLNull] convert
instance Convert (Maybe Text) [SQLData] where
  convert = maybe [SQLNull] convert
instance Convert (Maybe ByteString) [SQLData] where
  convert = maybe [SQLNull] convert

instance Convert [SQLData] (Maybe Int64,[SQLData]) where
  convert = convToMaybe SQLNull
instance Convert [SQLData] (Maybe Double,[SQLData]) where
  convert = convToMaybe SQLNull
instance Convert [SQLData] (Maybe Text,[SQLData]) where
  convert = convToMaybe SQLNull
instance Convert [SQLData] (Maybe ByteString,[SQLData]) where
  convert = convToMaybe SQLNull

instance SConvNames AllFld s Int64
instance SConvNames AllFld s Text
instance SConvNames AllFld s Double
instance SConvNames AllFld s ByteString
type instance GPlus Int64 = False
type instance GPlus Text = False
type instance GPlus Double = False
type instance GPlus ByteString = False
type instance GPlus (Maybe a) = False

-- instance SConvNames AllFld s (Maybe Int) where
-- instance SConvNames AllFld s a => SConvNames AllFld s (Maybe a) where
--   type SFldTypes AllFld s (Maybe a) = '[Maybe a]


instance DbOption Sqlite where
    type SessionParams Sqlite = Text
    type Conn Sqlite          = Database
    type FieldDB Sqlite       = SQLData
    type PrepCmd Sqlite       = Statement
    type GenKey Sqlite        = Int64

    paramName                 = formatS "?{}" . Only . (+1)
    afterCreateTableText      = "IF NOT EXISTS"
    deleteConstraintText _    = ""
    runSession par sm         = do
      liftIO $ print @String "Make Sqlite Connection!"
      conn <- liftIO $ open par
      -- liftIO $ catch (exec conn "PRAGMA foreign_keys = ON;")
      --             (\(_::SomeException) -> return ())
      catch (runReaderT sm conn
                <* liftIO (close conn >> print @String "closed!!!"))
            (\(e::SomeException) ->
                liftIO (close conn >> print @String "closed!!!") >> throwM e)
    prepareCommand cmd = do
      liftIO $ print $ "prepareCommand: " <> cmd
      ask >>= \conn -> liftIO (prepare conn cmd)
    preRunInAuto = return ()
    runPrepared stat ps = liftIO $ do
      putStrLn "runPrepared"
      print ps
      reset stat
      bind stat ps
      _ <- step stat
      return ()
    finalizePrepared = liftIO . finalize
    runSelect p ps = liftIO $ do
      putStrLn "runSelect"
      print ps
      reset p
      bind p ps
      loop id
     where
      loop frs = do
        res <- step p
        if res == Done
          then return (frs [])
          else fmap (\r -> frs . (r:)) (columns p) >>= loop

    getLastKey = ask >>= \conn -> liftIO (lastInsertRowId conn)
    execCommand cmd = do
      liftIO $ print $ "execCommand: " <> cmd
      ask >>= \conn -> liftIO (exec conn cmd)
