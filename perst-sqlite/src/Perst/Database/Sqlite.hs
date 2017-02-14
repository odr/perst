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
import           Perst.Database.DDL         (DDL (..), FieldDDL (..),
                                             HasNull (..), RowCreateConstr,
                                             rowCreate)
import           Perst.Database.Types       (DBOption (..), DataDef (..),
                                             RefType, SessionMonad,
                                             TableConstraint)
import           Perst.Types                (KindToStar (..))
import           Prelude                    as P


data Sqlite
sqlite :: Proxy Sqlite
sqlite = Proxy

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
    -- typeName pb (_::Proxy (Maybe a))
    --                     = typeName pb (Proxy :: Proxy a)
    toDb pb (Just a) = toDb pb a
    toDb _ Nothing   = SQLNull
    fromDb _ SQLNull = Just Nothing
    fromDb pb a      = Just <$> fromDb pb a
    -- nullStr _ _         = ""

instance FieldDDL Sqlite Int64 where
    -- typeName _ _            = "INTEGER"
    toDb _                = SQLInteger
    fromDb _ (SQLInteger a) = Just a
    fromDb _ _              = Nothing

instance FieldDDL Sqlite Text where
    -- typeName _ _            = "TEXT"
    toDb _                = SQLText
    fromDb _ (SQLText a) = Just a
    fromDb _ _           = Nothing

instance FieldDDL Sqlite Double where
    -- typeName _ _            = "FLOAT"
    toDb _                 = SQLFloat
    fromDb _ (SQLFloat a) = Just a
    fromDb _ _            = Nothing

instance FieldDDL Sqlite ByteString where
    -- typeName _ _            = "BLOB"
    toDb _                 = SQLBlob
    fromDb _ (SQLBlob a) = Just a
    fromDb _ _           = Nothing

instance KindToStar '(Sqlite, Int64     )   String where k2s _ = "INTEGER"
instance KindToStar '(Sqlite, Text      )   String where k2s _ = "TEXT"
instance KindToStar '(Sqlite, Double    )   String where k2s _ = "FLOAT"
instance KindToStar '(Sqlite, ByteString)   String where k2s _ = "BLOB"
instance KindToStar '(Sqlite,a) String => KindToStar '(Sqlite,Maybe a) String
    where k2s _ = k2s (proxy# :: Proxy# '(Sqlite,a))

instance KindToStar '(Sqlite, Int64     )   HasNull where k2s _ = HasNull False
instance KindToStar '(Sqlite, Text      )   HasNull where k2s _ = HasNull False
instance KindToStar '(Sqlite, Double    )   HasNull where k2s _ = HasNull False
instance KindToStar '(Sqlite, ByteString)   HasNull where k2s _ = HasNull False
instance KindToStar '(Sqlite, Maybe a   )   HasNull where k2s _ = HasNull True

instance (RowCreateConstr Sqlite r, TableConstraint n r p u f)
        => DDL Sqlite (TableDef n r p u f)
  where
    ddlCreate _
        = runSqliteDDL
            $ format "CREATE TABLE IF NOT EXISTS {} ({}, PRIMARY KEY ({}) {} {})"
                ( k2s (proxy# :: Proxy# n) :: String
                , TL.intercalate ","
                    $ map TL.pack
                    $ rowCreate (Proxy :: Proxy Sqlite) (Proxy :: Proxy r)
                , intercalate ","
                    (k2s (proxy# :: Proxy# p) :: [String])
                , foldMap (format ",UNIQUE ({})" . Only . intercalate ",")
                    (k2s (proxy# :: Proxy# u) :: [[String]])
                , foldMap ( format ",FOREIGN KEY ({}) REFERENCES {} ({})"
                          . ((,,) <$> intercalate "," . fst . fst
                                  <*> fst . snd
                                  <*> intercalate "," . snd . fst
                            )
                          . first unzip
                          )
                    (k2s (proxy# :: Proxy# f) :: [([(String, String)], (String, RefType))])
                )
    ddlDrop _
        = runSqliteDDL
            $ format "DROP TABLE {}" $ Only (k2s (proxy# :: Proxy# n) :: String)

runSqliteDDL :: (MonadIO m) => TL.Text -> SessionMonad Sqlite m ()
runSqliteDDL cmd = do
    liftIO $ P.print cmd
    ask >>= \(_,conn) -> liftIO (exec conn $ TL.toStrict cmd)
