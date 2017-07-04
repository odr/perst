{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

module Advita.Model.Fields where

import           Data.Int                (Int64)
import           Data.Maybe              (fromMaybe)
import           Data.String             (IsString)
import           Data.Text               (Text, pack, unpack)
import           Data.Time.Calendar      (Day)
import           Data.Time.Format        (FormatTime, ParseTime,
                                          defaultTimeLocale, formatTime,
                                          parseTimeM)
import           Data.Time.LocalTime     (LocalTime)
import           GHC.Generics            (Generic)
import           GHC.TypeLits

import           Data.Type.Grec          (Convert (..))
import           Perst.Database.DataDef  (showProxy)
import           Perst.Database.DbOption (DbTypeName)
import           Perst.Database.Sqlite   (SQLData, Sqlite)

newtype Ident a = Ident { getIdent :: SizedInt 10 }
  deriving (Show, Eq, Ord, Generic, Num, Integral, Real, Enum)

type instance DbTypeName Sqlite (Ident a) = "INTEGER"

instance Convert SQLData (Ident a) where
  convert = Ident . convert

instance Convert (Ident a) SQLData where
  convert = convert . getIdent

----------

newtype SizedInt (n :: Nat) = SizedInt { getSizedInt :: Integer }
  deriving (Show, Eq, Ord, Generic, Num, Integral, Real, Enum)

type instance DbTypeName Sqlite (SizedInt n) = "INTEGER"

instance Convert SQLData (SizedInt n) where
  convert = SizedInt . toInteger . (convert :: SQLData -> Int64)

instance (n <= 18) => Convert (SizedInt n) SQLData where
  convert = convert . (fromInteger :: Integer -> Int64) . getSizedInt

---------

newtype VarChar (n::Nat) = VarChar { getVarChar :: Text }
  deriving (Show, Eq, Ord, Generic, IsString, Monoid)

type instance DbTypeName Sqlite (VarChar n) = "TEXT"

instance Convert SQLData (VarChar n) where
  convert = VarChar . convert

instance Convert (VarChar n) SQLData where
  convert = convert . getVarChar

---------

newtype MediumText = MediumText { getMediumText :: Text }
  deriving (Show, Eq, Ord, Generic, IsString, Monoid)

type instance DbTypeName Sqlite MediumText = "TEXT"

instance Convert SQLData MediumText where
  convert = MediumText . convert

instance Convert MediumText SQLData where
  convert = convert . getMediumText

---------

newtype DateTime = DateTime { getDateTime :: LocalTime }
  deriving (Show, Eq, Ord, Generic, ParseTime, FormatTime)

type instance DbTypeName Sqlite DateTime = "TEXT"

instance Convert SQLData DateTime where
  convert = textToTime "datetime" "yyyy-mm-dd hh24:mi:ss" "%F %T" . convert

instance Convert DateTime SQLData where
  convert = convert . pack . formatTime defaultTimeLocale "%F %T"

---------

newtype Date = Date { getDate :: Day }
  deriving (Show, Eq, Ord, Generic, ParseTime, FormatTime)

type instance DbTypeName Sqlite Date = "TEXT"

instance Convert SQLData Date where
  convert = textToTime "date" "yyyy-mm-dd" "%F" . convert

instance Convert Date SQLData where
  convert = convert . pack . formatTime defaultTimeLocale "%F"

---------

newtype Timestamp = Timestamp { getTimestamp :: LocalTime }
  deriving (Show, Eq, Ord, Generic, ParseTime, FormatTime)

type instance DbTypeName Sqlite Timestamp = "TEXT"

instance Convert SQLData Timestamp where
  convert = textToTime "timestamp" "yyyy-mm-dd hh24:mi:ss" "%F %T%Q" . convert

instance Convert Timestamp SQLData where
  convert = convert . pack . formatTime defaultTimeLocale "%F %T%Q"

---------

textToTime ::  ParseTime t => String -> String -> String -> Text -> t
textToTime sType sFmt fmt = convert' . unpack
 where
  convert' s
    = fromMaybe (error  $ "String '" ++ s ++ "' has invalid " ++ sType
                        ++ " format. Valid format is '" ++ sFmt ++ "'")
    $ parseTimeM True defaultTimeLocale fmt s
