{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}

module Common where

import           Prelude                       ()
import           Prelude.Compat

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Aeson.Compat
import qualified Data.Aeson.Parser
import           Data.Aeson.Types
import           Data.Attoparsec.ByteString
import           Data.ByteString               (ByteString)
import           Data.Int                      (Int64)
import           Data.List
import           Data.Maybe
import           Data.String.Conversions
import           Data.Tagged
import           Data.Time.Calendar
import           GHC.Generics
import           GHC.Prim                      (Proxy#, proxy#)
import           Lucid
import           Network.HTTP.Media            ((//), (/:))
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.Directory
import           Text.Blaze
import qualified Text.Blaze.Html
import           Text.Blaze.Html.Renderer.Utf8

import           Perst.Database.Condition      (Condition)
import           Perst.Database.DbOption
import           Perst.Database.TreeDef        (ToTreeDef)
import           Perst.Servant.API
import           Perst.Test.Data
-- import           Perst.Test.Data.Article
import           Perst.Test.Data.Customer
import           Perst.Test.Data.CustomerTree
import           Perst.Test.Data.Db            (Db)
-- import           Perst.Test.Data.Order
-- import           Perst.Test.Data.OrderTree

data Position = Position
  { xCoord :: Int
  , yCoord :: Tagged "x" Int
  } deriving (Generic, Show)

instance ToJSON Position
instance FromJSON Position

newtype HelloMessage = HelloMessage { msg :: String }
  deriving (Generic, Show)

instance ToJSON HelloMessage
instance FromJSON HelloMessage

data ClientInfo = ClientInfo
  { clientName         :: String
  , clientEmail        :: String
  , clientAge          :: Int
  , clientInterestedIn :: [String]
  } deriving (Generic, Show)

instance FromJSON ClientInfo
instance ToJSON ClientInfo

data Email = Email
  { from    :: String
  , to      :: String
  , subject :: String
  , body    :: String
  } deriving (Generic, Show)

instance ToJSON Email
instance FromJSON Email

emailForClient :: ClientInfo -> Email
emailForClient c = Email from' to' subject' body'

  where from'    = "great@company.com"
        to'      = clientEmail c
        subject' = "Hey " ++ clientName c ++ ", we miss you!"
        body'    = "Hi " ++ clientName c ++ ",\n\n"
                ++ "Since you've recently turned " ++ show (clientAge c)
                ++ ", have you checked out our latest "
                ++ intercalate ", " (clientInterestedIn c)
                ++ " products? Give us a visit!"

serverAPI :: Proxy API
serverAPI = Proxy

type API = "position"   :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
      :<|> "hello"      :> QueryParam "name" String :> Get '[JSON] HelloMessage
      :<|> "marketing"  :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email
      :<|> "tcustomer"  :> PerstAPI TCustomerTree CustomerTree
      :<|> "customer"   :> PerstAPI (ToTreeDef TCustomer) Customer
