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

module Main where

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

type API = "position"   :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
      :<|> "hello"      :> QueryParam "name" String :> Get '[JSON] HelloMessage
      :<|> "marketing"  :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email
      :<|> "tcustomer"  :> PerstAPI TCustomerTree CustomerTree
      -- :<|> "customer"   :> PerstAPI (ToTreeDef TCustomer) Customer

data Position = Position
  { xCoord :: Int
  , yCoord :: Tagged "x" Int
  } deriving Generic

instance ToJSON Position

newtype HelloMessage = HelloMessage { msg :: String }
  deriving Generic

instance ToJSON HelloMessage

data ClientInfo = ClientInfo
  { clientName         :: String
  , clientEmail        :: String
  , clientAge          :: Int
  , clientInterestedIn :: [String]
  } deriving Generic

instance FromJSON ClientInfo
instance ToJSON ClientInfo

data Email = Email
  { from    :: String
  , to      :: String
  , subject :: String
  , body    :: String
  } deriving Generic

instance ToJSON Email

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
main :: IO ()
main = runSession @Db "test.db" $ do
  initTest
  ask >>= liftIO . run 8081 . app1

app1 :: Conn Db -> Application
app1 conn = serve positionAPI (server3 conn)

positionAPI :: Proxy API
positionAPI = Proxy

server3 :: Conn Db -> Server API
server3 conn = position
           :<|> hello
           :<|> marketing
           :<|> serverPerstAPI (proxy# :: Proxy# '(Db, TCustomerTree, CustomerTree)) conn
          --  :<|> serverPerstAPI (proxy# :: Proxy# '(Db, ToTreeDef TCustomer, Customer)) conn
  where
    position :: Int -> Int -> Handler Position
    position x y = return (Position x (Tagged y))

    hello :: Maybe String -> Handler HelloMessage
    hello mname = return . HelloMessage $ case mname of
      Nothing -> "Hello, anonymous coward"
      Just n  -> "Hello, " ++ n

    marketing :: ClientInfo -> Handler Email
    marketing clientinfo = return (emailForClient clientinfo)

    -- customerList :: Condition TCustomerTree CustomerTree -> Handler [CustomerTree]
    -- customerList = undefined
    --
    -- customerRec :: Tagged '["id"] Int64 -> Handler CustomerTree
    -- customerRec = undefined
