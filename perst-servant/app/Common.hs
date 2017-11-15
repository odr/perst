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

import           Data.Aeson.Types             (FromJSON, ToJSON)
import           Data.List                    (intercalate)
import           GHC.Generics                 (Generic)
-- import           GHC.Prim                     (Proxy#, proxy#)
import           Servant

import           Perst.Database.TreeDef       (ToTreeDef)
import           Perst.Servant.API            (PerstAPI)
import           Perst.Test.Data.Customer     (Customer, TCustomer)
import           Perst.Test.Data.CustomerTree (CustomerTree, TCustomerTree)
import           Perst.Test.Data.Db           (Db)

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
