{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Network.HTTP.Client      (defaultManagerSettings, newManager)
import           Servant.API              ((:<|>) (..))
import           Servant.Client

import           Data.Type.Grec           (Grec (..))
import           Perst.Database.Condition (Condition (..))
import           Perst.Servant.API        (getList, insRec)
import           Perst.Test.Data.Customer (Customer (..), Names (..))

import           Common

position :<|> hello :<|> marketing :<|> tcustomer :<|> customer = client serverAPI

-- queries :: ClientM (Position, HelloMessage, Emai, [Customer])
queries = do
  pos <- position 10 10
  message <- hello (Just "servant")
  em <- marketing (ClientInfo "Alp" "alp@foo.com" 26 ["haskell", "mathematics"])
  insRec customer (Customer 0 (Grec $ Names "ins_client" $ Just "test")
                            Nothing (Just "2") (Just "note 3"))
  clist <- getList customer mempty
  tclist <- getList tcustomer mempty
  return (pos, message, em, clist, tclist)

run :: IO ()
run = do
  manager <- newManager defaultManagerSettings
  res <- runClientM queries (ClientEnv manager (BaseUrl Http "localhost" 8081 ""))
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right (pos, message, em, clist, tclist) -> do
      print "hi"
      print pos
      print message
      print em
      print clist
      print tclist

main = run
