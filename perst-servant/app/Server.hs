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

import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Reader   (ask)
import           Data.Proxy                   (Proxy (..))
import qualified Data.Text                    as T
import           Data.Text.IO                 as T (readFile, writeFile)
import           GHC.Prim                     (Proxy#, proxy#)
import           Language.Javascript.JQuery   as JQ
import           Network.Wai.Handler.Warp     (defaultSettings, run,
                                               runSettings, setLogger, setPort)
import           Network.Wai.Logger           (withStdoutLogger)
import           Servant                      ((:<|>) (..), Application,
                                               Handler, Raw, Server,
                                               Tagged (..), serve,
                                               serveDirectoryFileServer)
import           Servant.JS                   (jquery, writeJSForAPI)

import           Perst.Database.Condition     (Condition)
import           Perst.Database.DbOption      (DbOption (..))
import           Perst.Database.TreeDef       (ToTreeDef)
import           Perst.Servant.API            (serverPerstAPI)
import           Perst.Test.Data              (initTest)
import           Perst.Test.Data.Customer     (Customer, TCustomer)
import           Perst.Test.Data.CustomerTree (CustomerTree, TCustomerTree)
import           Perst.Test.Data.Db           (Db)

import           Common

main :: IO ()
main = do
  writeJSFiles
  runSession @Db "test.db" $ do
    initTest
    ask >>= liftIO . main_wai . app1

main_wai app = do
    withStdoutLogger $ \aplogger -> do
        let settings = setPort 8081 $ setLogger aplogger defaultSettings
        runSettings settings app

type API' = API :<|> Raw
serverAPI' :: Proxy API'
serverAPI' = Proxy

app1 :: Conn Db -> Application
app1 conn = serve serverAPI' (server3' conn)

server3' :: Conn Db -> Server API'
server3' conn = server3 conn :<|> serveDirectoryFileServer "static"

server3 :: Conn Db -> Server API
server3 conn
  =    position
  :<|> hello
  :<|> marketing
  :<|> serverPerstAPI (proxy# :: Proxy# '(Db, TCustomerTree, CustomerTree)) conn
  :<|> serverPerstAPI (proxy# :: Proxy# '(Db, ToTreeDef TCustomer, Customer)) conn
  where
    position :: Int -> Int -> Handler Position
    position x y = return (Position x (Tagged y))

    hello :: Maybe String -> Handler HelloMessage
    hello mname = return . HelloMessage $ case mname of
      Nothing -> "Hello, anonymous coward"
      Just n  -> "Hello, " ++ n

    marketing :: ClientInfo -> Handler Email
    marketing clientinfo = return (emailForClient clientinfo)

-- apiJS :: T.Text
-- apiJS = jsForAPI serverAPI jquery

writeJSFiles :: IO ()
writeJSFiles = do
  writeJSForAPI serverAPI jquery "static/api.js"
  -- T.writeFile "static/api.js" apiJS1
  JQ.file >>= T.readFile >>= T.writeFile "static/jq.js"
