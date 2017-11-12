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

-- import           Prelude                       ()
-- import           Prelude.Compat
--
-- import           Control.Monad.Except
import           Control.Monad.Reader         (ask, liftIO)
-- import           Data.Aeson.Compat
-- import qualified Data.Aeson.Parser
-- import           Data.Aeson.Types
-- import           Data.Attoparsec.ByteString
-- import           Data.ByteString               (ByteString)
-- import           Data.Int                      (Int64)
-- import           Data.List
-- import           Data.Maybe
-- import           Data.String.Conversions
-- import           Data.Tagged
-- import           Data.Time.Calendar
-- import           GHC.Generics
import           GHC.Prim                     (Proxy#, proxy#)
-- import           Lucid
-- import           Network.HTTP.Media           ((//), (/:))
-- import           Network.Wai
import           Network.Wai.Handler.Warp     (run)
import           Servant                      ((:<|>) (..), Application,
                                               Handler, Server, Tagged (..),
                                               serve)
-- import           System.Directory
-- import           Text.Blaze
-- import qualified Text.Blaze.Html
-- import           Text.Blaze.Html.Renderer.Utf8

import           Perst.Database.Condition     (Condition)
import           Perst.Database.DbOption      (DbOption (..))
import           Perst.Database.TreeDef       (ToTreeDef)
import           Perst.Servant.API            (serverPerstAPI)
import           Perst.Test.Data              (initTest)
-- import           Perst.Test.Data.Article
import           Perst.Test.Data.Customer     (Customer, TCustomer)
import           Perst.Test.Data.CustomerTree (CustomerTree, TCustomerTree)
import           Perst.Test.Data.Db           (Db)

import           Common

main :: IO ()
main = runSession @Db "test.db" $ do
  initTest
  ask >>= liftIO . run 8081 . app1

app1 :: Conn Db -> Application
app1 conn = serve serverAPI (server3 conn)

server3 :: Conn Db -> Server API
server3 conn = position
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
