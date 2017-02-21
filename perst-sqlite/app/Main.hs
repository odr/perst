{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

import           Data.Int               (Int64)
import qualified Data.Text              as T

import           Control.Monad.Catch    (SomeException, catch)
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Proxy             (Proxy (..))
import           Data.Tagged
import           GHC.Generics           (Generic)
import           Perst.Database.DDL
import           Perst.Database.DML
import           Perst.Database.Sqlite
import           Perst.Database.Types
-- import           Perst.Types            ((:::))

newtype NInt = NInt Int64 deriving (Show, Eq, Ord, Generic)
type instance DbTypeName Sqlite NInt = "INTEGER NOT NULL"

data Tab = Rec
  { id   :: Int64
  , name :: T.Text
  , val  :: Maybe Double
  , x    :: Int64
  , z    :: NInt
  } deriving (Show, Eq, Generic)

data Tab1 = Rec1
  { id :: Int64
  , v0 :: Double
  , v1 :: Double
  , v2 :: Double
  , v3 :: Double
  , v4 :: Double
  , v5 :: Double
  -- , v6  :: Double
  -- , v7  :: Double
  -- , v8  :: Double
  -- , v9  :: Double
  -- , v10 :: Double
  -- , v11 :: Double
  -- , v12 :: Double
  -- , v13 :: Double
  -- , v14 :: Double
  -- , v15 :: Double
  -- , v16 :: Double
  -- , v17 :: Double
  -- , v18 :: Double
  -- , v19 :: Double
  -- , v20 :: Double
  -- , v21 :: Double
  -- , v22 :: Double
  -- , v23 :: Double
  -- , v24 :: Double
  -- , v25 :: Double
  -- , v26 :: Double
  -- , v27 :: Double
  -- , v28 :: Double
  -- , v29 :: Double
  -- , v30 :: Double
  -- , v31 :: Double
  -- , v32 :: Double
  -- , v33 :: Double
  -- , v34 :: Double
  -- , v35 :: Double
  -- , v36 :: Double
  -- , v37 :: Double
  -- , v38 :: Double
  -- , v39 :: Double
  -- , v40 :: Double
  } deriving (Eq, Show, Generic)

type TTab = TableDef Tab '["id"] '[ '["name"]] '[]

pTab :: Proxy TTab
pTab = Proxy

type TTab1 = TableDef Tab1 '["id"] '[] '[]

pTab1 :: Proxy TTab1
pTab1 = Proxy

data Customer = Customer
  { id    :: Int64
  , name  :: T.Text
  , email :: T.Text
  } deriving (Show, Generic)

data Orders = Order -- name ORDER is disbled in sqlite!
  { id           :: Int64
  , num          :: T.Text
  , customerId   :: Int64
  , coCustomerId :: Maybe Int64
  } deriving (Show, Generic)
type TCustomer = TableDef Customer '["id"] '[ '["name"]] '[]
type TOrder = TableDef Orders '["id"] '[ '["customerId", "num"]]
    '[ '( '[ '("customerId", "id")], '("Customer", DCCascade))
     , '( '[ '("coCustomerId", "id")], '("Customer", DCSetNull))
     ]
pCustomer = Proxy :: Proxy TCustomer
pOrder = Proxy :: Proxy TOrder


createTab :: TabConstrB Sqlite a => Proxy (a :: DataDef) -> SessionMonad Sqlite IO ()
createTab (p :: Proxy a) = do
    catch (dropTable p) (\(_::SomeException) -> return ())
    createTable sqlite p

o1 = Order 0 "1" 1 Nothing
main :: IO ()
main = runSession sqlite "test.db" $ do
  createTab pTab
  createTab pTab1
  createTab pCustomer
  createTab pOrder
  insertMany pCustomer [ Customer 1 "odr" "x"
                       , Customer 2 "dro" "y"
                       , Customer 3 "דוגמה" "דואר"
                       ]
  rs <- insertManyAuto pOrder [ Order 0 "1" 1 Nothing
                              , Order 0 "2" 1 (Just 2)
                              , Order 0 "3" 1 (Just 1)
                              , Order 0 "1" 2 (Just 3)
                              ]
  let ords = [ Order (rs!!3) "z4" 3 (Just 1)
             , Order (rs!!1) "z2" 2 Nothing
             ]
  updateByPKMany pOrder ords
  updateByKey pCustomer (Tagged "dro" :: Tagged '["name"] T.Text, Tagged "numnum" :: Tagged '["email"] T.Text)

  updateByKey pCustomer
              (Tagged "odr" :: Subrec TCustomer '["name"]
              , Tagged ("zu",(4,"odr1")) :: Subrec TCustomer '["email","id","name"]
              )

  updateByPK pCustomer $ Customer 2 "drodro" "z"

  deleteByPK pOrder (Order 3 "3" 1 (Just 1))

  return ()