{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

import           Data.Int              (Int64)
import qualified Data.Text             as T

import           Control.Monad.Catch   (SomeException, catch)
import           Data.Proxy            (Proxy (..))
import           GHC.Generics          (Generic)
import           Perst.Database.DDL
import           Perst.Database.Sqlite
import           Perst.Database.Types
import           Perst.Types           ((:::))

newtype NInt = NInt Int64 deriving (Show, Eq, Ord, Generic)
type instance DbTypeName Sqlite NInt = "INTEGER NOT NULL"

data Rec = Tab
  { id   :: Int64
  , name :: T.Text
  , val  :: Maybe Double
  , x    :: Int64
  , z    :: NInt
  } deriving (Show, Eq, Generic)

data Rec1 = Tab1
  { id  :: Int64
  , v0  :: Double
  , v1  :: Double
  , v2  :: Double
  , v3  :: Double
  , v4  :: Double
  , v5  :: Double
  , v6  :: Double
  , v7  :: Double
  , v8  :: Double
  , v9  :: Double
  , v10 :: Double
  , v11 :: Double
  , v12 :: Double
  , v13 :: Double
  , v14 :: Double
  , v15 :: Double
  , v16 :: Double
  , v17 :: Double
  , v18 :: Double
  , v19 :: Double
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

type Tab = TableDef Rec '["id"] '[ '["name"]] '[]

pTab :: Proxy Tab
pTab = Proxy

type Tab1 = TableDef Rec1 '["id"] '[] '[]

pTab1 :: Proxy Tab1
pTab1 = Proxy

createTab :: SessionMonad Sqlite IO ()
createTab = do
    catch (ddlDrop pTab) (\(_::SomeException) -> return ())
    ddlCreate pTab

main :: IO ()
main = runSession sqlite "test.db" $ do
  catch (ddlDrop pTab) (\(_::SomeException) -> return ())
  ddlCreate pTab
  catch (ddlDrop pTab1) (\(_::SomeException) -> return ())
  ddlCreate pTab1

-- main:: IO ()
-- main = undefined
