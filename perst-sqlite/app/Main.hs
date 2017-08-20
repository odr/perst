{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Main where

import           Data.Int                      (Int64)
import qualified Data.Text                     as T

import           Control.Applicative
import           Control.Monad.Catch           (SomeException, catch)
import           Control.Monad.IO.Class        (MonadIO (..))
import           Data.List                     (sort)
import           Data.Proxy                    (Proxy (..))
import           Data.Singletons.Prelude
import           Data.Singletons.Prelude.List
import           Data.Singletons.Prelude.Maybe
import           Data.Singletons.TypeRepStar
import           Data.Tagged                   (Tagged (..))
import           GHC.Generics                  (Generic)

import           Data.Type.Grec
-- import           Perst.Database.DataDef        (DataD, DelCons (..), Subrec,
--                                                 TableD)
import           Perst.Database.DbOption       (DbOption (..), DbTypeName,
                                                SessionMonad)
import           Perst.Database.DDL            as DDL
import           Perst.Database.DML
import           Perst.Database.DMLTree
import           Perst.Database.Sqlite
import           Perst.Types

import           Article
import           Customer
import           CustomerTree
import           Order

-- newtype NInt = NInt Int64 deriving (Show, Eq, Ord, Generic)
-- type instance DbTypeName Sqlite NInt = "INTEGER"
--
-- data Tab = Rec
--   { id   :: Int64
--   , name :: T.Text
--   , val  :: Maybe Double
--   , x    :: Int64
--   , z    :: NInt
--   } deriving (Show, Eq, Generic)
--
-- type TTab = DataD (TableD Tab '["id"] '[ '["name"]] True) '[]
--
-- pTab :: Sing TTab
-- pTab = sing
--


o1 = Order 0 "1" 1 Nothing

ct = [ CustomerTree 1 "odr" (Just "odr")
        [ OrderTree 1 "1" "01.01.2017"
          [ OrderPosition 1 1 2 11.11
          , OrderPosition 1 2 5 12.22
          ]
        , OrderTree 2 "2" "01.02.2017" [ OrderPosition 2 1 1 5.0 ]
        , OrderTree 3 "3" "01.03.2017" []
        ]
        [ Address 1 1 "My street" "11B"
        , Address 2 1 "My second street" "10"
        ]
    , CustomerTree 2 "dro" Nothing
        [ OrderTree 4 "1" "01.04.2017" [] ]
        [ Address 3 2 "Some street" "12C"]
    , CustomerTree 3 "zev" Nothing [] []
    ]
ct3 = CustomerTree 3 "zev1" (Just "zev") [] []

main :: IO ()
main = runSession @Sqlite "test.db" $ do
  dropCreate @Sqlite @TCustomer
  dropCreate @Sqlite @TOrder
  dropCreate @Sqlite @TArticle
  dropCreate @Sqlite @TOrderPosition
  dropCreate @Sqlite @TAddress

  insertTreeMany @Sqlite @TCustomerTree ct

  ct' <- selectTreeMany @Sqlite @TCustomerTree @CustomerTree
                        (map Tagged [1..3] :: [Tagged '["id"] Int64])
  liftIO $ do
    putStrLn ""
    putStrLn ""
    putStrLn $ "Check CustomerTree: " ++
      if ct' == map (:[]) ct
        then "Checked"
        else "Not Checked!!! ct' = " ++ show ct'
    putStrLn ""

  updateTreeMany @Sqlite @TCustomerTree (Prelude.drop 2 ct) [ct3]
  ct' <- selectTreeMany @Sqlite @TCustomerTree @CustomerTree
            (map Tagged [1..3] :: [Tagged '["id"] Int64])
  liftIO $ do
    putStrLn "After update:"
    print ct'

  return ()
