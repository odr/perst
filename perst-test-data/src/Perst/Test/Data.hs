{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Perst.Test.Data where

import           Data.Int                     (Int64)
import qualified Data.Text                    as T

import           Control.Monad.IO.Class       (MonadIO (..))
import           Data.Tagged                  (Tagged (..))

-- import           Data.Type.Grec               (Grec (..))
import           Perst.Database.DbOption      (DbOption (..), DbTypeName,
                                               SessionMonad)
import           Perst.Database.DDL           as DDL
import           Perst.Database.DMLTree
-- import           Perst.Database.Sqlite

import           Perst.Test.Data.Article
import           Perst.Test.Data.Customer
import           Perst.Test.Data.CustomerTree
import           Perst.Test.Data.Db           (Db)
import           Perst.Test.Data.Order
import           Perst.Test.Data.OrderTree


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

check :: IO ()
check = runSession @Db "test.db" $ do
  dropCreate @Db @TCustomer
  dropCreate @Db @TOrder
  dropCreate @Db @TArticle
  dropCreate @Db @TOrderPosition
  dropCreate @Db @TAddress

  insertTreeMany @Db @TCustomerTree ct

  ct' <- selectTreeMany @Db @TCustomerTree @CustomerTree
                        (map Tagged [1..3] :: [Tagged '["id"] Int64])
  liftIO $ do
    putStrLn ""
    putStrLn ""
    putStrLn $ "Check CustomerTree: " ++
      if ct' == map (:[]) ct
        then "Checked"
        else "Not Checked!!! ct' = " ++ show ct'
    putStrLn ""

  updateTreeMany @Db @TCustomerTree (Prelude.drop 2 ct) [ct3]
  ct' <- selectTreeMany @Db @TCustomerTree @CustomerTree
            (map Tagged [1..3] :: [Tagged '["id"] Int64])
  liftIO $ do
    putStrLn "After update:"
    print ct'

  return ()
