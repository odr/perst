{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeInType            #-}

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
import           Data.Tagged                   (Tagged (..))
import           GHC.Generics                  (Generic)

import           Data.Type.Grec
import           Perst.Database.DataDef        (DataDFC (..), DelCons (..),
                                                Subrec, TableD)
import           Perst.Database.DbOption       (DbOption (..), DbTypeName,
                                                SessionMonad)
import           Perst.Database.DDL            as DDL
import           Perst.Database.DML
import           Perst.Database.Sqlite
import           Perst.Database.TreeDef
import           Perst.Types

newtype NInt = NInt Int64 deriving (Show, Eq, Ord, Generic)
type instance DbTypeName Sqlite NInt = "INTEGER"

data Tab = Rec
  { id   :: Int64
  , name :: T.Text
  , val  :: Maybe Double
  , x    :: Int64
  , z    :: NInt
  } deriving (Show, Eq, Generic)

type TTab = DataD (TableD Tab '["id"] '[ '["name"]] True) '[]

pTab :: Proxy TTab
pTab = Proxy


data Customer = Customer
  { id    :: Int64
  , names :: GrecGroup Names
  -- , email :: T.Text
  } deriving (Show, Generic)

data Names = Names
  { name      :: T.Text
  , shortname :: Maybe T.Text
  } deriving (Show, Generic)

data Orders = Order -- name ORDER is disbled in sqlite!
  { id           :: Int64
  , num          :: T.Text
  , customerId   :: Int64
  , coCustomerId :: Maybe Int64
  , date         :: T.Text
  } deriving (Show, Generic)

data Article = Article
  { id    :: Int64
  , name  :: T.Text
  , price :: Double
  } deriving (Show, Generic)

data OrderPosition = OrderPosition
  { orderId   :: Int64
  , articleId :: Int64
  , quantity  :: Int64
  , cost      :: Double
  } deriving (Show, Generic, Eq, Ord)

data Address = Address
  { id         :: Int64
  , customerId :: Int64
  , street     :: T.Text
  } deriving (Show, Generic, Eq, Ord)

type TCustomerTab = TableD Customer '["id"] '[ '["name"]] False
type TCustomer = DataD TCustomerTab '[]

type TOrderTab = TableD Orders '["id"] '[ '["customerId", "num"]] True
type TOrder = DataD TOrderTab
                    '[ '(TCustomerTab, '[ '("customerId", "id")],  DcCascade)
                     , '(TCustomerTab, '[ '("coCustomerId", "id")], DcSetNull)
                     ]

type TArticleTab = TableD Article '["id"] '[ '["name"]] False
type TArticle = DataD TArticleTab '[]

type TOrderPosition
  = DataD (TableD OrderPosition '["orderId","articleId"] '[] False)
          '[ '(TOrderTab, '[ '("orderId"  ,"id")], DcCascade)
           , '(TArticleTab,  '[ '("articleId","id")], DcRestrict)
           ]

type TAddressTab = TableD Address '["id"] '[] False
type TAddress = DataD TAddressTab
                      '[ '(TCustomerTab, '[ '("customerId", "id")], DcCascade)]

pCustomer = Proxy :: Proxy TCustomer
pOrder    = Proxy :: Proxy TOrder
pArticle  = Proxy :: Proxy TArticle
pOrderPosition = Proxy :: Proxy TOrderPosition
pAddress  = Proxy :: Proxy TAddress

data CustomerTree = CustomerTree
  { id        :: Int64
  , name      :: T.Text
  , shortname :: Maybe T.Text
  , orders    :: [OrderTree]
  , address   :: [Address]
  } deriving (Show, Generic, Eq, Ord)

data OrderTree = OrderTree
  { id        :: Int64
  , num       :: T.Text
  , date      :: T.Text
  , positions :: [OrderPosition]
  } deriving (Show, Generic, Eq, Ord)

type TCustomerTree
  = TreeDefC TCustomer
    '[ '( "orders"
        , '( TreeDefC TOrder
            '[ '("positions"
                , '( TreeDefC TOrderPosition '[]
                  , '[ '("orderId", "id") ]
                  )
                )
            ]
          , '[ '("customerId","id") ]
          )
        )
     , '( "address"
        , '( TreeDefC TAddress '[]
          , '[ '("customerId","id") ]
          )
        )
     ]

pCustomerTree = Proxy :: Proxy TCustomerTree

createTab :: (DDL IO b a) => Proxy a -> SessionMonad b IO ()
createTab (p :: Proxy a) = do
  catch (DDL.drop p) (\(_::SomeException) -> return ())
  create p

o1 = Order 0 "1" 1 Nothing

ct = [ CustomerTree 1 "odr" (Just "odr")
        [ OrderTree 1 "1" "01.01.2017"
          [ OrderPosition 1 1 2 11.11
          , OrderPosition 1 2 5 12.22
          ]
        , OrderTree 2 "2" "01.02.2017" [ OrderPosition 2 1 1 5.0 ]
        , OrderTree 3 "3" "01.03.2017" []
        ]
        [ Address 1 1 "My street"
        , Address 2 1 "My second street"
        ]
    , CustomerTree 2 "dro" Nothing
        [ OrderTree 4 "1" "01.04.2017" [] ]
        [ Address 3 2 "Some street"]
    , CustomerTree 3 "zev" Nothing [] []
    ]
-- pct = Proxy :: Proxy (FieldsTree r)


main :: IO ()
main = runSession sqlite "test.db" $ do
  createTab pTab
  createTab pCustomer
  createTab pOrder
  createTab pArticle
  createTab pOrderPosition
  createTab pAddress

{-
  insertManyR pCustomer [ Customer 1 (GrecGroup $ Names "odr" (Just "odr")) "x"
                        , Customer 2 (GrecGroup $ Names "dro" Nothing) "y"
                        , Customer 3 (GrecGroup $ Names "zev" Nothing) "z"
                        ]
  rs <- insertManyR pOrder  [ Order 0 "1" 1 Nothing  "01.01.2017"
                            , Order 0 "2" 1 (Just 2) "01.02.2017"
                            , Order 0 "3" 1 (Just 1) "01.03.2017"
                            , Order 0 "1" 2 (Just 3) "01.04.2017"
                            ]
  insertManyR pArticle  [ Article 1 "art1" 12.22
                        , Article 2 "art2" 3.14
                        ]
  insertManyR pOrderPosition  [ OrderPosition 1 2 5 12.22
                              , OrderPosition 1 1 2 11.11
                              , OrderPosition 2 1 1 5
                              ]
  insertManyR pAddress  [ Address 1 1 "My street"
                        , Address 2 1 "My second street"
                        , Address 3 2 "Some street"
                        ]
-}

  insertTreeManyR pCustomerTree ct

  ct' <- selectTreeMany pCustomerTree
                (Proxy :: Proxy CustomerTree)
                (map Tagged [1..3] :: [Tagged '["id"] Int64])
        -- >>= liftIO . putStrLn . ("Check CustomerTree: " ++) . show
                   -- . (== map (:[]) ct) . sort
  liftIO $ putStrLn $ "Check CustomerTree: " ++
    if ct' == map (:[]) ct
      then "Checked"
      else "ct' = " ++ show ct'

{-
  let ords = [ Order (rs!!3) "z4" 3 (Just 1)
             , Order (rs!!1) "z2" 2 Nothing
             ]
  updateByPKManyR pOrder ords
  updateByKey pCustomer ( Tagged "dro"    :: Tagged '["name"]  T.Text
                        , Tagged "numnum" :: Tagged '["email"] T.Text
                        )
- }
  updateByKey pCustomer
              ( Tagged "odr"             :: Subrec TCustomer '["name"]
              -- , Tagged ("zu",(4,"odr1")) :: Subrec TCustomer '["email","id","name"]
              , Tagged ("zu",4,"odr1") :: Tagged '["email","id","name"] (T.Text,Int64,T.Text)
              )
 { -
  updateByPKR pCustomer $ Customer 2 "drodro" "z"

  deleteByPKR pOrder (Order 3 "3" 1 $ Just 1)
{-

-}
  selectManyR pCustomer
              (Proxy :: Proxy Customer)
              (map Tagged [2,3] :: [Tagged '["id"] Int64])
        >>= liftIO . print

-}
  return ()
