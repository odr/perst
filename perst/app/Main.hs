{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeInType            #-}

module Main where

-- main :: IO ()
-- main = return ()
import           Control.Applicative
import           Data.Bifunctor
import           Data.Functor.Compose
import           Data.Int                      (Int64)
import qualified Data.Text                     as T

import           Control.Monad.Catch           (SomeException, catch)
import           Control.Monad.IO.Class        (MonadIO (..))
import           Data.List                     (sort)
import           Data.Proxy                    (Proxy (..))
import           Data.Singletons.Prelude
import           Data.Singletons.Prelude.List
import           Data.Singletons.Prelude.Maybe
import           Data.Tagged                   (Tagged (..))
import           GHC.Generics                  (Generic)

import           Data.Text                     hiding (map)
import           Data.Type.Grec
import           Perst.Database.Constraints    (InsConstr)
import           Perst.Database.DataDef        (DataD (..), DdAutoIns, DdKey,
                                                DelCons (..), Subrec, TableD)
import           Perst.Database.DbOption       (DbOption (..), DbTypeName,
                                                SessionMonad)
import           Perst.Database.DDL            as DDL
import           Perst.Database.DML
import           Perst.Database.TreeDef
import           Perst.Lens                    (NamesGrecLens)
import           Perst.Types

----------------------
data DB

db :: Proxy DB
db = Proxy

type instance DbTypeName DB Int64      = "INTEGER"
type instance DbTypeName DB Text       = "TEXT"
type instance DbTypeName DB Double     = "DOUBLE"

data DBData = DBNull | DBText Text | DBInteger Int64 | DBDouble Double

instance Convert DBData Int64 where
  convert (DBInteger x) = x

instance Convert Int64 DBData  where
  convert = DBInteger

instance Convert DBData Double where
  convert (DBDouble x) = x

instance Convert Double DBData  where
  convert = DBDouble

instance Convert DBData Text where
  convert (DBText x) = x

instance Convert Text DBData  where
  convert = DBText

instance Convert a DBData => Convert (Maybe a) DBData where
  convert Nothing  = DBNull
  convert (Just a) = convert a

instance Convert DBData a => Convert DBData (Maybe a) where
  convert DBNull = Nothing
  convert x      = Just $ convert x

instance DbOption DB where
    type SessionParams DB   = Text
    type Conn DB            = DB
    type FieldDB DB         = DBData
    type PrepCmd DB         = DB
    type GenKey DB          = Int64
----------------------

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
  -- , email     :: T.Text
  , orders    :: [OrderTree]
  , address   :: [Address]
  } deriving (Show, Generic, Eq, Ord)

data OrderTree = OrderTree
  { id        :: Int64
  , num       :: T.Text
  , date      :: T.Text
  , positions :: [OrderPosition]
  } deriving (Show, Generic, Eq, Ord)

type TCustomerTree = TreeDefC TCustomer
  '[ '( "orders", '( TOrderTree, '[ '("customerId","id") ]))
   , '( "address", '( TreeDefC TAddress '[], '[ '("customerId","id") ]))
   ]
type TOrderTree = TreeDefC TOrder
  -- '[]
  '[ '("positions", '( TreeDefC TOrderPosition '[], '[ '("orderId", "id") ]))]

pCustomerTree = Proxy :: Proxy TCustomerTree

createTab :: (DDL IO b a) => Proxy a -> SessionMonad b IO ()
createTab (p :: Proxy a) = do
  catch (DDL.drop p) (\(_::SomeException) -> return ())
  create p

o1 = Order 0 "1" 1 Nothing

ct = [ CustomerTree 1 "odr" (Just "odr") -- "odr@ru"
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
    , CustomerTree 2 "dro" Nothing -- "dro@ru"
        [ OrderTree 4 "1" "01.04.2017" []
        ]
        [ Address 3 2 "Some street"]
    , CustomerTree 3 "zev" Nothing -- "zev@zev"
        [] []
    ]
-- pct = Proxy :: Proxy (FieldsTree r)

type ITC t r = InsertTreeConstraint IO ZipList DB t r

check :: ITC t r => Proxy '(t, r)
check = Proxy

type IC t r = InsConstr IO DB (TdData t) r
type ICH t r = InsertChilds IO ZipList DB (DdAutoIns (TdData t))
              (DdKey (TdData t)) (GrecChilds t r) r

type ICH' t r xs = InsertChilds IO ZipList DB (DdAutoIns (TdData t))
              (DdKey (TdData t)) xs r

check1 :: IC t r => Proxy '(t, r)
check1 = Proxy

check2 :: ICH t r => Proxy '(t, r)
check2 = Proxy

check2' :: ICH' t r xs => Proxy '(t, r, xs)
check2' = Proxy

checkGL :: GrecLens s [FieldByName s r] r => Proxy '(s,r)
checkGL = Proxy

checkGL2 :: NamesGrecLens (Fsts rs) (RecParent r rs) r => Proxy '(rs,r)
checkGL2 = Proxy

main :: IO ()
main = runSession db "test.db" $ do
  -- createTab pTab
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
