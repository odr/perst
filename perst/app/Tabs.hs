{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Tabs where

import           Data.Int                (Int64)
import           Data.Tagged             (Tagged (..))
import qualified Data.Text               as T
import           GHC.Generics            (Generic)
import           GHC.TypeLits            (Symbol)

import           Data.Type.Grec          (Grec (..), GrecGroup (..),
                                          GrecWith (..), GrecWithout (..))
import           Perst.Database.DataDef  (DataAutoIns, DataDef' (..),
                                          DataDefInfo (..), DataInfo (..),
                                          DelCons (..), FK (..))
import           Perst.Database.DbOption (DbTypeNames (..))
import           Perst.Database.DML      (Insert (..), SelectByKey (..))
import           Perst.Database.TreeDef  (InsertTree (..), InsertTreeR (..),
                                          SelectTree (..), TreeDef' (..))

import           DB


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

type TCustomer = '(Customer, DataDefC (TableInfo '["id"] '[ '["name"]] False) '[])
-- instance DbTypeNames DB (TCustomer)

type TOrder =
  '(Orders
  , DataDefC (TableInfo '["id"] '[ '["customerId", "num"]] True)
             '[ FKC "customer" DcCascade '[ '("customerId", "id")]
             ,  FKC "customer" DcSetNull '[ '("coCustomerId", "id")]
             ]
   )

type TArticle = '(Article, DataDefC (TableInfo '["id"] '[ '["name"]] False) '[])

type TOrderPosition =
  '(OrderPosition
  , DataDefC (TableInfo '["orderId","articleId"] '[] False)
            '[ FKC "orders" DcCascade '[ '("orderId"  ,"id")]
             , FKC "article" DcRestrict '[ '("articleId","id")]
             ]
  )

type TAddress =
  '(Address
  , DataDefC (TableInfo '["id"] '[] False)
            '[ FKC "customer" DcCascade '[ '("customerId", "id")]]
  )

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
  '[ '( "orders",  '(TOrderTree, '[ '("customerId","id") ]))
   , '( "address", '(TreeDefC TAddress '[], '[ '("customerId","id") ]))
   ]
type TOrderTree = TreeDefC TOrder
  '[ '("positions", '(TreeDefC TOrderPosition '[],  '[ '("orderId", "id") ]))]

instance Insert DB TCustomer (Grec CustomerTree)

type TCustomerOrderTree =
  ( Tagged '["customerId"] Int64
  , GrecWithout '["customerId"] (Grec OrderTree)
  )
instance Insert DB TOrder TCustomerOrderTree

type TOrderOrderPosTree =
  ( Tagged '["orderId"] Int64
  , GrecWithout '["orderId"] (Grec OrderPosition)
  )
instance Insert DB TOrderPosition TOrderOrderPosTree
-- instance InsertTree DB (TreeDefC TOrderPosition '[]) TOrderOrderPosTree

-- instance InsertTree DB TOrderTree TCustomerOrderTree
instance InsertTree DB TCustomerTree (Grec CustomerTree)

type TCustomerAddressTree =
  ( Tagged '["customerId"] Int64
  , GrecWithout '["customerId"] (Grec Address)
  )
instance Insert DB TAddress TCustomerAddressTree
-- instance InsertTree DB (TreeDefC TAddress '[]) TCustomerAddressTree

-- instance InsertTreeR DB TCustomerTree CustomerTree

instance SelectTree DB TCustomerTree CustomerTree (Tagged '["id"] Int64)
instance SelectTree DB TOrderTree OrderTree
                  (GrecWith '["customerId"] (Tagged '["customerId"] Int64))
instance SelectTree DB ('TreeDefC TAddress '[]) Address
                  (GrecWith '["customerId"] (Tagged '["customerId"] Int64))
instance SelectTree DB ('TreeDefC TOrderPosition '[]) OrderPosition
                  (GrecWith '["orderId"] (Tagged '["orderId"] Int64))


--------------------

-- by PK
instance SelectByKey DB TCustomer (Tagged '["id"] Int64, Grec CustomerTree)
                                  (Tagged '["id"] Int64)

-- by FK with PK
instance SelectByKey DB TOrder (Tagged '["id"] Int64, Grec OrderTree)
                     (GrecWith '["customerId"] (Tagged '["customerId"] Int64))

instance SelectByKey DB TAddress (Tagged ('[] :: [Symbol]) (), Grec Address)
                      (GrecWith '["customerId"] (Tagged '["customerId"] Int64))

instance SelectByKey DB TOrderPosition (Tagged ('[] :: [Symbol]) (), Grec OrderPosition)
                      (GrecWith '["orderId"] (Tagged '["orderId"] Int64))
