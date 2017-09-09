{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators         #-}
module Tabs where

import           Data.Int                (Int64)
import           Data.Tagged             (Tagged (..))
import qualified Data.Text               as T
import           GHC.Generics            (Generic)
import           GHC.TypeLits            (Symbol)

import           Data.Type.Grec          ((:::), Fields, Grec (..),
                                          GrecGroup (..), GrecWith (..),
                                          GrecWithout (..))
import           Perst.Database.DataDef  (DataAutoIns, DataDef' (..),
                                          DataDefInfo (..), DataInfo (..),
                                          DelCons (..), FK (..))
import           Perst.Database.DbOption (DbTypeNames (..))
import           Perst.Database.DDL      (DDL (..))
import           Perst.Database.DML      (DML (..))
import           Perst.Database.DMLTree  (DMLTree (..))
import           Perst.Database.Tree.Def (TreeDef' (..))

import           DB

-- data Customer = Customer
--   { id    :: Int64
--   , names :: GrecGroup Names
--   -- , email :: T.Text
--   } deriving (Show, Generic)
--
-- data Names = Names
--   { name      :: T.Text
--   , shortname :: Maybe T.Text
--   } deriving (Show, Generic)
--
-- data Order = Order -- name ORDER is disbled in sqlite!
--   { id           :: Int64
--   , num          :: T.Text
--   , customerId   :: Int64
--   , coCustomerId :: Maybe Int64
--   , date         :: T.Text
--   } deriving (Show, Generic)
--
-- data Article = Article
--   { id    :: Int64
--   , name  :: T.Text
--   , price :: Double
--   } deriving (Show, Generic)
--
data OrderPosition = OrderPosition
  { orderId   :: !Int64
  , articleId :: !Int64
  , quantity  :: !Int64
  , cost      :: !Double
  } deriving (Show, Generic, Eq, Ord)
--
data Address = Address
  { id         :: !Int64
  , customerId :: !Int64
  , street     :: !T.Text
  } deriving (Show, Generic, Eq, Ord)

type TCustomer
  = '( '[ "id"        ::: Int64
        , "name"      ::: T.Text
        , "shortname" ::: Maybe T.Text
        ]
    , DataDefC (TableInfo "customer" '["id"] '[ '["name"]] False) '[]
    )
-- instance DbTypeNames DB (TCustomer)

type TOrder =
  '( '[ "id"            ::: Int64
      , "num"           ::: T.Text
      , "customerId"    ::: Int64
      , "coCustomerId"  ::: Maybe Int64
      , "date"          ::: T.Text
      ]
  , DataDefC (TableInfo "orders" '["id"] '[ '["customerId", "num"]] True)
             '[ FKC "customer" DcCascade '[ '("customerId", "id")]
             ,  FKC "customer" DcSetNull '[ '("coCustomerId", "id")]
             ]
   )

type TArticle
  = '( '[ "id"    ::: Int64
        , "name"  ::: T.Text
        , "price" ::: Double
        ]
    , DataDefC (TableInfo "article" '["id"] '[ '["name"]] False) '[]
    )

type TOrderPosition =
  '( '[ "orderId"   ::: Int64
      , "articleId" ::: Int64
      , "quantity"  ::: Int64
      , "cost"      ::: Double
      ]
  , DataDefC (TableInfo "OrderPosition" '["orderId","articleId"] '[] False)
            '[ FKC "orders" DcCascade '[ '("orderId"  ,"id")]
             , FKC "article" DcRestrict '[ '("articleId","id")]
             ]
  )

type TAddress =
  '( '[ "id"         ::: Int64
      , "customerId" ::: Int64
      , "street"     ::: T.Text
      ]
  , DataDefC (TableInfo "address" '["id"] '[] False)
            '[ FKC "customer" DcCascade '[ '("customerId", "id")]]
  )
instance DDL DB TCustomer
instance DDL DB TOrder
instance DDL DB TOrderPosition
instance DDL DB TArticle
instance DDL DB TAddress

data CustomerTree = CustomerTree {-  # UNPACK # -}
  { id        :: !Int64
  , name      :: !T.Text
  , shortname :: Maybe T.Text
  -- , email     :: T.Text
  , orders    :: [Grec OrderTree]
  , address   :: [Grec Address]
  } deriving (Show, Generic, Eq, Ord)

data OrderTree = OrderTree  {-  # UNPACK #  -}
  { id        :: !Int64
  , num       :: !T.Text
  , date      :: !T.Text
  , positions :: [Grec OrderPosition]
  } deriving (Show, Generic, Eq, Ord)

type TCustomerTree = TreeDefC TCustomer
  '[ '( "orders",  '(TOrderTree, '[ '("customerId","id") ]))
   , '( "address", '(TreeDefC TAddress '[], '[ '("customerId","id") ]))
   ]
type TOrderTree = TreeDefC TOrder
  '[ '("positions", '(TreeDefC TOrderPosition '[],  '[ '("orderId", "id") ]))]

instance DML DB TCustomer       (Grec CustomerTree)
instance DML DB TOrder          (Grec OrderTree)
instance DML DB TOrderPosition  (Grec OrderPosition)
instance DML DB TAddress        (Grec Address)
instance DMLTree DB TCustomerTree (Grec CustomerTree)


type CustomerTree'
  = Tagged '["id","name","shortname","orders","address"]
            (Int64,(T.Text,(Maybe T.Text,([OrderTree'],[Address']))))
type OrderTree'
  = Tagged '["id","num","date","positions"]
            (Int64,(T.Text,(T.Text,[OrderPosition'])))
type Address'
  = Tagged '["id","customerId","street"]
            (Int64,(Int64,T.Text))
type OrderPosition'
  = Tagged '["orderId","articleId","quantity","cost"]
            (Int64,(Int64,(Int64,Double)))

-- instance DML DB TCustomer CustomerTree'
-- instance DML DB TOrder OrderTree'
-- instance DML DB TOrderPosition OrderPosition'
-- instance DML DB TAddress Address'
-- instance DMLTree DB TCustomerTree CustomerTree'
