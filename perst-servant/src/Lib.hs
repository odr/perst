{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import           Data.Text
import           Data.Time   (UTCTime)
import           Servant.API

-- type UserAPI = "users" :> QueryParam "sortby" SortBy :> Get '[JSON] [User]
--
-- data SortBy = Age | Name
--
-- data User = User {
--   name              :: String,
--   age               :: Int,
--   email             :: String,
--   registration_date :: UTCTime
-- }
--
-- type RootEndpoint =
--   Get '[JSON] User
--
-- type UserAPI2 = "users" :> "list-all" :> Get '[JSON] [User]
--            :<|> "list-all" :> "users" :> Get '[JSON] [User]
