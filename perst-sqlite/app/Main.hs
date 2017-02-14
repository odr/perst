module Main where

import           Data.Int              (Int64)
import qualified Data.Text             as T

import           Control.Monad.Catch   (SomeException, catch)
import           Data.Proxy            (Proxy (..))
import           Perst.Database.DDL
import           Perst.Database.Sqlite
import           Perst.Database.Types
import           Perst.Types           ((:::))


type Rec1 = '["id":::Int64,"name":::T.Text,"val":::Maybe Double
             ,"x":::Int64, "z":::T.Text, "y"::: Maybe T.Text
             ,"_1":::Int64,"_2":::Int64,"_3":::Int64
             ,"_4":::Int64,"_5":::Int64,"_6":::Int64
            --  ,"_7":::Int64,"_8":::Int64,"_9":::Int64
            --  ,"_10":::Int64,"_11":::Int64,"_12":::Int64
            --  ,"_21"  :::Int64,"_22"  :::Int64,"_23"  :::Int64
             --
            --  ,"_24"  :::Int64,"_25"  :::Int64,"_26"  :::Int64
            --  ,"_27"  :::Int64,"_28"  :::Int64,"_29"  :::Int64
            --  ,"_210" :::Int64,"_211" :::Int64,"_212" :::Int64
            --  ,"_321" :::Int64,"_322" :::Int64,"_323" :::Int64
            --  ,"_324" :::Int64,"_325" :::Int64,"_326" :::Int64
            --  ,"_327" :::Int64,"_328" :::Int64,"_329" :::Int64
            --  ,"_3210":::Int64,"_3211":::Int64,"_3212":::Int64
             {-  -}
             ]
type Tab1 = TableDef "tab1" Rec1 '["id"] '[ '["name"]] '[]

-- instance DDL Sqlite Tab1

pTab1 :: Proxy Tab1
pTab1 = Proxy

createTab1 :: SessionMonad Sqlite IO ()
createTab1 = do
    catch (ddlDrop pTab1) (\(_::SomeException) -> return ())
    ddlCreate pTab1

main :: IO ()
main = runSession sqlite "test.db"
    createTab1

-- main:: IO ()
-- main = undefined
