module Main where

import           Data.Promotion.Prelude.List
import           Data.Type.Lstt
import           GHC.Prim                    (Proxy#, proxy#)
import           Lens.Micro

main :: IO ()
main = print test

type Rec1 = '[ '("t1", Integer), "a":::String , "z":::Char , "d"::: [Integer]
             , "zt1":::Integer, "za":::String, "zz":::Char, "zd":::[Integer]
             , "at1":::Integer, "aa":::String, "az":::Char, "ad":::[Integer]
            --  , "xt1":::Integer, "xa":::String, "xz":::Char, "xd":::[Integer]
            --  , "bt1":::Integer, "ba":::String, "bz":::Char, "bd":::[Integer]
            --  , "zxt1":::Integer, "zxa":::String, "zxz":::Char, "zxd":::[Integer]
            --  , "zbt1":::Integer, "zba":::String, "zbz":::Char, "zbd":::[Integer]
             , "ct1":::Integer, "ca":::String, "cz":::Char, "cd":::[Integer]
             ]

v1 = 1
v2 = (2,"v2")
v3 = (3, ("v3", 'x'))
v4 = (4, ("v4", ('4', [1..4])))
v8 = (8, ("v8", ('8', ([1..8],(81, ("v81", ('9', [1..9])))))))
val =   ( 1::Integer,("sdd",('c',([1..5::Integer]
        , (1::Integer,("sdd",('c',([1..5::Integer]
        -- , (1,("sdd",('c',([1..5]
        -- , (1,("sdd",('c',([1..5]
        -- , (2,("sdd",('c',([1..5]
        -- , (1,("sdd",('c',([1..5]
        , (2::Integer,("sdd",('c',([1..5::Integer]
        , (2::Integer,("sdd",('c',[1..5::Integer]
        )))
        ))))
        -- ))))
        -- ))))
        -- ))))
        -- ))))
        ))))
        )))) :: ToStar Rec1
-- x = undefined :: (Rep Rec1, Rep (ListToBst Rec1))

test = val ^. repLens (proxy# :: Proxy# Rec1) (proxy# :: Proxy# (Sort Rec1))
{--------------------------------------------------
test = Proxy :: Proxy Rec
-- test = Proxy :: Proxy (ListToBst (Map HeadSym0 (Group (Sort Rec))))
-}
