module Main where

import           Data.Promotion.Prelude.List
import           Data.Type.Bst
import           GHC.Prim                    (Proxy#, proxy#)

main :: IO ()
main = putStr $ showBst 0 bst

type Rec1 = '[ '("t1", Integer), "a":::String , "z":::Char , "d"::: [Integer]
             , "zt1":::Integer, "za":::String, "zz":::Char, "zd":::[Integer]
            --  , "at1":::Integer, "aa":::String, "az":::Char, "ad":::[Integer]
            --  , "xt1":::Integer, "xa":::String, "xz":::Char, "xd":::[Integer]
            --  , "bt1":::Integer, "ba":::String, "bz":::Char, "bd":::[Integer]
            --  , "zxt1":::Integer, "zxa":::String, "zxz":::Char, "zxd":::[Integer]
            --  , "zbt1":::Integer, "zba":::String, "zbz":::Char, "zbd":::[Integer]
            --  , "ct1":::Integer, "ca":::String, "cz":::Char, "cd":::[Integer]
             ]
type R1 = '[ '("t1", Integer)]
type R2 = '[ '("t1", Integer), "a":::String]
type R3 = '[ '("t1", Integer), "a":::String , "z":::Char]
type R4 = '[ '("t1", Integer), "a":::String , "z":::Char , "d"::: [Integer]]
type R8 = '[ '("t1", Integer), "a":::String , "z":::Char , "d"::: [Integer]
           , "zt1":::Integer, "za":::String, "zz":::Char, "zd":::[Integer]
           ]

v1 = 1
v2 = (2,"v2")
v3 = (3, ("v3", 'x'))
v4 = (4, ("v4", ('4', [1..4])))
v8 = (8, ("v8", ('8', ([1..8],(81, ("v81", ('9', [1..9])))))))
val =   ( 1,("sdd",('c',([1..5]
        -- , (1,("sdd",('c',([1..5]
        -- , (1,("sdd",('c',([1..5]
        -- , (1,("sdd",('c',([1..5]
        -- , (2,("sdd",('c',([1..5]
        -- , (1,("sdd",('c',([1..5]
        -- , (2,("sdd",('c',([1..5]
        , (2,("sdd",('c',[1..5]
        )))
        -- ))))
        -- ))))
        -- ))))
        -- ))))
        -- ))))
        -- ))))
        )))) :: Rep Rec1
-- x = undefined :: (Rep Rec1, Rep (ListToBst Rec1))

bst = bstCreate (proxy# :: Proxy# Rec1) (proxy# :: Proxy# (ListToBst Rec1)) val
        {--------------------------------------------------
test = Proxy :: Proxy Rec
-- test = Proxy :: Proxy (ListToBst (Map HeadSym0 (Group (Sort Rec))))
-}
