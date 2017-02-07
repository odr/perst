module Main where

import           Data.Promotion.Prelude.List
import           Data.Type.Bst
import           GHC.Prim                    (Proxy#, proxy#)

main :: IO ()
main = print bst

type Rec = ListToBst (Map HeadSym0 (Group (Sort '[
        3,4,2,1,6,7,12,32,12,42,35
        -- ,64,78,23,32,45,65,84,32,18
        -- ,43,55,64,1,2,3,4,5,6,7
        -- ,24,8,9,10,11,12,13,14,15,16
        -- ,81,82,83,84,85,86,87,88,89,90
        -- ,51,52,53,54,55,56,57,58,59,60
        -- ,17,18,19,20,21,22
        ,23,24,25,26,27,28,29,30])))
type Rec1 = '[ '("t1", Integer), "a":::String , "z":::Char , "d"::: [Integer]
             , "zt1":::Integer, "za":::String, "zz":::Char, "zd":::[Integer]
             , "at1":::Integer, "aa":::String, "az":::Char, "ad":::[Integer]
             , "xt1":::Integer, "xa":::String, "xz":::Char, "xd":::[Integer]
             , "bt1":::Integer, "ba":::String, "bz":::Char, "bd":::[Integer]
             , "ct1":::Integer, "ca":::String, "cz":::Char, "cd":::[Integer]
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
        , (1,("sdd",('c',([1..5]
        , (1,("sdd",('c',([1..5]
        , (1,("sdd",('c',([1..5]
        , (2,("sdd",('c',([1..5]
        , (2,("sdd",('c',[1..5]
        ))))
        ))))
        ))))
        ))))
        ))))
        ))) :: Rep Rec1

bst = bstCreate (proxy# :: Proxy# '(Rec1, ListToBst Rec1)) val
        {--------------------------------------------------
test = Proxy :: Proxy Rec
-- test = Proxy :: Proxy (ListToBst (Map HeadSym0 (Group (Sort Rec))))
-}
