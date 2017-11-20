{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Main where

import           Data.Type.GTree
import           GHC.Generics

main :: IO ()
main = return ()

data Pis = Pis Int String deriving (Eq, Show, Ord, Generic)

data DatBig = RecBig
    { fb1  :: Int
    , fb2  :: Char
    , fb3  :: Pis
    , fb4  :: Int
    , fb5  :: Char
    , fb6  :: Pis
    , fb7  :: Int
    , fb8  :: Char
    , fb9  :: Pis
    , fb10 :: Int
    , fb11 :: Char
    , fb12 :: Pis
    , fb13 :: Int
    , fb14 :: Char
    , fb15 :: Pis
    , fb16 :: Int
    , fb17 :: Char
    , fb18 :: Pis
    , fb19 :: Int
    , fb20 :: Int
    , fb21 :: Char
    , fb22 :: Pis
    , fb23 :: Int
    , fb24 :: Char
    , fb25 :: Pis
    , fb26 :: Int
    , fb27 :: Char
    , fb28 :: Pis
    , fb29 :: Int
    , f30::Int,f31::Int,f32::Int,f33::Int,f34::Int,f35::Int,f36::Int,f37::Int,f38::Int,f39::Int
    , f40::Int,f41::Int,f42::Int,f43::Int,f44::Int, f45::Int,f46::Int,f47::Int,f48::Int,f49::Int
    } deriving (Show, Generic)
instance CGrec DatBig
rdb = RecBig 1 'x' (Pis 2 "y") 1 'x' (Pis 2 "y") 1 'x' (Pis 2 "y")
       1 'x' (Pis 2 "y") 1 'x' (Pis 2 "y") 1 'x' (Pis 2 "y") 2
       1 'x' (Pis 2 "y") 1 'x' (Pis 2 "y") 1 'x' (Pis 2 "y") 2
        1 2 3 4 5 6 7 8 9 0
        1 2 3 4 5 6 7 8 9 0


data Dat4 = Rec4
  { f41 :: Int
  , f42 :: Char
  , f43 :: Pis
  , f44 :: Int
  , f45 :: Int
  , f46 :: Char
  } deriving (Show, Eq, Generic)
instance CGrec Dat4
vr4 = Rec4 1 'x' (Pis 2 "test") 3 4 'z' :: Dat4
