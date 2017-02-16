module Main where

import           Data.Type.Equality ((:~:) (..))
import           Data.Type.Grec
import           GHC.Generics
import           Lens.Micro

main :: IO ()
main = print test

test = 1

data Dat0 = Rec0 deriving (Show, Eq, Generic)
vd0 = Refl :: Fields Dat0 :~: '[]

data Dat1 = Rec1 { f11 :: Int } deriving (Show, Eq, Generic)
vd1 = Refl :: Fields Dat1 :~: '["f11":::Int]

data Dat2 = Rec2
    { f21 :: Int
    , f22 :: Char
    , f23 :: (Int,String)
    } deriving (Show, Eq, Generic)
--instance Lensed Dat2
vr2 = Rec2 1 'x' (2,"test")
vd2 = Refl :: Fields Dat2 :~: '[ '("f21",Int),'("f22",Char), "f23":::(Int,String)]
-- vr221 = vr2 ^. nlens (FieldName :: FieldName "f21")
-- vr2212 = vr2 ^. nlens (fromLabel (proxy# :: Proxy# "f21") :: Proxy "f21")
vr2213 = vr2 ^. #f22

data X = XInt Int | XChar Char | XIS (Int,String) deriving (Eq,Show)

instance Convert X Int where
  convert (XInt x) = x
  convert _        = error "not converted"

instance Convert X Char where
  convert (XChar x) = x
  convert _         = error "not converted"

instance Convert X (Int,String) where
  convert (XIS x) = x
  convert _       = error "not converted"

instance Convert Int X where
  convert = XInt

instance Convert Char X where
  convert = XChar

instance Convert (Int,String) X where
  convert = XIS

vr2' = toGrec [XInt 5, XChar 'z', XIS (7,"odr")] :: Dat2
x2 = fromGrec vr2' :: [X]

newtype Nt1 = Nt1 { unNt1 :: Dat2 } deriving (Show, Eq, Generic)
vnt1 = Refl :: Fields Nt1 :~: '[ '("f21",Int),'("f22",Char), "f23":::(Int,String)]
rnt1 = Nt1 vr2
nt11 = Nt1 vr2 ^. #f23
nt1 = toGrec [XInt 5, XChar 'z', XIS (7,"odr")] :: Nt1
x1 = fromGrec nt1 :: [X]

data DatS = Rd1 Int | Rd2 deriving (Show, Eq, Generic)
-- vd5 = Refl :: Fields DatS) :~: '[ '("f21",Int),'("f22",Char), "f23":::(Int,String)]

data DatBig = RecBig
    { fb1  :: Int
    , fb2  :: Char
    , fb3  :: (Int,String)
    , fb4  :: Int
    , fb5  :: Char
    , fb6  :: (Int,String)
    , fb7  :: Int
    , fb8  :: Char
    , fb9  :: (Int,String)
    , fb10 :: Int
    , fb11 :: Char
    , fb12 :: (Int,String)
    , fb13 :: Int
    , fb14 :: Char
    , fb15 :: (Int,String)
    , fb16 :: Int
    , fb17 :: Char
    , fb18 :: (Int,String)
    , fb19 :: Int
    , fb20 :: Int
    , fb21 :: Char
    , fb22 :: (Int,String)
    , fb23 :: Int
    , fb24 :: Char
    , fb25 :: (Int,String)
    , fb26 :: Int
    , fb27 :: Char
    , fb28 :: (Int,String)
    , fb29 :: Int
    ,f30::Int,f31::Int,f32::Int,f33::Int,f34::Int,f35::Int,f36::Int,f37::Int,f38::Int,f39::Int
    } deriving (Show, Eq, Generic)
vdb = Refl :: (Fields DatBig :~: '[
    "fb1":::Int,"fb2":::Char,"fb3":::(Int,String),"fb4":::Int,"fb5":::Char,"fb6":::(Int,String)
    ,"fb7":::Int,"fb8":::Char,"fb9":::(Int,String)
    ,"fb10":::Int,"fb11":::Char,"fb12":::(Int,String),"fb13":::Int,"fb14":::Char
    ,"fb15":::(Int,String),"fb16":::Int,"fb17":::Char,"fb18":::(Int,String),"fb19":::Int
    ,"fb20":::Int         ,"fb21":::Char,"fb22":::(Int,String),"fb23":::Int         ,"fb24":::Char
    ,"fb25":::(Int,String),"fb26":::Int ,"fb27":::Char        ,"fb28":::(Int,String),"fb29":::Int
    ,"f30":::Int,"f31":::Int,"f32":::Int,"f33":::Int,"f34":::Int,"f35":::Int,"f36":::Int,"f37":::Int,"f38":::Int,"f39":::Int
    ])
rdb = RecBig 1 'x' (2,"y") 1 'x' (2,"y") 1 'x' (2,"y") 1 'x' (2,"y") 1 'x' (2,"y") 1 'x' (2,"y") 2
       1 'x' (2,"y") 1 'x' (2,"y") 1 'x' (2,"y") 2
       1 2 3 4 5 6 7 8 9 0
xdb = rdb ^. #fb15
