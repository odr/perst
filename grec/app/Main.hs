module Main where

import           Data.Proxy
import           Data.Tagged
import           Data.Type.Equality ((:~:) (..))
import           Data.Type.Grec
import           GHC.Generics
import           Lens.Micro

main :: IO ()
main = print test

test = 1

data Dat0 = Rec0 deriving (Show, Eq, Generic)
-- vd0 = Refl :: Fields Dat0 :~: '[]

data Dat1 = Rec1 { f11 :: Int } deriving (Show, Eq, Generic)
vd1 = Refl :: Fields Dat1 :~: '["f11":::Int]

data Dat2 = Rec2
    { f21 :: Int
    , f22 :: Char
    , f23 :: (Int,String)
    } deriving (Show, Eq, Generic)
--instance Lensed Dat2
vr2 = Rec2 1 'x' (2,"test") :: Dat2
vd2 = Refl :: Fields Dat2 :~: '[ '("f21",Int),'("f22",Char), "f23":::(Int,String)]
-- vr221 = vr2 ^. nlens (FieldName :: FieldName "f21")
-- vr2212 = vr2 ^. nlens (fromLabel (proxy# :: Proxy# "f21") :: Proxy "f21")
vr2213 = vr2 ^. #f22
gpe = Refl :: FieldsGrec (Grec Dat2)
          :~: FieldsGrec (Tagged ["f21","f22","f23"] (Int,(Char,(Int,String))))

data X = XInt Int | XChar Char | XIS (Int,String) | XS String deriving (Eq,Show)

instance Convert X Int where
  convert (XInt x) = x
  convert _        = error "not converted"

instance Convert X Char where
  convert (XChar x) = x
  convert _         = error "not converted"

instance Convert X String where
  convert (XS x) = x
  convert _      = error "not converted"

instance Convert X (Int,String) where
  convert (XIS x) = x
  convert _       = error "not converted"

instance Convert Int X where
  convert = XInt

instance Convert Char X where
  convert = XChar

instance Convert String X where
  convert = XS

instance Convert (Int,String) X where
  convert = XIS

pair = convToGrec [XInt 1, XInt 2, XChar 'x', XIS (4,"xxx")] :: (Tagged '["x"] Int, Grec Dat2)

pair2 = convToGrec [XInt 1, XInt 1, XChar '1', XInt 2, XChar 'x', XIS (4,"xxx")]
      :: (Tagged '["x","y","z"] (Int,Int,Char), Grec Dat2)

pair3 = convToGrec [XInt 1, XInt 1, XIS (1,"x"), XInt 2, XChar 'x', XIS (4,"xxx")]
      :: (Tagged '["x","y","z"] (Int,Int,(Int,String)), Grec Dat2)

-- zz = convert
--     (Tagged ([XInt 5, XChar 'z', XIS (7,"odr")], Rec2)
--       :: Tagged (FieldTypesGrec (Grec Dat2)) ([X], Int -> Char -> (Int,String) -> Dat2))
--     :: Dat2

vr2' = convToGrec [XInt 5, XChar 'z', XIS (7,"odr")] :: Grec Dat2
x2 = convFromGrec vr2' :: [X]

newtype Nt1 = Nt1 { unNt1 :: Dat2 } deriving (Show, Eq, Generic)
vnt1 = Refl :: Fields Nt1 :~: '[ '("f21",Int),'("f22",Char), "f23":::(Int,String)]
rnt1 = Nt1 vr2
nt11 = Nt1 vr2 ^. #f23
-- nt1 = listToGrec [XInt 5, XChar 'z', XIS (7,"odr")] :: Grec Nt1
-- x1 = grecToList nt1 :: [X]

data DatS = Rd1 Int | Rd2 deriving (Show, Eq, Generic)
-- vd5 = Refl :: Fields DatS :~: '[ '("f21",Int),'("f22",Char), "f23":::(Int,String)]

data DatBig = RecBig
    { fb1 :: Int
    , fb2 :: Char
    , fb3 :: (Int,String)
    , fb4 :: Int
    , fb5 :: Char
    , fb6 :: (Int,String)
    , fb7 :: Int
    , fb8 :: Char
    , fb9 :: (Int,String)
    -- , fb10 :: Int
    -- , fb11 :: Char
    -- , fb12 :: (Int,String)
    -- , fb13 :: Int
    -- , fb14 :: Char
    -- , fb15 :: (Int,String)
    -- , fb16 :: Int
    -- , fb17 :: Char
    -- , fb18 :: (Int,String)
    -- , fb19 :: Int
    -- , fb20 :: Int
    -- , fb21 :: Char
    -- , fb22 :: (Int,String)
    -- , fb23 :: Int
    -- , fb24 :: Char
    -- , fb25 :: (Int,String)
    -- , fb26 :: Int
    -- , fb27 :: Char
    -- , fb28 :: (Int,String)
    -- , fb29 :: Int
    -- ,f30::Int,f31::Int,f32::Int,f33::Int,f34::Int,f35::Int,f36::Int,f37::Int,f38::Int,f39::Int
    } deriving (Show, Generic)
vdb = Refl :: (Fields DatBig :~: '[
    "fb1":::Int,"fb2":::Char,"fb3":::(Int,String),"fb4":::Int,"fb5":::Char,"fb6":::(Int,String)
    ,"fb7":::Int,"fb8":::Char,"fb9":::(Int,String)
    -- ,"fb10":::Int,"fb11":::Char,"fb12":::(Int,String),"fb13":::Int,"fb14":::Char
    -- ,"fb15":::(Int,String),"fb16":::Int,"fb17":::Char,"fb18":::(Int,String),"fb19":::Int
    -- ,"fb20":::Int         ,"fb21":::Char,"fb22":::(Int,String),"fb23":::Int         ,"fb24":::Char
    -- ,"fb25":::(Int,String),"fb26":::Int ,"fb27":::Char        ,"fb28":::(Int,String),"fb29":::Int
    -- ,"f30":::Int,"f31":::Int,"f32":::Int,"f33":::Int,"f34":::Int,"f35":::Int,"f36":::Int,"f37":::Int,"f38":::Int,"f39":::Int
    ])
rdb = RecBig 1 'x' (2,"y") 1 'x' (2,"y") 1 'x' (2,"y")
      --  1 'x' (2,"y") 1 'x' (2,"y") 1 'x' (2,"y") 2
      --  1 'x' (2,"y") 1 'x' (2,"y") 1 'x' (2,"y") 2
      --  1 2 3 4 5 6 7 8 9 0
xdb = rdb ^. #fb5

data Orders = Order -- name ORDER is disbled in sqlite!
  { id           :: Int
  , num          :: String
  , customerId   :: Int
  , coCustomerId :: Maybe Int
  } deriving (Show, Generic)
o1 = Order 1 "x" 2 Nothing
-- po1 = grecToPair $ Grec o1
--
{-
-}

data DD0 = DD0 { d01 :: Int, d02 :: Char }
  deriving (Show, Generic)
data DD1 = DD1 { d11 :: Int, d12 :: [DD0], d13 :: Char, d14 :: Int }
  deriving (Show, Generic)
data DD2 = DD2 { d21 :: Int, d22 :: String, d23 :: [DD1], d24 :: [DD0]}
  deriving (Show, Generic)

dd2 = DD2 1 "'x'"
    [ DD1 2 [DD0 3 'a', DD0 4 'b'] 'c' 6
    , DD1 4 [] 'd' 7
    ]
    [ DD0 5 'e']

-- ctdd2 = convert (Grec dd2) :: ConvTree X
-- dd2' = convert ctdd2 :: Grec DD2
--
-- ttre = Refl :: FieldsTree DD2 :~:
--   'TreeTC '[ '("d21", Int), '("d22", String)]
--         '[ '("d23",
--             'TreeTC '[ '("d11", Int), '("d13", Char), '("d14", Int)]
--                   '[ '("d12",
--                       'TreeTC '[ '("d01", Int), '("d02", Char)] '[])]),
--           '("d24",
--             'TreeTC '[ '("d01", Int), '("d02", Char)] '[])]
