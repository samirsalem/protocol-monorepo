module Superfluid.Instances.Simple.Wad
    ( Wad(..)
    , toWad
    , wad4humanN
    , wad4human
    ) where

import           Data.Default
import           Text.Printf  (printf)

import           Superfluid   (Liquidity)


{-
# Wad Type

- 18 decimal digit fixed-precision integer
- an instance of Liquidity
-}
newtype Wad = Wad Integer

toWad :: (RealFrac a) => a -> Wad
toWad x = Wad (round $ x * (10 ^ (18::Int)))

wad4humanN :: Wad -> Int -> String
wad4humanN (Wad wad) n
    | n >= 0 && n <= 18 = printf
        ("%0."++(show n)++"f")
        ((fromIntegral wad / (10 ^ (18::Int))) :: Double)
    | otherwise = error "Invalid parameter"

wad4human :: Wad -> String
wad4human wad = wad4humanN wad 4

instance Default Wad where def = 0
instance Num Wad where
    (+) (Wad a) (Wad b)= Wad (a + b)
    (*) (Wad a) (Wad b)= Wad (a * b)
    abs (Wad x)= Wad (abs x)
    signum (Wad x)= Wad (signum x)
    fromInteger = Wad
    negate (Wad x) = Wad (negate x)
instance Eq Wad where
    (==) (Wad a) (Wad b)= a == b
instance Ord Wad where
    compare (Wad a) (Wad b)= compare a b
instance Show Wad where
    show = wad4human
instance Liquidity Wad where
