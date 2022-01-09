module Superfluid.Types where

import Text.Printf (printf)

{-
# Common Types
-}
type Timestamp = Int

{-
# WAD (18 decimal digit fixed-precision integer) Utilities
-}
type WAD = Integer
toWad :: (RealFrac a) => a -> WAD
toWad x = round $ x * (10 ^ (18::Int))

wad4humanN :: WAD -> Int -> String
wad4humanN wad n
    | n >= 0 && n <= 18 = printf
        ("%0."++(show n)++"f")
        ((fromIntegral wad / (10 ^ (18::Int))) :: Double)
    | otherwise = error "Invalid parameter"

wad4human :: WAD -> String
wad4human wad = wad4humanN wad 6
