module Superfluid.Core.RealtimeBalance
    ( RealtimeBalance(..)
    , liquidityFromRTB
    , liquidityToRTB
    , integralToLiquidity
    ) where

import Superfluid.Core.Types (Liquidity)

data RealtimeBalance liq = RealtimeBalance
    { availableBalance :: liq
    , deposit :: liq
    , owedDeposit :: liq
    }

liquidityFromRTB :: (Liquidity liq) => (RealtimeBalance liq) -> liq
liquidityFromRTB (RealtimeBalance ab d od) = ab + max 0 (d - od)

liquidityToRTB ::(Liquidity liq) => liq -> (RealtimeBalance liq)
liquidityToRTB x = let o = fromInteger(0) in RealtimeBalance x o o

integralToLiquidity :: (Integral num, Liquidity liq) => num -> liq
integralToLiquidity = fromInteger.toInteger

instance (Liquidity liq) => Num (RealtimeBalance liq) where
    (+) (RealtimeBalance a1 a2 a3) (RealtimeBalance b1 b2 b3) =
        RealtimeBalance (a1 + b1) (a2 + b2) (a3 + b3)
    (*) (RealtimeBalance a1 a2 a3) (RealtimeBalance b1 b2 b3) =
        RealtimeBalance (a1 * b1) (a2 * b2) (a3 * b3)
    fromInteger = liquidityToRTB . fromInteger
    signum = liquidityToRTB . signum . liquidityFromRTB
    abs = liquidityToRTB . abs . liquidityFromRTB
    negate = liquidityToRTB . negate . liquidityFromRTB

{- --# LANGUAGE FlexibleInstances #-}
-- type RealtimeBalanceInteger = RealtimeBalance Integer
-- instance Show RealtimeBalanceInteger where
--     show (RealtimeBalance ab d od) =
--         "RTB("
--         ++ show ab ++ ", "
--         ++ show d ++ "/" ++ show od
--         ++ ")"
