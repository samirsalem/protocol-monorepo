module Superfluid.Concepts.RealtimeBalance
    ( RealtimeBalance (..)
    , liquidityFromRTB
    , liquidityToRTB
    , integralToLiquidity
    ) where

import           Data.Default
import           Superfluid.BaseTypes (Liquidity)


-- | RealtimeBalance type
--
-- Naming conventions:
--  * Type name: rtb
--  * Type family name: RTB
--
data RealtimeBalance lq = RealtimeBalance
    { availableBalance :: lq
    , deposit          :: lq
    , owedDeposit      :: lq
    }

instance (Liquidity lq) => Default (RealtimeBalance lq) where
    def = RealtimeBalance
        { availableBalance = def
        , deposit = def
        , owedDeposit = def
        }

liquidityFromRTB :: (Liquidity lq) => (RealtimeBalance lq) -> lq
liquidityFromRTB (RealtimeBalance ab d od) = ab + max 0 (d - od)

liquidityToRTB ::(Liquidity lq) => lq -> (RealtimeBalance lq)
liquidityToRTB x = let o = fromInteger(0) in RealtimeBalance x o o

integralToLiquidity :: (Integral int, Liquidity lq) => int -> lq
integralToLiquidity = fromInteger.toInteger

instance (Liquidity lq) => Num (RealtimeBalance lq) where
    (+) (RealtimeBalance a1 a2 a3) (RealtimeBalance b1 b2 b3) =
        RealtimeBalance (a1 + b1) (a2 + b2) (a3 + b3)
    (*) (RealtimeBalance a1 a2 a3) (RealtimeBalance b1 b2 b3) =
        RealtimeBalance (a1 * b1) (a2 * b2) (a3 * b3)
    fromInteger = liquidityToRTB . fromInteger
    signum = liquidityToRTB . signum . liquidityFromRTB
    abs = liquidityToRTB . abs . liquidityFromRTB
    negate = liquidityToRTB . negate . liquidityFromRTB

instance (Liquidity lq) => Show (RealtimeBalance lq) where
    show (RealtimeBalance ab d od) =
        "RTB("
        ++ show ab ++ ", "
        ++ show d ++ "/" ++ show od
        ++ ")"
