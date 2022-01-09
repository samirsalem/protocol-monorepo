module Superfluid.RealtimeBalance
    ( RealtimeBalance(..)
    , requiredLiquidity
    ) where

data RealtimeBalance = RealtimeBalance
    { availableBalance :: Integer
    , deposit :: Integer
    , owedDeposit :: Integer
    }

instance Num RealtimeBalance where
    fromInteger a = RealtimeBalance a 0 0
    (RealtimeBalance a1 a2 a3) + (RealtimeBalance b1 b2 b3) =
        RealtimeBalance (a1 + b1) (a2 + b2) (a3 + b3)
    signum _ = error "Not implemented"
    abs _ = error "Not implemented"
    (*) _ _ = error "Not implemented"
    (-) _ _ = error "Not implemented"

instance Show RealtimeBalance where
    show (RealtimeBalance ab d od) =
        "RTB("
        ++ show ab ++ ", "
        ++ show d ++ "/" ++ show od
        ++ ")"

requiredLiquidity :: RealtimeBalance -> Integer
requiredLiquidity (RealtimeBalance ab d od) = ab + max 0 (d - od)
