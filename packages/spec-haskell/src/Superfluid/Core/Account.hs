{-# LANGUAGE MultiParamTypeClasses #-}

module Superfluid.Core.Account
    ( Account(..)
    ) where

import Superfluid.Core.Types (Liquidity, Timestamp)
import Superfluid.Core.RealtimeBalance
    ( RealtimeBalance
    , liquidityToRTB
    )
import Superfluid.Core.Agreement
    ( AnyAgreementAccountData
    , providedBalanceOfAgreement
    )

class (Liquidity liq) => Account a liq where
    staticBalanceOf :: a -> liq
    agreementsOf :: a -> [AnyAgreementAccountData liq]
    balanceOf :: a -> Timestamp -> RealtimeBalance liq
    balanceOf a t = foldr
        (+)
        (liquidityToRTB.fromInteger $ 0)
        (map (flip providedBalanceOfAgreement t) (agreementsOf a))
