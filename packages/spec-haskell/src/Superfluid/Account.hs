module Superfluid.Account
    ( Account(..)
    ) where

import Superfluid.Types (Timestamp)
import Superfluid.RealtimeBalance (RealtimeBalance(..))
import Superfluid.SuperAgreement
    ( SuperAgreementAccountData
    , providedBalanceOfAgreement)

class Account a where
    staticBalanceOf :: a -> Integer
    agreementsOf :: a -> [SuperAgreementAccountData]
    balanceOf :: a -> Timestamp -> RealtimeBalance
    balanceOf a t = foldr
        (+)
        (fromInteger(0) :: RealtimeBalance)
        (map (flip providedBalanceOfAgreement t) (agreementsOf a))
