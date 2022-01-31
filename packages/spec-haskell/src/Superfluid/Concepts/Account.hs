{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Superfluid.Concepts.Account
    ( Account (..)
    ) where

import           Superfluid.Concepts.Agreement       (AnyAgreementAccountData, providedBalanceOfAnyAgreement)
import           Superfluid.Concepts.Liquidity       (Liquidity)
import           Superfluid.Concepts.RealtimeBalance (RealtimeBalance,
                                                      liquidityToRTB)
import           Superfluid.Concepts.Timestamp       (Timestamp)


{- # Account type class

## Type parameters:

* @a@ - account type
* @liq@ - liquidity type, instance of Liquidity
-}
class (Liquidity liq) => Account a liq
    | a -> liq where

    agreementsOf :: a -> [AnyAgreementAccountData liq]

    balanceOf :: a -> Timestamp -> RealtimeBalance liq
    balanceOf a t = foldr
        (+)
        (liquidityToRTB . fromInteger $ 0)
        (map (flip providedBalanceOfAnyAgreement t) (agreementsOf a))
