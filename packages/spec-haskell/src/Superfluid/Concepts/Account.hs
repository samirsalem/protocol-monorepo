{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Superfluid.Concepts.Account
    ( Account (..)
    ) where

import           Superfluid.Concepts.Agreement       (AnyAgreementAccountData, providedBalanceOfAnyAgreement)
import           Superfluid.Concepts.Liquidity       (Liquidity)
import           Superfluid.Concepts.RealtimeBalance (RealtimeBalance, liquidityToRTB)
import           Superfluid.Concepts.Timestamp       (Timestamp)


-- Account type class
--
-- Naming conventions:
--   * Type name: acc
--   * Type family name: ACC
--   * Term name: *Account
class (Liquidity lq, Timestamp ts)
    => Account acc lq ts | acc -> lq, acc -> ts where

    agreementsOf :: acc -> [AnyAgreementAccountData lq ts]

    balanceOf :: acc -> ts -> RealtimeBalance lq
    balanceOf holderAccount t = foldr
        (+)
        (liquidityToRTB . fromInteger $ 0)
        (map (flip providedBalanceOfAnyAgreement t) (agreementsOf holderAccount))
