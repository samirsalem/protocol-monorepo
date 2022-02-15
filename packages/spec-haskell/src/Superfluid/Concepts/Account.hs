{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Superfluid.Concepts.Account
    ( Account (..)
    ) where

import           Superfluid.BaseTypes                (Address, Liquidity, Timestamp)
import           Superfluid.Concepts.Agreement       (AnyAgreementAccountData, providedBalanceOfAnyAgreement)
import           Superfluid.Concepts.RealtimeBalance (RealtimeBalance, liquidityToRTB)


-- | Account type class
--
-- Naming conventions:
--   * Type name: acc
--   * Type family name: ACC
--   * Term name: *Account
class (Liquidity lq, Timestamp ts, Address addr)
    => Account acc lq ts addr | acc -> lq, acc -> ts, acc -> addr where

    address :: acc -> addr

    -- TODO return type polymorphism
    -- getAgreement :: (AgreementAccountData aad lq ts) => acc -> aad

    agreementsOf :: acc -> [AnyAgreementAccountData lq ts]

    balanceOf :: acc -> ts -> RealtimeBalance lq
    balanceOf holderAccount t = foldr
        (+)
        (liquidityToRTB . fromInteger $ 0)
        (map (flip providedBalanceOfAnyAgreement t) (agreementsOf holderAccount))
