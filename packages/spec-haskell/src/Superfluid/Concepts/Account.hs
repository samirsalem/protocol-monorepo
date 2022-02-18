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
--   * Type family name: SF_ACC
--   * Term name: *Account
class (Liquidity lq, Timestamp ts, RealtimeBalance rtb lq, Address addr)
    => Account acc lq ts rtb addr
    | acc -> lq, acc -> ts, acc -> addr, acc -> rtb where

    addressOf :: acc -> addr

    -- TODO return type polymorphism
    -- getAgreement :: (AgreementAccountData aad lq ts) => acc -> aad

    agreementsOf :: acc -> [AnyAgreementAccountData lq ts rtb]

    balanceOf :: acc -> ts -> rtb
    balanceOf holderAccount t = foldr
        (+)
        (liquidityToRTB . fromInteger $ 0)
        (map (flip providedBalanceOfAnyAgreement t) (agreementsOf holderAccount))
