{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Superfluid.Concepts.Account
    ( Account (..)
    , sumAccounts
    ) where

import           Data.Default

import           Superfluid.Concepts.Agreement       (AnyAgreementAccountData, providedBalanceOfAnyAgreement)
import           Superfluid.Concepts.SuperfluidTypes (Address, Liquidity, RealtimeBalance, Timestamp, liquidityToRTB)


-- | Account type class
--
-- Naming conventions:
--   * Type name: acc
--   * Type family name: SF_ACC
--   * Term name: *Account
class (Liquidity lq, Timestamp ts, RealtimeBalance rtb lq, Address addr)
    => Account acc lq ts rtb addr
    | acc -> lq, acc -> ts, acc -> addr, acc -> rtb where

    showAccountAt :: acc -> ts -> String

    addressOfAccount :: acc -> addr

    agreementsOfAccount :: acc -> [AnyAgreementAccountData lq ts rtb]

    balanceOfAccountAt :: acc -> ts -> rtb
    balanceOfAccountAt holderAccount t = foldr
        (+)
        (liquidityToRTB . fromInteger $ 0)
        (map (flip providedBalanceOfAnyAgreement t) (agreementsOfAccount holderAccount))

sumAccounts
    :: (Liquidity lq, Timestamp ts, RealtimeBalance rtb lq, Address addr, Account acc lq ts rtb addr)
    => [acc] -> ts -> rtb
sumAccounts alist t = foldr (+) def (map (flip balanceOfAccountAt t) alist)
