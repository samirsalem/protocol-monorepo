{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Superfluid.Concepts.Agreement
    ( AgreementContractData
    , AgreementAccountData (..)
    , AnyAgreementAccountData (MkAgreementAccountData)
    , providedBalanceOfAnyAgreement
    ) where

import           Data.Default
import           Superfluid.BaseTypes                (Liquidity, Timestamp)
import           Superfluid.Concepts.RealtimeBalance (RealtimeBalance (..))


-- | AgreementContractData type class
--
-- Naming conventions:
--  * Type name: acd
--  * Term name: *ACD
class (Liquidity lq, RealtimeBalance rtb lq, Timestamp ts, Default acd, Show acd)
    => AgreementContractData acd lq ts rtb where

-- | AgreementAccountData type class
--
-- Naming conventions:
--  - Type name: aad
--  - Term name: *AAD
class (Liquidity lq, RealtimeBalance rtb lq, Timestamp ts, Default aad, Show aad)
    => AgreementAccountData aad lq ts rtb where

    providedBalanceOf :: aad -> ts -> rtb

-- | AnyAgreementAccountData type
--
-- Naming conventions:
--  - Type name: aad
--  - Term name: AAAD
--
-- Notes:
-- - To Enumerate all supported agreements using GADTs
--   See: https://wiki.haskell.org/Heterogenous_collections
-- - MkAgreementAccountData is the constructor
-- - providedBalanceOfAnyAgreement is convenience wrapper of providedBalanceOf
data AnyAgreementAccountData lq ts rtb where
    MkAgreementAccountData
        :: (Liquidity lq, Timestamp ts, RealtimeBalance rtb lq, AgreementAccountData aad lq ts rtb)
        => aad -> AnyAgreementAccountData lq ts rtb

-- | providedBalanceOf wrapper for AnyAgreementAccountData
providedBalanceOfAnyAgreement
    :: (Liquidity lq, Timestamp ts, RealtimeBalance rtb lq)
    => AnyAgreementAccountData lq ts rtb -> ts -> rtb
providedBalanceOfAnyAgreement (MkAgreementAccountData g) = providedBalanceOf g
