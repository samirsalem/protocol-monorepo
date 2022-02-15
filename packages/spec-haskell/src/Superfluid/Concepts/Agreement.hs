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
class (Liquidity lq, Timestamp ts, Default acd, Show acd) => AgreementContractData acd lq ts where

-- | AgreementAccountData type class
--
-- Naming conventions:
--  - Type name: aad
--  - Term name: *AAD
class (Liquidity lq, Timestamp ts, Default aad, Show aad) => AgreementAccountData aad lq ts where
    providedBalanceOf :: aad -> ts -> RealtimeBalance lq

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
data AnyAgreementAccountData lq ts where
    MkAgreementAccountData
        :: (Liquidity lq, Timestamp ts, AgreementAccountData aad lq ts)
        => aad -> AnyAgreementAccountData lq ts

-- | providedBalanceOf wrapper for AnyAgreementAccountData
providedBalanceOfAnyAgreement
    :: (Liquidity lq, Timestamp ts)
    => AnyAgreementAccountData lq ts -> ts -> RealtimeBalance lq
providedBalanceOfAnyAgreement (MkAgreementAccountData g) = providedBalanceOf g
