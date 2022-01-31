{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Superfluid.Concepts.Agreement
    ( AgreementAccountData (..)
    , AnyAgreementAccountData (MkAgreementAccountData)
    , providedBalanceOfAnyAgreement
    ) where

import           Data.Default
import           Superfluid.Concepts.Liquidity       (Liquidity)
import           Superfluid.Concepts.RealtimeBalance (RealtimeBalance (..))
import           Superfluid.Concepts.Timestamp       (Timestamp)



-- class (Default aad) => AgreementContractData aad where

-- AgreementAccountData type class
--
class (Default aad, Liquidity liq) => AgreementAccountData aad liq where
    providedBalanceOf :: aad -> Timestamp -> RealtimeBalance liq

{- Any AgreementAccountData type

Notes:
- To Enumerate all supported agreements using GADTs
  See: https://wiki.haskell.org/Heterogenous_collections
- MkAgreementAccountData is the constructor
- providedBalanceOfAnyAgreement is convenience wrapper of providedBalanceOf
-}
data AnyAgreementAccountData liq where
    MkAgreementAccountData
        :: (AgreementAccountData aad liq)
        => aad -> AnyAgreementAccountData liq

providedBalanceOfAnyAgreement
    :: (Liquidity liq)
    => AnyAgreementAccountData liq -> Timestamp -> RealtimeBalance liq
providedBalanceOfAnyAgreement (MkAgreementAccountData g) = providedBalanceOf g
