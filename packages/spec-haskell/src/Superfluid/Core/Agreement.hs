{-# LANGUAGE MultiParamTypeClasses, GADTs #-}

module Superfluid.Core.Agreement
    ( AgreementAccountDataClass(..)
    , AnyAgreementAccountData
    , makeAgreementData
    , providedBalanceOfAgreement
    ) where

import Superfluid.Core.Types (Liquidity, Timestamp)
import Superfluid.Core.RealtimeBalance (RealtimeBalance(..))

class (Liquidity liq) => AgreementAccountDataClass a liq where
    providedBalanceOf :: a -> Timestamp -> RealtimeBalance liq

-- Enumerate all supported agreements
-- See: https://wiki.haskell.org/Heterogenous_collections
data AnyAgreementAccountData liq where
    MkAgreementAccountData :: AgreementAccountDataClass a liq =>
        a -> AnyAgreementAccountData liq

makeAgreementData ::
    (Liquidity liq, AgreementAccountDataClass a liq) =>
    a -> AnyAgreementAccountData liq
makeAgreementData = MkAgreementAccountData

providedBalanceOfAgreement :: (Liquidity liq) =>
    AnyAgreementAccountData liq -> Timestamp -> RealtimeBalance liq
providedBalanceOfAgreement (MkAgreementAccountData g) = providedBalanceOf g
