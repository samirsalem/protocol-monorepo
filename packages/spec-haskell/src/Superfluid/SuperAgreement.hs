{-# LANGUAGE ExistentialQuantification #-}

module Superfluid.SuperAgreement
    ( SuperAgreementAccountDataClass(..)
    , SuperAgreementAccountData
    , makeSuperAgreementData
    , providedBalanceOfAgreement
    , DummyAgreementAccountData(..)
    ) where

import Superfluid.RealtimeBalance (RealtimeBalance)
import Superfluid.Types (Timestamp)

class SuperAgreementAccountDataClass g where
    providedBalanceOf :: g -> Timestamp -> RealtimeBalance

-- Enumerate all supported agreements
-- See: https://wiki.haskell.org/Heterogenous_collections
data SuperAgreementAccountData =
    forall a . SuperAgreementAccountDataClass a
    => MKSuperAgreementAccountData a
makeSuperAgreementData :: SuperAgreementAccountDataClass a => a -> SuperAgreementAccountData
makeSuperAgreementData = MKSuperAgreementAccountData
providedBalanceOfAgreement :: SuperAgreementAccountData -> Timestamp -> RealtimeBalance
providedBalanceOfAgreement (MKSuperAgreementAccountData g) = providedBalanceOf g

-- Dummy Agreement
data DummyAgreementAccountData = DummyAgreementAccountData
    { label :: String
    , value :: RealtimeBalance
    }
instance SuperAgreementAccountDataClass DummyAgreementAccountData where
    providedBalanceOf DummyAgreementAccountData{value = rtb} _ = rtb
