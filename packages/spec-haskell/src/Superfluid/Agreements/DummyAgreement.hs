{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Superfluid.Agreements.DummyAgreement where

import Superfluid.Core.Types (Liquidity)
import Superfluid.Core.RealtimeBalance (RealtimeBalance)
import Superfluid.Core.Agreement (AgreementAccountDataClass(..))

data DummyAgreementAccountData liq = DummyAgreementAccountData
    { label :: String
    , value :: RealtimeBalance liq
    }

instance (Liquidity liq) =>
    AgreementAccountDataClass (DummyAgreementAccountData liq) liq where
    providedBalanceOf a _ = value a
