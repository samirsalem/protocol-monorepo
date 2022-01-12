{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification, GADTs #-}

module Superfluid.Core.SuperAgreement
    ( SuperAgreementAccountDataClass(..)
    , SuperAgreementAccountData
    , makeSuperAgreementData
    , providedBalanceOfAgreement
    ) where

import Superfluid.Core.Types (Liquidity, Timestamp)
import Superfluid.Core.RealtimeBalance (RealtimeBalance)

class (Liquidity liq) => SuperAgreementAccountDataClass a liq where
    providedBalanceOf :: a -> Timestamp -> RealtimeBalance liq

-- Enumerate all supported agreements
-- See: https://wiki.haskell.org/Heterogenous_collections
-- data SuperAgreementAccountData = forall a liq.
--     (Liquidity liq, SuperAgreementAccountDataClass a liq) =>
--     MkSuperAgreementAccountData a
data SuperAgreementAccountData liq where
    MkSuperAgreementAccountData ::
        SuperAgreementAccountDataClass a liq =>
        a -> SuperAgreementAccountData liq

makeSuperAgreementData ::
    (Liquidity liq, SuperAgreementAccountDataClass a liq) =>
    a -> SuperAgreementAccountData liq
makeSuperAgreementData = MkSuperAgreementAccountData

providedBalanceOfAgreement :: (Liquidity liq) =>
    SuperAgreementAccountData liq -> Timestamp -> RealtimeBalance liq
providedBalanceOfAgreement (MkSuperAgreementAccountData g) = providedBalanceOf g
