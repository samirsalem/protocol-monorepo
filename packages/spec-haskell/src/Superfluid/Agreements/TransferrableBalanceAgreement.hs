{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Superfluid.Agreements.TransferrableBalanceAgreement
    ( TBAAccountData (..)
    ) where

import           Data.Default
import           Superfluid.Concepts.Agreement       (AgreementAccountData (..))
import           Superfluid.Concepts.Liquidity       (Liquidity)
import           Superfluid.Concepts.RealtimeBalance (liquidityToRTB)


-- FIXME not so dummy! it is transferrable balance agreement
--
data TBAAccountData liq = TBAAccountData { liquidity :: liq }

instance (Liquidity liq) => Show (TBAAccountData liq) where
    show TBAAccountData { liquidity = l } = "liquidity " ++ (show l)

instance (Liquidity liq) => Default (TBAAccountData liq) where
    def = TBAAccountData { liquidity = def }

instance (Liquidity liq) =>
    AgreementAccountData (TBAAccountData liq) liq where
    providedBalanceOf a _ = liquidityToRTB $ liquidity a
