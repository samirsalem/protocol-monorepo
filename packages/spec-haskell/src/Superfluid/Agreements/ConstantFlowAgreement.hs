{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Superfluid.Agreements.ConstantFlowAgreement
    ( CFAAgreementData(..)
    , CFAAccountData(..)
    , updateFlow
    ) where

import Superfluid.Core.Types
    ( Liquidity
    , Timestamp)
import Superfluid.Core.RealtimeBalance
    ( liquidityToRTB
    , integralToLiquidity
    )
import Superfluid.Core.Agreement (AgreementAccountDataClass(..))

data CFAAgreementData liq = CFAAgreementData
    { flowLastUpdatedAt :: Timestamp
    , flowRate :: liq
    }

data CFAAccountData liq = CFAAccountData
    { settledAt :: Timestamp
    , settledBalance :: liq
    , netFlowRate :: liq
    }

instance Liquidity liq =>Show (CFAAccountData liq) where
    show CFAAccountData
        { netFlowRate = r
        , settledBalance = b_s
        , settledAt = t_s
        } = " net flowrate " ++ (show r)
        ++  ", settled balance " ++ (show b_s)
        ++  ", settled at " ++ (show t_s)

instance Liquidity liq =>
    AgreementAccountDataClass (CFAAccountData liq) liq where
    providedBalanceOf
        CFAAccountData
            { netFlowRate = r
            , settledBalance = b_s
            , settledAt = t_s
            }
        t = liquidityToRTB $ integralToLiquidity(t - t_s) * r + b_s

_updateFlowRate :: Liquidity liq =>
    CFAAccountData liq -> liq -> Timestamp -> CFAAccountData liq
_updateFlowRate
    CFAAccountData
        { netFlowRate = r
        , settledBalance = b_s
        , settledAt = t_s
        }
    r_delta t =
    CFAAccountData
        { netFlowRate = r + r_delta
        , settledBalance = b_s + integralToLiquidity(t - t_s) * r
        , settledAt = t
        }

updateFlow :: Liquidity liq =>
    CFAAgreementData liq ->
    CFAAccountData liq ->
    CFAAccountData liq ->
    liq ->
    Timestamp ->
    (CFAAgreementData liq, CFAAccountData liq, CFAAccountData liq)
updateFlow cfa sender receiver newFlowRate t =
    let flowRateDelta = newFlowRate - (flowRate cfa)
    in
    ( CFAAgreementData
        { flowLastUpdatedAt = t
        , flowRate = newFlowRate
        }
    , _updateFlowRate sender (negate flowRateDelta) t
    , _updateFlowRate receiver flowRateDelta t
    )
