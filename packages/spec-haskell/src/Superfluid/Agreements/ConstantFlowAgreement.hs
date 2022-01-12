{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Superfluid.Agreements.ConstantFlowAgreement
    ( CFAv1AgreementData(..)
    , CFAv1AccountData(..)
    , updateFlow
    ) where

import Superfluid.Core.Types (Liquidity, Timestamp)
import Superfluid.Core.RealtimeBalance
    ( liquidityToRTB
    , integralToLiquidity)
import Superfluid.Core.SuperAgreement (SuperAgreementAccountDataClass(..))

data CFAv1AgreementData liq = CFAv1AgreementData
    { flowLastUpdatedAt :: Timestamp
    , flowRate :: liq
    }

data CFAv1AccountData liq = CFAv1AccountData
    { settledAt :: Timestamp
    , settledBalance :: liq
    , netFlowRate :: liq
    }

instance Liquidity liq =>Show (CFAv1AccountData liq) where
    show CFAv1AccountData
        { netFlowRate = r
        , settledBalance = b_s
        , settledAt = t_s
        } = " net flowrate " ++ (show r)
        ++  ", settled balance " ++ (show b_s)
        ++  ", settled at " ++ (show t_s)

instance Liquidity liq =>
    SuperAgreementAccountDataClass (CFAv1AccountData liq) liq where
    providedBalanceOf
        CFAv1AccountData
            { netFlowRate = r
            , settledBalance = b_s
            , settledAt = t_s
            }
        t = liquidityToRTB $ integralToLiquidity(t - t_s) * r + b_s

_updateFlowRate :: Liquidity liq =>
    CFAv1AccountData liq -> liq -> Timestamp -> CFAv1AccountData liq
_updateFlowRate
    CFAv1AccountData
        { netFlowRate = r
        , settledBalance = b_s
        , settledAt = t_s
        }
    r_delta t =
    CFAv1AccountData
        { netFlowRate = r + r_delta
        , settledBalance = b_s + integralToLiquidity(t - t_s) * r
        , settledAt = t
        }

updateFlow :: Liquidity liq =>
    CFAv1AgreementData liq ->
    CFAv1AccountData liq ->
    CFAv1AccountData liq ->
    liq ->
    Timestamp ->
    (CFAv1AgreementData liq, CFAv1AccountData liq, CFAv1AccountData liq)
updateFlow cfa sender receiver newFlowRate t =
    let flowRateDelta = newFlowRate - (flowRate cfa)
    in
    ( CFAv1AgreementData
        { flowLastUpdatedAt = t
        , flowRate = newFlowRate
        }
    , _updateFlowRate sender (negate flowRateDelta) t
    , _updateFlowRate receiver flowRateDelta t
    )
