module Superfluid.Agreements.ConstantFlowAgreement
    ( CFAv1AgreementData(..)
    , CFAv1AccountData(..)
    , updateFlow
    ) where

import Superfluid.Types (Timestamp)
import Superfluid.RealtimeBalance (RealtimeBalance(..))
import Superfluid.SuperAgreement (SuperAgreementAccountDataClass(..))

data CFAv1AgreementData = CFAv1AgreementData
    { flowLastUpdatedAt :: Timestamp
    , flowRate :: Integer
    }

data CFAv1AccountData = CFAv1AccountData
    { settledAt :: Timestamp
    , settledBalance :: Integer
    , netFlowRate :: Integer
    }

instance Show CFAv1AccountData where
    show CFAv1AccountData
        { netFlowRate = r
        , settledBalance = b_s
        , settledAt = t_s
        } = " net flowrate " ++ (show r)
        ++  ", settled balance " ++ (show b_s)
        ++  ", settled at " ++ (show t_s)

instance SuperAgreementAccountDataClass CFAv1AccountData where
    providedBalanceOf
        CFAv1AccountData
            { netFlowRate = r
            , settledBalance = b_s
            , settledAt = t_s
            }
        t =
        RealtimeBalance (toInteger(t - t_s) * r + b_s) 0 0

_updateFlowRate :: CFAv1AccountData -> Integer -> Timestamp -> CFAv1AccountData
_updateFlowRate
    CFAv1AccountData
        { netFlowRate = r
        , settledBalance = b_s
        , settledAt = t_s
        }
    r_delta t =
    CFAv1AccountData
        { netFlowRate = r + r_delta
        , settledBalance = b_s + (toInteger(t - t_s) * r)
        , settledAt = t
        }

updateFlow :: CFAv1AgreementData -> CFAv1AccountData -> CFAv1AccountData -> Integer -> Timestamp -> (CFAv1AgreementData, CFAv1AccountData, CFAv1AccountData)
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
