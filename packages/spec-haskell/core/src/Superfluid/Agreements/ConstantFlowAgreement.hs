{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Superfluid.Agreements.ConstantFlowAgreement
    ( CFAContractData
    , CFAAccountData
    , getNetFlowRate
    , updateFlow
    ) where

import           Data.Default
import           Text.Printf

import           Superfluid.Concepts.Agreement       (AgreementAccountData (..), AgreementContractData)
import           Superfluid.Concepts.SuperfluidTypes
    ( Liquidity
    , RealtimeBalance
    , Timestamp
    , integralToLiquidity
    , liquidityToRTB
    )


-- ============================================================================
-- | CFAContractData Type
--
data (Liquidity lq, Timestamp ts, RealtimeBalance rtb lq)
    => CFAContractData lq ts rtb = CFAContractData
    { flowLastUpdatedAt :: ts
    , flowRate          :: lq
    }

instance (Liquidity lq, Timestamp ts, RealtimeBalance rtb lq)
    => Default (CFAContractData lq ts rtb) where
    def = CFAContractData { flowLastUpdatedAt = def, flowRate = def }

instance (Liquidity lq, Timestamp ts, RealtimeBalance rtb lq)
    => Show (CFAContractData lq ts rtb) where
    show x = printf "{ flowLastUpdatedAt = %s, flowRate = %s }"
        (show $ flowLastUpdatedAt x) (show $ flowRate x)

instance (Liquidity lq, Timestamp ts, RealtimeBalance rtb lq)
    => AgreementContractData (CFAContractData lq ts rtb) lq ts rtb

-- ============================================================================
-- | CFAAccountData Type (is AgreementAccountData)
--
data (Liquidity lq, Timestamp ts, RealtimeBalance rtb lq)
    => CFAAccountData lq ts rtb = CFAAccountData
    { settledAt      :: ts
    , settledBalance :: lq
    , netFlowRate    :: lq
    }

instance (Liquidity lq, Timestamp ts, RealtimeBalance rtb lq)
    => Default (CFAAccountData lq ts rtb) where
    def = CFAAccountData { settledAt = def, settledBalance = def, netFlowRate = def }

instance (Liquidity lq, Timestamp ts, RealtimeBalance rtb lq)
    => AgreementAccountData (CFAAccountData lq ts rtb) lq ts rtb where
    providedBalanceOfAgreement CFAAccountData { netFlowRate = r, settledBalance = b_s, settledAt = t_s } t =
        liquidityToRTB $ integralToLiquidity(t - t_s) * r + b_s

instance (Liquidity lq, Timestamp ts, RealtimeBalance rtb lq)
    => Show (CFAAccountData lq ts rtb) where
    show x = printf "{ settledAt = %s, settledBalance = %s, netFlowRate = %s }"
        (show $ settledAt x) (show $ settledBalance x) (show $ netFlowRate x)

-- ============================================================================
-- CFA Operations
--
getNetFlowRate
    :: (Liquidity lq, Timestamp ts, RealtimeBalance rtb lq)
    => CFAAccountData lq ts rtb -> lq
getNetFlowRate = netFlowRate

updateFlow
    :: (Liquidity lq, Timestamp ts, RealtimeBalance rtb lq)
    -- (cfaACD, senderAAD, receiverAAD)
    => (CFAContractData lq ts rtb, CFAAccountData lq ts rtb, CFAAccountData lq ts rtb)
    -- newFlowRate, t
    -> lq -> ts
    -- (cfaACD', senderAAD', receiverAAD')
    -> (CFAContractData lq ts rtb, CFAAccountData lq ts rtb, CFAAccountData lq ts rtb)
updateFlow (cfaACD, senderAAD, receiverAAD) newFlowRate t =
    ( CFAContractData
        { flowLastUpdatedAt = t
        , flowRate = newFlowRate
        }
    , updateFlowRate senderAAD (negate flowRateDelta) t
    , updateFlowRate receiverAAD flowRateDelta t
    )
    where
        flowRateDelta = newFlowRate - (flowRate cfaACD)
        updateFlowRate CFAAccountData { netFlowRate = r, settledBalance = b_s, settledAt = t_s } r_delta t_s' =
            CFAAccountData
                { netFlowRate = r + r_delta
                , settledBalance = b_s + integralToLiquidity(t - t_s) * r
                , settledAt = t_s'
                }
