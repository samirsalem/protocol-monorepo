{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Superfluid.Agreements.ConstantFlowAgreement
    ( CFAContractData (..)
    , CFAAccountData (..)
    , updateFlow
    ) where

import           Data.Default
import           Text.Printf

import           Superfluid.Concepts.Agreement       (AgreementAccountData (..))
import           Superfluid.Concepts.Liquidity       (Liquidity)
import           Superfluid.Concepts.RealtimeBalance (integralToLiquidity, liquidityToRTB)
import           Superfluid.Concepts.Timestamp       (Timestamp)


-- ============================================================================
-- CFAContractData Type
-- ============================================================================

data (Liquidity lq, Timestamp ts) => CFAContractData lq ts = CFAContractData
    { flowLastUpdatedAt :: ts
    , flowRate          :: lq
    }

-- ============================================================================
-- CFAAccountData Type
-- ============================================================================

data (Liquidity lq, Timestamp ts) => CFAAccountData lq ts = CFAAccountData
    { settledAt      :: ts
    , settledBalance :: lq
    , netFlowRate    :: lq
    }

instance (Liquidity lq, Timestamp ts) => Default (CFAAccountData lq ts) where
    def = CFAAccountData { settledAt = def, settledBalance = def, netFlowRate = def }

instance (Liquidity lq, Timestamp ts) => AgreementAccountData (CFAAccountData lq ts) lq ts where
    providedBalanceOf CFAAccountData { netFlowRate = r, settledBalance = b_s, settledAt = t_s } t =
        liquidityToRTB $ integralToLiquidity(t - t_s) * r + b_s

instance (Liquidity lq, Timestamp ts) => Show (CFAAccountData lq ts) where
    show x = printf "{ settledAt = %s, settledBalance = %s, netFlowRate = %s }"
        (show $ settledAt x) (show $ settledBalance x) (show $ netFlowRate x)

-- ============================================================================
-- CFA Operations
-- ============================================================================

_updateFlowRate :: (Liquidity lq, Timestamp ts) => CFAAccountData lq ts -> lq -> ts -> CFAAccountData lq ts
_updateFlowRate CFAAccountData { netFlowRate = r, settledBalance = b_s, settledAt = t_s } r_delta t =
    CFAAccountData
        { netFlowRate = r + r_delta
        , settledBalance = b_s + integralToLiquidity(t - t_s) * r
        , settledAt = t
        }

updateFlow :: (Liquidity lq, Timestamp ts)
    => (CFAContractData lq ts, CFAAccountData lq ts, CFAAccountData lq ts) -- (cfaACD, senderAAD, receiverAAD)
    -> lq -> ts -- newFlowRate, t
    -> (CFAContractData lq ts, CFAAccountData lq ts, CFAAccountData lq ts) -- (cfaACD', senderAAD', receiverAAD')
updateFlow (cfaACD, senderAAD, receiverAAD) newFlowRate t =
    ( CFAContractData
        { flowLastUpdatedAt = t
        , flowRate = newFlowRate
        }
    , _updateFlowRate senderAAD (negate flowRateDelta) t
    , _updateFlowRate receiverAAD flowRateDelta t
    )
    where flowRateDelta = newFlowRate - (flowRate cfaACD)
