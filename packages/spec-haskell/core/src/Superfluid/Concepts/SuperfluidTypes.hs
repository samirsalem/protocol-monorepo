{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Superfluid.Concepts.SuperfluidTypes
    ( module Superfluid.Concepts.BaseTypes
    , module Superfluid.Concepts.RealtimeBalance
    ) where

import           Superfluid.Concepts.BaseTypes
import           Superfluid.Concepts.RealtimeBalance

-- | SuperfluidTypes Indexed Type Families
--
-- Naming conventions:
--  * Type name : sft
--
class ( Liquidity (SF_LQ sft)
      , Timestamp (SF_TS sft)
      , RealtimeBalance (SF_RTB sft) (SF_LQ sft)
      ) => SuperfluidTypes sft where
    type SF_LQ sft :: *
    type SF_TS sft :: *
    type SF_RTB sft :: *
