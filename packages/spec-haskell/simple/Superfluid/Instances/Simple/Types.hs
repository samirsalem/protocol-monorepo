module Superfluid.Instances.Simple.Types
    ( SimpleRealtimeBalance
    , SimpleTBAAccountData
    , SimpleCFAContractData
    , SimpleCFAAccountData
    ) where

import           Superfluid                      (CFAAccountData,
                                                  CFAContractData,
                                                  RealtimeBalance,
                                                  TBAAccountData)

import           Superfluid.Instances.Simple.Wad (Wad)

-- Wad-based Simple Types
type SimpleRealtimeBalance = RealtimeBalance Wad
type SimpleTBAAccountData = TBAAccountData Wad
type SimpleCFAContractData = CFAContractData Wad
type SimpleCFAAccountData = CFAAccountData Wad
