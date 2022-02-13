{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Superfluid.Instances.Simple.Account
    ( SimpleAccount(..)
    , createSimpleAccount
    , sumAllSimpleAccount
    ) where

import           Data.Default
import           Superfluid                                         (Account, SuperfluidAccount)
import qualified Superfluid.Agreements.TransferableBalanceAgreement as TBA
import qualified Superfluid.Concepts.Account                        as Account (Account (..), balanceOf)
import           Superfluid.Concepts.Agreement                      (AnyAgreementAccountData (..))
import qualified Superfluid.System                                  as SF

import           Superfluid.Instances.Simple.BaseTypes
    ( SimpleAddress
    , SimpleCFAAccountData
    , SimpleRealtimeBalance
    , SimpleTBAAccountData
    , SimpleTimestamp
    , Wad
    , createSimpleAddress
    )

-- SimpleAccount Type
--
data SimpleAccount = SimpleAccount
    { address       :: SimpleAddress
    , staticBalance :: Wad
    , tba           :: SimpleTBAAccountData
    , cfa           :: SimpleCFAAccountData
    , lastUpdatedAt :: SimpleTimestamp
    }

instance Account SimpleAccount Wad SimpleTimestamp where
    agreementsOf a =
        [ MkAgreementAccountData $ tba a
        , MkAgreementAccountData $ cfa a
        ]

instance SuperfluidAccount SimpleAccount Wad SimpleTimestamp where
    showAt a t =
        "Account: " ++ show(address a) ++
        "\n  Balance: " ++ show((Account.balanceOf a t) :: SimpleRealtimeBalance) ++
        "\n  TBA Data: " ++ show(tba a) ++
        "\n  CFA Data: " ++ show(cfa a)

    getTBAAccountData :: SimpleAccount -> SimpleTBAAccountData
    getTBAAccountData = tba

    getCFAAccountData :: SimpleAccount -> SimpleCFAAccountData
    getCFAAccountData = cfa

createSimpleAccount :: String -> Wad -> SimpleTimestamp -> SimpleAccount
createSimpleAccount toAddress initBalance t = SimpleAccount
    { address = createSimpleAddress toAddress
    , staticBalance = 0
    , lastUpdatedAt = t
    , tba = (def :: SimpleTBAAccountData){ TBA.liquidity = initBalance }
    , cfa = def
    }

sumAllSimpleAccount :: [SimpleAccount] -> SimpleTimestamp -> SimpleRealtimeBalance
sumAllSimpleAccount alist t = foldr
    (+)
    (def :: SimpleRealtimeBalance)
    (map (flip Account.balanceOf t) alist)
