{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Superfluid.Instances.Simple.Account
    ( SimpleAccount(..)
    , createSimpleAccount
    , sumAllSimpleAccount
    ) where

import           Data.Default
import           Superfluid                                          (Account, SuperfluidAccount,
                                                                      TBAAccountData,
                                                                      Timestamp)
import qualified Superfluid.Concepts.Account                         as Account (Account (..),
                                                                                 balanceOf)
import           Superfluid.Concepts.Agreement                       (AnyAgreementAccountData (..))

import qualified Superfluid.Agreements.TransferrableBalanceAgreement as TBA

import qualified Superfluid.System.SuperfluidAccount                 as SuperfluidAccount


import           Superfluid.Instances.Simple.Address                 (SimpleAddress,
                                                                      createSimpleAddress)
import           Superfluid.Instances.Simple.Types                   (SimpleCFAAccountData,
                                                                      SimpleRealtimeBalance,
                                                                      SimpleTBAAccountData)
import           Superfluid.Instances.Simple.Wad                     (Wad (..))



{- Simple Superfluid Account type
-}
data SimpleAccount = SimpleAccount
    { address       :: SimpleAddress
    , staticBalance :: Wad
    , tba           :: SimpleTBAAccountData
    , cfa           :: SimpleCFAAccountData
    , lastUpdatedAt :: Timestamp
    }

instance Account SimpleAccount Wad where
    agreementsOf a =
        [ MkAgreementAccountData $ tba a
        , MkAgreementAccountData $ cfa a
        ]

instance SuperfluidAccount SimpleAccount Wad where
    getCFAAccountData :: SimpleAccount -> SimpleCFAAccountData
    getCFAAccountData = cfa

    showAt a t =
        "Account: " ++ show(address a) ++
        "\n  Balance: " ++ show((Account.balanceOf a t) :: SimpleRealtimeBalance) ++
        "\n  TBA Data: " ++ show(tba a) ++
        "\n  CFA Data: " ++ show(cfa a)

createSimpleAccount :: String -> Wad -> Timestamp -> SimpleAccount
createSimpleAccount toAddress initBalance t = SimpleAccount
    { address = createSimpleAddress toAddress
    , staticBalance = 0
    , lastUpdatedAt = t
    , tba = (def :: TBAAccountData Wad){ TBA.liquidity = initBalance }
    , cfa = def
    }

sumAllSimpleAccount :: [SimpleAccount] -> Timestamp -> SimpleRealtimeBalance
sumAllSimpleAccount alist t = foldr
    (+)
    (def :: SimpleRealtimeBalance)
    (map (flip Account.balanceOf t) alist)
