{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}

module Superfluid.System
    ( Address
    , Account (..)
    , balanceOfAccountAt
    , sumAccounts
    , SFStorageInstruction (..)
    , SuperfluidToken (..)
    ) where

import           Control.Monad                                      (Monad)
import           Data.Default

import           Superfluid.Concepts.Agreement
    ( AnyAgreementAccountData
    , providedBalanceOfAnyAgreement
    )
import           Superfluid.Concepts.RealtimeBalance                (liquidityToRTB)
import           Superfluid.Concepts.SuperfluidTypes                (Liquidity, RealtimeBalance, Timestamp)
--
import qualified Superfluid.Agreements.ConstantFlowAgreement        as CFA
import qualified Superfluid.Agreements.TransferableBalanceAgreement as TBA


-- | Address Type Class
--
-- Naming conventions:
--  * Type name: addr
--  * Type family name: SF_ADDR
class (Eq addr, Show addr) => Address addr

-- | Account type class
--
-- Naming conventions:
--   * Type name: acc
--   * Type family name: SF_ACC
--   * Term name: *Account
class (Liquidity lq, Timestamp ts, RealtimeBalance rtb lq, Address addr)
    => Account acc lq ts rtb addr
    | acc -> lq, acc -> ts, acc -> addr, acc -> rtb where

    getTBAAccountData :: acc -> TBA.TBAAccountData lq ts rtb

    getCFAAccountData :: acc -> CFA.CFAAccountData lq ts rtb

    showAccountAt :: acc -> ts -> String

    addressOfAccount :: acc -> addr

    agreementsOfAccount :: acc -> [AnyAgreementAccountData lq ts rtb]


balanceOfAccountAt
    :: (Liquidity lq, Timestamp ts, RealtimeBalance rtb lq, Address addr, Account acc lq ts rtb addr)
    => acc -> ts -> rtb
balanceOfAccountAt holderAccount t = foldr
    (+)
    (liquidityToRTB . fromInteger $ 0)
    (map (flip providedBalanceOfAnyAgreement t) (agreementsOfAccount holderAccount))

sumAccounts
    :: (Liquidity lq, Timestamp ts, RealtimeBalance rtb lq, Address addr, Account acc lq ts rtb addr)
    => [acc] -> ts -> rtb
sumAccounts alist t = foldr (+) def (map (flip balanceOfAccountAt t) alist)

-- ============================================================================
-- | SFStorageInstruction Sum Type
--
data SFStorageInstruction lq ts rtb addr where
    UpdateLiquidity :: (Liquidity lq, Timestamp ts, RealtimeBalance rtb lq)
        => (addr, TBA.TBAAccountData lq ts rtb) -> SFStorageInstruction lq ts rtb addr
    UpdateFlow :: (Liquidity lq, Timestamp ts, RealtimeBalance rtb lq)
        => (addr, addr, CFA.CFAContractData lq ts rtb) -> SFStorageInstruction lq ts rtb addr
    UpdateAccountFlow :: (Liquidity lq, Timestamp ts, RealtimeBalance rtb lq, Address addr)
        => (addr, CFA.CFAAccountData lq ts rtb) -> SFStorageInstruction lq ts rtb addr

-- ============================================================================
-- | SuperfluidToken Type Class
--
-- Naming conventions:
--   * Type name: tk
--   * Term name: *Token
--
-- Notes:
--
-- * SuperfluidToken is a monadic type, where all its functions run within the monadic context.
-- * SuperfluidToken provides:
--   * addressable account,
--   * and agreement (TBA/CFA/GDA) operations.
-- * Instructions for write operations are executed in `execSFStorageInstructions`.
--
class ( Monad tk
      , Liquidity (SF_LQ tk)
      , Timestamp (SF_TS tk)
      , Address (SF_ADDR tk)
      , RealtimeBalance (SF_RTB tk) (SF_LQ tk)
      , Account (SF_ACC tk) (SF_LQ tk) (SF_TS tk) (SF_RTB tk)(SF_ADDR tk))
    => SuperfluidToken tk where

    -- Associated type families
    type SF_LQ tk :: *
    type SF_TS tk :: *
    type SF_RTB tk :: *
    type SF_ADDR tk :: *
    type SF_ACC tk :: *

    --
    -- System operations
    --
    getCurrentTime :: tk (SF_TS tk)

    execSFStorageInstructions
        :: SF_TS tk
        -> [SFStorageInstruction (SF_LQ tk) (SF_TS tk) (SF_RTB tk) (SF_ADDR tk)]
        -> tk ()

    --
    -- Account operations
    --
    getAccount :: SF_ADDR tk -> tk (SF_ACC tk)

    balanceOfAccount :: SF_ADDR tk -> tk (SF_RTB tk)
    balanceOfAccount addr = do
        t <- getCurrentTime
        account <- getAccount addr
        return $ balanceOfAccountAt account t

    --
    -- TBA functions
    --
    mintLiquidity :: SF_ADDR tk -> SF_LQ tk -> tk ()
    mintLiquidity addr liquidity = do
        t <- getCurrentTime
        account <- getAccount addr
        let account' = (TBA.mintLiquidity . getTBAAccountData) account liquidity
        execSFStorageInstructions t [ UpdateLiquidity (addr, account') ]

    --
    -- CFA functions
    --
    getFlow :: SF_ADDR tk -> SF_ADDR tk -> tk (CFA.CFAContractData (SF_LQ tk) (SF_TS tk) (SF_RTB tk))

    updateFlow :: SF_ADDR tk -> SF_ADDR tk -> SF_LQ tk -> tk ()
    updateFlow senderAddr receiverAddr newFlowRate = do
        t <- getCurrentTime
        senderAccount <- getAccount senderAddr
        receiverAccount <- getAccount receiverAddr
        flowACD <- getFlow senderAddr receiverAddr
        let (flowACD', senderFlowAAD', receiverFlowAAD') = CFA.updateFlow
                (flowACD, (getCFAAccountData senderAccount), (getCFAAccountData receiverAccount))
                newFlowRate t
        execSFStorageInstructions t
            [ UpdateFlow (senderAddr, receiverAddr, flowACD')
            , UpdateAccountFlow (senderAddr, senderFlowAAD')
            , UpdateAccountFlow (receiverAddr, receiverFlowAAD')
            ]
