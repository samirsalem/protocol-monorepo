{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE FlexibleContexts      #-}
-- {-# LANGUAGE TypeSynonymInstances  #-}

module Superfluid.Instances.Simple.Token
    ( SimpleToken (..)
    , SimpleTokenUpdate
    , SimpleTokenState
    , createSimpleToken
    , addAccount
    , listAccounts)
    where

import           Control.Monad.State
import           Data.Default
import qualified Data.Map                                    as M

import           Superfluid                                  (Timestamp)
import qualified Superfluid.Agreements.ConstantFlowAgreement as CFA
import           Superfluid.Instances.Simple.Account         (SimpleAccount)
import qualified Superfluid.Instances.Simple.Account         as SimpleAccount
import           Superfluid.Instances.Simple.Address         (SimpleAddress)
import           Superfluid.Instances.Simple.Types           (SimpleCFAContractData)
import           Superfluid.Instances.Simple.Wad             (Wad)
import           Superfluid.System.SuperfluidToken


data SimpleToken = SimpleToken
    { accounts      :: M.Map SimpleAddress SimpleAccount
    , cfaAgreements :: M.Map String SimpleCFAContractData
    }

instance Default SimpleToken
    where def = SimpleToken { accounts = def, cfaAgreements = def }

createSimpleToken :: [(SimpleAddress, SimpleAccount)] -> SimpleToken
createSimpleToken alist = SimpleToken
    { accounts = M.fromList alist
    , cfaAgreements = M.fromList []
    }

type SimpleTokenUpdate = SuperfluidTokenUpdate Wad SimpleAddress

type SimpleTokenState = State SimpleToken

instance SuperfluidToken SimpleTokenState Wad SimpleAddress SimpleAccount
    where

    getAccount :: SimpleAddress -> SimpleTokenState SimpleAccount
    getAccount a = get >>= \s -> return $
        case M.lookup a (accounts s) of
            Just value -> value
            Nothing    -> error "No such address"

    getFlow :: SimpleAddress -> SimpleAddress -> SimpleTokenState SimpleCFAContractData
    getFlow a b = get >>= \s -> return $
        case M.lookup (show(a)++":"++show(b)) (cfaAgreements s) of
            Just value -> value
            Nothing    -> CFA.CFAContractData 0 0

    updateFlow :: SimpleAddress -> SimpleAddress -> Wad -> Timestamp -> SimpleTokenState ()
    updateFlow sender receiver newFlowRate t = do
        updates <- _updateFlow sender receiver newFlowRate t
        mapM_ (\u -> case u of
            UpdateFlow flow' -> do
                modify (\vs -> vs {
                    cfaAgreements = M.insert
                        (show(sender)++":"++show(receiver))
                        flow'
                        (cfaAgreements vs)
                })
            UpdateAccountFlow (accountAddr, accountFlow') -> do
                account <- getAccount accountAddr
                modify (\vs -> vs {
                    accounts = M.insert
                        accountAddr
                        (account { SimpleAccount.cfa = accountFlow' })
                        (accounts vs)
                })
            ) updates

addAccount :: SimpleAddress -> SimpleAccount -> SimpleTokenState ()
addAccount accountAddr account = modify (\vs -> vs {
    accounts = M.insert
        accountAddr
        account
        (accounts vs)
    })

listAccounts :: SimpleTokenState [(SimpleAddress, SimpleAccount)]
listAccounts = get >>= \s -> return $ M.toList (accounts s)

-- FIXME create a SuperfluidTokenT
