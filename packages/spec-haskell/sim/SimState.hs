module SimState where

import qualified Data.Map as M
import Control.Monad.State
import Superfluid (Timestamp)
import Superfluid.Core.Account as Account
    ( Account(..)
    )
import Superfluid.Agreements.ConstantFlowAgreement as CFA
import Superfluid.Testing.SimpleAccount as SimpleAccount
    ( WAD
    , AccountAddress
    , createAccountAddress
    , SimpleAccount(..)
    , SimpleRealtimeBalance
    , SimpleCFAAgreementData
    , sumAllSimpleAccount)

data SimState = SimState
    { currentTime :: Timestamp
    , accounts :: M.Map AccountAddress SimpleAccount
    , cfaAgreements :: M.Map String SimpleCFAAgreementData}
type SimStateMonad a = StateT SimState IO a

printAccount :: SimpleAccount -> Timestamp -> IO ()
printAccount a t = do
    print $ "Account: " ++ show(SimpleAccount.address a)
    print $ "  Balance: " ++ show((Account.balanceOf a t) :: SimpleRealtimeBalance)
    print $ "  CFA Data: " ++ show(SimpleAccount.cfa a)

getCurrentTime :: SimStateMonad Timestamp
getCurrentTime = do
    s <- get
    return (currentTime s)

timeTravel :: Int -> SimStateMonad Timestamp
timeTravel d = do
    s <- get
    let t' = d + (currentTime s)
    modify (\vs -> vs { currentTime = t' })
    return t'

findAccount :: SimState -> AccountAddress -> SimpleAccount
findAccount s a = case M.lookup a (accounts s) of
        Just value -> value
        Nothing -> error "No such address"

findCFA :: SimState -> AccountAddress -> AccountAddress -> SimpleCFAAgreementData
findCFA s a b = case M.lookup (show(a)++":"++show(b)) (cfaAgreements s) of
        Just value -> value
        Nothing -> CFA.CFAAgreementData 0 0

updateFlow :: AccountAddress -> AccountAddress -> WAD -> Timestamp -> SimStateMonad ()
updateFlow sender receiver newFlowRate t = do
    s <- get
    let cfaAgreement = findCFA s sender receiver
    let senderAccount = findAccount s sender
    let receiverAccount = findAccount s receiver
    let (cfaAgreement', senderCFA', receiverCFA') = CFA.updateFlow
            cfaAgreement
            (SimpleAccount.cfa senderAccount)
            (SimpleAccount.cfa receiverAccount)
            newFlowRate
            t
    modify (\vs -> vs
        { accounts = foldr
            (\(x, y) z -> M.insert x y z)
            (accounts vs)
            [(sender, senderAccount { SimpleAccount.cfa = senderCFA' })
            ,(receiver, receiverAccount { SimpleAccount.cfa = receiverCFA' })]
        , cfaAgreements = M.insert
            (show(sender)++":"++show(receiver))
            cfaAgreement'
            (cfaAgreements vs)
        })

printMainState :: SimStateMonad ()
printMainState = do
    s <- get
    let t = (currentTime s)
    liftIO $ print "====="
    liftIO $ printAccount (findAccount s (createAccountAddress "alice")) t
    liftIO $ printAccount (findAccount s (createAccountAddress "bob")) t
    liftIO $ printAccount (findAccount s (createAccountAddress "carol")) t
    liftIO $ print $ "Total Balance: "
        ++ (show $ sumAllSimpleAccount (map (\(_,a) -> a) $ M.toList $ accounts s) t)
    liftIO $ print "====="
