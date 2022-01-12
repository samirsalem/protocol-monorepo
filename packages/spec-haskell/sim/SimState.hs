module SimState where

import qualified Data.Map as M
import Control.Monad.State
import Superfluid.Core.Types (Timestamp)
import Superfluid(Account(..))
import qualified Superfluid.Agreements.ConstantFlowAgreement as CFAv1
import qualified Superfluid.Testing.SimpleAccount as SimpleAccount
import Superfluid.Testing.SimpleAccount
    ( Address
    , SimpleAccount
    , sumAllSimpleAccount)

data SimState = SimState
    { currentTime :: Timestamp
    , accounts :: M.Map Address SimpleAccount
    , cfaAgreements :: M.Map String (CFAv1.CFAv1AgreementData Integer)}
type SimStateMonad a = StateT SimState IO a

printAccount :: SimpleAccount -> Timestamp -> IO ()
printAccount a t = do
    print $ "Account: " ++ SimpleAccount.address a
    print $ "  Balance: " ++ (show $ balanceOf a t)
    print $ "  CFA Data: " ++ (show $ SimpleAccount.cfa a)

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

findAccount :: SimState -> Address -> SimpleAccount
findAccount s a = case M.lookup a (accounts s) of
        Just value -> value
        Nothing -> error "No such address"

findCFA :: SimState -> Address -> Address -> CFAv1.CFAv1AgreementData Integer
findCFA s a b = case M.lookup (a++":"++b) (cfaAgreements s) of
        Just value -> value
        Nothing -> CFAv1.CFAv1AgreementData 0 0

updateFlow :: Address -> Address -> Integer -> Timestamp -> SimStateMonad ()
updateFlow sender receiver newFlowRate t = do
    s <- get
    let cfaAgreement = findCFA s sender receiver
    let senderAccount = findAccount s sender
    let receiverAccount = findAccount s receiver
    let (cfaAgreement', senderCFA', receiverCFA') = CFAv1.updateFlow
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
            (sender++":"++receiver)
            cfaAgreement'
            (cfaAgreements vs)
        })

printMainState :: SimStateMonad ()
printMainState = do
    s <- get
    let t = (currentTime s)
    liftIO $ print "====="
    liftIO $ printAccount (findAccount s "alice") t
    liftIO $ printAccount (findAccount s "bob") t
    liftIO $ printAccount (findAccount s "carol") t
    liftIO $ print $ "Total Balance: "
        ++ (show $ sumAllSimpleAccount (map (\(_,a) -> a) $ M.toList $ accounts s) t)
    liftIO $ print "====="
