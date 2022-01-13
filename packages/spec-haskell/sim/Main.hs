module Main where

import Control.Monad.State
import qualified Data.Map as M
import Data.Time.Clock.POSIX (getPOSIXTime)
import Superfluid.Core.Types (Timestamp)
import Superfluid.Testing.SimpleAccount
    ( createAccountAddress
    , toWad
    , SimpleAccount
    , createSimpleAccount)
import SimState

now :: IO Int
now =  do
    t <- getPOSIXTime
    return $ fromIntegral ((round t) :: Int)

createTestAccount :: String -> Timestamp -> SimpleAccount
createTestAccount a t = createSimpleAccount a b t
    where b = toWad (100.0 :: Double)

mainLoop :: SimStateMonad ()
mainLoop = do
    let alice = createAccountAddress "alice"
    let bob = createAccountAddress "bob"
    let carol = createAccountAddress "carol"
    -- t1
    t1 <- getCurrentTime
    liftIO $ print $ "# t1.a: initial state " ++ (show t1)
    printMainState
    liftIO $ print $ "# t1.b: create flows"
    updateFlow alice bob (toWad (0.0001::Double)) t1
    updateFlow alice carol (toWad (0.0002::Double)) t1
    printMainState
    t2 <- timeTravel $ (3600 * 24)
    liftIO $ print $ "t2: advanced one full day" ++ (show t2)
    printMainState

main :: IO ()
main = do
    t1 <- now
    _ <- runStateT mainLoop SimState
        { currentTime = t1
        , accounts = M.fromList $
            map (\x ->
                ( createAccountAddress x
                , createTestAccount x t1))
            ["alice", "bob", "carol"]
        , cfaAgreements = M.fromList []
        }
    print "# THE END"
