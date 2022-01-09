module Main where

import Control.Monad.State
import qualified Data.Map as M
import Data.Time.Clock.POSIX (getPOSIXTime)
import Superfluid.Types (Timestamp, toWad)
import Superfluid.Testing.SimpleAccount
    (Address
    , SimpleAccount
    , createSimpleAccount)
import SimState

now :: IO Int
now =  do
    t <- getPOSIXTime
    return $ fromIntegral ((round t) :: Int)

createTestAccount :: Address -> Timestamp -> SimpleAccount
createTestAccount a t = createSimpleAccount a b t
    where b = fromInteger (toWad (100.0::Double))

mainLoop :: SimStateMonad ()
mainLoop = do
    -- t1
    t1 <- getCurrentTime
    liftIO $ print $ "# t1.a: initial state " ++ (show t1)
    printMainState
    liftIO $ print $ "# t1.b: create flows"
    updateFlow "alice" "bob" (toWad (0.0001::Double)) t1
    updateFlow "alice" "carol" (toWad (0.0002::Double)) t1
    printMainState
    t2 <- timeTravel $ (3600 * 24)
    liftIO $ print $ "t2: advanced one full day " ++ (show t2)
    printMainState

main :: IO ()
main = do
    t1 <- now
    _ <- runStateT mainLoop SimState
        { currentTime = t1
        , accounts = M.fromList
            [ ("alice", createTestAccount "alice" t1)
            , ("bob", createTestAccount "bob" t1)
            , ("carol", createTestAccount "carol" t1)
            ]
        , cfaAgreements = M.fromList []
        }
    print "# THE END"
