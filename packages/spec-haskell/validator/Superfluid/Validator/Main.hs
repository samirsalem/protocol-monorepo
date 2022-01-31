-- module Superfluid.Validator.Main where

import           Control.Monad.State
import           Data.Default
import           Data.Time.Clock.POSIX             (getPOSIXTime)

import           Superfluid                        (Timestamp)

import qualified Superfluid.System.SuperfluidToken as SuperfluidToken

import           Superfluid.Instances.Simple       (SimpleAccount,
                                                    createSimpleAccount,
                                                    createSimpleAddress, toWad)

import           Superfluid.Validator.SimState


now :: IO Int
now =  do
    t <- getPOSIXTime
    return $ fromIntegral ((round t) :: Int)

createTestAccount :: String -> Timestamp -> SimpleAccount
createTestAccount a t = createSimpleAccount a b t
    where b = toWad (100.0 :: Double)

mainLoop :: SimStateMonad ()
mainLoop = do
    t0 <- liftIO $ now

    let t1 = t0
    let alice = createSimpleAddress "alice"
    let bob = createSimpleAddress "bob"
    let carol = createSimpleAddress "carol"

    liftIO $ putStrLn $ "# t1.a: create test accounts " ++ (show (t1 - t0))
    initSimState t1 (map (\a -> (a, createTestAccount (show a) t1)) [alice, bob, carol])
    printSimState

    liftIO $ putStrLn $ "# t1.b: create flows " ++ (show (t1 - t0))
    execTokenState $ SuperfluidToken.updateFlow alice bob (toWad (0.0001::Double)) t1
    execTokenState $ SuperfluidToken.updateFlow alice carol (toWad (0.0002::Double)) t1
    printSimState

    t2 <- timeTravel $ (3600 * 24)
    liftIO $ putStrLn $ "# t2: advanced one full day " ++ (show (t2 - t0))
    printSimState

main :: IO ()
main = evalStateT mainLoop def
