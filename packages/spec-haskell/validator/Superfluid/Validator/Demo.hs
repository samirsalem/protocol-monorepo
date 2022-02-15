module Superfluid.Validator.Demo (demo) where

import           Control.Monad.IO.Class
import           Data.Time.Clock.POSIX             (getPOSIXTime)

import qualified Superfluid.System                 as SF

import           Superfluid.Instances.Simple.Types
    ( SimpleAccount
    , SimpleAddress
    , SimpleTimestamp
    , createSimpleAccount
    , createSimpleAddress
    , toWad
    )

import           Superfluid.Validator.SimState


now :: IO SimpleTimestamp
now =  do
    t <- getPOSIXTime
    return $ fromIntegral ((round t) :: Integer)

createTestAccount :: SimpleAddress -> SimpleTimestamp -> SimpleAccount
createTestAccount a t = createSimpleAccount a b t
    where b = toWad (100.0 :: Double)

demo :: SimMonad ()
demo = do
    t0 <- liftIO $ now
    printSimState
    liftIO $ putStrLn $ "# T0: initial state"
    printSimState

    let t1 = t0
    let alice = createSimpleAddress "alice"
    let bob = createSimpleAddress "bob"
    let carol = createSimpleAddress "carol"
    liftIO $ putStrLn $ "# T1: create test accounts & flows" ++ (show (t1 - t0))
    initSimState t1 (map (\a -> (a, createTestAccount a t1)) [alice, bob, carol])
    SF.updateFlow alice bob (toWad (0.0001::Double)) t1
    SF.updateFlow alice carol (toWad (0.0002::Double)) t1
    printSimState

    t2 <- timeTravel $ 3600 * 24
    liftIO $ putStrLn $ "# T2: advanced one full day " ++ (show (t2 - t0))
    printSimState
