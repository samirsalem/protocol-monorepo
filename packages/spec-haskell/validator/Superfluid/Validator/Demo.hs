module Superfluid.Validator.Demo (demo) where

import           Control.Monad.IO.Class
import           Data.Maybe
import           Data.Time.Clock.POSIX              (getPOSIXTime)
import           GHC.Stack

import qualified Superfluid.Instances.Simple.System as SF
import           Superfluid.Instances.Simple.Types  (SimpleTimestamp, Wad, createSimpleAddress, toWad)

import           Superfluid.Validator.Simulation


now :: IO SimpleTimestamp
now =  do
    t <- getPOSIXTime
    return $ fromIntegral ((round t) :: SimpleTimestamp)

initBalance :: Wad
initBalance = toWad (100.0 :: Double)

demo :: HasCallStack => SimMonad ()
demo = do
    let token = "DAI"
    t0 <- liftIO $ now
    timeTravel $ t0
    liftIO $ putStrLn $ "# T0: create test accounts"
    let alice = fromJust $ createSimpleAddress "alice"
    let bob = fromJust $ createSimpleAddress "bob"
    let carol = fromJust $ createSimpleAddress "carol"
    createToken token [alice, bob, carol] initBalance
    runSimTokenOp token printTokenState

    let t1 = t0
    liftIO $ putStrLn $ "# T1: create flows" ++ (show (t1 - t0))
    runToken token $ SF.updateFlow alice bob (toWad (0.0001::Double))
    runToken token $ SF.updateFlow alice carol (toWad (0.0002::Double))
    runSimTokenOp token printTokenState

    timeTravel $ 3600 * 24
    t2 <- getCurrentTime
    liftIO $ putStrLn $ "# T2: advanced one full day " ++ (show (t2 - t0))
    runSimTokenOp token printTokenState
