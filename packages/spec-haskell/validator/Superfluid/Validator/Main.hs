-- module Superfluid.Validator.Main where

import           Data.Time.Clock.POSIX             (getPOSIXTime)

import           Superfluid.Instances.Simple.Types (SimpleTimestamp)

import           Superfluid.Validator.Demo
import           Superfluid.Validator.Simulation   (runSimMonad)

now :: IO SimpleTimestamp
now =  do
    t <- getPOSIXTime
    return $ fromIntegral ((round t) :: SimpleTimestamp)

main :: IO ()
main = do
    t <- now
    runSimMonad t demo
