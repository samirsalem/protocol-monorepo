-- module Superfluid.Validator.Main where

import           Superfluid.Validator.Demo
import           Superfluid.Validator.Simulation (runSimMonad)

main :: IO ()
main = runSimMonad demo
