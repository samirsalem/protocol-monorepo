-- module Superfluid.Validator.Main where

import           Superfluid.Validator.Demo
import           Superfluid.Validator.SimState (runSimMonad)

main :: IO ()
main = runSimMonad demo
