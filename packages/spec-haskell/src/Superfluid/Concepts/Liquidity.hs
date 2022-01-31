module Superfluid.Concepts.Liquidity where

import           Data.Default

class (Default num, Num num, Ord num, Show num) => Liquidity num where
