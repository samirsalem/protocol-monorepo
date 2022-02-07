module Superfluid.Concepts.Liquidity where

import           Data.Default

class (Default lq, Num lq, Ord lq, Show lq) => Liquidity lq where
