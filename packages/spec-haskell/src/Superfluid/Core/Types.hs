module Superfluid.Core.Types where

{-
# Common Type Classes
-}
class (Num num, Ord num, Show num) => Liquidity num where

type Timestamp = Int
