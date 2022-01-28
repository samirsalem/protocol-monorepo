module Superfluid.Core.Types where

{-
# Common Type Classes
-}
class (Num num, Ord num, Show num) => Liquidity num where

-- FIXME make timestamp a type class
type Timestamp = Int
