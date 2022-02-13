module Superfluid.Concepts.Timestamp where

import           Data.Default

-- Timestamp type class
--
-- Naming conventions:
--  * Type name: ts
--  * Type family name: TS
class (Default ts, Integral ts, Ord ts, Show ts) => Timestamp ts
