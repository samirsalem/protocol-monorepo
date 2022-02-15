module Superfluid.BaseTypes where

import           Data.Default

-- | Liquidity Type Class
--
-- Naming conventions:
--  * Type name: lq
--  * Type family name: LQ
class (Default lq, Num lq, Ord lq, Show lq) => Liquidity lq

-- | Timestamp Type Class
--
-- Naming conventions:
--  * Type name: ts
--  * Type family name: TS
class (Default ts, Integral ts, Ord ts, Show ts) => Timestamp ts

-- | Address Type Class
--
-- Naming conventions:
--  * Type name: ts
--  * Type family name: TS
class (Eq addr, Show addr) => Address addr
