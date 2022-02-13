module Superfluid.Concepts.Liquidity where

import           Data.Default

-- Liquidity type class
--
-- Naming conventions:
--  * Type name: lq
--  * Type family name: LQ
class (Default lq, Num lq, Ord lq, Show lq) => Liquidity lq
