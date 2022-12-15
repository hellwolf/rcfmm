module Finance.Trading.Models.ReactiveConstantFunctionMarketMaker where

import           Data.Coerce                      (coerce)

import           Finance.Trading.Models.CoreTypes
    ( Amount
    , FlowRate
    , LiquidityPool (flowRate, liquidity)
    , PairedLiquidityPool (PairedLiquidityPool, settledAt)
    , Timestamp (..)
    )

-- | Settle the pool liquidity position since last settlement.
settle :: PairedLiquidityPool -> Timestamp -> PairedLiquidityPool
settle (PairedLiquidityPool t pA pB) t' =
    PairedLiquidityPool t'
    pA { liquidity = lA' }
    pB { liquidity = lB' }
    where tΔ = fromInteger . coerce $ t' - t
          rA = flowRate pA
          rB = flowRate pB
          lA = liquidity pA
          lB = liquidity pB
          nA = rA * tΔ + lA
          nB = rB * tΔ + lB
          lB' = -(rB * tΔ + lB) * rA * tΔ / nA
          lA' = -(rA * tΔ + lA) * rB * tΔ / nB


change_liquidity :: (Amount -> Amount -> Amount)
                 -> PairedLiquidityPool -> Amount -> Timestamp -> (PairedLiquidityPool, Amount, Amount)
change_liquidity op p nA t' =
    ( PairedLiquidityPool t'
      pA { liquidity = lA `op` nA' }
      pB { liquidity = lB `op` nB' }
    , nA'
    , nB')
    where (PairedLiquidityPool _ pA pB) = settle p t'
          lA = liquidity pA
          lB = liquidity pB
          lC = lA * lB
          nB' = lC / nA
          nA' = lC / nB' -- rounding error adjustment

-- | Add liquidity amount of asset A.
addLiquidity :: PairedLiquidityPool -> Amount -> Timestamp -> (PairedLiquidityPool, Amount, Amount)
addLiquidity = change_liquidity (+)

-- | Remove liquidity amount of asset A.
removeLiquidity :: PairedLiquidityPool -> Amount -> Timestamp -> (PairedLiquidityPool, Amount, Amount)
removeLiquidity = change_liquidity (-)

-- | Instant swap amount of asset A for B.
--
--   * returns - New state of the pool, and the actual amount of A & B swapped.
instantSwap :: PairedLiquidityPool -> Amount -> Timestamp -> (PairedLiquidityPool, Amount, Amount)
instantSwap p nA t' =
    ( PairedLiquidityPool t'
      pA { liquidity = lA + nA' }
      pB { liquidity = lB - nB' }
    , nA'
    , nB')
    where (PairedLiquidityPool _ pA pB) = settle p t'
          lA = liquidity pA
          lB = liquidity pB
          lC = lA * lB
          nB' = lB - lC / (lA + nA)
          nA' = lC / nB'  -- rounding error adjustment

-- | Continuously swap assets at constant flow rates.
flowSwap :: PairedLiquidityPool -> (FlowRate, FlowRate) -> Timestamp -> PairedLiquidityPool
flowSwap p (rAΔ, rBΔ) t'=
    PairedLiquidityPool t'
    pA { flowRate = rA + rAΔ }
    pB { flowRate = rB + rBΔ }
    where (PairedLiquidityPool _ pA pB) = settle p t'
          rA = flowRate pA
          rB = flowRate pB
