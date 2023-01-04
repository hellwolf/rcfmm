module Finance.Trading.Models.ReactiveConstantLiquidityProductMarketMaker
    ( clp
    , settle
    , addLiquidity
    , removeLiquidity
    , instantSwap
    , flowSwap
    ) where

import           Data.Coerce                      (coerce)

import           Finance.Trading.Models.CoreTypes
    ( Amount
    , FlowRate
    , LiquidityPool (..)
    , PairedLiquidityPool (PairedLiquidityPool)
    , Timestamp (..)
    )

-- | Constant liquidity product of the pool.
clp :: PairedLiquidityPool -> Amount
clp (PairedLiquidityPool _ pA pB) =
    let lA = settledLiquidity pA
        lB = settledLiquidity pB
    in lA * lB

type DistributionIndexes = (Amount, Amount)

-- | Settle the pool liquidity position since last settlement.
settle :: PairedLiquidityPool
       -> Timestamp
       -> (PairedLiquidityPool, DistributionIndexes)
settle (PairedLiquidityPool t pA pB) t' =
    let tΔ = fromInteger . coerce $ t' - t
        rA = flowRate pA
        rB = flowRate pB
        lA = settledLiquidity pA
        lB = settledLiquidity pB
        iAΔ = (rA * tΔ + lA) * rB * tΔ / (rB * tΔ + lB)
        iBΔ = (rB * tΔ + lB) * rA * tΔ / (rA * tΔ + lA)
        lAΔ = rA * tΔ - iAΔ
        lBΔ = rB * tΔ - iBΔ
    in ( PairedLiquidityPool t'
         pA { settledLiquidity = lA + lAΔ }
         pB { settledLiquidity = lB + lBΔ }
       , (iAΔ, iBΔ)
       )

-- | Change liquidity.
change_liquidity :: (Amount -> Amount -> Amount)
                 -> PairedLiquidityPool -> Amount
                 -> Timestamp
                 -> (PairedLiquidityPool, DistributionIndexes)
change_liquidity op p lAΔ t' =
    let (PairedLiquidityPool _ pA pB, dΔ) = settle p t'
        lA = settledLiquidity pA
        lB = settledLiquidity pB
        lBΔ = lB * lAΔ / lA
    in ( PairedLiquidityPool t'
         pA { settledLiquidity = lA `op` lAΔ }
         pB { settledLiquidity = lB `op` lBΔ }
       , dΔ
       )

addLiquidity = change_liquidity (+)
removeLiquidity = change_liquidity (-)

-- | Instant swap amount of asset A for B.
--
--   * returns - New state of the pool, and the actual amount of A & B swapped.
instantSwap :: PairedLiquidityPool -> Amount
            -> Timestamp
            -> (PairedLiquidityPool, DistributionIndexes)
instantSwap p aΔ t' =
    let (PairedLiquidityPool _ pA pB, dΔ) = settle p t'
        lA = settledLiquidity pA
        lB = settledLiquidity pB
        lC = lA * lB
        lB' = lC / (lA + aΔ)
        lA' = lC / lB' -- rounding error adjustment
    in ( PairedLiquidityPool t'
         pA { settledLiquidity = lA' }
         pB { settledLiquidity = lB' }
       , dΔ
       )

-- | Continuously swap asset A for B at constant flow rates.
flowSwap :: PairedLiquidityPool -> (FlowRate, FlowRate)
         -> Timestamp
         -> (PairedLiquidityPool, DistributionIndexes)
flowSwap p (rAΔ, rBΔ) t'=
    let (PairedLiquidityPool _ pA pB, dΔ) = settle p t'
        rA = flowRate pA
        rB = flowRate pB
    in ( PairedLiquidityPool t'
         pA { flowRate = rA + rAΔ }
         pB { flowRate = rB + rBΔ }
       , dΔ)
