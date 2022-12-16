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
    , LiquidityPool (flowRate, liquidity)
    , PairedLiquidityPool (PairedLiquidityPool)
    , Timestamp (..)
    )

clp :: PairedLiquidityPool -> Amount
clp (PairedLiquidityPool _ pA pB) =
    let lA = liquidity pA
        lB = liquidity pB
    in lA * lB

-- | Settle the pool liquidity position since last settlement.
settle :: PairedLiquidityPool -> Timestamp -> PairedLiquidityPool
settle (PairedLiquidityPool t pA pB) t' =
    let tΔ = fromInteger . coerce $ t' - t
        rA = flowRate pA
        rB = flowRate pB
        lA = liquidity pA
        lB = liquidity pB
        lAΔ = rA * tΔ - (rA * tΔ + lA) * rB * tΔ / (rB * tΔ + lB)
        lBΔ = rB * tΔ - (rB * tΔ + lB) * rA * tΔ / (rA * tΔ + lA)
    in PairedLiquidityPool t'
       pA { liquidity = lA + lAΔ }
       pB { liquidity = lB + lBΔ }

-- | Change liquidity.
change_liquidity :: (Amount -> Amount -> Amount)
                 -> PairedLiquidityPool -> Amount -> Timestamp
                 -> (PairedLiquidityPool, Amount, Amount)
change_liquidity op p lAΔ t' =
    let (PairedLiquidityPool _ pA pB) = settle p t'
        lA = liquidity pA
        lB = liquidity pB
        lBΔ = lB * lAΔ / lA
    in ( PairedLiquidityPool t'
         pA { liquidity = lA `op` lAΔ }
         pB { liquidity = lB `op` lBΔ }
       , lAΔ
       , lBΔ
       )

addLiquidity = change_liquidity (+)
removeLiquidity = change_liquidity (-)

-- | Instant swap amount of asset A for B.
--
--   * returns - New state of the pool, and the actual amount of A & B swapped.
instantSwap :: PairedLiquidityPool -> Amount -> Timestamp
            -> (PairedLiquidityPool, Amount, Amount)
instantSwap p aΔ t' =
    let (PairedLiquidityPool _ pA pB) = settle p t'
        lA = liquidity pA
        lB = liquidity pB
        lC = lA * lB
        lB' = lC / (lA + aΔ)
        lA' = lC / lB' -- rounding error adjustment
    in ( PairedLiquidityPool t'
         pA { liquidity = lA' }
         pB { liquidity = lB' }
       , lA' - lA
       , lB - lB'
       )

-- | Continuously swap asset A for B at constant flow rates.
flowSwap :: PairedLiquidityPool -> (FlowRate, FlowRate) -> Timestamp
         -> PairedLiquidityPool
flowSwap p (rAΔ, rBΔ) t'=
    let (PairedLiquidityPool _ pA pB) = settle p t'
        rA = flowRate pA
        rB = flowRate pB
    in PairedLiquidityPool t'
       pA { flowRate = rA + rAΔ }
       pB { flowRate = rB + rBΔ }

-- | The current flow index value of B.
-- flowIndex ::
