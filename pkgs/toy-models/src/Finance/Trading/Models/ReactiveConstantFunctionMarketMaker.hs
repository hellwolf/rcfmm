module Finance.Trading.Models.ReactiveConstantFunctionMarketMaker where

import           Finance.Trading.Models.CoreTypes (Amount, FlowRate, PairedLiquidityPool, Timestamp)

-- | Instant swap the asset A for B at certain amount.
instantSwap :: PairedLiquidityPool -> Amount -> Timestamp -> (PairedLiquidityPool, Amount)
instantSwap = undefined

-- | Continuously swap asset A for B at a constant flow rate.
flowSwap :: PairedLiquidityPool -> FlowRate -> Timestamp -> (PairedLiquidityPool, FlowRate)
flowSwap = undefined

-- | Add liquidity amount of asset A.
addLiquidity :: PairedLiquidityPool -> Amount -> Timestamp -> (PairedLiquidityPool, Amount)
addLiquidity = undefined

-- | Remove liquidity amount of asset A.
removeLiquidity :: PairedLiquidityPool -> Amount -> Timestamp -> (PairedLiquidityPool, Amount)
removeLiquidity = undefined
