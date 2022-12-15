module Finance.Trading.Models.CoreTypes where

newtype Timestamp = Timestamp Integer

newtype Amount = Amount Integer

newtype FlowRate = FlowRate Integer

data LiquidityPool = LiquidityPool
  { liquidity :: Amount
  , flowRate  :: FlowRate
  }

data PairedLiquidityPool = PairedLiquidityPool
  { liquidityPoolA :: LiquidityPool
  , liquidityPoolB :: LiquidityPool
  }
