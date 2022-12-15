{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Finance.Trading.Models.CoreTypes where

newtype Timestamp = Timestamp Integer deriving (Eq, Ord, Num)

newtype NumType = NumType Double deriving (Eq, Ord, Num, Fractional, Real, RealFrac)

type Amount = NumType

type FlowRate = NumType

data LiquidityPool = LiquidityPool
  { liquidity :: Amount
  , flowRate  :: FlowRate
  }

data PairedLiquidityPool = PairedLiquidityPool
  { settledAt      :: Timestamp
  , liquidityPoolA :: LiquidityPool
  , liquidityPoolB :: LiquidityPool
  }
