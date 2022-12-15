{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Finance.Trading.Models.CoreTypes where

-- | Timestamp type.
newtype Timestamp = Timestamp Integer deriving (Eq, Ord, Num)

-- | Universal number type used in the models.
--
-- TODO being lazy and toyish for now, will check out fixed-precision library.
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
