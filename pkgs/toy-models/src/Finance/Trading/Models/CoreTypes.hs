{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia    #-}

module Finance.Trading.Models.CoreTypes where

import           Data.Default
import           GHC.Generics

-- | Timestamp type.
newtype Timestamp = Timestamp Integer deriving newtype (Eq, Ord, Num, Default, Show)

-- | Universal number type used in the models.
--
-- TODO being lazy and toyish for now, will check out fixed-precision library.
newtype NumType = NumType Double deriving newtype (Eq, Ord, Num, Fractional, Real, RealFrac, Default, Show)

type Amount = NumType

type FlowRate = NumType

data LiquidityPool = LiquidityPool
  { liquidity :: Amount
  , flowRate  :: FlowRate
  } deriving stock (Generic, Show)
deriving instance Default LiquidityPool

data PairedLiquidityPool = PairedLiquidityPool
  { settledAt      :: Timestamp
  , liquidityPoolA :: LiquidityPool
  , liquidityPoolB :: LiquidityPool
  } deriving stock (Generic, Show)
deriving instance Default PairedLiquidityPool

initPairedLiquidityPool :: Amount -> Amount -> Timestamp -> PairedLiquidityPool
initPairedLiquidityPool lA lB t =
    PairedLiquidityPool { settledAt = t
                        , liquidityPoolA = def { liquidity = lA }
                        , liquidityPoolB = def { liquidity = lB }
                        }

-- | Opposite of the paired liquidity pool.
opPLP :: PairedLiquidityPool -> PairedLiquidityPool
opPLP (PairedLiquidityPool t a b) = PairedLiquidityPool t b a
