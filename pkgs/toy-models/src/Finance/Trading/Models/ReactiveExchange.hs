module Finance.Trading.Models.ReactiveExchange where

import           Finance.Trading.Models.CoreTypes

data SwapDir = A2B | B2A

class Monad m => ReactiveExchange m where
  -- | Instant swap the left-side asset of SwapDir for another at certain amount.
  instantSwap :: (SwapDir, Amount) -> m ()

  -- | Continuously swap left-side asset of SwapDir for another at a constant flow rate.
  flowSwap :: (SwapDir, FlowRate) -> m ()

  addLiquidity :: Amount -> m ()

  removeLiquidity :: Amount -> m ()
