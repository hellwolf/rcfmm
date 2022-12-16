{-# OPTIONS_GHC -Wno-missing-signatures -Wno-orphans #-}

module Finance.Trading.Models.ReactiveConstantLiquidityProductMarketMaker_prop (tests) where

import           Test.Hspec
import           Test.QuickCheck

import           Data.Coerce                                                        (coerce)
import           Math.Extras.Double                                                 ((~=))

import           Finance.Trading.Models.CoreTypes
import           Finance.Trading.Models.ReactiveConstantLiquidityProductMarketMaker

deriving instance Arbitrary Timestamp
deriving instance Arbitrary NumType

add_remove_same_liquidity (NonZero m) (NonZero n) x =
    coerce x1 ~= coerce x && coerce x2 ~= coerce x &&
    coerce y1 ~= coerce y2 &&
    coerce clp p ~= coerce clp p2
    where p = initPairedLiquidityPool m n 0
          (p1, x1, y1) = addLiquidity p x 0
          (p2, x2, y2) = removeLiquidity p1 x 0

iswap_in_and_out (NonZero m) (NonZero n) x =
    coerce clp p ~= coerce clp p2 && coerce clp p1 ~= coerce clp p2 &&
    coerce x1 ~= coerce x && coerce x2 ~= coerce x &&
    coerce y1 ~= coerce y2
    where p = initPairedLiquidityPool m n 0
          (p1, x1, y1) = instantSwap p x 0
          (p2, y2, x2) = instantSwap (opPLP p1) y1 0

single_fswap_clp (NonZero m) (NonZero n) r tΔ =
    coerce clp p ~= coerce clp p2 &&
    (coerce.liquidity.liquidityPoolA) p2 ~= coerce ((liquidity.liquidityPoolA) p + aΔ)
    where p  = initPairedLiquidityPool m n 0
          p1 = flowSwap p (r, 0) 0
          p2 = settle p1 tΔ
          aΔ = r * (fromInteger.coerce)(tΔ)

single_fswap_plus_iswap_clp (NonZero m) (NonZero n) x r tΔ1 tΔ2 =
    coerce clp p ~= coerce clp p4
    where p  = initPairedLiquidityPool m n 0
          p1 = flowSwap p (r, 0) 0
          p2 = settle p1 tΔ1
          (p3, x3, y3) = instantSwap p2 x tΔ1
          p4 = settle p3 tΔ2
          aΔ = r * (fromInteger.coerce)(tΔ1 + tΔ2)

tests = describe "Reactive CLPMM properties" $ do
    it "Add remove same liquidity" $ property add_remove_same_liquidity
    it "Instant swap in and out" $ property iswap_in_and_out
    it "Single flow swap" $ property single_fswap_clp
    it "Single flow swap plus instant swap" $ property single_fswap_plus_iswap_clp
