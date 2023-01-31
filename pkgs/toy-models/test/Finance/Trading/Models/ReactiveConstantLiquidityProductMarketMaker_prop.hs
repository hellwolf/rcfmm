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

pool_a_delta p p' = (settledLiquidity.liquidityPoolA) p' - (settledLiquidity.liquidityPoolA) p
pool_b_delta p p' = (settledLiquidity.liquidityPoolB) p' - (settledLiquidity.liquidityPoolB) p
fuzz_eq x y = coerce x ~= coerce y

add_remove_same_liquidity (NonZero m) (NonZero n) x =
    clp p0 `fuzz_eq` clp p2 &&
    pool_a_delta p0 p1 `fuzz_eq` x &&
    pool_b_delta p0 p1 `fuzz_eq` pool_b_delta p2 p1
    where p0 = initPairedLiquidityPool m n 0
          (p1, _) = addLiquidity p0 x 0
          (p2, _) = removeLiquidity p1 x 0

iswap_in_and_out (NonZero m) (NonZero n) x =
    clp p0 `fuzz_eq` clp p1 &&
    clp p1 `fuzz_eq` clp p2 &&
    pool_a_delta p0 p1 `fuzz_eq` x &&
    pool_a_delta p2 p1 `fuzz_eq` x &&
    pool_b_delta p0 p1 `fuzz_eq` pool_b_delta p2 p1
    where p0 = initPairedLiquidityPool m n 0
          (p1, _) = instantSwap p0 x 0
          (p2', _) = instantSwap (opPLP p1) (pool_b_delta p1 p0) 0
          p2 = opPLP p2'

single_fswap_clp (NonZero m) (NonZero n) r tΔ =
    clp p0 `fuzz_eq` clp p1 &&
    clp p1 `fuzz_eq` clp p2 &&
    (settledLiquidity.liquidityPoolA) p2 `fuzz_eq` ((settledLiquidity.liquidityPoolA) p0 + aΔ)
    where p0 = initPairedLiquidityPool m n 0
          (p1, _) = flowSwap p0 (r, 0) 0
          (p2, _) = settle p1 tΔ
          aΔ = r * (fromInteger.coerce) tΔ

single_fswap_plus_iswap_clp (NonZero m) (NonZero n) r x tΔ1 tΔ2 =
    clp p0 `fuzz_eq` clp p1 &&
    clp p1 `fuzz_eq` clp p2 &&
    clp p2 `fuzz_eq` clp p3 &&
    clp p3 `fuzz_eq` clp p4 &&
    (settledLiquidity.liquidityPoolA) p4 `fuzz_eq` ((settledLiquidity.liquidityPoolA) p0 + x + aΔ)
    where p0  = initPairedLiquidityPool m n 0
          (p1, _) = flowSwap p0 (r, 0) 0
          (p2, _) = settle p1 tΔ1
          (p3, _) = instantSwap p2 x tΔ1
          (p4, _) = settle p3 (tΔ1 + tΔ2)
          aΔ = r * (fromInteger.coerce) (tΔ1 + tΔ2)

tests = describe "Reactive CLPMM properties" $ do
    it "Add remove same liquidity" $ property add_remove_same_liquidity
    it "Instant swap in and out" $ property iswap_in_and_out
    it "Single flow swap" $ property single_fswap_clp
    it "Single flow swap plus instant swap" $ property single_fswap_plus_iswap_clp
