module Finance.Trading.Models.ReactiveConstantFunctionMarketMaker_prop (tests) where

import           Test.Hspec
import           Test.QuickCheck

import           Finance.Trading.Models.ReactiveConstantFunctionMarketMaker

always = True

tests = describe "ConstantFlowAgreement properties" $ do
    it "" $ property always
