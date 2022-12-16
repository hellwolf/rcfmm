import           Test.Hspec

import qualified Finance.Trading.Models.ReactiveConstantLiquidityProductMarketMaker_prop

main :: IO ()
main = hspec $ do
    Finance.Trading.Models.ReactiveConstantLiquidityProductMarketMaker_prop.tests
