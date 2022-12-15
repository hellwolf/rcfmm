import           Test.Hspec

import qualified Finance.Trading.Models.ReactiveConstantFunctionMarketMaker_prop

main :: IO ()
main = hspec $ do
    Finance.Trading.Models.ReactiveConstantFunctionMarketMaker_prop.tests
