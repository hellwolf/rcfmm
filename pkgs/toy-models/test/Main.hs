import           Test.Hspec

import qualified Finance.Trading.Models.ReactiveConstantLiquidityProductMarketMaker_prop
import qualified Money.Theory.SemanticMoney_prop

main :: IO ()
main = hspec $ do
    Money.Theory.SemanticMoney_prop.tests
    Finance.Trading.Models.ReactiveConstantLiquidityProductMarketMaker_prop.tests
