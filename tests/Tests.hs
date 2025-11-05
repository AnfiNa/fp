import Test.Hspec
import qualified One
import qualified Thirty

main :: IO ()
main = hspec $ do
  describe "One" $ do
    it "should pass basic test" $ do
      True `shouldBe` True

  describe "Thirty" $ do
    it "should calculate digit powers correctly" $ do
      Thirty.fift 123 `shouldBe` (1^5 + 2^5 + 3^5)