import Test.Hspec
import BreakingRepeatingKeyXor

main :: IO ()
main = hspec $ do
  describe "BreakingRepeatingKeyXor.hammingDistance" $ do
    it "computes the Hamming distance between two strings" $ do
      BreakingRepeatingKeyXor.hammingDistance "this is a test" "wokka wokka!!!" `shouldBe` 37
