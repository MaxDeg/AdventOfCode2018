import           Test.Hspec

import           Day2                           ( calculateCheckSumOfBoxIds )

main :: IO ()
main = hspec $ do
  it "Number of words with 2 times same character"
    $          calculateCheckSumOfBoxIds testData
    `shouldBe` 12
 where
  testData =
    ["abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab"]
