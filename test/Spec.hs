import           Test.Hspec

import           Day2                           ( calculateCheckSumOfBoxIds )

main :: IO ()
main = hspec $ do
  it "Number of words with 2 times same character"
    $          calculateCheckSumOfBoxIds testData
    `shouldBe` 12
  it "should return tuple of all words" $ do 
    [] `shouldBe` []
 where
  testData =
    ["abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab"]
  testData2 = ["abcde", "fghij", "klmno", "pqrst", "fguij", "axcye", "wvxyz"]
  tuples = filter (uncurry (/=)) [ (id1, id2) | id1 <- testData2, id2 <- testData2 ]

