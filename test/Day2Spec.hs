module Day2Spec
  ( spec
  )
where

import           Test.Hspec

import           Day2                           ( calculateCheckSumOfBoxIds
                                                , findCommonLetters
                                                )

spec :: Spec
spec = do
  it "Number of words with 2 times same character"
    $          calculateCheckSumOfBoxIds testData
    `shouldBe` 12
  it "should return tuple of all words"
    $          findCommonLetters testData2
    `shouldBe` "fgij"
 where
  testData =
    ["abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab"]
  testData2 = ["abcde", "fghij", "klmno", "pqrst", "fguij", "axcye", "wvxyz"]
  tuples :: [(String, String)]
  tuples = [ (id1, id2) | id1 <- testData2, id2 <- testData2, id1 /= id2 ]
