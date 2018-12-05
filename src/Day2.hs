{-# LANGUAGE OverloadedStrings #-}

module Day2
  ( calculateCheckSumOfBoxIds
  , calculateCheckSum
  , findCommonLetters
  , findCommonLettersInBoxIds
  )
where

-- | Imports -------------------------------------------------------------------

import           Common                         ( readData )

import           Data.List                      ( filter
                                                , group
                                                , sort
                                                , length
                                                , head
                                                , intersect
                                                )



-- | Functions -----------------------------------------------------------------

readBoxIds :: IO [String]
readBoxIds = lines <$> readData "Day2.txt"

countLettersInWord :: String -> [(Char, Int)]
countLettersInWord = map (\ls -> (head ls, length ls)) . group . sort

calculateCheckSumOfBoxIds :: [String] -> Int
calculateCheckSumOfBoxIds boxIds = exactlyTwice boxIds * exactly3Times boxIds
 where
  countLetter :: ((Char, Int) -> Bool) -> [String] -> Int
  countLetter f =
    length . filter (not . null) . map (filter f . countLettersInWord)
  exactlyTwice :: [String] -> Int
  exactlyTwice  = countLetter $ (==) 2 . snd
  exactly3Times = countLetter $ (==) 3 . snd

calculateCheckSum :: IO Int
calculateCheckSum = calculateCheckSumOfBoxIds <$> readBoxIds

findCommonLettersInBoxIds :: IO String
findCommonLettersInBoxIds = findCommonLetters <$> readBoxIds

findCommonLetters :: [String] -> String
findCommonLetters boxIds = w1 `intersect` w2
 where
  makeTuples :: [String] -> [(String, String)]
  makeTuples boxIds = [ (id1, id2) | id1 <- boxIds, id2 <- boxIds, id1 /= id2 ]
  (w1, w2) =
    head $ filter (uncurry filterWordsWithOneCharDiff) $ makeTuples boxIds


findDifferentCharactersInWord :: String -> String -> [Char]
findDifferentCharactersInWord w1 w2 =
  fmap fst $ filter (uncurry (/=)) $ zip w1 w2

filterWordsWithOneCharDiff :: String -> String -> Bool
filterWordsWithOneCharDiff w1 w2 =
  length (findDifferentCharactersInWord w1 w2) == 1
