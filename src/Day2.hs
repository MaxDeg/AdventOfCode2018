{-# LANGUAGE OverloadedStrings #-}

module Day2
  ( calculateCheckSumOfBoxIds
  , calculateCheckSum
  )
where

-- | Imports -------------------------------------------------------------------

import           Common                         ( readData )

import           Data.List                      ( filter
                                                , group
                                                , sort
                                                , length
                                                , head
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
