{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Day1
import           Day2
import           Day3

main :: IO ()
main = do
  putStrLn "-------------------------------------------------------------------"
  putStrLn "Day 1 - Calibration"
  putStrLn "-------------------------------------------------------------------"
  f <- calibrate
  putStrLn $ "what is the resulting frequency? " <> show f
  freqReachedTwice <- firstFrequencyReachedTwice
  putStrLn
    $  "What is the first frequency your device reaches twice? "
    <> show freqReachedTwice

  putStrLn "-------------------------------------------------------------------"
  putStrLn "Day 2 - Calibration"
  putStrLn "-------------------------------------------------------------------"
  checksum <- calculateCheckSum
  putStrLn $ "What is the checksum for your list of box IDs? " <> show checksum
  commonLetters <- findCommonLettersInBoxIds
  putStrLn
    $  "What letters are common between the two correct box IDs? "
    <> show commonLetters

  putStrLn "-------------------------------------------------------------------"
  putStrLn "Day 3 - Calibration"
  putStrLn "-------------------------------------------------------------------"
  overlappingCount <- findOverlappingSquareInches
  putStrLn
    $  "How many square inches of fabric are within two or more claims? "
    <> show overlappingCount
  nonOverlappingClaims <- findNotOverlappingClaim
  putStrLn
    $  "What is the ID of the only claim that doesn't overlap? "
    <> show nonOverlappingClaims

  putStrLn "It's done for now"

